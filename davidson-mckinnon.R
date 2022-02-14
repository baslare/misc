require(parallel)
require(tidyverse)
require(ggsci)
require(ggpubr)
require(extrafont)
require(gridExtra)
require(grid)



#change the number of replications
#takes a while to run! try it for lower sample size and replication numbers!
rep_max <- 100000

cl <- makeCluster(detectCores())

clusterEvalQ(cl, require(parallel))

#clusterEvalQ(cl,rep_max <- 1000)

rep_level <- function(x,B_=399, sample_size, coeff_a, coeff_ro){
  
  #creation of w1, scaled random vector with a norm of 1, detailed in the beginning of section 4
  w_inst <- rnorm(sample_size)
  w_inst_sc <- w_inst/sqrt(sum(w_inst)^2)
  
  #DGP
  u_1 <- rnorm(sample_size)
  v <- rnorm(sample_size)
  
  y_1 <- sqrt(sample_size)*abs(w_inst_sc)*u_1
  
  u_2 <- coeff_ro*sqrt(sample_size)*abs(w_inst_sc)*u_1 + sqrt(1- coeff_ro^2)*v
  
  y_2 <- coeff_a*w_inst_sc + u_2
  
  
  #a vector of constants
  Z_ <- rep(1,sample_size)
  
  #Preparing 2 SLS : 
  #1st stage explanatory variables in matrix form. W1, 10 random columms, and the column Z for the intercept
  W_ <- cbind(w_inst_sc,matrix(ncol = 12-2,data = matrix(rnorm(sample_size*10), ncol = 12-2)),Z_)
  
  #Projection matrices for W and Z
  #could use crossprod() for matrix multiplications, slightly faster, better for matrices
  P_w <- W_%*%solve(t(W_)%*%W_)%*%t(W_)
  P_z <- Z_%*%solve(t(Z_)%*%Z_)%*%t(Z_)
  
  # 1st stage OLS, read bar as tilde
  mod2 <- solve(t(W_)%*%W_)%*%t(W_)%*%y_2 #1st stage coefficients lx1 vector
  u2_bar <- (diag(sample_size) - P_w)%*%y_2 # 1st stage residuals nx1 vector
  
  #2nd stage explanatory variables in matrix form. Z_ for the intercept and the fitted values from 1st stage OLS, computed by P_w*y_2
  
  mod2_fits <- P_w%*%y_2
  
  R_ <- cbind(Z_,mod2_fits)
  
  #2nd stage OLS
  mod1 <- solve(t(R_)%*%R_)%*%t(R_)%*%y_1 
  
  #2nd stage residuals, read bar as tilde
  u1_bar <- y_1 - R_%*%mod1
  
  
  # the expression required for Th
  th_matrix <- (P_w%*%y_2 - P_z%*%y_2)
  
  #Ar and Th, hypothesis tested : ß0 = 0
  ar_real_sub <- y_1 - mod1[2]*mod2_fits
  ar_stat_real <- ((sample_size - 12)*t(u1_bar)%*%(P_w - P_z)%*%u1_bar)/((12-1)*t(u1_bar)%*%(diag(sample_size)-P_w)%*%u1_bar)
  th_statistic_real <- (mod1[2] - 0)/(sqrt(sum((u1_bar^2)*(th_matrix^2)))/(sqrt(sum(th_matrix^2))^2))
  
  #going to the bootstrap level
  results <- sapply(1:B_, bootstrap_level, sample_size=sample_size,u1_bar=u1_bar,u2_bar=u2_bar,P_w=P_w,P_z=P_z,W_=W_,Z_=Z_,mod1=mod1,mod2=mod2,mod2_fits=mod2_fits)
  

  results_ar <- results[1,]
  results_th <- results[2,]
  
  #construction of the P values, different for AR and Th due to their some properties
  
  Pval_ar <- sum(results_ar > ar_stat_real[1,1])/B_
  Pval_th <- 2*min(sum(results_th > th_statistic_real)/B_,sum(results_th <= th_statistic_real)/B_)
  
  
  
  return(c(P_AR=Pval_ar,P_Th=Pval_th))  
  
  
}

clusterEvalQ(cl, bootstrap_level <- function(x,sample_size,u1_bar,u2_bar,P_w,P_z,W_,Z_,mod1,mod2,mod2_fits){
  
  
  #WRE Bootstrap procedure
  v_star_rademacher <- ifelse(runif(sample_size) < 1/2,1,-1)
  
  
  u1_star <- sqrt(sample_size/(sample_size-1))*u1_bar*v_star_rademacher
  u2_star <- sqrt(sample_size/(sample_size-12))*u2_bar*v_star_rademacher
  
  #as per (14) and (16)
  y1_star <- u1_star
  y2_star <- mod2_fits + u2_star
  
  y2_star_fits <- P_w%*%y2_star
  
  
  #2SLS using bootstrap sample
  mod2_bootstrap <- solve(t(W_)%*%W_)%*%t(W_)%*%y2_star 
  
  R_star <- cbind(Z_,y2_star_fits)
  mod1_bootstrap <- solve(t(R_star)%*%R_star)%*%t(R_star)%*%y1_star
  
  
  #residuals from bootstrap 2sls
  u1_bootstrap <- y1_star - R_star%*%mod1_bootstrap
  u2_bootstrap <- (diag(sample_size) - P_w)%*%y2_star
  
  
  # ar_sub : to construct 23a in the paper
  ar_sub <- y1_star - mod1_bootstrap[2]*y2_star_fits
  th_matrix_star <- (P_w - P_z)%*%y2_star
  
  
  #Calculation of AR* and Th*
  ar_statistic <- ((sample_size - 12)*t(ar_sub)%*%(P_w - P_z)%*%ar_sub)/((12-1)*t(ar_sub)%*%(diag(sample_size)-P_w)%*%ar_sub)
  th_statistic <- (mod1_bootstrap[2] - 0)/(sqrt(sum((u1_bootstrap^2)*(th_matrix_star^2)))/(sqrt(sum(th_matrix_star^2))^2))
  
  return(c(ar_statistic,th_statistic))
  
})


#parallelized, parameters can be changed here
t0 <- Sys.time()
res_1 <- parSapply(cl,1:10000,rep_level,B_=399, sample_size=25,coeff_a = 2, coeff_ro = 0.1)
Sys.time() - t0

stopCluster(cl)


Pvalues_AR <- res_1[1,]
Pvalues_Th <- res_1[2,]

#Rejection Rates
sum(Pvalues_AR < 0.05)/length(Pvalues_AR)
sum(Pvalues_Th < 0.05)/length(Pvalues_Th)





###### plotting #######


sim_res <- readxl::read_excel("HW2_Q2_results.xlsx") # should be in the working directory

sim_res$exp_comb <- paste("a=",sim_res$a,"- ro=",sim_res$ro)

sim_res_long <- sim_res %>% pivot_longer(cols = AR:th)

sim_res_long$value <- as.numeric(sim_res_long$value)
sim_res_long$n <- as.numeric(sim_res_long$n)


p1 <- ggplot(aes(x=n,y=value,group=name,color=name),data = sim_res_long[sim_res_long$exp_comb == "a= 2 - ro= 0.9",]) +
  geom_line() + 
  geom_point()+ 
  geom_hline(aes(yintercept=0.05),linetype="dashed",size=0.1,color="purple") +
  theme_bw() +
  theme(strip.background = element_rect(fill="#ffab91"),plot.title=element_text(hjust = 0.5),text=element_text(family = "Noto Sans")) +
  scale_color_aaas(name="Statistic",labels=c("AR","T_h")) +
  labs(x="Sample Size",y="Rejection Frequency") +
  ggtitle("a= 2 - rho= 0.9") +
  scale_x_continuous(breaks = c(25,50,100,200,500))

p2 <- ggplot(aes(x=n,y=value,group=name,color=name),data = sim_res_long[sim_res_long$exp_comb == "a= 2 - ro= 0.1",]) +
  geom_line() + 
  geom_point()+ 
  geom_hline(aes(yintercept=0.05),linetype="dashed",size=0.1,color="purple") +
  theme_bw() +
  theme(strip.background = element_rect(fill="#ffab91"),plot.title=element_text(hjust = 0.5),text=element_text(family = "Noto Sans")) +
  scale_color_aaas(name="Statistic",labels=c("AR","T_h")) +
  labs(x="Sample Size",y="Rejection Frequency") +
  ggtitle("a= 2 - rho= 0.1") +
  scale_x_continuous(breaks = c(25,50,100,200,500))

p3 <- ggplot(aes(x=n,y=value,group=name,color=name),data = sim_res_long[sim_res_long$exp_comb == "a= 8 - ro= 0.9",]) +
  geom_line() + 
  geom_point()+ 
  geom_hline(aes(yintercept=0.05),linetype="dashed",size=0.1,color="purple") +
  theme_bw() +
  theme(strip.background = element_rect(fill="#ffab91"),plot.title=element_text(hjust = 0.5),text=element_text(family = "Noto Sans")) +
  scale_color_aaas(name="Statistic",labels=c("AR","T_h")) +
  labs(x="Sample Size",y="Rejection Frequency") +
  ggtitle("a= 8 - rho= 0.9") +
  scale_x_continuous(breaks = c(25,50,100,200,500))

p4 <- ggplot(aes(x=n,y=value,group=name,color=name),data = sim_res_long[sim_res_long$exp_comb == "a= 8 - ro= 0.1",]) +
  geom_line() + 
  geom_point()+ 
  geom_hline(aes(yintercept=0.05),linetype="dashed",size=0.1,color="purple") +
  theme_bw() +
  theme(strip.background = element_rect(fill="#ffab91"),plot.title=element_text(hjust = 0.5),text=element_text(family = "Noto Sans")) +
  scale_color_aaas(name="Statistic",labels=c("AR","T_h")) +
  labs(x="Sample Size",y="Rejection Frequency") +
  ggtitle("a= 8 - rho= 0.1") +
  scale_x_continuous(breaks = c(25,50,100,200,500))


grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}

grid_arrange_shared_legend(p1,p2,p3,p4,nrow=2)


ggsave('hw2_q2.jpeg',plot = grid_arrange_shared_legend(p1,p2,p3,p4,nrow=2),height = 16,width = 20,dpi = 300,units = "cm" )
