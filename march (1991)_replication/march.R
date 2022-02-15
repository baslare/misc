ifelse(require(tidyverse),"",{install.packages("tidyverse")
  require(tidyverse)})
ifelse(require(gganimate),"",{install.packages("gganimate")
  require(gganimate)})
ifelse(require(modeest),"",{install.packages("modeest")
  require(modeest)})
ifelse(require(extrafont),"",{install.packages("extrafont")
  require(extrafont)}) # extrafont requires a first time setup, please refer to to its documentation. However, this is just a cosmetic adjustment that changes the display font.


set.seed(10000)

prob1 = c(rep(0.1,25),rep(0.1,25)) # individual belief changes

prob2 = 0.9 # organizational learning

prob3 = 0.0   # turnover probability
  
prob4 = 0.00   #reality change, 0 for no environmental turbulence



no_dimensions = 30 #m
no_individuals = 50 #n
time_periods = 100


reality <- ifelse(rbernoulli(n = no_dimensions) == 0, -1, 1) 

code <-  rep(0,no_dimensions)

ind_list  <- lapply(1:no_individuals, function(x) rdunif(n = no_dimensions,b = -1,a=1))

ind_all <- list(ind_list)
codes_all <- list(code)
reality_list <- list(reality)

i <- 1

realityChange <- 0 

while(i <= time_periods){
  
  code <- codes_all[[i]]
  beliefs <- lapply(ind_all[[i]], function(x){if(runif(1) < prob3){
    return(rdunif(n = no_dimensions,b = -1,a=1))
  }else{
    return(x)
    
  }})
  
  
  reality  <- sapply(reality_list[[i]],function(x){
    if(runif(1) < prob4){
      realityChange <<- realityChange + 1 # to count the total number of times any dimension of reality changes, just wanted to make sure it works as intended
      return(ifelse(x==1,-1,1))}
    else{x}})
  
  
  
  code_score  <- sum(code == reality)
  
  ind_scores <- sapply(beliefs, function(x) sum(x == reality))
  
  
  
  superior_index <- which(ind_scores > code_score)
  
  
  
  if(!identical(integer(),superior_index)){
    
    message(paste(superior_index))
    superior_group <- beliefs[superior_index]
    
    diff_no <- sapply(superior_group, function(x) x == code) #mxn matrix
    
    no_agree <- apply(diff_no,MARGIN = 1,sum)
    
    k <- (length(superior_group) - no_agree) - no_agree 
    
    k <- ifelse(k > 0, k, 0)
    
    superior_group_matrix <- do.call("rbind",superior_group)
    
    dominant_beliefs <- apply(superior_group_matrix,MARGIN = 2,function(x) modeest::mlv(x,method="mfv")[1])
    
    new_code <- mapply(function(x,y,z){
      if(x!=z){
        if(runif(1) > (1-prob2)**y){
          return(z)
        }else{
          return(x)
        }
        
      }else{
        return(x)
        
      }},x=code,y=k,z=dominant_beliefs,SIMPLIFY = T)
  }
  

  
  new_beliefs <- Map(function(x,y){
    
    mapply(function(a,b) if(a!=b & b != 0){
      if(runif(1) < y){
        return(b)
      }else{
        return(a)
      }
      
    }else{
      return(a)
    }, a=x, b=code )
    
  },x=beliefs,y=prob1)
  
  ind_all[[i + 1]] <- new_beliefs
  codes_all[[i + 1]] <- new_code
  reality_list[[i+1]] <- reality
  
  if(i == 20){
    print(sum(reality == new_code)/no_dimensions)
  }
  
  
  i <- i + 1
  
}


inds  <- lapply(ind_all, function(x) data.frame(do.call("rbind",x)))
inds  <- Map(function(x,y) x %>% mutate(ite_no=y,ind_no = 1:no_individuals),x=inds,y=0:100)

codes_df <- data.frame(do.call("rbind",codes_all))
codes_df <- codes_df %>% mutate(ite_no=0:100,ind_no = -1)


inds_df <- inds %>% bind_rows()
reality_df <- data.frame(do.call("rbind",reality_list)) %>% mutate(ite_no=0:100,ind_no = -3)

master_df <- rbind(inds_df,codes_df,reality_df)

master_df$knowledge <- apply(master_df %>% select(X1:X30),MARGIN = 1, function(x) sum(x == reality)/no_dimensions) ##for different number of dimensions, X1:X30 has to change

individual_knowledge <- master_df %>% filter(ind_no > 0)%>% group_by(ite_no) %>% summarise(knowledge=mean(knowledge))

individual_knowledge$knowledge[101]

sum(reality == new_code)/30

master_df_long <- master_df %>% pivot_longer(cols = X1:X30)








anim <- ggplot(master_df_long) + 
  geom_tile(aes(x=ind_no,y=name,fill=as.factor(value))) + 
  transition_time(ite_no) + 
  ease_aes("linear") +
  xlab("individuals") +
  ylab("dimensions") + labs(title = "period: {frame_time}") +
  scale_fill_discrete("Beliefs") +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 90,size=10),
        plot.title = element_text(hjust = 0.5,size=15),
        text = element_text(family="Noto Sans"))+
  scale_x_continuous(breaks=c(-3.5,-1.5,9.5,19.5,29.5,39.5,49.5),labels=c("reality","code","10","20","30","40","50")) 


#to animate without saving anything:
#animate(anim, nframes = 100, fps = 10,width=800,height=600)


to_save <- animate(anim, nframes = 100, fps = 10,width=800,height=600)

anim_save("insert_filename.gif",to_save)
