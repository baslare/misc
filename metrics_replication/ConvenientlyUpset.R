setwd("conveniently_upset/")


####Install packages####
if(!require("haven")) install.packages("haven"); library("haven")
if(!require("data.table")) install.packages("data.table"); library("data.table")
#if(!require("sqldf")) install.packages("sqldf"); library("sqldf")
if(!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
#if(!require("dplyr")) install.packages("dplyr"); library("dplyr")
if(!require("tidyverse")) install.packages("tidyverse"); library("tidyverse")
if(!require("stargazer")) install.packages("stargazer"); library("stargazer")
if(!require("ivreg")) install.packages("ivreg"); library("ivreg")


##Import datasets
basic_game       = read_dta("conveniently_upset/basic_game.dta")
modified_game    = read_dta("conveniently_upset/modified_game.dta")
#conditional_game = read_dta("conditional_game.dta")
forced_allocator = read_dta("conveniently_upset/forced_allocator.dta")
forced_seller    = read_dta("conveniently_upset/forced_seller.dta")

#Replace NAs in modified_game with 0
#modified_game[is.na(modified_game)] <- 1
#summary(modified_game)

####Figures####

#par(mfrow=c(2,1))  #Set the margin so we have enough space for legend

##Figure 1a: 
fig1a <- basic_game %>%
  group_by(treat, tokenstaken) %>%
  summarise(freq = length(tokenstaken)) %>%
  arrange(treat, tokenstaken)

plot1a <- ggplot(fig1a, aes(x = factor(tokenstaken), y = freq, fill = factor(treat))) + 
  geom_bar(position = "dodge", stat="identity") +
  theme_bw() + 
  scale_fill_brewer(labels = c("Able=2", "Able=8")) +
  labs(x = "", y = "Frequency", 
       title = "Panel A. Tokens taken by the allocator", 
       fill = "") +
  scale_x_discrete(drop=FALSE)


##Figure 1b: 
fig1b <- basic_game %>%
  group_by(treat, perc_corrupt) %>%
  summarise(freq = length(perc_corrupt)) %>%
  arrange(treat, perc_corrupt)

#create the labels
plot1b_labels <- sapply(0:9, function(x) paste0(10*x,"%-",10*(x+1),"%"))

plot1b <- ggplot(fig1b, aes(x = factor(perc_corrupt), y = freq, fill = factor(treat))) + 
  geom_bar(position = "dodge", stat="identity") +
  theme_bw() + 
  scale_fill_brewer(labels = c("Able=2", "Able=8")) +
  labs(x = "", y = "Frequency", 
       title = "Panel B. Allocator's belief about the seller (%-Corrupt)", 
       fill = "") +
  scale_x_discrete(drop=FALSE,labels=plot1b_labels)+
  theme(axis.text.x = element_text(angle = 45,vjust = 0.5))


##display 1a and 1b together

gridExtra::grid.arrange(plot1a,plot1b,nrow=1)



##Figure 2a: 
fig2a <- modified_game %>%
  group_by(treat, tokenstaken) %>%
  summarise(freq = length(tokenstaken)) %>%
  arrange(treat, tokenstaken)

plot2a <- ggplot(fig2a, aes(x = factor(tokenstaken), y = freq, fill = factor(treat))) + 
  geom_bar(position = "dodge", stat="identity") +
  theme_bw() + 
  scale_fill_brewer(labels = c("Able=2", "Able=8")) +
  labs(x = "", y = "Frequency", 
       title = "Panel A. Tokens taken by the allocator", 
       fill = "") +
  scale_x_discrete(drop=FALSE)


##Figure 2b: 
fig2b <- modified_game %>%
  group_by(treat, perc_corrupt) %>%
  summarise(freq = length(perc_corrupt)) %>%
  arrange(treat, perc_corrupt)

plot2b <- ggplot(fig2b, aes(x = factor(perc_corrupt), y = freq, fill = factor(treat))) + 
  geom_bar(position = "dodge", stat="identity") +
  theme_bw() + 
  scale_fill_brewer(labels = c("Able=2", "Able=8")) +
  labs(x = "", y = "Frequency", 
       title = "Panel B. Allocator's belief about the seller (%-Corrupt)", 
       fill = "") +
  scale_x_discrete(drop=FALSE,labels=plot1b_labels) +
  theme(axis.text.x = element_text(angle = 45,vjust = 0.5))


gridExtra::grid.arrange(plot2a,plot2b,nrow=1)

####Tables####

####Table 2 - Summary statistics in the four samples (for allocators)####
tb2col1 <- basic_game %>% 
  summarise(across(everything(), .f = list(mean = mean, sd = sd), na.rm = TRUE)) %>%
  gather(key=var, value=value) %>% 
  extract(col="var", into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>% 
  mutate(across(where(is.numeric), round, 2)) %>% 
  mutate(sd  = paste0("(", sd, ")")) %>%
  unite("stat", mean:sd, sep= " ") %>%
  arrange(variable) 

tb2col2 <- modified_game %>% 
  summarise(across(everything(), .f = list(mean = mean, sd = sd), na.rm = TRUE)) %>%
  gather(key=var, value=value) %>% 
  extract(col="var", into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>% 
  mutate(across(where(is.numeric), round, 2)) %>% 
  mutate(sd  = paste0("(", sd, ")")) %>%
  unite("stat", mean:sd, sep= " ") %>%
  arrange(variable) 

tb2col3 <- forced_seller %>% 
  summarise(across(everything(), .f = list(mean = mean, sd = sd), na.rm = TRUE)) %>%
  gather(key=var, value=value) %>% 
  extract(col="var", into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>% 
  mutate(across(where(is.numeric), round, 2)) %>% 
  mutate(sd  = paste0("(", sd, ")")) %>%
  unite("stat", mean:sd, sep= " ") %>%
  arrange(variable) 

tb2col4 <- forced_allocator %>% 
  summarise(across(everything(), .f = list(mean = mean, sd = sd), na.rm = TRUE)) %>%
  gather(key=var, value=value) %>% 
  extract(col="var", into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>% 
  mutate(across(where(is.numeric), round, 2)) %>% 
  mutate(sd  = paste0("(", sd, ")")) %>%
  unite("stat", mean:sd, sep= " ") %>%
  arrange(variable) 

tb2 = left_join(tb2col1, tb2col2, by='variable') %>%
        left_join(., tb2col3, by='variable') %>%
        left_join(., tb2col4, by='variable') %>% 
     mutate(across(c(variable), ~ recode(., age = "Age",
                                            female = "Female",
                                            is_corrupt = "Is_Corrupt",
                                            class = "Socialeconomic class", 
                                            perc_corrupt = "%_Corrupt", 
                                            tokenstake = "Tokens taken",
                                            treat = "Able=8"))) %>% 
      rename("Basic game" = stat.x,
             "Modified game" = stat.y,
             "Forced seller" = stat.x.x,
             "Forced allocator" = stat.y.y
            )


tb2 <- tb2 %>% mutate(`Forced seller` = replace_na(`Forced seller`,""),
                      `Forced allocator` = replace_na(`Forced allocator`,"")
                      )
colnames(tb2)[1] <- ""
kableExtra::kable(tb2,format="latex",booktabs=T)  %>% 
  add_header_above(header = c(" ","First subject pool","Second subject pool"=3)) %>% 
  kable_styling(font_size = 8)




####Table 3 - Choices, Beliefs, and Characteristics in the Four Games, by Treatment Group (lack: p-values)####
#Panel A - Basic Game
tb3a2 = subset(basic_game, treat == 0)  %>%
  summarise(across(everything(), .f = list(mean = mean, sd = sd), na.rm = TRUE)) %>%
  gather(key=var, value=value) %>% 
  extract(col="var", into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>% 
  mutate(across(where(is.numeric), round, 2))

tb3a8 = subset(basic_game, treat == 1)  %>%
  summarise(across(everything(), .f = list(mean = mean, sd = sd), na.rm = TRUE)) %>%
  gather(key=var, value=value) %>% 
  extract(col="var", into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>% 
  mutate(across(where(is.numeric), round, 2))

tb3a = left_join(tb3a2, tb3a8, by='variable')


t_test_results <- Map(function(x,y) t.test(x,y),
                      x=basic_game %>% filter(treat == 1) %>% select(-treat),
                      y=basic_game %>% filter(treat == 0)%>% select(-treat))
vars <- names(t_test_results)
t_test_results <- sapply(t_test_results, function(x) x["p.value"])
t_test_results <- t_test_results %>% as.numeric()
t_test_results <- data.frame(variable=vars,p_value=t_test_results)


tb3a <- left_join(tb3a, t_test_results, by="variable")

tb3a <- tb3a %>% 
  mutate(across(c(variable), ~ recode(., age = "Age",
                                      female = "Female",
                                      is_corrupt = "Is_Corrupt",
                                      class = "Socialeconomic class", 
                                      perc_corrupt = "%_Corrupt", 
                                      tokenstaken = "Tokens taken"))) %>% 
  rename("able2_mean" = mean.x,
         "able2_sd" = sd.x,
         "able8_mean" = mean.y,
         "able8_sd" = sd.y
  )




tb3a <- tb3a %>% mutate(able2_sd  = sprintf("(%.2f)",able2_sd),
                        able8_sd  = sprintf("(%.2f)",able8_sd),
                        p_value = format(round(p_value,digits = 2),digits=2))
tb3a = tb3a[-7, ]



kableExtra::kable(tb3a, format="latex",booktabs=T)  %>% 
  add_header_above(header = c(" ","Able=2"=2,"Able=8"=2)) %>% 
  add_header_above(header = c(" ","Panel A. Basic game"=5)) %>% 
  add_header_above(header = c(" ","First subject pool"=5))  %>%                 
  kable_styling(font_size = 8)



#Panel B - Modified Game
tb3b2 = subset(modified_game, treat == 0)  %>%
  summarise(across(everything(), .f = list(mean = mean, sd = sd), na.rm = TRUE)) %>%
  gather(key=var, value=value) %>% 
  extract(col="var", into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>% 
  mutate(across(where(is.numeric), round, 2))

tb3b8 = subset(modified_game, treat == 1)  %>%
  summarise(across(everything(), .f = list(mean = mean, sd = sd), na.rm = TRUE)) %>%
  gather(key=var, value=value) %>% 
  extract(col="var", into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>% 
  mutate(across(where(is.numeric), round, 2))


tb3b = left_join(tb3b2, tb3b8, by='variable')

#compute t test results
t_test_results <- Map(function(x,y) t.test(x,y),
                      x=modified_game %>% filter(treat == 1) %>% select(-treat),
                      y=modified_game %>% filter(treat == 0)%>% select(-treat))
vars <- names(t_test_results)
t_test_results <- sapply(t_test_results, function(x) x["p.value"])
t_test_results <- t_test_results %>% as.numeric()
t_test_results <- data.frame(variable=vars,p_value=t_test_results)

#merge results to mean-sd
tb3b <- left_join(tb3b, t_test_results, by="variable")

#rename etc
tb3b <- tb3b %>% 
  mutate(across(c(variable), ~ recode(., age = "Age",
                                      female = "Female",
                                      is_corrupt = "Is_Corrupt",
                                      class = "Socialeconomic class", 
                                      perc_corrupt = "%_Corrupt", 
                                      tokenstaken = "Tokens taken"))) %>% 
  rename("able2_mean" = mean.x,
         "able2_sd" = sd.x,
         "able8_mean" = mean.y,
         "able8_sd" = sd.y
  )



#formatting
tb3b <- tb3b %>% mutate(able2_sd  = sprintf("(%.2f)",able2_sd),
                        able8_sd  = sprintf("(%.2f)",able8_sd),
                        p_value = format(round(p_value,digits = 2),digits=2))
tb3b = tb3b[-7, ]


#voila
kableExtra::kable(tb3b, format="latex",booktabs=T)  %>% 
  add_header_above(header = c(" ","Able=2"=2,"Able=8"=2)) %>% 
  add_header_above(header = c(" ","Panel B. Modified game"=5)) %>% 
  add_header_above(header = c(" ","Second subject pool"=5))  %>%                 
  kable_styling(font_size = 8)



###### Panel C - Forced seller ######
tb3c2 = subset(forced_seller, treat == 0)  %>%
  summarise(across(everything(), .f = list(mean = mean, sd = sd), na.rm = TRUE)) %>%
  gather(key=var, value=value) %>% 
  extract(col="var", into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>% 
  mutate(across(where(is.numeric), round, 2))

tb3c8 = subset(forced_seller, treat == 1)  %>%
  summarise(across(everything(), .f = list(mean = mean, sd = sd), na.rm = TRUE)) %>%
  gather(key=var, value=value) %>% 
  extract(col="var", into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>% 
  mutate(across(where(is.numeric), round, 2))

tb3c_a = subset(forced_seller, seller_B == 0)  %>%
  summarise(across(everything(), .f = list(mean = mean, sd = sd), na.rm = TRUE)) %>%
  gather(key=var, value=value) %>% 
  extract(col="var", into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>% 
  mutate(across(where(is.numeric), round, 2))

tb3c_b = subset(forced_seller, seller_B == 1)  %>%
  summarise(across(everything(), .f = list(mean = mean, sd = sd), na.rm = TRUE)) %>%
  gather(key=var, value=value) %>% 
  extract(col="var", into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>% 
  mutate(across(where(is.numeric), round, 2))

tb3c_p1 = left_join(tb3c2, tb3c8, by='variable')


t_test_results_3c_1 <- Map(function(x,y) t.test(x,y),
                      x=forced_seller %>% filter(treat == 1) %>% select(-treat),
                      y=forced_seller %>% filter(treat == 0)%>% select(-treat))

vars <- names(t_test_results_3c_1)
t_test_results_3c_1<- sapply(t_test_results_3c_1, function(x) x["p.value"])
t_test_results_3c_1 <- t_test_results_3c_1 %>% as.numeric()
t_test_results_3c_1 <- data.frame(variable=vars,p_value=t_test_results_3c_1)

#merge results to mean-sd
tb3c_p1 <- left_join(tb3c_p1, t_test_results_3c_1, by="variable")


### seller etc

tb3c_p2 = left_join(tb3c_a, tb3c_b, by='variable')


t_test_results_3c_2<- Map(function(x,y) t.test(x,y),
                           x=forced_seller %>% filter(seller_B != 1) %>% select(-seller_B ),
                           y=forced_seller %>% filter(seller_B != 0)%>% select(-seller_B ))

vars <- names(t_test_results_3c_2)

t_test_results_3c_2 <- sapply(t_test_results_3c_2, function(x) x["p.value"])
t_test_results_3c_2 <- t_test_results_3c_2 %>% as.numeric()
t_test_results_3c_2 <- data.frame(variable=vars,p_value=t_test_results_3c_2)

#merge results to mean-sd
tb3c_p2 <- left_join(tb3c_p2, t_test_results_3c_2, by="variable")


tb3c <- left_join(tb3c_p1,tb3c_p2,by="variable") 


tb3c = tb3c[-c(4, 6), ] %>%  mutate(across(c(variable), ~ recode(., age = "Age",
                                                                 female = "Female",
                                                                 class = "Socialeconomic class", 
                                                                 tokenstaken = "Tokens taken")))





tb3c = tb3c %>%  rename("able2_mean" = mean.x.x,
            "able2_sd" = sd.x.x,
            "able8_m" = mean.y.x,
            "able8_sd" = sd.y.x,
            "SellA_m" = mean.x.y,
            "SellA_sd" = sd.x.y,
            "SellB_m" = mean.y.y,
            "SellB_sd" = sd.y.y,
            "p_able" = p_value.x,
            "p_sell" = p_value.y
)

tb3c <- tb3c %>% mutate(able2_sd  = sprintf("(%.2f)",able2_sd),
                        able8_sd  = sprintf("(%.2f)",able8_sd),
                        SellA_sd  = sprintf("(%.2f)",SellA_sd),
                        SellB_sd  = sprintf("(%.2f)",SellB_sd),
                        p_able = format(round(p_able,digits = 2),digits=2),
                        p_sell = format(round(p_sell,digits = 2),digits=2))


#voila
kableExtra::kable(tb3c, format="latex",booktabs=T)  %>% 
  add_header_above(header = c(" ","Able=2"=2,"Able=8"=2,"SellA"=2,"sellB"=2)) %>% 
  add_header_above(header = c(" ","Panel C. Modified game"=9)) %>% 
  add_header_above(header = c(" ","Second subject pool"=9))  %>%                 
  kable_styling(font_size = 8)



####### Panel D - Forced allocator ######
tb3d2 = subset(forced_allocator, tokenstaken == 2)  %>%
  summarise(across(everything(), .f = list(mean = mean, sd = sd), na.rm = TRUE)) %>%
  gather(key=var, value=value) %>% 
  extract(col="var", into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>% 
  mutate(across(where(is.numeric), round, 2))

tb3d8 = subset(forced_allocator, tokenstaken == 8)  %>%
  summarise(across(everything(), .f = list(mean = mean, sd = sd), na.rm = TRUE)) %>%
  gather(key=var, value=value) %>% 
  extract(col="var", into=c("variable", "statistic"), regex=("(.*)_(.*)$")) %>%
  spread(key=statistic, value=value) %>% 
  mutate(across(where(is.numeric), round, 2))

tb3d = left_join(tb3d2, tb3d8, by='variable') 


#compute t test results
t_test_results_d <- Map(function(x,y) t.test(x,y),
                      x=forced_allocator %>% filter(tokenstaken == 2) %>% select(-tokenstaken),
                      y=forced_allocator %>% filter(tokenstaken == 8)%>% select(-tokenstaken))

vars <- names(t_test_results_d)
t_test_results_d <- sapply(t_test_results_d, function(x) x["p.value"])
t_test_results_d <- t_test_results_d %>% as.numeric()
t_test_results_d <- data.frame(variable=vars,p_value=t_test_results_d)

#merge results to mean-sd
tb3d <- left_join(tb3d, t_test_results_d, by="variable")

#rename etc
tb3d <- tb3d %>% 
  mutate(across(c(variable), ~ recode(., age = "Age",
                                      female = "Female",
                                      is_corrupt = "Is_Corrupt",
                                      class = "Socialeconomic class", 
                                      perc_corrupt = "%_Corrupt", 
                                      tokenstaken = "Tokens taken"))) %>% 
  rename("Take2_mean" = mean.x,
         "Take2_sd" = sd.x,
         "Take8_mean" = mean.y,
         "Take8_sd" = sd.y
  )


#formatting
tb3d <- tb3d %>% mutate(Take2_sd  = sprintf("(%.2f)",Take2_sd),
                        Take8_sd  = sprintf("(%.2f)",Take8_sd),
                        p_value = format(round(p_value,digits = 2),digits=2))
tb3d = tb3d[-7, ]



#voila
kableExtra::kable(tb3d, format="latex",booktabs=T)  %>% 
  add_header_above(header = c(" ","Take 2"=2,"Take 8"=2)) %>% 
  add_header_above(header = c(" ","Panel D. Forced allocator"=5)) %>% 
  add_header_above(header = c(" ","Second subject pool"=5))  %>%                 
  kable_styling(font_size = 8)




reg_key <- expand.grid(list(basic_game,modified_game),c(FALSE,TRUE))

reg_results <- Map(function(x,y){
  
  if(!y){
    return(list(
      "token" = lm(tokenstaken ~ treat, data = x),
      "is_corrupt" = lm(is_corrupt ~ treat, data = x),
      "p_corrupt" =  lm(perc_corrupt ~ treat, data = x),
      "is_corrupt_iv" = ivreg(is_corrupt ~ tokenstaken | treat, data = x),
      "p_corrupt_iv" = ivreg(perc_corrupt ~ tokenstaken | treat, data = x)
      
    ))
    
  }else{
    return(list(
      "token" = lm(tokenstaken ~ treat + female + class + age, data = x),
      "is_corrupt" = lm(is_corrupt ~ treat + female + class + age, data = x),
      "p_corrupt" =  lm(perc_corrupt ~ treat + female + class + age, data = x),
      "is_corrupt_iv" = ivreg(is_corrupt ~ tokenstaken + female + class + age | treat + female + class + age, data = x),
      "p_corrupt_iv" = ivreg(perc_corrupt ~ tokenstaken + female + class + age | treat + female + class + age, data = x)
    ))
    
  }
  
}, x= reg_key$Var1, y=reg_key$Var2)


reg_results <- lapply(reg_results, function(x) lapply(x, function(x){
  data.frame(coef(summary(x)),var_name=rownames(coef(summary(x))))
}))

reg_results <- lapply(reg_results, function(x) x %>% bind_rows(.id = "model"))

reg_results <- lapply(reg_results, function(x) x %>% filter(var_name == "treat" | var_name == "tokenstaken"))

reg_results <- lapply(reg_results, function(x) x %>% select(model,Estimate,"Std..Error"))


table4a <- reg_results[c(1,3)]
table4b <- reg_results[c(2,4)]


table4a <- table4a %>% bind_cols()
table4b <- table4b %>% bind_cols()


####table 4 basic game
table4a <- table4a[,-4]



colnames(table4a) <- c("model","est_A8","sd_A8","est_A2","sd_A2")
rownames(table4a) <- table4a$model
table4a <- table4a %>% select(-model)

table4a <-table4a %>% mutate_all(.funs = ~round(.,digits = 2),digits=2)
table4a <- table4a %>% mutate(sd_A8 = sprintf("(%.2f)",sd_A8),
                              sd_A2 = sprintf("(%.2f)",sd_A2))
                        

kableExtra::kable(table4a, format="latex",booktabs=T)  %>% 
  add_header_above(header = c(" ","(1)"=2,"(2)"=2)) %>% 
  add_header_above(header = c(" ","Basic Game"=4)) %>% 
  kable_styling(font_size = 8)


#### table 4 modified game ####

table4b <- table4b[,-4]



colnames(table4b) <- c("model","est_A8","sd_A8","est_A2","sd_A2")
rownames(table4b) <- table4b$model
table4b <- table4b %>% select(-model)

table4b <-table4b %>% mutate_all(.funs = ~round(.,digits = 2),digits=2)
table4b <- table4b %>% mutate(sd_A8 = sprintf("(%.2f)",sd_A8),
                              sd_A2 = sprintf("(%.2f)",sd_A2))


kableExtra::kable(table4b, format="latex",booktabs=T)  %>% 
  add_header_above(header = c(" ","(1)"=2,"(2)"=2)) %>% 
  add_header_above(header = c(" ","Modified Game"=4)) %>% 
  kable_styling(font_size = 8)















####Table 4 - Summary of Treatment Effects in the Basic and Modified Games####
#Col 1
col1up_token <- lm(tokenstaken ~ treat, data = basic_game)
col1up_is_corrupt <- lm(is_corrupt ~ treat, data = basic_game)
col1up_perc_corrupt <- lm(perc_corrupt ~ treat, data = basic_game)

col1low_is_corrupt <- ivreg(is_corrupt ~ tokenstaken | treat, data = basic_game)
col1low_perc_corrupt <- ivreg(perc_corrupt ~ tokenstaken | treat, data = basic_game)


#Col 2
col2up_token <- lm(tokenstaken ~ treat + female + class + age, data = basic_game)
col2up_is_corrupt <- lm(is_corrupt ~ treat + female + class + age, data = basic_game)
col2up_perc_corrupt <- lm(perc_corrupt ~ treat + female + class + age, data = basic_game)

col2low_is_corrupt <- ivreg(is_corrupt ~ tokenstaken + female + class + age | treat + female + class + age , data = basic_game)
col2low_perc_corrupt <- ivreg(perc_corrupt ~ tokenstaken + female + class + age | treat + female + class + age, data = basic_game)


#Col 3
col3up_token <- lm(tokenstaken ~ treat, data = modified_game)
col3up_is_corrupt <- lm(is_corrupt ~ treat, data = modified_game)
col3up_perc_corrupt <- lm(perc_corrupt ~ treat, data = modified_game)

col3low_is_corrupt <- ivreg(is_corrupt ~ tokenstaken | treat, data = modified_game)
col3low_perc_corrupt <- ivreg(perc_corrupt ~ tokenstaken | treat, data = modified_game)


#Col 4
col4up_token <- lm(tokenstaken ~ treat + female + class + age, data = modified_game)
col4up_is_corrupt <- lm(is_corrupt ~ treat + female + class + age, data = modified_game)
col4up_perc_corrupt <- lm(perc_corrupt ~ treat + female + class + age, data = modified_game)

col4low_is_corrupt <- ivreg(is_corrupt ~ tokenstaken + female + class + age | treat+ female + class + age, data = modified_game)
col4low_perc_corrupt <- ivreg(perc_corrupt ~ tokenstaken + female + class + age | treat+ female + class + age, data = modified_game)


summary(col4up_perc_corrupt)

############################################


rm(tb2col1, tb2col2, tb2col3, tb2col4, 
   tb3a2, tb3a8, tb3b2, tb3b8, tb3c_a, tb3c_b, tb3c2, tb3c8, tb3d2, tb3d8)







