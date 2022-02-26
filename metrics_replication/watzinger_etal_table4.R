require(haven)
require(tidyverse)
require(fastDummies)




main_data <- haven::read_dta("watzinger_etal/Kit_WNFS/data/auxAnalysis_AEJ.dta")
conc_data <- haven::read_dta("watzinger_etal/Kit_WNFS/data/concentration_nclass0.dta")
comm_data <- haven::read_dta("watzinger_etal/Kit_WNFS/data/communication_sic_nclass0.dta")

main_data$nclass0 <- main_data$class
main_data <- main_data %>% filter(appln_y >= 1945)

main_data$treated <- main_data$treated>0
main_data <- full_join(main_data,conc_data,by="nclass0",suffix=c("","c"))

main_data$hhi_nclass0 <- main_data$hhi_nclass0 > 0.15
main_data$hhi_young_nclass0 <- main_data$hhi_young_nclass0 > 0.15
main_data$hhi<- main_data$hhi > 0.15
main_data$hhi_young <- main_data$hhi_young  > 0.15
main_data$Conc_4 <- main_data$Conc_4 > 44
main_data$Conc_8 <- main_data$Conc_8 > 60


main_data <- full_join(main_data,comm_data,by="nclass0")
main_data <- main_data %>% group_by()

main_data <- main_data %>% group_by(class) %>% mutate(m_transistor=max(transistor>0))
main_data$m_transistor <- main_data$m_transistor %>% replace_na(1)
main_data <- main_data %>% group_by(class,appln_y) %>% mutate(groupID=cur_group_id())
main_data <- main_data %>% group_by(class,subclass) %>% mutate(size_subclass=sum(count_patents*(appln_y<1950)))
main_data$size_subclass <- main_data$size_subclass%>% replace_na(0)


#### summarise and merge

md1 <- main_data %>% group_by(treated,subclass,appln_y, class, groupID, impact,hhi_nclass0,hhi_young_nclass0,hhi,hhi_young,Conc_4,Conc_8,subcat) %>% 
  summarise_at(c("count_patents_cites","young_small", "count_patents_all", "count_patents"),sum)

md2 <- main_data %>% group_by(treated,subclass,appln_y, class, groupID, impact,hhi_nclass0,hhi_young_nclass0,hhi,hhi_young,Conc_4,Conc_8,subcat) %>% 
  summarise_at(c("age_class", "m_transistor","m_t"),mean)

main_data <- inner_join(md1,md2,by=c("treated","subclass","appln_y", "class", "groupID", "impact","hhi_nclass0","hhi_young_nclass0","hhi","hhi_young","Conc_4","Conc_8","subcat"))


#### treated

main_data <- main_data %>% group_by(treated) %>% mutate(m = max(count_patents*(appln_y == 1949)))
main_data <- main_data %>% mutate(m=replace_na(m,0))

#### class

main_data <- main_data %>% group_by(class,appln_y) %>% mutate(m_treat = mean(treated))
main_data <- main_data %>% filter(!(m_treat == 0 | m_treat == 1))
main_data$year <- main_data$appln_y
main_data$appln_y <- as.factor(main_data$appln_y)
main_data<- main_data %>% group_by(class,treated) %>% mutate(subclassID=cur_group_id())

main_data$post <- main_data$year > 1955
main_data$treated_post <- main_data$treated*main_data$post
main_data <- main_data %>% mutate(count_other=count_patents- young_small)

main_data <- main_data %>% filter(!(year > 1970 | year < 1949))


model_1 <-  lfe::felm(count_patents~appln_y+post+treated+treated_post | class | 0 | class ,data = main_data)
model_2 <-  lfe::felm(count_patents~appln_y+post+treated+treated_post| class | 0 | class,data = main_data %>% filter(impact >= 15))
model_3 <- lfe::felm(count_patents~appln_y+post+treated+treated_post| class | 0 | class,data = main_data %>% filter(impact < 15))
model_4 <- lfe::felm(count_patents~appln_y+post+treated+treated_post| class | 0 | class,data = main_data %>% filter(impact < 15 & m_t == 0))
model_5 <- lfe::felm(count_patents~appln_y+post+treated+treated_post| class | 0 | class,data = main_data %>% filter(impact < 15 & m_t == 0 & Conc_8 == 1))
model_6 <- lfe::felm(count_patents~appln_y+post+treated+treated_post| class | 0 | class,data = main_data %>% filter(impact < 15 & m_t == 0 & Conc_8 == 0))
model_7 <- lfe::felm(young_small~appln_y+post+treated+treated_post| class | 0 | class,data = main_data %>% filter(impact < 15& m_t == 0))
model_8 <- lfe::felm(count_other~appln_y+post+treated+treated_post| class | 0 | class,data = main_data %>% filter(impact < 15& m_t == 0))

stargazer::stargazer(model_1,model_2,model_3,model_4,model_5,model_6,model_7,model_8,font.size = "scriptsize",omit = 1:23,omit.stat = c("f","ser"),header = FALSE)
