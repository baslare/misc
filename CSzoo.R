require(RSelenium)
require(tidyverse)
require(rvest)

driver <- rsDriver(browser = "firefox",port = 4569L)
maind <- driver$client


maind$navigate("https://www.zooniverse.org/projects")


main_nav <- maind$findElement("class","hero-nav")$findChildElements("tag","button")


k <- 1
while(k <= length(main_nav)){
  
  main_nav[[k]]$clickElement()
  status <- main_nav[[k]]$getElementText()[[1]]
  source <- maind$getPageSource()[[1]]
  hmm <- xml2::read_html(source)
  
  fields <- hmm %>% html_node(".filmstrip__strip") %>% html_nodes("li") %>% html_text()
  fields <- fields[-1]
  
  fields_button <- maind$findElement(using = "class","filmstrip__strip")$findChildElements("tag","button")
  
  fields_button <- fields_button[-1]
  
  
  
  
  
  i <- 1
  
  while(i <= length(fields_button)){
    fields_button[[i]]$clickElement()
    
    Sys.sleep(0.5)
    
    nav_buttons <- try(maind$findElement("class","pagination")$findChildElements("tag","button"))
    
    if(length(nav_buttons) ==1)  {
      
      proj <- maind$findElement("xpath","/html/body/div/div/div/div/div/div/section[2]/section/div[3]")$findChildElements("tag","a")
      proj_url <- sapply(proj, function(x) x$getElementAttribute("href")[[1]])
      proj_name <- sapply(proj, function(x) x$getElementText()[[1]])
      fields_name <- rep(fields[i],length(proj))
      status_name <- rep(status,length(proj))
      proj_list <- append(proj_list,list(data.frame(proj_url,proj_name,fields_name,status_name)))
      
      
    }else{
      j <- 1
      
      while( j<= length(nav_buttons)){
        
        nav_buttons[[j]]$clickElement()
        
        Sys.sleep(0.5)
        
        proj <- maind$findElement("xpath","/html/body/div/div/div/div/div/div/section[2]/section/div[3]")$findChildElements("tag","a")
        proj_url <- sapply(proj, function(x) x$getElementAttribute("href")[[1]])
        proj_name <- sapply(proj, function(x) x$getElementText()[[1]])
        fields_name <- rep(fields[i],length(proj))
        status_name <- rep(status,length(proj))
        
        proj_list <- append(proj_list,list(data.frame(proj_url,proj_name,fields_name,status_name)))
        
        j <- j + 1
      }
    }
    
    
    i <- i + 1
  }
  
  k <- k + 1
}


proj_df <- proj_list %>% bind_rows()
proj_df_unique <- proj_df %>% distinct(proj_name,.keep_all = T)
proj_df_unique_sub <- proj_df_unique %>% filter(str_detect(proj_url,"zooniverse"))


i <- 1


stats_list <- list()

while(i <= length(proj_df_unique_sub$proj_url)){
  
  maind$navigate(proj_df_unique_sub$proj_url[i])
  
  
  Sys.sleep(2)
 
  stats <- maind$getPageSource()[[1]] %>% xml2::read_html() %>% html_nodes(".project-metadata-stat__value") %>% html_text()
  
  stats_list <- append(stats_list,list(data.frame(matrix(stats,nrow = 1,ncol = 4))))
  
  
  i <- i + 1
  
}

i <- 1 

stats_list_df <- stats_list %>% bind_rows()
stats_list2 <- list()
for(i in rpr){
  
  maind$navigate(proj_df_unique_sub$proj_url[i])
  
  
  Sys.sleep(3)
  
  stats <- maind$getPageSource()[[1]] %>% xml2::read_html() %>% html_nodes(".project-metadata-stat__value") %>% html_text()
  
  stats_list2 <- append(stats_list2,list(data.frame(matrix(stats,nrow = 1,ncol = 4))))
  
  
  i <- i + 1
  
}


rpr <- which(is.na(stats_list_df$X1))
rpr_df <- stats_list2 %>% bind_rows()

stats_list_df[rpr,] <- rpr_df

colnames(stats_list_df) <- c("volunteers","classifications","subjects","completed_subjects")
stats_df <- data.frame(apply(stats_list_df, 2, function(x) x %>% str_remove_all("[.]") %>% as.numeric()))

master_df <- cbind(proj_df_unique_sub,stats_df)

pubs <- html_session("https://www.zooniverse.org/about/publications") %>% html_nodes("h3") %>% html_text()

pubs_count <- pubs %>% str_extract("([0-9]+)")

pubs_df <- data.frame(pubs=pubs %>% toupper() %>% str_remove_all("[([0-9]+)]") %>% str_squish(),count=pubs_count %>% as.numeric())

master_df <- left_join(master_df,pubs_df,by=c("proj_name"="pubs"))
master_df$count <- master_df$count %>% replace_na(0)
master_df$status_name <- master_df$status_name %>% replace_na("Live")

type_cross <- master_df %>% group_by(status_name) %>% summarise(occ=n(),hasPub=sum(hasPub),pubs=sum(count),prt=sum(volunteers,na.rm = T),prt_ave=prt/(occ-1))

stargazer::stargazer(type_cross[order(type_cross$occ,decreasing = T),] %>% mutate(ratio=ratio %>% round(3)),type = 'text',summary = FALSE,digits = 1,digits.extra = 1)


master_huge <- left_join(master_df,pubs_df,by=c("proj_name"="pubs"))



stats_df$proj_name <- proj_df_unique_sub$proj_name
proj_df_big <- left_join(proj_df,stats_df,by="proj_name")
proj_df_big <- left_join(proj_df_big,pubs_df,by=c("proj_name"="pubs"))
proj_df_big$count <- proj_df_big$count %>% replace_na(0)
proj_df_big$hasPub <- proj_df_big$count > 0

fields_df <- proj_df_big %>% group_by(fields_name) %>% summarise(occ=n(),hasPub=sum(hasPub),pubs=sum(count))

stargazer::stargazer(fields_df[order(fields_df$ratio,decreasing = T),] %>% mutate(ratio=ratio %>% round(3)),type = 'text',summary = FALSE,digits = 1,digits.extra = 1)


tess <- which(proj_df_big$proj_name == "PLANET HUNTERS TESS")

proj_df_big[tess,]$volunteers <- master_df[65,]$volunteers
proj_df_big[tess,]$subjects <- master_df[65,]$subjects
proj_df_big[tess,]$completed_subjects <- master_df[65,]$completed_subjects
proj_df_big[tess,]$classifications <- master_df[65,]$classifications

proj_df_big$status_name <- proj_df_big$status_name %>% replace_na("Live")

proj_df_big_ <- proj_df_big %>% filter(!is.na(volunteers))

pg <- proj_df_big[which(proj_df_big$status_name!="Live"),]
pg_ <- pg %>% filter(!is.na(volunteers))


write.xlsx(fields_df[order(fields_df$pubRatio,decreasing = T),],"pg.xls")

fields_df_ <- pg_ %>% group_by(fields_name) %>% summarise(occ=n(),hasPub=sum(hasPub),pubs=sum(count),prt=sum(volunteers),ave_prt = prt/occ)
fields_df <- pg %>% group_by(fields_name) %>% summarise(occ=n(),hasPub=sum(hasPub),pubs=sum(count),prt=sum(volunteers),ave_prt = prt/occ,pubRatio=hasPub/occ)

basket <- sapply(pg$proj_name, function(x) pg$fields_name[pg$proj_name == x])
basket <- sapply(basket, function(x) paste(x, collapse = ","))





pg$basket <- basket$basket
pg_sub <- pg %>% distinct(proj_name,.keep_all = T)

write.csv(pg_sub$basket,"basket.csv",quote = F)

trs <- read.transactions("basket.csv",format = "basket",sep = ",")
association.rules <- apriori(trs, parameter = list(supp=0.001, conf=0.8,maxlen=10))

subRules<-association.rules[quality(association.rules)$count>5]
write.xlsx(inspect(head(subRules,n=15,by="count")),"rules.xls")
