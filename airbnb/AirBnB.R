
require(ggmap)
require(tidyverse)
require(rvest)

westlimit <- 28.986278 
southlimit <- 41.039883 
eastlimit <- 29.009785 
northlimit <- 41.061114
quadTree <- function(input=list(c(westlimit= 28.986278,southlimit = 41.039883),c(eastlimit = 29.009785,northlimit = 41.061114))){
  
  x1 <- input[[1]][1] 
  y1 <-  input[[1]][2] 
  x2 <- input[[2]][1] 
  y2 <- input[[2]][2]
  
  
  

  
  
  message(paste("Searching, SW:",x1,y1,"NW:",x2,y2))
  
  rootURL <- "https://www.airbnb.com/s/homes?tab_id=home_tab&refinement_paths[]=%2Fhomes&source=structured_search_input_header&search_type=unknown&"
  abURL <- paste0("ne_lat=",y2,"&ne_lng=",x2,"&sw_lat=",y1,"&sw_lng=",x1,"&")
  offsetURL <- paste0("search_by_map=true&items_offset=0&section_offset=2")
  
  
  listingLanding <- html_session(paste0(rootURL,abURL,offsetURL))
  
  
  
  
  noListings <- listingLanding %>% html_node("._1h559tl") %>% html_text() 
  numericList <- (noListings %>% str_split("of "))[[1]][2] %>% parse_number()
  if(is.na(numericList)){
    numericList <- 0
  }
  if(numericList == 300){
    message("found more than 300 entries, zooming in")
    
    sub1 <- list(c((x1+x2)/2,(y1+y2)/2),c(x2,y2))
    sub2 <- list(c(x1,(y1+y2)/2),c((x1+x2)/2,y2))
    sub3 <- list(c(x1,y1),c((x1+x2)/2,(y1+y2)/2))
    sub4 <- list(c((x1+x2)/2,y1),c(x2,(y1+y2)/2))
    
    return(list(quadTree(input=sub1),quadTree(input=sub2),quadTree(input=sub3),quadTree(input=sub4)))
    
    
    
  }else if(numericList == 0 ){
    
    
    message("no entries!")
    return(list(data.frame(matrix(nrow=0,ncol=27))))
    
  }else{
    message(paste("found",numericList,"entries"))
    maxPage <- listingLanding %>% html_nodes("._1y623pm") %>% html_text() %>% parse_number() %>% max()
    i <- 1
    message(paste("parsing page:",i))
    
    listJSON <- listingLanding %>% rvest::html_node("[id='data-state']") %>% html_text() %>% jsonlite::fromJSON()
    
    if(listJSON$niobeMinimalClientDataLegacy %>% identical(list())){
      try(pricingDF <- listJSON$niobeClientDataLegacy$`__niobe_denormalized`$queries[[1]][[2]]$dora$exploreV3$sections$items[[1]]$pricingQuote %>% data.frame(row.names = NULL))
      try(listingDF <- listJSON$niobeClientDataLegacy$`__niobe_denormalized`$queries[[1]][[2]]$dora$exploreV3$sections$items[[1]]$listing %>% data.frame(row.names = NULL))  
      
    }else{
      try(pricingDF <- listJSON$niobeMinimalClientDataLegacy[[1]][[2]]$data$dora$exploreV3$sections$items[[1]]$pricingQuote %>% data.frame(row.names = NULL))
      try(listingDF <- listJSON$niobeMinimalClientDataLegacy[[1]][[2]]$data$dora$exploreV3$sections$items[[1]]$listing %>% data.frame(row.names = NULL))
    }
    #pricingDF <- listJSON$niobeClientDataLegacy$`__niobe_denormalized`$queries[[1]][[2]]$dora$exploreV3$sections$items[[1]]$pricingQuote %>% data.frame(row.names = NULL)
    #listingDF <- listJSON$niobeClientDataLegacy$`__niobe_denormalized`$queries[[1]][[2]]$dora$exploreV3$sections$items[[1]]$listing %>% data.frame(row.names = NULL)
    
    #pricingDF <- listJSON$niobeMinimalClientDataLegacy[[1]][[2]]$data$dora$exploreV3$sections$items[[1]]$pricingQuote %>% data.frame(row.names = NULL)
    #listingDF <- listJSON$niobeMinimalClientDataLegacy[[1]][[2]]$data$dora$exploreV3$sections$items[[1]]$listing %>% data.frame(row.names = NULL)
    
    
    listingDF$priceString <- pricingDF$priceString
    listingDF$monthlyFactor <- pricingDF$monthlyPriceFactor
    listingDF$weeklyFactor <- pricingDF$weeklyPriceFactor
    listingDF$bbox <- paste(x1,y1,x2,y2)
    
    listingDF <- listingDF %>% jsonlite::flatten() %>% select(id,avgRating,bathroomLabel,bedLabel,bedroomLabel,city,guestLabel,hostLanguages,hostThumbnailUrlSmall,isBusinessTravelReady,isSuperhost,lat,lng,localizedCity,localizedNeighborhood,name,previewAmenityNames,roomAndPropertyType,roomType,starRating,priceString,monthlyFactor,weeklyFactor,bbox,user.id,user.createdAt,reviewsCount)
    
    i <- 2
    if(!is.na(maxPage)){
      
      while(i <= maxPage){
        Sys.sleep(0.3)
        
        offsetURL <- paste0("search_by_map=true&items_offset=",(i-1)*20,"&section_offset=2")
        listingLanding <- html_session(paste0(rootURL,abURL,offsetURL))
        
        message(paste("parsing page:",i))
        
        listJSON <- listingLanding %>% rvest::html_node("[id='data-state']") %>% html_text() %>% jsonlite::fromJSON() 
        
        #try(pricingDFsub <- listJSON$niobeMinimalClientDataLegacy[[1]][[2]]$data$dora$exploreV3$sections$items[[1]]$pricingQuote %>% data.frame(row.names = NULL))
        #try(listingDFsub <- listJSON$niobeMinimalClientDataLegacy[[1]][[2]]$data$dora$exploreV3$sections$items[[1]]$listing %>% data.frame(row.names = NULL))
        
        
        if(listJSON$niobeMinimalClientDataLegacy %>% identical(list())){
          try(pricingDFsub <- listJSON$niobeClientDataLegacy$`__niobe_denormalized`$queries[[1]][[2]]$dora$exploreV3$sections$items[[1]]$pricingQuote %>% data.frame(row.names = NULL))
          try(listingDFsub <- listJSON$niobeClientDataLegacy$`__niobe_denormalized`$queries[[1]][[2]]$dora$exploreV3$sections$items[[1]]$listing %>% data.frame(row.names = NULL))  
          
        }
        else{
          try(pricingDFsub <- listJSON$niobeMinimalClientDataLegacy[[1]][[2]]$data$dora$exploreV3$sections$items[[1]]$pricingQuote %>% data.frame(row.names = NULL))
          try(listingDFsub <- listJSON$niobeMinimalClientDataLegacy[[1]][[2]]$data$dora$exploreV3$sections$items[[1]]$listing %>% data.frame(row.names = NULL))
        }
        
        
        if(!(is.data.frame(pricingDFsub))&(is.data.frame(listingDFsub))){
          Sys.sleep(2)
          listingLanding <- html_session(paste0(rootURL,abURL,offsetURL))
          
          message(paste("parsing page again:",i))
          
          listJSON <- listingLanding %>% rvest::html_node("[id='data-state']") %>% html_text() %>% jsonlite::fromJSON() 
          
          if(listJSON$niobeMinimalClientDataLegacy %>% identical(list())){
            try(pricingDFsub <- listJSON$niobeClientDataLegacy$`__niobe_denormalized`$queries[[1]][[2]]$dora$exploreV3$sections$items[[1]]$pricingQuote %>% data.frame(row.names = NULL))
            try(listingDFsub <- listJSON$niobeClientDataLegacy$`__niobe_denormalized`$queries[[1]][[2]]$dora$exploreV3$sections$items[[1]]$listing %>% data.frame(row.names = NULL))  
            
          }
          else{
            try(pricingDFsub <- listJSON$niobeMinimalClientDataLegacy[[1]][[2]]$data$dora$exploreV3$sections$items[[1]]$pricingQuote %>% data.frame(row.names = NULL))
            try(listingDFsub <- listJSON$niobeMinimalClientDataLegacy[[1]][[2]]$data$dora$exploreV3$sections$items[[1]]$listing %>% data.frame(row.names = NULL))
          }
        }
        
        if(!(is.data.frame(pricingDFsub))&(is.data.frame(listingDFsub))){
          listingDFsub <- data.frame(matrix(nrow=0,ncol=27))
        }else{
          listingDFsub$priceString <- pricingDFsub$priceString
          listingDFsub$monthlyFactor <- pricingDFsub$monthlyPriceFactor
          listingDFsub$weeklyFactor <- pricingDFsub$weeklyPriceFactor
          listingDFsub$bbox <- paste(x1,y1,x2,y2)
          
          listingDFsub <- listingDFsub %>% jsonlite::flatten() %>% select(id,avgRating,bathroomLabel,bedLabel,bedroomLabel,city,guestLabel,hostLanguages,hostThumbnailUrlSmall,isBusinessTravelReady,isSuperhost,lat,lng,localizedCity,localizedNeighborhood,name,previewAmenityNames,roomAndPropertyType,roomType,starRating,priceString,monthlyFactor,weeklyFactor,bbox)
        }
        
        
        listingDF <- bind_rows(listingDF,listingDFsub)
        i <- i + 1
      }
    }
    
    
    return(listingDF)
    
    
  }
  
  
}
input = list(c(westlimit=11.360777, southlimit=48.061624), c(eastlimit=11.722908, northlimit=48.248116))
mnh <- quadTree(input = input)


holder <- data.frame(matrix(nrow=0,ncol=27))


checkFun <- function(dnmList){
  if(is.data.frame(dnmList)){
    holder <<- bind_rows(holder,dnmList)
    return(TRUE)
  }else{
    return(list(try(checkFun(dnmList[[1]])),try(checkFun(dnmList[[2]])),try(checkFun(dnmList[[3]])),try(checkFun(dnmList[[4]]))))
  }
}



palet <- c("#2d00f7","#6a00f4","#8900f2","#a100f2","#b100e8","#bc00dd","#d100d1","#db00b6","#e500a4","#f20089")

palet2 <- c("#ddfff7ff", "#93e1d8ff", "#ffa69eff", "#aa4465ff", "#861657ff")

palet3 <- c("#247ba0ff", "#70c1b3ff", "#b2dbbfff", "#f3ffbdff", "#ff1654ff")

palet4 <- c("#3d348bff", "#7678edff", "#f7b801ff", "#f18701ff", "#f35b04ff") 

palet5 <- c("#af43be","#fd8090","#c4ffff","#08deea","#1261d1") %>% rev()

palet6 <- c("#335c67ff", "#fff3b0ff", "#e09f3eff", "#9e2a2bff", "#540b0eff")

palet7 <- c("#0d3b66ff", "#faf0caff", "#f4d35eff", "#ee964bff", "#f95738ff")

holder <- holder[,28:54]
holder <- holder %>% distinct(id,.keep_all = T)
holder$price <- holder$priceString %>% parse_number()
holder$reviewsCount <- holder$reviewsCount %>% replace_na(0)
holder$roomCount <- ifelse(holder$bedroomLabel== "Studio",1,holder$bedroomLabel %>% str_extract("[0-9]+") %>% as.numeric())
holder$pricePerRoom <- holder$price/holder$roomCount

owners <- data.frame(table(holder$hostThumbnailUrlSmall))
ownerRatio <- length(owners$Var1[owners$Freq==1])/length(owners$Var1)
roomTypes <- data.frame(table(holder$roomType))

gmap <- get_stamenmap(bbox=c(left=as.numeric(input[[1]][1]), bottom=as.numeric(input[[1]][2]), right=as.numeric(input[[2]][1]), top=as.numeric(input[[2]][2])),zoom = 12,maptype = "toner-background")

ggmap(gmap)  +  geom_point(data=holder,aes(lng,lat,fill=price,color=price),size=0.2,alpha=0.3,shape=22) + 
  scale_fill_gradientn(colors = palet6,oob=scales::squish,limits=c(25,300)) + 
  scale_color_gradientn(colors = palet6,oob=scales::squish,limits=c(25,300))+ 
  theme(legend.position = "bottom", 
        legend.key.height = unit(2,"points"),
        text=element_text(family = "Noto Sans",color="beige"),
        legend.key.width = unit(50,"points"),
        plot.background = element_rect(fill="#847577"),
        legend.background = element_rect(fill="#847577"),
        panel.background = element_rect(fill="#847577"),
        axis.ticks = element_blank(),
        panel.border = element_rect(color="#322F3D",fill="transparent"),
        panel.grid = element_blank(),
        strip.background = element_rect(fill="#847577"))
palet6 <- c("#335c67ff", "#fff3b0ff", "#e09f3eff", "#9e2a2bff", "#540b0eff")
