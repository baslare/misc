require(RSelenium)
require(tidyverse)
require(lubridate)
require(httr)
require(magick)
require(rvest)


#sudo docker run -d -p 4445:4444 -e SCREEN_WIDTH=1920 -e SCREEN_HEIGHT=1080 selenium/standalone-firefox:2.53.0
#sudo 
maind <- remoteDriver(port = 4445L)
maind$open()
maind$navigate("https://ekap.kik.gov.tr/EKAP/Ortak/IhaleArama/index.html")

eCaps <- list(chromeOptions = list(
  args = c('--headless', '--disable-gpu', '--window-size=1280,800')
))

eCapsHead <- list(chromeOptions = list(
  args = c('--window-size=1280,800')
))

error <- "Error : \t Summary: NoSuchElement\n \t Detail: An element could not be located on the page using the given search parameters.\n \t class: org.openqa.selenium.NoSuchElementException\n\t Further Details: run errorDetails method\n"


#driver <- rsDriver(browser = "chrome",chromever = "77.0.3865.10", extraCapabilities = eCaps) #for headless browsing, i
#driver <- rsDriver(browser = "chrome",chromever = "77.0.3865.10") #in case headless browsing is not suitable for some reason
#maind <- driver$client

maind$maxWindowSize() #this is fairly necessary to make the left panel visible. without a maximized window, none of the below is going to work.
#another necessity for a maximized window arises when taking screenshots for captcha solving. The pixel coordinates for image cropping are based on a browser with a max window at 1920x1080 resolution. My windows 10 task bar is also snapped to the right side of the screen.
#if needed, the pixel coordinates in the image cropping part can be changed accordingly.

#maind$setWindowSize(height=800,width=1280) #this is required instead of previous line if the browser is not run in the headless mode

APIkey <- "" #API key for 2captcha.com



#necessary functions for captcha solving, authenticating and error handling
#the captchas must be split in two parts, since they feature a question in Turkish to be answered and a string of numbers to be written
#both parts are sent to 2captcha via the API seperately. If at least one part is proven to be malfunct, the entire process must start anew.
#captcha solving works on a HTTP POST and GET basis. We post our request with the screenshot and the required paramaters asked by the API. (POST)
#after a while, we take the ID assigned to our request and check whether the captcha has been solved. (GET)
#if it isn't solved yet, we check again until we get a solved captcha or an error message. (This sequence was coded with a recursive approach, it will keep trying until either of the aforementioned conditions take place )
#the result we get might be labeled as "solved", yet it might not be correct. We need to try and see whether it is correct or not.
#If the result is incorrect, the page displays an error message, then the pixel coordinates in the image cropping part must be offset a bit. (The values might change when run on an another device)

#httr provides a framework for handling HTTP exchanges.
#rvest is another package similar to beautiful soup for parsing html documents. In this case, our POST request is answered with an HTML file.
POSTfun <- function(tr){
  if(tr){
    output <- httr::content(x = POST(url = "https://2captcha.com/in.php",encode = "multipart",body = list(method="post",key=APIkey,submit="Upload and get the ID",lang="tr",numeric=4,file= upload_file("captchaTR.jpeg"))))
    output <- output %>% rvest::html_node(css = "body") %>% rvest::html_text()
    message(paste("ID: ",output))
    return(output %>% str_remove("OK+[|]"))
    
    
  }else{
    output <- postOutput <- httr::content(x = POST(url = "https://2captcha.com/in.php",encode = "multipart",body = list(method="post",key=APIkey,submit="Upload and get the ID",file= upload_file("captchaNum.jpeg"))))
    output <- output %>% rvest::html_node(css = "body") %>% rvest::html_text()
    message(paste("ID: ",output))
    return(output %>% str_remove("OK+[|]"))
  }
}


GETfun <- function(id){
  
  Sys.sleep(5)
  result <- httr::content(x = httr::GET(url = "https://2captcha.com/res.php",query=list(key=APIkey,id=id,action="get")))
  
  if(result %>% str_detect("NOT_READY")){
    message(paste(result %>% str_detect("NOT_READY"),"Captcha is NOT Ready"))
    message("Waiting for 5 seconds and trying again!")
    return(GETfun(id))
  }else if(result %>% str_detect("OK+[|]")){
    message("Captcha is READY: ",result %>% str_detect("OK+[|]"))
    return(result)
    
  }else{
    message("Unsolvable Captcha, Refreshing")
    return(-1)
    
  }
  
  
  
}

#magick is a package for image editing. It has its own syntax for image geometries. It allows the usage of the pipe (%>%) operator.
#All we need to know is what "305x100+530+100" represents: we're cropping an image of size 305x100 starting at the point (530,100)
#image files should better be written to the disk, because they need to be uploaded to the 2captcha server.


captchaFun <- function(tr, maind, clean){
  if(clean){
    maind$screenshot(file = "captcha.jpeg")
    img <- image_read("captcha.jpeg")
    img %>% magick::image_crop("305x100+250+110") %>% image_crop("200x100") %>% image_write("captchaTR.jpeg")
    img %>% image_crop("305x100+250+110") %>% image_crop("200x100+200") %>% image_write("captchaNum.jpeg")
    
  }else if(try(maind$findElement("xpath","//*[@id='ctl00_pnlEkapCaptchaKontrol']")$getElementText()[[1]] == error)){
    maind$screenshot(file = "captcha.jpeg")
    img <- image_read("captcha.jpeg")
    img %>% magick::image_crop("305x100+250+195") %>% image_crop("200x100") %>% image_write("captchaTR.jpeg")
    img %>% image_crop("305x100+250+195") %>% image_crop("200x100+200") %>% image_write("captchaNum.jpeg")
    
  }else{
    maind$screenshot(file = "captcha.jpeg")
    img <- image_read("captcha.jpeg")
    img %>% magick::image_crop("305x100+250+419") %>% image_crop("200x100") %>% image_write("captchaTR.jpeg")
    img %>% image_crop("305x100+250+419") %>% image_crop("200x100+200") %>% image_write("captchaNum.jpeg")
    maind$findElement("xpath","//*[@id='ctl00_pnlEkapCaptchaKontrol']")$clickElement()
    maind$findElement("xpath","//*[@id='ctl00_pnlEkapCaptchaKontrol']")$sendKeysToElement(list(key="end"))
  }
  
  
  id <- POSTfun(tr)
  captcha <- GETfun(id)
  if(captcha == -1){
    maind$findElement("xpath","//*[@id='ctl00_capEkapMaster_refBtn']")$clickElement()
    return(captchaFun(tr,maind,clean))
    
  }else{
    return(captcha)
    
  }
  
}

captchaEntry <- function(maind,clean){
  
  capTR <- captchaFun(TRUE,maind,clean)
  capNum <- captchaFun(FALSE,maind,clean)
  message(paste("solved Captcha: ",capTR %>% str_remove("OK+[|]"),capNum %>% str_remove("OK+[|]")))
  maind$findElement("xpath","//*[@id='ctl00_capEkapMaster_txtCaptcha']")$sendKeysToElement(list(paste(capTR %>% str_remove("OK+[|]"),capNum %>% str_remove("OK+[|]"))))
  maind$findElement("xpath","//*[@id='ctl00_btnCaptchaProtect']")$clickElement()
  Sys.sleep(0.25)
  
  if(try(maind$findElement("xpath","//*[@id='ctl00_ContentPlaceHolder1_lblIlan']/table/tbody")$getElementText()[[1]]) == error){
    return(captchaEntry(maind,FALSE))
    
    
  }else{
    message(paste("Captcha Authentication Successful!"))
    
  }
  
}



url <- "https://ekap.kik.gov.tr/EKAP/Ortak/IhaleArama/index.html"

maind$navigate(url) #to navigate to the URL


#these vectors are not used. They are here in case the need arises to scrape multiple months/years in a single session.
aylar <- c("Ocak","Þubat","Mart","Nisan","Mayýs","Haziran","Temmuz","Aðustos","Eylül","Ekim","Kasým","Aralýk")
yýllar <- c("2010","2011","2012","2013","2014","2015","2016","2017","2018","2019")



#generalized procedure for handling the starting and the ending months of a stretch.

startDate <- function(maind,ay,yil){
  
  maind$findElement("xpath","//*[@id='autoScroll']/div[2]/div/div[1]/input")$clickElement()
  maind$findElement("xpath","//*[@id='autoScroll']/div[2]/div/div[1]/div[1]/header/span[2]")$clickElement()
  maind$findElement("xpath","//*[@id='autoScroll']/div[2]/div/div[1]/div[2]/header/span[2]")$clickElement()
  
  years <- maind$findElement("xpath","//*[@id='autoScroll']/div[2]/div/div[1]/div[3]")$findChildElements("tag","span")
  yearsText <- sapply(years, function(x) x$getElementText()[[1]])
  indexYear <- which(yearsText == yil) - 3
  
  maind$findElement("xpath",paste("//*[@id='autoScroll']/div[2]/div/div[1]/div[3]/span[",indexYear,"]",sep = ""))$clickElement()
  
  months <- maind$findElement("xpath","//*[@id='autoScroll']/div[2]/div/div[1]/div[2]")$findChildElements("tag","span")
  monthsText <- sapply(months, function(x) x$getElementText()[[1]])
  indexMonth <- which(monthsText == ay) - 3
  
  maind$findElement("xpath",paste("//*[@id='autoScroll']/div[2]/div/div[1]/div[2]/span[",indexMonth,"]"))$clickElement()
  
  calendarBegin <- maind$findElement("xpath","//*[@id='autoScroll']/div[2]/div/div[1]/div[1]")
  calendarBegin <- calendarBegin$findChildElements("tag","span")
  dateCheck <- sapply(calendarBegin, function(x) x$getElementText()[[1]])
  startIndex <- which(dateCheck == "1") -3
  
  if(!identical(which(dateCheck == "31"),integer(0))){
    endIndex <- which(dateCheck == "31") -3
  }else if(!identical(which(dateCheck == "30"),integer(0))){
    endIndex <- which(dateCheck == "30") -3
    
  }else if(!identical(which(dateCheck == "29"),integer(0))){
    endIndex <- which(dateCheck == "29") -3
  }else{
    endIndex <- which(dateCheck == "28") -3
  }
  
  
  
  maind$findElement("xpath",paste("//*[@id='autoScroll']/div[2]/div/div[1]/div[1]/span[",startIndex,"]",sep = ""))$getElementText()[[1]]
  
  maind$findElement("xpath",paste("//*[@id='autoScroll']/div[2]/div/div[1]/div[1]/span[",startIndex,"]",sep = ""))$clickElement()
}


#endDate
endDate <- function(maind, ay, yil){
  maind$findElement("xpath","//*[@id='autoScroll']/div[2]/div/div[2]/input")$clickElement()
  
  
  maind$findElement("xpath","//*[@id='autoScroll']/div[2]/div/div[2]/div[1]/header/span[2]")$clickElement()
  maind$findElement("xpath","//*[@id='autoScroll']/div[2]/div/div[2]/div[2]/header/span[2]")$clickElement()
  
  years <- maind$findElement("xpath","//*[@id='autoScroll']/div[2]/div/div[2]/div[3]")$findChildElements("tag","span")
  yearsText <- sapply(years, function(x) x$getElementText()[[1]])
  indexYear <- which(yearsText == yil) - 3
  
  maind$findElement("xpath",paste("//*[@id='autoScroll']/div[2]/div/div[2]/div[3]/span[",indexYear,"]",sep = ""))$clickElement()
  
  months <- maind$findElement("xpath","//*[@id='autoScroll']/div[2]/div/div[2]/div[2]")$findChildElements("tag","span")
  monthsText <- sapply(months, function(x) x$getElementText()[[1]])
  indexMonth <- which(monthsText == ay) - 3
  
  maind$findElement("xpath",paste("//*[@id='autoScroll']/div[2]/div/div[2]/div[2]/span[",indexMonth,"]"))$clickElement()
  
  calendarBegin <- maind$findElement("xpath","//*[@id='autoScroll']/div[2]/div/div[2]/div[1]")
  calendarBegin <- calendarBegin$findChildElements("tag","span")
  dateCheck <- sapply(calendarBegin, function(x) x$getElementText()[[1]])
  startIndex <- which(dateCheck == "1") -3
  
  if(!identical(which(dateCheck == "31"),integer(0))){
    endIndex <- which(dateCheck == "31") -3
  }else if(!identical(which(dateCheck == "30"),integer(0))){
    endIndex <- which(dateCheck == "30") -3
    
  }else if(!identical(which(dateCheck == "29"),integer(0))){
    endIndex <- which(dateCheck == "29") -3
  }else{
    endIndex <- which(dateCheck == "28") -3
  }
  
  
  
  
  maind$findElement("xpath",paste("//*[@id='autoScroll']/div[2]/div/div[2]/div[1]/span[",endIndex,"]",sep = ""))$getElementText()[[1]]
  
  maind$findElement("xpath",paste("//*[@id='autoScroll']/div[2]/div/div[2]/div[1]/span[",endIndex,"]",sep=""))$clickElement()
  
}



#####################
###the actual code###

#initialization of the required variables:

missingData <- character()

#################################
######TIME WINDOW SELECTION######
#################################

#I suggest each session to cover only a single month,  

#startDate(maind,"Temmuz","2019")
#endDate(maind,"Temmuz","2019")

if(TRUE){
  if(readline(prompt = "Would you like to scrape the part behind the captchawall? [y/n]: ") %>% str_detect("y")){
    captchaIncluded <- TRUE
  }else{
    captchaIncluded <- FALSE
  }
  
  if(captchaIncluded){
    message("The script is going to get the info behind the captcha every n'th iteration, systematically sampling from the distribution. \nWhat this n should be? (Values less than 100 are not recommended.)")
    nCheck <- scan(n = 1)
    
  }else{
    nCheck <- 999
  }
  
  message("Will go behind the captcha every ",nCheck," item(s)")
}

dateChildren <- maind$findElement("xpath","/html/body/div/div[1]/div/div/div[4]/div[1]/select")
maxDate <- length(dateChildren$findChildElements(using="tag","option"))

while(counter <= maxDate){
  dateChildren <- maind$findElement("xpath","/html/body/div/div[1]/div/div/div[4]/div[1]/select")
  maxDate <- length(dateChildren$findChildElements(using="tag","option"))
  currentYear <- dateChildren$findChildElements(using="tag","option")[[counter]]$getElementText()[[1]]
  maind$findElement("xpath",paste("/html/body/div/div[1]/div/div/div[4]/div[1]/select/option[",counter,"]"))$clickElement()
  provincesChildren <- maind$findElement("xpath","/html/body/div/div[1]/div/div/div[4]/div[4]/div[4]/div/div/select")
  maxProvince <- length(provincesChildren$findChildElements(using="tag","option"))
  firstRun <- TRUE
  
  while(pCounter <= maxProvince){
    maind <- remoteDriver(port = 4445L)
    maind$open()
    maind$navigate("https://ekap.kik.gov.tr/EKAP/Ortak/IhaleArama/index.html")
    Sys.sleep(10)
    maind$findElement("xpath",paste("/html/body/div/div[1]/div/div/div[4]/div[1]/select/option[",counter,"]"))$clickElement()
    
    provincesChildren <- maind$findElement("xpath","/html/body/div/div[1]/div/div/div[4]/div[4]/div[4]/div/div/select")
    
    currentProvince <- provincesChildren$findChildElements(using="tag","option")[[pCounter]]$getElementText()[[1]]
    
    maind$findElement("xpath",paste("/html/body/div/div[1]/div/div/div[4]/div[4]/div[4]/div/div/select/option[",pCounter,"]"))$clickElement()
    message(paste("Moving on to",currentProvince))
    
    
    test <- character()
    
    
    maind$findElement("xpath","//*[@id='autoScroll']/div[3]/select")$clickElement()
    maind$findElement("xpath","//*[@id='autoScroll']/div[3]/select/option[7]")$clickElement()
    
    turCounter <- 1
    while(turCounter <= 4){
      ihaleNo <- character()
      ihaleAdi <- character()
      ihaleUsulu <- character()
      kismiTeklif <- character()
      kisimSayisi <- numeric()
      OKAS <- character()
      ilaninSekli <- character()
      isYeri <- character()
      ihaleYeri <- character()
      
      enUstIdare <- character()
      bagliIdare <- character()
      idareAdi <- character()
      idareÝli <- character()
      
      yaklasikMaliyet <- character()
      sozlesmeyeEsasYM <- character()
      sure <- character()
      dokSatinAlanSay <- numeric()
      eImzaliDL <- numeric()
      toplamTeklif <- numeric()
      toplamGecerliTeklif <- numeric()
      yerliLehine <- character()
      sozTarihi <- character()
      sozBedeli <- character()
      ihaleOnayTarihi <- date()
      sozYuklenici <- character()
      yukUyruk <- character()
      yukAdres <- character()
      sozlesmeBilgileri <- character()
      
      sirketAdi <- character()
      enYuksekTeklif <- character()
      enDusukTeklif <- character()
      
      maind <- remoteDriver(port = 4445L)
      maind$open()
      maind$navigate("https://ekap.kik.gov.tr/EKAP/Ortak/IhaleArama/index.html")
      Sys.sleep(5)
      
      maind$findElement("xpath",paste("/html/body/div/div[1]/div/div/div[4]/div[1]/select/option[",counter,"]",sep = ""))$clickElement()
      Sys.sleep(0.25)
      maind$findElement("xpath",paste("/html/body/div/div[1]/div/div/div[4]/div[4]/div[4]/div/div/select/option[",pCounter,"]",sep=""))$clickElement()
      Sys.sleep(0.25)
      message(paste("After Refreshing, moving on to",currentProvince))
      maind$findElement("xpath","//*[@id='autoScroll']/div[3]/select")$clickElement()
      Sys.sleep(0.25)
      maind$findElement("xpath","//*[@id='autoScroll']/div[3]/select/option[7]")$clickElement()
      Sys.sleep(0.25)
      maind$findElement("xpath",paste("//*[@id='autoScroll']/div[1]/div/div[1]/div/div[",turCounter,"]/label/span[2]",sep = ""))$clickElement()
      Sys.sleep(0.25)
      #button
      turName <- maind$findElement("xpath",paste("//*[@id='autoScroll']/div[1]/div/div[1]/div/div[",turCounter,"]/label/span[2]"))$getElementText()[[1]]
      maind$findElement("xpath","//*[@id='pnlFiltreBtn']/button")$clickElement()
      Sys.sleep(10)
      maind$findElement("xpath","//*[@id='pnlFiltreBtn']/button")$clickElement()
      
      #sonuçlar --
      Sys.sleep(10) #must wait until the results load
      
      results <- maind$findElement("xpath","//*[@id='sonuclar']")$findChildElements("class","col-sm-12")
      
      if(results[[1]]$getElementText()[[1]] != errorNotFound[[1]]){
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        message(paste("Moving on to: ",currentProvince,turName,sep=" "))
        results <- maind$findElement("xpath","//*[@id='sonuclar']")$findChildElements("class","col-sm-12")
        
        #The main loop for traversing and scraping the listed entries
        k <- 1
        while (!(try(results[[k]]$getElementText()[[1]],silent = T) %>% str_detect("out of bounds|altindis"))) {
          
          
          begin <- Sys.time()
          
          
          nav <- results[[k]]$findChildElements("class","nav-item")
          
          nav[[1]]$clickElement()
          
          try(maind$mouseMoveToLocation(webElement = results[[k+3]]))
          resultsSize <- length(results)
          Sys.sleep(0.75)
          
          #frame switching - to bilgiler
          
          if(try(maind$findElement("tag","iframe")$getElementText()[[1]])==error){
            nav[[1]]$clickElement()
          }
          iFrame <- maind$findElement("tag","iframe")
          maind$switchToFrame(iFrame) #the website features "frames". The items under different frames are not detectable if the current frame is another frame.
          
          #the variable slots are filled with the corresponding entries. The values are found by their "id", "xpath" adress or "css" selector.
          Sys.sleep(0.75)
          if(try(maind$findElement("id","ucBirBakistaIhale_lblIKNo")$getElementText()[[1]]) == error){
            Sys.sleep(2)
          }
          if(try(maind$findElement("id","ucBirBakistaIhale_lblIKNo")$getElementText()[[1]]) != error){
            if(maind$findElement("id","ucBirBakistaIhale_lblIKNo")$getElementText()[[1]]==""){
              Sys.sleep(5)
              message("encountered a blank entry, waiting...")
            }
            ihaleNo[k] <- maind$findElement("id","ucBirBakistaIhale_lblIKNo")$getElementText()[[1]]
            ihaleAdi[k] <- maind$findElement("id","ucBirBakistaIhale_lblIhaleAdi")$getElementText()[[1]]
            ihaleUsulu[k] <- maind$findElement("id","ucBirBakistaIhale_lblIhaleTuruUsulu")$getElementText()[[1]]
            kismiTeklif[k] <- maind$findElement("id","ucBirBakistaIhale_lblKismiTeklif")$getElementText()[[1]] == "Verilebilir"
            if(kismiTeklif[k]){
              kisimSayisi[k] <-  as.numeric(maind$findElement("id","ucBirBakistaIhale_lblKisimSayisi")$getElementText()[[1]])
            }else{
              kisimSayisi[k] <- NA
            }
            
            if(try(maind$findElement("id","ucBirBakistaIhale_lblOKASBilgileri")$getElementText()[[1]]) == error){
              OKAS[k] <- NA
              
            }else{
              OKAS[k] <- maind$findElement("id","ucBirBakistaIhale_lblOKASBilgileri")$getElementText()[[1]]
            }
            ihaleOnayTarihi[k] <- maind$findElement("id","ucBirBakistaIhale_lblIhaleOnayTarihi")$getElementText()[[1]]
            ilaninSekli[k] <- maind$findElement("id","ucBirBakistaIhale_lblIlaninTuru")$getElementText()[[1]]
            isYeri[k] <- maind$findElement("id","ucBirBakistaIhale_lblIsinYapilacagiYer")$getElementText()[[1]]
            ihaleYeri[k] <- maind$findElement("id","ucBirBakistaIhale_lblIhaleYerTarihSaat")$getElementText()[[1]]
            
            maind$findElement("xpath","//*[@id='ulTabs']/li[2]/a")$clickElement()
            Sys.sleep(0.25)
            
            enUstIdare[k] <- maind$findElement("id","ucBirBakistaIhale_lblBagliOlduguEnUstIdare")$getElementText()[[1]]
            bagliIdare[k] <- maind$findElement("id","ucBirBakistaIhale_lblBagliOlduguIdare")$getElementText()[[1]]
            idareAdi[k] <- maind$findElement("id","ucBirBakistaIhale_lblIdareAdi")$getElementText()[[1]]
            idareÝli[k] <- maind$findElement("id","ucBirBakistaIhale_lblIdareIli")$getElementText()[[1]]
            
            maind$findElement("xpath","//*[@id='ulTabs']/li[5]/a")$clickElement()
            Sys.sleep(0.25)
            sozlesmeBilgileri[k] <- maind$findElement("xpath","//*[@id='ucBirBakistaIhale_sozlesmeUpdate']/div[1]/div/div[2]/div/div[2]/p/span[2]")$getElementText()[[1]]
            yaklasikMaliyet[k] <- maind$findElement("xpath","//*[@id='ucBirBakistaIhale_sozlesmeUpdate']/div[1]/div/div[2]/div/div[3]/p/span[2]")$getElementText()[[1]]
            sirketAdi[k] <- maind$findElement("xpath","//*[@id='ucBirBakistaIhale_sozlesmeUpdate']/div[1]/div/div[1]")$getElementText()[[1]]
            enYuksekTeklif[k] <- maind$findElement("xpath","//*[@id='ucBirBakistaIhale_sozlesmeUpdate']/div[1]/div/div[2]/div/div[5]/p/span[2]")$getElementText()[[1]]
            enDusukTeklif[k] <- maind$findElement("xpath","//*[@id='ucBirBakistaIhale_sozlesmeUpdate']/div[1]/div/div[2]/div/div[6]/p/span[2]")$getElementText()[[1]]
            
            
            maind$switchToFrame(NA)
            #this part is related to the section with captcha and what lies behind.
            
            
            if(captchaIncluded & k %% nCheck == 0 ){ #this part will be ignored if the user does not want to scrape what's behind the captcha.
              
              maind$findElement("xpath","//*[@id='ulTabs']/li[4]/a")$clickElement()
              
              
              maind$findElement("xpath","//*[@id='ucBirBakistaIhale_dataListSonucTarihleri_ctl00_lnkNav']")$clickElement()
              
              
              
              
              #to the main frame
              #before selecting the frame in the pop-up window, we need to switch back to the main frame.
              
              Sys.sleep(0.25)
              maind$switchToFrame(NA)
              frames <- maind$findElements("tag","iframe")
              maind$switchToFrame(frames[[1]])
              
              
              
              
              
              
              if(try(maind$findElement("xpath","//*[@id='ctl00_btnCaptchaProtect']")$getElementText()[[1]],silent = T) != error){
                
                
                captchaEntry(maind,TRUE)
                
                
              }
              
              Sys.sleep(0.25)
              maind$switchToFrame(NA)
              frames <- maind$findElements("tag","iframe")
              maind$switchToFrame(frames[[1]])
              
              
              
              table <- maind$findElement("xpath","//*[@id='ctl00_ContentPlaceHolder1_lblIlan']/table/tbody")$getElementText() %>% str_split("\n")
              
              yaklasikMaliyet[k] <- (table[[1]][which(str_detect(table[[1]],"Yaklaþýk Maliyet") & !str_detect(table[[1]],"Sözleþme"))] %>% str_split("[:]"))[[1]][2] %>% str_trim()
              
              if(identical((table[[1]][which(str_detect(table[[1]],"Yaklaþýk Maliyet") & str_detect(table[[1]],"Sözleþme"))]),test)){
                sozlesmeyeEsasYM[k] <- NA
              }else{
                sozlesmeyeEsasYM[k] <- (table[[1]][which(str_detect(table[[1]],"Yaklaþýk Maliyet") & str_detect(table[[1]],"Sözleþme"))] %>% str_split("[:]"))[[1]][2] %>% str_trim()
              }
              
              
              sure[k] <- (table[[1]][which(str_detect(table[[1]],"Süresi"))] %>% str_split("[:]"))[[1]][2] %>% str_trim()
              
              
              dokSatinAlanSay[k] <- as.numeric((table[[1]][which(str_detect(table[[1]],"Doküman Satýn Alan Sayýsý"))] %>% str_split("[:]"))[[1]][2] %>% str_trim())
              eImzaliDL[k] <- as.numeric((table[[1]][which(str_detect(table[[1]],"e-imza kullanarak"))] %>% str_split("[:]"))[[1]][2] %>% str_trim())
              toplamTeklif[k] <- as.numeric((table[[1]][which(str_detect(table[[1]],"Toplam Teklif"))] %>% str_split("[:]"))[[1]][2] %>% str_trim())
              toplamGecerliTeklif[k] <- as.numeric((table[[1]][which(str_detect(table[[1]],"Geçerli Teklif"))] %>% str_split("[:]"))[[1]][2] %>% str_trim())
              yerliLehine[k] <- (table[[1]][which(str_detect(table[[1]],"fiyat avantajý"))] %>% str_split("[:]"))[[1]][2] %>% str_trim()
              sozTarihi[k] <- (table[[1]][which(str_detect(table[[1]],"Tarihi"))] %>% str_split("[:]"))[[1]][2] %>% str_trim()
              sozBedeli[k] <- (table[[1]][which(str_detect(table[[1]],"Bedeli"))] %>% str_split("[:]"))[[1]][2] %>% str_trim()
              sozYuklenici[k] <- (table[[1]][which(str_detect(table[[1]],"Yüklenici"))] %>% str_split("[:]"))[[1]][2] %>% str_trim()
              yukUyruk[k] <- (table[[1]][which(str_detect(table[[1]],"uyruðu"))] %>% str_split("[:]"))[[1]][2] %>% str_trim()
              yukAdres[k] <- (table[[1]][which(str_detect(table[[1]],"dresi"))] %>% str_split("[:]"))[[1]][2] %>% str_trim()
              
              maind$switchToFrame(NA)
              #close the pop-up (modal)
              maind$findElement("xpath","//*[@id='app']/div[1]/div/div[2]/div[1]/div/div/div[1]/button[2]/span")$clickElement()
              maind$switchToFrame(NA)
            }
            
          }else{
            
            maind$switchToFrame(NA)
            missingData <- append(missingData,results[[k]]$findChildElement("tag","h6")$getElementText()[[1]])
            
          }
          
          
          
          
          
          #close the frame
          try(maind$findElement("xpath",paste("//*[@id='sonuclar']/div[",k,"]/div/div[2]/button/span",sep = ""))$clickElement())
          
          
          
          
          
          
          if(k %% 32 == 0){
            results <- maind$findElement("xpath","//*[@id='sonuclar']")$findChildElements("class","col-sm-12")
            
          }
          
          if( k %% 10 == 0){
            message("Harvesting Ýhale Number: ",k)
            
          }
          
          k <- k+1
          message(paste("Elapsed time for a single element: ",Sys.time()-begin))
        }
        
        
        if(length(ihaleAdi) > 0){
          
          if(captchaIncluded){
            sdf <- data.frame(row.names = NULL,ihaleNo,ihaleAdi,ihaleUsulu,kismiTeklif,kisimSayisi,OKAS,ihaleOnayTarihi,ilaninSekli,isYeri,ihaleYeri,enUstIdare,bagliIdare,idareAdi,idareÝli,yaklasikMaliyet,sozlesmeyeEsasYM,sure,dokSatinAlanSay,eImzaliDL,toplamTeklif,toplamGecerliTeklif,yerliLehine,sozTarihi,sozBedeli,sozYuklenici,yukUyruk,sirketAdi,enDusukTeklif,enYuksekTeklif,yukAdres)
          }else{
            sdf <- data.frame(row.names = NULL,ihaleNo,ihaleAdi,ihaleUsulu,kismiTeklif,kisimSayisi,OKAS,ihaleOnayTarihi,ilaninSekli,isYeri,ihaleYeri,enUstIdare,bagliIdare,idareAdi,idareÝli,yaklasikMaliyet,sirketAdi,enDusukTeklif,enYuksekTeklif)
          }
          saveName <- paste(currentYear,currentProvince,turName,".rds",sep="")
          saveRDS(sdf,file = saveName)
        }
        
        
        
        
        
      }
      
      
      turCounter <- turCounter + 1
      
      
      
    }
    
    
    
    
    pCounter <- pCounter + 1
    
  }
  
  counter <- counter + 1 
} #durum form - "Sonuç Ýlaný Yayýmlanmýþ"