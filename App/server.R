library(shiny)
library(ggplot2)
library(caret)
library(reshape2)
Sys.setlocale("LC_TIME", "C")
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  weather <- read.csv("weather_2014.csv", sep=";", stringsAsFactors=FALSE, na.strings = c("Na", "NaN"))
  weather$dir.wind.8 <- weather$dir.wind 
  
  weather$dir.wind.8 <- ifelse(weather$dir.wind %in%  c("NNE","ENE"),
                               "NE",as.character(weather$dir.wind.8)) 
  
  weather$dir.wind.8 <- ifelse(weather$dir.wind %in% c("NNW","WNW"),
                               "NW",as.character(weather$dir.wind.8)) 
  
  weather$dir.wind.8 <- ifelse(weather$dir.wind %in% c("WSW","SSW"),
                               "SW",as.character(weather$dir.wind.8)) 
  
  weather$dir.wind.8 <- ifelse(weather$dir.wind %in% c("ESE","SSE"),
                               "SE",as.character(weather$dir.wind.8)) 
  
  # create factors, ordered by "levels" 
  weather$dir.wind.8 <- factor(weather$dir.wind.8,
                               levels = c("N","NE","E","SE","S","SW","W","NW"))
  
  
  weather$season <- factor(weather$season, levels = c("Spring","Summer","Autumn","Winter"))
  weather$day <- as.factor(weather$day)
  weather$month <- as.factor(weather$month)
  weather$dir.wind <- as.factor(weather$dir.wind)
  
  first.day <- "2014-01-01"
  first.day <- as.Date(first.day)
  weather$date  <- first.day + weather$day.count - 1 
  
  l.temp.time.date <- as.POSIXlt(paste(weather$date, weather$l.temp.time), tz = "GMT")
  #head(l.temp.time.date)
  
  # Round to the nearest hour
  l.temp.time.date <- round(l.temp.time.date,"hours")
  
  #attributes(l.temp.time.date)
  
  # Extract the value of the hour attribute as a number and add it to the data set
  weather$l.temp.hour <- l.temp.time.date[["hour"]]
  
  h.temp.time.date <- as.POSIXlt(paste(weather$date, weather$h.temp.time), tz = "GMT")
  #head(h.temp.time.date)
  
  h.temp.time.date <- round(h.temp.time.date,"hours")
  
  weather$h.temp.hour <- h.temp.time.date[["hour"]]
  
  gust.wind.time.date <- as.POSIXlt(paste(weather$date, weather$gust.wind.time), tz = "GMT")
  #head(gust.wind.time.date)
  
  gust.wind.time.date <- round(gust.wind.time.date,"hours")
  
  weather$gust.wind.time.hour <- gust.wind.time.date[["hour"]]
  
  weather$l.temp.hour <- as.numeric(weather$l.temp.hour)
  weather$h.temp.hour <- as.numeric(weather$h.temp.hour)
  weather$gust.wind.time.hour <- as.numeric(weather$gust.wind.time.hour)
  
  weather$month <- factor(weather$month,
                          labels = c("Jan","Feb","Mar","Apr",
                                     "May","Jun","Jul","Aug","Sep",
                                     "Oct","Nov","Dec"))
  
  weather$rained <- ifelse(weather$rain >= 1, "Yes", "No")
  weather$rained <- as.factor(weather$rained)
  
  weather$h.temp.quant <- cut(weather$h.temp, breaks = quantile(weather$h.temp),
                              labels = c("Cool","Mild","Warm","Hot"),include.lowest = T)
  
  temp <- weather[,sapply(weather,is.numeric)]
  temp <- temp[,!names(temp) %in% "day.count"]
  longtemp <- melt(temp, measure.vars = names(temp))
  vline.dat.mean <- aggregate(longtemp[,2], list(longtemp$variable), mean)
  vline.dat.median <- aggregate(longtemp[,2], list(longtemp$variable), median)
  names(vline.dat.mean)[1] <- "variable"
  names(vline.dat.median)[1] <- "variable" 
  
  longtemp <- cbind(longtemp, weather[,"rained"])
  names(longtemp) <- c("variable", "value", "rained")
  vline.dat.mean <- aggregate(longtemp[,2], list(longtemp$variable), mean)
  vline.dat.median <- aggregate(longtemp[,2], list(longtemp$variable), median)
  names(vline.dat.mean)[1] <- "variable"
  names(vline.dat.median)[1] <- "variable" 

  
  output$p1 <- renderPlot({
   
    bins <- input$bins

    ggplot(longtemp,aes(x=value))+
      geom_histogram(aes(y = ..density..,  fill = rained), colour = "black", bins=bins)+
      geom_density() + 
      theme(axis.line = element_line(), 
            axis.text=element_text(color='black'), axis.title = element_text(colour = 'black'), 
            legend.text=element_text(), legend.title=element_text())+
      geom_vline(aes(xintercept = x), data = vline.dat.mean, linetype = "longdash", color = "blue")+
      geom_vline(aes(xintercept = x), data = vline.dat.median, linetype = "longdash", color = "red")+
      xlab("")+ 
      facet_wrap(~ variable, ncol = 3, scales = "free")
  })
  
  output$p2 <- renderPlot({
    
    a <- input$percent
    
    index <- createDataPartition(weather$rain, p = 2/3, list = FALSE)
    train <- weather[ index,]
    test  <- weather[-index,]
    
    train$h.temp.hour <- as.numeric(train$h.temp.hour)
    train$l.temp.hour <- as.numeric(train$l.temp.hour)
    train$gust.wind.time.hour <- as.numeric(train$gust.wind.time.hour)
    test$h.temp.hour <- as.numeric(test$h.temp.hour)
    test$l.temp.hour <- as.numeric(test$l.temp.hour)
    test$gust.wind.time.hour <- as.numeric(test$gust.wind.time.hour)
    
    group <- rep(NA,nrow(train) + nrow(test))
    group <- ifelse(seq(1,nrow(train) + nrow(test)) %in% index,"Train","Test")
    df <- data.frame(date=weather$date,rain=weather$rain, group)
    
    grid <- data.frame(ncomp = input$percent)
    
    ctrl <- trainControl(method = "cv",
                         number = 3,
                         savePredictions = TRUE)
    
    pcr <- train(rain ~ month + season + l.temp + h.temp + ave.temp + ave.wind +
                   gust.wind + dir.wind.8 + h.temp.hour + l.temp.hour +
                   gust.wind.time.hour, 
                 data = train,
                 method = "pcr",
                 trControl = ctrl,
                 metric="RMSE",
                 tuneGrid = grid)
    test.pred.comp <- predict(pcr,test)
    test.pred.comp <- ifelse(test.pred.comp < 0 , 0, test.pred.comp)
    pcr.pred <- data.frame(date = test$date, actual = test$rain,
                           predicted = test.pred.comp)
    pcr.pred <- melt(pcr.pred, c("actual", "predicted"), id = "date")
    
    ggplot(pcr.pred, aes(x = date, y = value, color = variable))+
      geom_point() + 
      scale_color_discrete(name="") + 
      ggtitle("Actual vs Predicted for Principal Components") + 
      theme(legend.position="right") + 
      ylab("Rain (mm)") + 
      xlab("Date")+ 
      theme(axis.line = element_line(),
            axis.text=element_text(color='black'),
            axis.title = element_text(colour = 'black'), 
            legend.text=element_text(), 
            legend.title=element_text(), 
            legend.key = element_rect(colour = "black")) 
  })
})