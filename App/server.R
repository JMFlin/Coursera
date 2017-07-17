library(shiny)
library(ggplot2)
library(caret)
library(reshape2)
Sys.setlocale("LC_TIME", "C")
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  weather <- mtcars
  
  weather$mpg_ind <- ifelse(weather$mpg > 20, 1, 0)
  temp <- weather[,sapply(weather,is.numeric)]
  temp <- temp[,!names(temp) %in% "day.count"]
  longtemp <- melt(temp, measure.vars = names(temp))
  vline.dat.mean <- aggregate(longtemp[,2], list(longtemp$variable), mean)
  vline.dat.median <- aggregate(longtemp[,2], list(longtemp$variable), median)
  names(vline.dat.mean)[1] <- "variable"
  names(vline.dat.median)[1] <- "variable" 
  
  longtemp <- cbind(longtemp, weather[,"mpg_ind"])
  names(longtemp) <- c("variable", "value", "mpg_ind")
  vline.dat.mean <- aggregate(longtemp[,2], list(longtemp$variable), mean)
  vline.dat.median <- aggregate(longtemp[,2], list(longtemp$variable), median)
  names(vline.dat.mean)[1] <- "variable"
  names(vline.dat.median)[1] <- "variable" 
  
  index <- createDataPartition(weather$mpg, p = 2/3, list = FALSE)
  train <- weather[ index,]
  test  <- weather[-index,]
  
  group <- rep(NA,nrow(train) + nrow(test))
  group <- ifelse(seq(1,nrow(train) + nrow(test)) %in% index,"train","Test")
  df <- data.frame(disp=weather$disp,mpg=weather$mpg, group)
  
  
  
  ctrl <- trainControl(method = "cv",
                       number = 3,
                       savePredictions = TRUE)
  
  output$p1 <- renderPlot({
    
    bins <- input$bins
    
    ggplot(longtemp,aes(x=value))+
      geom_histogram(aes(y = ..density..,  fill = mpg_ind), colour = "black", bins=bins)+
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
    
   
    
    grid <- data.frame(ncomp = seq(2,3,1))
    
    pcr <- train(mpg ~ cyl+disp+hp+drat+wt+qsec+vs+am+gear+carb, 
                 data = train,
                 method = "lm",
                 trControl = ctrl,
                 metric="RMSE")
    test.pred.comp <- predict(pcr,test)
    test.pred.comp <- ifelse(test.pred.comp < 0 , 0, test.pred.comp)
    pcr.pred <- data.frame(disp = test$disp, actual = test$mpg,
                           predicted = test.pred.comp)
    pcr.pred <- melt(pcr.pred, c("actual", "predicted"), id = "disp")
    
    ggplot(pcr.pred, aes(x = disp, y = value, color = variable))+
      geom_point() + 
      scale_color_discrete(name="") + 
      ggtitle("Actual vs Predicted for Principal Components") + 
      theme(legend.position="right") + 
      ylab("mpg") + 
      xlab("disp")+ 
      theme(axis.line = element_line(),
            axis.text=element_text(color='black'),
            axis.title = element_text(colour = 'black'), 
            legend.text=element_text(), 
            legend.title=element_text(), 
            legend.key = element_rect(colour = "black")) 
  })
})