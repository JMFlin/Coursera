rankall <- function(outcome, num = "best") {
    my_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    data_attack <- suppressWarnings(data.frame(as.numeric(my_data[, 11]), my_data[, 2], my_data[, 7])) # heart attack
    data_failure <- suppressWarnings(data.frame(as.numeric(my_data[, 17]), my_data[, 2], my_data[, 7])) # heart failure
    data_pneumonia <- suppressWarnings(data.frame(as.numeric(my_data[, 23]), my_data[, 2], my_data[, 7])) # pneumonia
     
    disease_names <- c("heart attack", "heart failure", "pneumonia")
    
    if(!outcome %in% disease_names){
        stop("invalid outcome")
    }else if(num == "best"){
        data_attack
    }else if(num == "worst"){
        
        if(outcome == "heart attack") {
            
            names(data_attack) <- c("score", "hospital", "state")
            
            sorted <- data_attack[order(data_attack$state, -data_attack$score), ]
            sorted <- na.omit(sorted)
            i <- 1
            result <- data.frame(matrix(ncol = 3, nrow = 1))
            names(result) <- c("score", "hospital", "state")
            for(i in 1:54){
                k <- as.vector(sorted[1,3])
                me <- data.frame(subset(sorted,sorted$state == k))
                me <- me[order(-me$score, me$hospital), ]
                
                result <- rbind(result,me[1, ])
                
                sorted <- sorted[-c(1:dim(me)[1]), ]
            }
            
            result <- na.omit(result)
            result$score <- NULL
            row.names(result) <- result$state
            result
            
        }else if(outcome == "pneumonia") {
            
            names(data_pneumonia) <- c("score", "hospital", "state")
            
            sorted <- data_pneumonia[order(data_pneumonia$state, -data_pneumonia$score), ]
            sorted <- na.omit(sorted)
            i <- 1
            result <- data.frame(matrix(ncol = 3, nrow = 1))
            names(result) <- c("score", "hospital", "state")
            for(i in 1:54){
                k <- as.vector(sorted[1,3])
                me <- data.frame(subset(sorted,sorted$state == k))
                me <- me[order(-me$score, me$hospital), ]
        
                result <- rbind(result,me[1, ])
                
                sorted <- sorted[-c(1:dim(me)[1]), ]
            }
            
            result <- na.omit(result)
            result$score <- NULL
            row.names(result) <- result$state
            result
        }else if(outcome == "heart failure") {
            
            names(data_failure) <- c("score", "hospital", "state")
            
            sorted <- data_failure[order(data_failure$state, -data_failure$score), ]
            sorted <- na.omit(sorted)
            i <- 1
            result <- data.frame(matrix(ncol = 3, nrow = 1))
            names(result) <- c("score", "hospital", "state")
            for(i in 1:54){
                k <- as.vector(sorted[1,3])
                me <- data.frame(subset(sorted,sorted$state == k))
                me <- me[order(-me$score, me$hospital), ]
                
                result <- rbind(result,me[1, ])
                
                sorted <- sorted[-c(1:dim(me)[1]), ]
            }
            
            result <- na.omit(result)
            result$score <- NULL
            row.names(result) <- result$state
            result
        }
        
    }else if(!num == "worst" & !num == "best"){
        if(outcome == "heart attack") {
            
            names(data_attack) <- c("score", "hospital", "state")
            
            sorted <- data_attack[order(data_attack$state, data_attack$score), ]
            sorted <- na.omit(sorted)
            i <- 1
            result <- data.frame(sorted[1,])
            for(i in 1:54){
                k <- as.vector(sorted[1,3])
                me <- data.frame(subset(sorted,sorted$state == k))
                me <- me[order(me$score, me$hospital), ]
                if(dim(me)[1] >= num){
                    result <- rbind(result,me[num, ])
                }else{
                    me[1,2] <- NA
                    result <- rbind(result,me[1,])
                }
                sorted <- sorted[-c(1:dim(me)[1]), ]
            }
            
            result <- result[-c(1),]
            result$score <- NULL
            row.names(result) <- result$state
            result
        }else if(outcome == "pneumonia") {
            
            names(data_pneumonia) <- c("score", "hospital", "state")
            
            sorted <- data_pneumonia[order(data_pneumonia$state, data_pneumonia$score), ]
            sorted <- na.omit(sorted)
            i <- 1
            result <- data.frame(sorted[1,])
            for(i in 1:54){
                k <- as.vector(sorted[1,3])
                me <- data.frame(subset(sorted,sorted$state == k))
                me <- me[order(me$score, me$hospital), ]
                if(dim(me)[1] >= num){
                    result <- rbind(result,me[num, ])
                }else{
                    me[1,2] <- NA
                    result <- rbind(result,me[1,])
                }
                sorted <- sorted[-c(1:dim(me)[1]), ]
            }
        
            result <- result[-c(1),]
            result$score <- NULL
            row.names(result) <- result$state
            result
        }else if(outcome == "heart failure") {
            
            names(data_failure) <- c("score", "hospital", "state")
            
            sorted <- data_failure[order(data_failure$state, data_failure$score), ]
            sorted <- na.omit(sorted)
            i <- 1
            result <- data.frame(sorted[1,])
            for(i in 1:54){
                k <- as.vector(sorted[1,3])
                me <- data.frame(subset(sorted,sorted$state == k))
                me <- me[order(me$score, me$hospital), ]
                if(dim(me)[1] >= num){
                    result <- rbind(result,me[num, ])
                }else{
                    me[1,2] <- NA
                    result <- rbind(result,me[1,])
                }
                sorted <- sorted[-c(1:dim(me)[1]), ]
            }
            
            result <- result[-c(1),]
            result$score <- NULL
            row.names(result) <- result$state
            result
        }
    }
}

tail(rankall("pneumonia", "worst"), 3)
rankall("pneumonia", "worst")
rankall("heart failure", 10)

#rankall("heart attack", "best") NOT DONE
