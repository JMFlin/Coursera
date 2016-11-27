best <- function(state, outcome) {
    my_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    data_attack <- suppressWarnings(data.frame(as.numeric(my_data[, 11]), my_data[, 2], my_data[, 7])) # heart attack
    data_failure <- suppressWarnings(data.frame(as.numeric(my_data[, 17]), my_data[, 2], my_data[, 7])) # heart failure
    data_pneumonia <- suppressWarnings(data.frame(as.numeric(my_data[, 23]), my_data[, 2], my_data[, 7])) # pneumonia
    
    disease_names <- c("heart attack", "heart failure", "pneumonia")
    
    if(!state %in% my_data$State){
        stop("invalid state")
    } else if(!outcome %in% disease_names){
        stop("invalid outcome")
    } else if(outcome == "heart attack") {
        
        state_subset <- data.frame(data_attack[data_attack[ ,3] == state, 1], data_attack[data_attack[ ,3] == state, 2])
        
        state_subset <- na.omit(state_subset)
                
        min_attack <- min(state_subset[,1], na.rm = T)
        name_attack <- state_subset[state_subset == min_attack, 2]
        data.frame(name_attack)
        as.vector(name_attack)
        
    }else if(outcome == "heart failure") {
        
        state_subset <- data.frame(data_failure[data_failure[ ,3] == state, 1], data_failure[data_failure[ ,3] == state, 2])
        
        state_subset <- na.omit(state_subset)
        
        min_failure <- min(state_subset[,1], na.rm = T)
        name_failure <- state_subset[state_subset == min_failure, 2]
        data.frame(name_failure)
        as.vector(name_failure)
        
    }else if(outcome == "pneumonia") {
        
        state_subset <- data.frame(data_pneumonia[data_pneumonia[ ,3] == state, 1], data_pneumonia[data_pneumonia[ ,3] == state, 2])
        
        state_subset <- na.omit(state_subset)
        
        min_pneumonia <- min(state_subset[,1], na.rm = T)
        name_pneumonia <- state_subset[state_subset == min_pneumonia, 2]
        data.frame(name_pneumonia)
        as.vector(name_pneumonia)
    }
}

best("AL", "pneumonia")

