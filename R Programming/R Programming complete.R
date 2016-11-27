complete <- function(directory, id = 1:332){
    
    if(directory == "specdata") {
        directory <- ("./specdata/")
    }
    
    id_len <- length(id)
    complete_data <- numeric(id_len)

    all_files <- as.character(list.files(directory))
    file_paths <- paste(directory, all_files, sep="")
    
    j <- 1 
    for (i in id) {
        current_file <- read.csv(file_paths[i], header=T, sep=",")
        complete_data[j] <- sum(complete.cases(current_file))
        j <- j + 1
    }
    result <- data.frame(id = id, nobs = complete_data)
    return(result)
}



cc <- complete("specdata", c(6,10,20,34,100,200,310))
print(cc$nobs)

cc <- complete("specdata", 54)
print(cc$nobs)

set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332,10)
print(cc[use, "nobs"])
