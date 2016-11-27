corr <- function(directory, threshold = 0) {
    
    if(directory == "specdata") {
        directory <- ("./specdata/")
    }
    
    a <- complete("specdata", 1:332)
    nobs <- a$nobs

    b <- a$id[nobs > threshold]

    b_length <- length(b)
    corr_vector <- numeric(b_length)

    all_files <- as.character(list.files(directory))
    file_paths <- paste(directory, all_files, sep="")
    
    j <- 1
    for(i in b) {
        file <- read.csv(file_paths[i], header=T, sep=",")
        corr_vector[j] <- cor(file$sulfate, file$nitrate, use="complete.obs")
        j <- j + 1
    }
    
    result <- corr_vector
    return(result) 
}

cr <- corr("specdata")
cr <- sort(cr)
set.seed(868)
out <- round(cr[sample(length(cr), 5)],4)
out

cr <- corr("specdata", 129)
cr <- sort(cr)
n <- length(cr)
set.seed(197)
out <- c(n, round(cr[sample(n,5)],4))
out

cr <- corr("specdata", 2000)
n <- length(cr)
cr <- corr("specdata", 1000)
cr <- sort(cr)
c(n, round(cr,4))
