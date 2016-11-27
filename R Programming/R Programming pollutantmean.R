pollutantmean <- function(directory, pollutant, id = 1:332) {
    
    files_list <- list.files(directory, full.names = T)
    dat <- data.frame()
    
    for(i in id){
        dat <- rbind(dat, read.csv(files_list[i]))
    }
    
    mean(dat[, pollutant], na.rm =T)
}

pollutantmean("specdata", "sulfate", 1:10)

pollutantmean("specdata", "nitrate", 70:72)

pollutantmean("specdata", "sulfate", 34)

pollutantmean("specdata", "nitrate")
