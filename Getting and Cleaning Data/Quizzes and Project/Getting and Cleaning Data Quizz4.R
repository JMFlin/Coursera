fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileurl, destfile = "ACS_US.csv")

my_data <- read.csv("ACS_US.csv")

strsplit(names(my_data)[123], split = "wgtp")

#-------------------

fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(fileurl, destfile = "GDP_DATA.csv")

a <- read.csv("GDP_DATA.csv", skip = 4, header = TRUE)
a <- a[1:190,]
b <- gsub(",","",a$X.4)
c <- as.numeric(b)
mean(c)

#-----------------

names(a) <- c("code", "rank", "NA","countryNames", "GDP", "Empty", "a", "b", "c", "d")
countryNames <- a[1:190, 4]
grep("*United",a$countryNames)
grep("*United",a$countryNames)
grep("^United",a$countryNames)
grep("^United",a$countryNames)

#----------------
GDP <- read.csv("GDP_DATA.csv", header = FALSE, skip = 5)
GDP <- GDP[1:190,]
TEST <- read.csv("TEST_DATA.csv")
a <- merge(GDP, TEST, by.x = "V1", by.y = "CountryCode", sort = T)

length(grep("Fiscal year end: June", a$Special.Notes))

#----------------

library(quantmod)
library(lubridate)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn) 

bool1 <- year(sampleTimes) == 2012
table(bool1)

bool2 <- year(sampleTimes) == 2012 & weekdays(sampleTimes) == "Monday"
table(bool2)

