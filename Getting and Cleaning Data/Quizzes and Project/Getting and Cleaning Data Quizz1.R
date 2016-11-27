library(data.table)
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileURL, destfile="ACS2.csv")

DT <- fread("ACS2.csv")

a <- DT$VAL
table(a[a == 24])

#-----------------------

library(xlsx)

fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx "
download.file(fileURL, destfile = "NattyGas.xlsx")

rowIndex <- 18:27
colIndex <- 7:15
dat <- read.xlsx("NattyGas.xlsx", sheetIndex = 1, rowIndex = rowIndex, colIndex = colIndex, header = TRUE)

#-----------------------

library(XML)
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
download.file(fileURL, destfile = "balt.xml")

doc <- xmlTreeParse('balt.xml', useInternalNodes = TRUE)

rootNode <- xmlRoot(doc)

zipcode <- xpathSApply(rootNode, "//zipcode", xmlValue)

length(zipcode[zipcode==21231])

#------------------------

fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
download.file(fileURL, destfile="Fss.csv")

DT <- fread("Fss.csv")

system.time(DT[,mean(pwgtp15),by=SEX])
system.time(mean(DT[DT$SEX==1,]$pwgtp15))+system.time(mean(DT[DT$SEX==2,]$pwgtp15))
system.time(sapply(split(DT$pwgtp15,DT$SEX),mean))
system.time(mean(DT$pwgtp15,by=DT$SEX))
system.time(tapply(DT$pwgtp15,DT$SEX,mean))
system.time(rowMeans(DT)[DT$SEX==1])+system.time(rowMeans(DT)[DT$SEX==2])
