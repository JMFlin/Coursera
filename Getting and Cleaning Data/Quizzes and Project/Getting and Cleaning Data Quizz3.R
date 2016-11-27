fileURL <- ("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv ")
download.file(fileURL, destfile = "ACS_2")

ACS <- read.csv("ACS_2")
agricultureLogical <- ifelse(ACS$AGS > 5 & ACS$ACR > 2, TRUE,FALSE)
which(agricultureLogical)

#-----------------------------
install.packages("jpeg")
library(jpeg)

fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg "
download.file(fileURL, destfile = "jpeg.jpeg", mode = "wb")

picture <- readJPEG("jpeg.jpeg", native = TRUE)

quantile(picture, probs = c(0.3,0.8))

#----------------------------

#WRONG

fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(fileURL, destfile = "GDP_DATA.csv")
download.file(fileurl, destfile = "TEST_DATA.csv")

GDP <- read.csv("GDP_DATA.csv", header = FALSE, skip = 5)
GDP <- GDP[1:190,]
TEST <- read.csv("TEST_DATA.csv")
a <- merge(GDP, TEST, by.x = "V1", by.y = "CountryCode", sort = T)

arrange(a, a$V2)

#-------------------------
b <- data.frame(a$Income.Group, a$V2)
b <- filter(b, a.Income.Group == "High income: OECD")
mean(b$a.V2)

c <- data.frame(a$Income.Group, a$V2) 
c <- filter(c, a.Income.Group == "High income: nonOECD")
mean(c$a.V2)

#------------------------

quantiles <- c(0.2,0.4,0.6,0.8,1)
q <- quantile(a$V2, quantiles)
q1 <- a$V2 <= 38

xtabs(q1 ~ a$Income.Group)
