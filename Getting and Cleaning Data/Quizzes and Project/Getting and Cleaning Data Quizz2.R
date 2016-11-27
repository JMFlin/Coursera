library(httr)
oauth_endpoints("github")

myapp <- oauth_app("github", "ClientID", "ClientSecret")
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)
req <- GET("https://api.github.com/rate_limit", config(token = github_token))
stop_for_status(req)
content(req)

BROWSE("https://api.github.com/users/jtleek/repos",authenticate("Access Token","x-oauth-basic","basic"))

#---------------------
library(data.table)
library(sqldf)
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv "
download.file(fileURL, destfile="ACS_data.csv")

acs <- fread("ACS_data.csv")
acs <- data.frame(acs)

sqldf("select pwgtp1 from acs where AGEP < 50")
sqldf("select distinct AGEP from acs")

#---------------------
library(XML)
fileURL <- "http://biostat.jhsph.edu/~jleek/contact.html"
download.file(fileURL, destfile = "contact.xml")

con <- url(fileURL)
htmlCode <- readLines(con)
close(con)
sapply(htmlCode[c(10, 20, 30, 100)], nchar)

#--------------------

a <- read.csv("getdata_wksst8110.for", header = T)
head(a)
dim(a)

df <- read.fwf(file = "getdata_wksst8110.for", widths=c(-1,9,-5,4,4,-5,4,4,-5,4,4,-5,4,4), skip = 4)
head(df)
sum(df[,4])
