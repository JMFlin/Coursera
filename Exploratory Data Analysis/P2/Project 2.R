setwd("C:/Users/JMFlin/Desktop/R/R files/Exploratory Data Analysis/Data")

## load data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
#Make a plot showing the total PM2.5 emission from all sources for each of the years
#1999, 2002, 2005, and 2008.

tmp <- aggregate(NEI$Emissions, by = list(NEI$year), FUN = "sum")

png("plot1.png", width=480, height=480)
plot(tmp, type = "l", xlab = "Year", 
     main = "Total Emissions (PM2.5) in the United States from 1999 to 2008", ylab = "PM2.5")
dev.off()


#---2

## load data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008?

tmp <- subset(NEI, fips == "24510", select = c(Emissions, year))

tmp <- aggregate(tmp$Emissions, by = list(tmp$year), FUN = "sum")

png("plot2.png", width=480, height=480)
plot(tmp, type = "l", xlab = "Year", 
     main = "Total Emissions (PM2.5) in Maryland from 1999 to 2008", ylab = "PM2.5")
dev.off()

#---3

## load data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
#which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
#Which have seen increases in emissions from 1999-2008?

library(ggplot2)
library(gridExtra)

tmp <- subset(NEI, fips == "24510", select = c(Emissions, year, type))
a <- aggregate(Emissions ~ type + year, data = tmp, sum)

png("plot3.png", width=480, height=480)
b1 <- ggplot(a[a$type == unique(a$type)[1],], aes(x = year, y = Emissions))+geom_line(alpha = .7) + geom_point() +
    labs(title = paste(unique(a$type)[1]), x = 'Year', y = 'Emissions')
b2 <- ggplot(a[a$type == unique(a$type)[2],], aes(x = year, y = Emissions))+geom_line(alpha = .7) + geom_point() +
    labs(title = paste(unique(a$type)[2]), x = 'Year', y = 'Emissions')
b3 <- ggplot(a[a$type == unique(a$type)[3],], aes(x = year, y = Emissions))+geom_line(alpha = .7) + geom_point() +
    labs(title = paste(unique(a$type)[3]), x = 'Year', y = 'Emissions')
b4 <- ggplot(a[a$type == unique(a$type)[4],], aes(x = year, y = Emissions))+geom_line(alpha = .7) + geom_point() +
    labs(title = paste(unique(a$type)[4]), x = 'Year', y = 'Emissions')
grid.arrange(b1, b2, b3, b4, ncol=2)
dev.off()


#---4

## load data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Across the United States, how have emissions from coal combustion-related sources changed from 1999-2008?

library(ggplot2)

coal <- grep("coal", SCC$Short.Name, ignore.case = T)
coal <- SCC[coal, ]
coal <- NEI[NEI$SCC %in% coal$SCC, ]

coalEmissions <- aggregate(coal$Emissions, list(coal$year), FUN = "sum")

png("plot4.png", width=480, height=480)
ggplot(coalEmissions, aes(x = Group.1, y = x))+geom_line(alpha = .7) + geom_point() +
    labs(title = "Total Emissions From Coal Combustion-related\n Sources from 1999 to 2008", x = 'Year', y = 'Emissions')
dev.off()


#---5

## load data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?

library(ggplot2)

tmp <- subset(NEI, fips == "24510", select = c(Emissions, year, SCC))

motor <- grep("Vehicle", SCC$EI.Sector, ignore.case = T)
motor <- SCC[motor, ]
motor <- tmp[tmp$SCC %in% motor$SCC, ]

motorEmissions <- aggregate(motor$Emissions, list(motor$year), FUN = "sum")

png("plot5.png", width=480, height=480)
ggplot(motorEmissions, aes(x = Group.1, y = x))+geom_line(alpha = .7) + geom_point() +
    labs(title = "Total Emissions From Motor Vehicle\n Sources from 1999 to 2008", x = 'Year', y = 'Emissions')
dev.off()


#---6

## load data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Compare emissions from motor vehicle sources in Baltimore City with emissions 
#from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
#Which city has seen greater changes over time in motor vehicle emissions?

tmp_Balt <- subset(NEI, fips == "24510", select = c(Emissions, year, SCC))

motor_Balt <- grep("Vehicle", SCC$EI.Sector, ignore.case = T)
motor_Balt <- SCC[motor_Balt, ]
motor_Balt <- tmp_Balt[tmp_Balt$SCC %in% motor_Balt$SCC, ]

motorEmissions_Balt <- aggregate(motor_Balt$Emissions, list(motor_Balt$year), FUN = "sum")


tmp_LA <- subset(NEI, fips == "06037", select = c(Emissions, year, SCC))

motor_LA <- grep("motor", SCC$Short.Name, ignore.case = T)
motor_LA <- SCC[motor_LA, ]
motor_LA <- tmp_LA[tmp_LA$SCC %in% motor_LA$SCC, ]

motorEmissions_LA <- aggregate(motor_LA$Emissions, list(motor_LA$year), FUN = "sum")

png("plot6.png", width=480, height=480)
b1 <- ggplot(motorEmissions_Balt, aes(x = Group.1, y = x))+geom_line(alpha = .7) + geom_point() +
    labs(title = "Maryland", x = 'Year', y = 'Emissions')
b2 <- ggplot(motorEmissions_LA, aes(x = Group.1, y = x))+geom_line(alpha = .7) + geom_point() +
    labs(title = "California", x = 'Year', y = 'Emissions')
grid.arrange(b1, b2, ncol=1)
dev.off()