my_data <- read.table("household_power_consumption.txt", header = TRUE, sep = ";")
my_subset <- my_data[my_data$Date %in% c("1/2/2007", "2/2/2007"), ]

global <- as.numeric(my_subset$Global_active_power)
png("plot1.png", width=480, height=480)
hist(global, col="red", main="Global Active Power", xlab="Global Active Power (kilowatts)")
dev.off()
