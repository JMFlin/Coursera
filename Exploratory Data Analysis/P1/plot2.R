my_data <- read.table("household_power_consumption.txt", header = TRUE, sep = ";")
my_subset <- my_data[my_data$Date %in% c("1/2/2007", "2/2/2007"), ]

a <- strptime(paste(my_subset$Date, my_subset$Time, sep=" "), "%d/%m/%Y %H:%M:%S") 
global <- as.numeric(my_subset$Global_active_power)

png("plot2.png", width=480, height=480)
plot(a, global, type="l", xlab="", ylab="Global Active Power (kilowatts)")
dev.off()

