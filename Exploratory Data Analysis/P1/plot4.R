my_data <- read.table("household_power_consumption.txt", header = TRUE, sep = ";")
my_subset <- my_data[my_data$Date %in% c("1/2/2007", "2/2/2007"), ]

a <- strptime(paste(my_subset$Date, my_subset$Time, sep=" "), "%d/%m/%Y %H:%M:%S") 
global <- as.numeric(my_subset$Global_active_power)

sub1 <- as.numeric(my_subset$Sub_metering_1)
sub2 <- as.numeric(my_subset$Sub_metering_2)
sub3 <- as.numeric(my_subset$Sub_metering_3)

globalPower <- as.numeric(my_subset$Global_reactive_power)
volt <- as.numeric(my_subset$Voltage)

png("plot4.png", width=480, height=480)
par(mfrow = c(2, 2)) 
plot(a, global, type="l", xlab="", ylab="Global Active Power", cex=0.2)
plot(a, volt, type="l", xlab="datetime", ylab="Voltage")
plot(a, sub1, type="l", ylab="Energy Submetering", xlab="")
lines(a, sub2, type="l", col="red")
lines(a, sub3, type="l", col="blue")
legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lwd = 2,col=c("black", "red", "blue"), bty="o")
plot(a, globalPower, type="l", xlab="datetime", ylab="Global_reactive_power")
dev.off()

?plot
