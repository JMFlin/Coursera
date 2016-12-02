my_data <- read.table("household_power_consumption.txt", header = TRUE, sep = ";")
my_subset <- my_data[my_data$Date %in% c("1/2/2007", "2/2/2007"), ]

a <- strptime(paste(my_subset$Date, my_subset$Time, sep=" "), "%d/%m/%Y %H:%M:%S") 
global <- as.numeric(my_subset$Global_active_power)

sub1 <- as.numeric(my_subset$Sub_metering_1)
sub2 <- as.numeric(my_subset$Sub_metering_2)
sub3 <- as.numeric(my_subset$Sub_metering_3)


png("plot3.png", width=480, height=480)
plot(a, sub1, type="l", ylab="Energy Submetering", xlab = "")
lines(a, sub2, type="l", col="red")
lines(a, sub3, type="l", col="blue")
legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=1, lwd=2.5, col=c("black", "red", "blue"))
dev.off()

