plot3 <- function() {
  ## Read the provided Dataset: "Electric power consumption"
  myData <- read.table("household_power_consumption.txt", header = TRUE, sep = ";", colClasses = "character", na.strings = "?")
  
  ## Convert the Date column and find the measurements over a 2-day period in February, 2007 (2007-02-01 and 2007-02-02)
  myData$Date = strptime(myData$Date, "%d/%m/%Y", tz="")
  mySubsetData <- subset(myData, Date >= "2007-02-01" & Date <= "2007-02-02")
  
  ## Remove the missing NA values
  mySubsetData <- na.omit(mySubsetData)
  
  ## Set the plot layout with 1 row and 1 column
  par(mfrow = c(1, 1))
  
  ## Open PNG graphic file device with a width of 480 pixels and a height of 480 pixels
  png(file = "plot3.png", width = 480, height = 480)
  
  ## Generate & annotate a blank plot & add lines and legend for 'Energy sub metering' measurements for the 2-day period using Date & Time observations
  with(mySubsetData, {
    plot(strptime(paste(Date, Time), "%Y-%m-%d %H:%M:%S"), as.numeric(Sub_metering_1), type="n", xlab = "", ylab = "Energy sub metering")
    lines(strptime(paste(mySubsetData$Date, mySubsetData$Time), "%Y-%m-%d %H:%M:%S"), as.numeric(mySubsetData$Sub_metering_1), col = "black")
    lines(strptime(paste(mySubsetData$Date, mySubsetData$Time), "%Y-%m-%d %H:%M:%S"), as.numeric(mySubsetData$Sub_metering_2), col = "red")
    lines(strptime(paste(mySubsetData$Date, mySubsetData$Time), "%Y-%m-%d %H:%M:%S"), as.numeric(mySubsetData$Sub_metering_3), col = "blue")
    legend("topright", lty=c(1, 1, 1), col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  })
  
  ## Close the file device 
  dev.off()
}


