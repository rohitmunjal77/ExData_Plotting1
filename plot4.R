plot4 <- function() {
  ## Read the provided Dataset: "Electric power consumption"
  myData <- read.table("household_power_consumption.txt", header = TRUE, sep = ";", colClasses = "character", na.strings = "?")
  
  ## Convert the Date column and find the measurements over a 2-day period in February, 2007 (2007-02-01 and 2007-02-02)
  myData$Date = strptime(myData$Date, "%d/%m/%Y", tz="")
  mySubsetData <- subset(myData, Date >= "2007-02-01" & Date <= "2007-02-02")
  
  ## Remove the missing NA values
  mySubsetData <- na.omit(mySubsetData)
  
  ## Set the plot layout with 1 row and 1 column
  par(mfcol = c(2, 2), mar = c(4.5, 4.5, 1, 1), oma = c(0, 0, 0, 0))
  
  ## Generate 4 plots
  with(mySubsetData, {
    ## Generate & annotate histogram for 'Global Active Power' measurements for the 2-day period using Date observations
    plot(strptime(paste(Date, Time), "%Y-%m-%d %H:%M:%S"), as.numeric(Global_active_power), type="n", xlab = "", ylab = "Global Active Power")
    lines(strptime(paste(mySubsetData$Date, mySubsetData$Time), "%Y-%m-%d %H:%M:%S"), as.numeric(mySubsetData$Global_active_power))
    
    ## Generate & annotate a blank plot & add lines and legend for 'Energy sub metering' measurements for the 2-day period using Date & Time observations
    plot(strptime(paste(Date, Time), "%Y-%m-%d %H:%M:%S"), as.numeric(Sub_metering_1), type="n", xlab = "", ylab = "Energy sub metering")
    lines(strptime(paste(mySubsetData$Date, mySubsetData$Time), "%Y-%m-%d %H:%M:%S"), as.numeric(mySubsetData$Sub_metering_1), col = "black")
    lines(strptime(paste(mySubsetData$Date, mySubsetData$Time), "%Y-%m-%d %H:%M:%S"), as.numeric(mySubsetData$Sub_metering_2), col = "red")
    lines(strptime(paste(mySubsetData$Date, mySubsetData$Time), "%Y-%m-%d %H:%M:%S"), as.numeric(mySubsetData$Sub_metering_3), col = "blue")
    legend("topright", lty=c(1, 1, 1), col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), cex = 0.8)
    
    ## Generate & annotate a blank plot & add lines for 'Voltage' measurements for the 2-day period using Date & Time observations
    plot(strptime(paste(Date, Time), "%Y-%m-%d %H:%M:%S"), as.numeric(Voltage), type="n", xlab = "datetime", ylab = "Voltage")
    lines(strptime(paste(mySubsetData$Date, mySubsetData$Time), "%Y-%m-%d %H:%M:%S"), as.numeric(mySubsetData$Voltage))
    
    ## Generate & annotate a blank plot & add lines for 'Global_reactive_power' measurements for the 2-day period using Date & Time observations
    plot(strptime(paste(Date, Time), "%Y-%m-%d %H:%M:%S"), as.numeric(Global_reactive_power), type="n", xlab = "datetime", ylab = "Global_reactive_power")
    lines(strptime(paste(mySubsetData$Date, mySubsetData$Time), "%Y-%m-%d %H:%M:%S"), as.numeric(mySubsetData$Global_reactive_power))
  })
  
  ## Copy plot to PNG graphic file device with a width of 480 pixels and a height of 480 pixels
  dev.copy(png, file = "plot4.png", width = 480, height = 480)
  ## Close the file device 
  dev.off()
}


