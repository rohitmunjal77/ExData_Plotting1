plot1 <- function() {
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
  png(file = "plot1.png", width = 480, height = 480)
  
  ## Generate & annotate histogram for 'Global Active Power' measurements for the 2-day period using Date observations
  with(mySubsetData, hist(as.numeric(Global_active_power), col = "red", xlab = "Global Active Power (kilowatts)", main = "Global Active Power"))

  ## Close the file device 
  dev.off()
}
  
  
  