loadDataMakePlot <- function()   {
  sqldf()
  data <- read.csv2.sql("household_power_consumption.txt", sql = "select * from file where Date == '1/2/2007' or Date == '2/2/2007'", na.strings = "?") 
  
  png(file = "plot4.png")
  data$Date <- as.Date(data$Date, format = "%d/%m/%Y")      
  newData <- within(data, Data_Time <- paste(data$Date, data$Time)) 
  dataTime<- strptime(newData$Data_Time, "%Y-%m-%d %H:%M:%S") # Appropriate time format
  par(mfrow = c(2,2))
  
  plot(dataTime, newData$Global_active_power, type = "l", xlab = "", ylab = "Global Active Power") 
  plot(dataTime, newData$Voltage, type = "l", xlab = "datetime", ylab = "Voltage")
  yLabel <- "Energy sub metering"
  plot  (dataTime, newData$Sub_metering_1, type = "l", xlab = "", ylab = yLabel, col = "Black")
  points(dataTime, newData$Sub_metering_2, type = "l", xlab = "", ylab = yLabel, col = "Red")
  points(dataTime, newData$Sub_metering_3, type = "l", xlab = "", ylab = yLabel, col = "Blue")
  plotColors <- c("black", "red", "blue")
  plotLegend <- c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
  legend("topright", lty = 1, col = plotColors, legend = plotLegend, bty = "n")
  plot(dataTime, newData$Global_reactive_power, type = "l", xlab = "datetime", ylab = "Global_reactive_power")
  dev.off()
  sqldf()
}