library(dplyr)

output_file = "plot4.png"
input_file = "household_power_consumption.txt"
from <- "2007-02-01"
to <-  "2007-02-02"
label_y <- 'Energy Submetering'
label_x <- ''
colors <- c("black", "red", "blue")

readPowerData <- function(fileName) {
  df <- read.delim(file = fileName,header = TRUE,dec = '.',na.strings = "?",
                   colClasses = 
                     c("character", #date
                       "character", #time
                       "numeric", #Global_active_power
                       "numeric", #Global_reactive_power
                       "numeric", #Voltage
                       "numeric", #Global_intensity
                       "numeric", #Sub_metering_1
                       "numeric", #Sub_metering_2
                       "numeric"),
                   sep = ";")
  df <- tbl_df(df)
  df <- mutate(df, DateTime=paste(Date,Time))
  df <- select(df, -Date, -Time)
  df$DateTime <- as.POSIXct(strptime(df$DateTime, format="%d/%m/%Y %H:%M:%S", tz=""))
  df <- mutate(df, DateText=format(DateTime, "%Y-%m-%d"))
  df
}

drawPlot <- function() {
  all_household_data <- readPowerData(input_file)
  
  columns <- select(all_household_data, 
                    -Global_intensity)
  rm(all_household_data)
  
  one <- grepl(columns$DateText,pattern = from, fixed = TRUE)
  two <- grepl(columns$DateText,pattern = to, fixed = TRUE)
  combined <- one | two
  
  days_of_interest <- columns[three,]
  
  par(mfrow = c(2, 2)) 
  
  with(days_of_interest, 
       plot(DateTime, Global_active_power, type="l", ylab="Global Active Power", xlab=""))
  
  with(days_of_interest, 
       plot(DateTime, Voltage, type="l", ylab="Voltage", xlab="datetime"))
  
  with(days_of_interest,
       plot(DateTime, Sub_metering_1, type="l", ylab="Energy Submetering", xlab="", col=colors[1] ))
  with(days_of_interest, lines(DateTime, Sub_metering_2, type="l", col=colors[2]))
  with(days_of_interest, lines(DateTime, Sub_metering_3, type="l", col=colors[3]))
  
  legend("topright", 
         c("Sub_metering_1", 
           "Sub_metering_2", 
           "Sub_metering_3"), 
         lty=1, lwd=2, col=colors)
  
  with(days_of_interest, 
       plot(DateTime, Global_reactive_power, type="l", ylab="Global_reactive_power", xlab="datetime"))
}

# Prepare output device

png(filename = output_file,
    width = 480,
    height = 480,
    units = "px",
    pointsize = 12,
    bg="white")

# Draw
drawPlot()

# Flush output device
dev.off()

