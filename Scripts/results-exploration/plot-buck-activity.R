library(overlap)
library(dplyr)
library(lubridate)
library(ggplot2)

cam_file_names <- list.files("Data/all-hopland-ct-data", full.names = TRUE)
buck_records_all <- lapply(cam_file_names, read.csv) %>% 
    dplyr::bind_rows() %>% 
    dplyr::filter(BuckClass == "Legal") %>% 
    dplyr::select(Camera, DateTimeOriginal, Date, Time)


# Subset dates ------------------------------------------------------------

subsetRecordTable <- function (record.table, start.date, end.date, timeperiod.name) {
    
    # format date column as date (should have been done above, but just in case)
    record.table$Date <- as.Date(record.table$Date)
    
    # format start and end dates as dates
    start.date <- as.Date(start.date)
    end.date <- as.Date(end.date)
    
    # subset record table to date of interest
    record.table.subset <- record.table[record.table$Date >= start.date & record.table$Date <= end.date,]
    
    # add new column for time period
    record.table.subset$TimePeriod <- timeperiod.name
    
    return(record.table.subset)
    
}

buck2016 <- subsetRecordTable(buck_records_all, "2016-08-13", "2016-08-28", "2016")
buck2017 <- subsetRecordTable(buck_records_all, "2017-08-12", "2017-08-27", "2017")
buck2019 <- subsetRecordTable(buck_records_all, "2019-08-17", "2019-09-22", "2019")
buck2020 <- subsetRecordTable(buck_records_all, "2020-08-08", "2020-09-20", "2020")
buck_records <- dplyr::bind_rows(buck2016, buck2017, buck2019, buck2020)

# Scale time --------------------------------------------------------------

# Get time to radians
coords <- matrix(c(-123.079, 39.0013), nrow=1) # note it is c(longitude, latitude)
Coords <- sp::SpatialPoints(coords,
                            proj4string=sp::CRS("+proj=longlat +datum=WGS84"))

# Format time
buck_records$Date <- as.POSIXct(buck_records$Date)
buck_records$Time_HMS <- lubridate::hms(buck_records$Time)
buck_records$Time_Decimal <- buck_records$Time_HMS$hour + buck_records$Time_HMS$minute/60 + buck_records$Time_HMS$second/3600
buck_records$Time_Scaled <- buck_records$Time_Decimal / 24
buck_records$Time_Radians <- buck_records$Time_Scaled * 2 * pi
buck_records$Time_Sun <- overlap::sunTime(buck_records$Time_Radians, buck_records$Date, Coords)

# Plot diel activity
bwA <- getBandWidth(buck_records$Time_Sun, kmax = 3)

xsc <- 24/(2 * pi)
xxRad <- seq(0, 2 * pi, length = 128)
xx <- xxRad * xsc
densA <- densityFit(buck_records$Time_Sun, xxRad, bwA)/xsc

plot(0, 0, type = "n", ylim = c(0, 0.08), xlim = c(6,18), 
     xlab = "Time", 
     ylab = "Activity", 
     xaxt = "n")

axis(1, at = c(6, 12, 18), labels = c("Sunrise", "Noon", "Sunset"))
polygon(c(c(2 * pi, 0), xx,24), c(0, 0, densA,0), border = NA, 
        col = "lightgrey")
lines(xx, densA, lty = 1, col = "black")


