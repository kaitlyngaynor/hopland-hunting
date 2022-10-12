# Correct the time stamps for track 081316_12
# Subtract 7 hours from all times to get the correct times

library(lubridate)
library(dplyr)

df <- read.csv("Data/Hunting/igotu_raw/081316_12.csv")

# Only run if it hasn't already been corrected (confirm first row)
if(df$Time[1] == "4:57:08") {
    
    df$DateTime <- as.POSIXct(paste(df$Date, df$Time, sep = ""), 
                          "%m/%d/%y %H:%M:%S",
                          tz = "America/Los_Angeles")
    
    df$DateTime <- df$DateTime - hours(7)
    
    df$Date <- as.Date(df$DateTime,
                               tz = "America/Los_Angeles")
    df$Time <- format(as.POSIXct(df$DateTime), format = "%H:%M:%S") 
    
    df <- df %>% 
        # remove the 08-12 times
        dplyr::filter(Date == as.Date("2016-08-13")) %>% 
        # remove DateTime field
        dplyr::select(-DateTime)
    
    write.csv(df, "Data/Hunting/igotu_raw/081316_12.csv", row.names = FALSE)
    
}
