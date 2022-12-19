# Correct the time stamps for track 081316_12
# Subtract 7 hours from all times to get the correct times

# Did a lot of exploration, but at the end of the day, can't use track
# because it wasn't recording when harvest occurred (06:10) - didn't start
# until 3 hours later. Drop track entirely.

library(lubridate)
library(dplyr)

df <- read.csv("Data/Hunting/081316_12_manual_clean.csv")
    
df$DateTime <- as.POSIXct(paste(df$Date, df$Time, sep = ""), 
                      "%m/%d/%y %H:%M:%S",
                      tz = "America/Los_Angeles")

df$DateTime2 <- df$DateTime - hours(7)

df$Date <- as.Date(df$DateTime,
                           tz = "America/Los_Angeles")
df$Time <- format(as.POSIXct(df$DateTime), format = "%H:%M:%S") 

df <- df %>% 
    # remove the 08-12 times
    dplyr::filter(Date == as.Date("2016-08-13")) %>% 
    # remove DateTime field
    dplyr::select(-DateTime)

write.csv(df, "Data/Hunting/igotu_raw/081316_12.csv", row.names = FALSE)



library(ggplot2)
library(sf)
library(mapview)
date_breaks <- diff(range(df$DateTime2)) * 0:4 / 4 + min(df$DateTime2)
date_labels <- format(date_breaks, "%Y-%m-%d %H:%M:%S")

# bring in shape file
hrec_shp <- st_read("Data/Spatial data/Raw from Alex/HREC_boundary.shp") 

igotu_sf <- st_as_sf(df,
                     coords = c("Longitude", "Latitude"),
                     crs = "+proj=longlat +ellps=WGS84")

ggplot() + 
    geom_sf(data = hrec_shp) +
    geom_sf(data = igotu_sf, aes(col = as.numeric(DateTime2))) +
    theme_bw() +
    scale_colour_gradientn(colours=rev(rainbow(6)),
                           breaks = as.numeric(date_breaks),
                           labels = date_labels)

# bring in alternative track
df2 <- read.csv("Data/Hunting/igotu_raw/081316_11.csv")
igotu_sf2 <- st_as_sf(df2,
                     coords = c("Longitude", "Latitude"),
                     crs = "+proj=longlat +ellps=WGS84")

# interactive map
mapview(igotu_sf)
mapview(igotu_sf2)
