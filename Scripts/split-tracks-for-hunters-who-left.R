# two hunters left the property for lunch, went home; split their tracks in two and re-export

library(mapview)
library(sf)

# import tracks
track1 <- read.csv("Data/Hunting/igotu_raw/081316_11_1.csv") 
track2 <- read.csv("Data/Hunting/igotu_raw/081316_12.csv") 

# map tracks

track1_sf <- track1 %>% 
    st_as_sf(coords = c("Longitude", "Latitude"),
             crs = "+proj=longlat +ellps=WGS84")

mapview(track1_sf)
    

# left around 11:35
# returned around 15:00

track2_sf <- track2 %>% 
    st_as_sf(coords = c("Longitude", "Latitude"),
             crs = "+proj=longlat +ellps=WGS84")

mapview(track2_sf)

# hmm, time stamp is all fucked up??