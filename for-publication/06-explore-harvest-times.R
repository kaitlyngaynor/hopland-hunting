# 06. Explore diel patterns of deer harvest by hunters across hunting modes

library(dplyr)
library(sp)
library(kSamples)
`%notin%` <- Negate(`%in%`)

success <- read.csv("hunter_cluster_success_long.csv") %>%
    tidyr::pivot_wider(names_from = "State", values_from = "Percentage") %>% 
    dplyr::filter(Harvest == "Y") %>% 
    dplyr::select(ID, Cluster)

# Bring in metadata
metadata <- read.csv("igotu_metadata.csv") %>% 
    dplyr::select(ID, Date, Harvest_time)
success <- left_join(success, metadata)  %>%
    dplyr::filter(ID %notin% c("082419_06", "082419_13")) # missing harvest times

# Scale times
success$Date <- as.POSIXct(success$Date, format = "%Y-%m-%d")
success$Time_HM <- lubridate::hm(success$Harvest_time)
success$Time_Decimal <- success$Time_HM$hour + success$Time_HM$minute/60
success$Time_Scaled <- success$Time_Decimal / 24
success$Time_Radians <- success$Time_Scaled * 2 * pi
coords <- matrix(c(-123.079, 39.0013), nrow=1)
Coords <- sp::SpatialPoints(coords,
                            proj4string=sp::CRS("+proj=longlat +datum=WGS84"))
success$Time_Sun <- overlap::sunTime(success$Time_Radians, success$Date, Coords)

# See if time of day varies by hunting mode - Anderson-Darling test
coursing <- dplyr::filter(success, Cluster == "Coursing")
stalking <- dplyr::filter(success, Cluster == "Stalking")
sitandwait <- dplyr::filter(success, Cluster == "Sit-and-wait")

set.seed(12)
ad.test(coursing$Time_Sun, stalking$Time_Sun, sitandwait$Time_Sun,
        method = "exact", dist = FALSE, Nsim = 1000)
