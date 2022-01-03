# Clean up & export data for HMM modeling, removing long steps

library(dplyr)
library(moveHMM)
library(tidyr)
library(ggplot2)

# Load cleaned data
igotu_data <- read.csv("Data/igotu_data_3min_covariates.csv")

# Select columns of interest
igotu_data_fewer <- igotu_data %>% 
    dplyr::select(ID, Longitude, Latitude, DateTime,
                  rugged49.clean, rugged25.clean, rugged9.clean,
                  hq_dist, vegetation.coarser.clean2, view,
                  veg.edges.dist.clean,
                  grass_120m, chap_120m, wood_120m)

data_hmm <- moveHMM::prepData(igotu_data_fewer, 
                              type="LL", 
                              coordNames=c("Longitude","Latitude"))
head(data_hmm)

# remove 300 step lengths of 'NA'
data_hmm <- data_hmm %>% 
    drop_na(step) 


# Explore different step length cut-offs ----------------------------------

##  1 mile/hr = 0.0268224 km/min = 0.0804672 km step length (km/3 min)
##  10 mile/hr = 0.268224 km/min, or 0.804672 km step length (km/3 min)
##  15 mile/hr = 0.402336 km/min, or 1.207008 km step length (km/3 min)
##  20 mile/hr = 0.536448 km/min, or 1.609344 km step length (km/3 min)
##  25 mile/hr = 0.67056 km/min, or 2.01168 km step length (km/3 min)
##  30 mile/hr = 0.804672 km/min, or 2.414016 km step length (km/3 min)

# 10mph cutoff - 2% of points (1,619 points)
nrow(filter(data_hmm, step > 0.804672))/nrow(data_hmm)

# 15mph cutoff - 0.4% of points (295 points)
nrow(filter(data_hmm, step > 1.207008))/nrow(data_hmm)

# 20mph cutoff - 0.17% of points (124 points)
nrow(filter(data_hmm, step > 1.609344))/nrow(data_hmm)

# 25mph cutoff - 0.10% of points (76 points)
nrow(filter(data_hmm, step > 2.01168))/nrow(data_hmm)

# 30mph cutoff - 0.06% of points (47 points)
nrow(filter(data_hmm, step > 2.414016))/nrow(data_hmm)

# histogram for only steps >= 10mph
fast_steps <- data_hmm %>% 
    filter(step > 0.804672) %>% 
    mutate(speed = step/3 * 37.2823)


# Filter data -------------------------------------------------------------

# LET'S USE 15 mph cut-off since it seems like there is a big drop there
data_hmm <- data_hmm %>% 
    filter(step < 1.207008)

# Explore 0 step lengths --------------------------------------------------

# determine proportion of step lengths equal to 0
whichzero <- which(data_hmm$step == 0)
length(whichzero)/nrow(data_hmm)

# 0.19 for 3 min

zeros <- data_hmm %>% 
    filter(step == 0)

ggplot(zeros, aes(x = x, y = y)) + geom_point()

#  data_hmm_copy <- data_hmm
#  data_hmm_copy[is.na(data_hmm_copy)] <- 0
#  
#  # determine which ones follow a zero
#  data_hmm_copy$after_zero <- "NOT_AFTER_ZERO" 
#  for(i in 2:nrow(data_hmm_copy)) {
#    if(data_hmm_copy$step[i] == 0) {
#      data_hmm_copy$after_zero[i] <- "ZERO"
#    } else if(data_hmm_copy$step[i-1] == 0) {
#      data_hmm_copy$after_zero[i] <- "AFTER_ZERO"
#    } else {
#      data_hmm_copy$after_zero[i] <- "NOT_AFTER_ZERO"
#    }
#  }
#  
#  ggplot(data_hmm_copy, aes(x = step)) +
#    geom_histogram() +
#    facet_wrap(~after_zero)

# remove step length & turn angle columns and explort
data_hmm <- data_hmm %>% 
    dplyr::select(-c("step", "angle"))
write.csv(data_hmm, "Data/igotu_data_3min_covariates_for_hmm.csv", 
          row.names = FALSE)
