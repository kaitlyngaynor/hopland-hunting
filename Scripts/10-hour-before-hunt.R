# Take a closer look at the hour before a successful hunt


# Explore states in the hour before successful hunt ----------------------------------------

library(hms)
library(dplyr)
library(tidyr)

# Bring in model results
data_hmm <- read.csv("Results/hmm-data-with-model-predictions.csv")

# Bring in raw data
igotu_data <- read.csv("Data/igotu_data_3min_covariates.csv")

# Identify 31 harvest tracks with associated time
metadata <- read.csv("Data/Hunting/igotu_metadata_times_cleaned_03Feb2022.csv") %>% 
    filter(Harvest == "Y") %>% 
    filter(Harvest_time != "tbd") %>% 
    filter(Harvest_time != "TBD")

# Filter tracks to just the hour before harvest
harvest_hour <- igotu_data %>% 
    filter(ID %in% metadata$ID) %>% 
    left_join(metadata)

# Calculate time from harvest
harvest_hour$Time_to_Hunt <- NA
for(j in 1:nrow(harvest_hour)) {
    harvest_hour$Time_to_Hunt[j] <- as.numeric(difftime(as_hms(harvest_hour$Time[j]),
                                                        as_hms(strptime(harvest_hour$Harvest_time[j], "%H:%M")),
                                                        units = "hours"))
}

# Take just the hour before harvest
harvest_hour <- harvest_hour %>% 
    filter(Time_to_Hunt < 0) %>% 
    filter(Time_to_Hunt > -1)

# Join with state
harvest_hour2 <- inner_join(harvest_hour,
                          dplyr::select(data_hmm, ID, DateTime, state)) %>% 
    drop_na()

count(harvest_hour, state)



# Join with cluster


# Look at vegetation types in hour before hunt

count(harvest_hour, vegetation.coarser.clean2)

# also look at veg types for rest of period

# look up which habitat type correponds to which number

# Bring in associated clusters & see if habitat types in hour before hunt varied by hunting mode
