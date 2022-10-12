# Clean iGotU Data

library(dplyr)
library(tidyr)
library(hms)
library(stringr)
library(sf)
library(suncalc)

# import all data and join into single dataframe
igotu_file_names <- list.files(path = "Data/Hunting/igotu_raw/",
                               pattern = "*.csv", full.names = TRUE, recursive = TRUE) 

# read all individual csvs into a list
igotu_raw_dfs <- lapply(igotu_file_names, read.csv) 

# make the file names into the object names in the list
names(igotu_raw_dfs) <- igotu_file_names

# combine all collar files into one dataframe
igotu_raw <- bind_rows(igotu_raw_dfs, .id = "FileName")

# clean columns
igotu_data_all <- igotu_raw %>% 
    mutate(DateTime = as.POSIXct(paste(Date, Time, sep = ""), 
                                 "%m/%d/%y %H:%M:%S",
                                 tz = "America/Los_Angeles"),
           ID = str_replace(FileName, "Data/Hunting/igotu_raw//", ""),
           ID = str_replace(ID, ".csv", "")) %>% 
    select(-c(FileName, Altitude, Speed, Distance, Essential, Track, Course, Type))

# add the metadata
metadata <- read.csv(here::here("Data/Hunting/igotu_metadata_times_cleaned_11Oct2022.csv")) %>% 
    mutate(Start_time = paste0(Start_time, ":00"),
           End_time = paste0(End_time, ":00"),
           Date = as.Date(Date, format = "%m/%d/%y"))

# determine sunrise & sunset time
suntimes <- getSunlightTimes(date = unique(metadata$Date), 
                 lat = 39.012, lon = -123.079,
                 keep = c("sunrise", "sunset"), 
                 tz = "America/Los_Angeles") %>% 
    select(date, sunrise, sunset) %>% 
    rename(Date = date, Sunrise = sunrise, Sunset = sunset)

metadata <- left_join(metadata, suntimes) %>% 
    select(-Date)

# join metadata
igotu_data_all <- left_join(igotu_data_all, metadata)

# format time
igotu_data_all$Time <- format(igotu_data_all$DateTime, format = "%H:%M:%S")

# remove tracks with issues
igotu_data_all <- igotu_data_all %>% 
    filter(Good_track == "Y") 

# remove points before start or after end, export cleaned data
for(i in unique(igotu_data_all$ID)) {
    
    if(file.exists(paste0("Data/Hunting/igotu_cleaned/", i, ".csv"))) {
        next
    }
    
    # create temporary dataframe for each individual
    temp_df <- filter(igotu_data_all, ID == i)
    temp_df$Keep <- "NO"
    temp_df$Elapsed_Time <- NA
    temp_df$Elapsed_Time_Sunrise <- NA
    temp_df$Elapsed_Time_Sunset <- NA
    
    # filter to points within start and end
    for(j in 1:nrow(temp_df)) {
        if(as_hms(temp_df$Time[j]) > as_hms(temp_df$Start_time[j]) &&
           as_hms(temp_df$Time[j]) < as_hms(temp_df$End_time[j])) {
            temp_df$Keep[j] <- "YES"
            temp_df$Elapsed_Time[j] <- as.numeric(difftime(as_hms(temp_df$Time[j]),
                                                           as_hms(temp_df$Start_time[j]),
                                                           units = "hours"))
            temp_df$Elapsed_Time_Sunrise[j] <- as.numeric(difftime(as_hms(temp_df$Time[j]),
                                                           as_hms(temp_df$Sunrise[j]),
                                                           units = "hours"))
            temp_df$Elapsed_Time_Sunset[j] <- as.numeric(difftime(as_hms(temp_df$Sunset[j]),
                                                                   as_hms(temp_df$Time[j]),
                                                                   units = "hours"))
        }
    }
    
    temp_df <- temp_df %>% 
        # remove dropped points
        filter(Keep == "YES") %>% 
        # only take columns of interest
        select(ID, Latitude, Longitude, Date, Time, DateTime, Party_ID, Harvest, Elapsed_Time, Elapsed_Time_Sunrise, Elapsed_Time_Sunset)
    
    write.csv(temp_df, paste0("Data/Hunting/igotu_cleaned/", i, ".csv"), row.names = F)
    
    
}
