# Clean iGotU Data

library(dplyr)
library(tidyr)
library(hms)
library(stringr)
library(sf)

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
metadata <- read.csv(here::here("Data/Hunting/igotu_metadata_times_cleaned_15Jan2022.csv")) %>% 
    mutate(Start_time = paste0(Start_time, ":00"),
           End_time = paste0(End_time, ":00")) %>% 
    select(-Date)
igotu_data_all <- left_join(igotu_data_all, metadata)

# format time
igotu_data_all$Time <- format(igotu_data_all$DateTime, format = "%H:%M:%S")

# remove tracks with issues, or duplicates from same hunting party
igotu_data_all <- igotu_data_all %>% 
    filter(Use_track == "Y") 

# remove points outside of HREC boundary (just using bounding box)
hrec_boundary <- read_sf("Data/Spatial data/Raw from Alex/HREC_boundary.shp") %>% 
    st_transform("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
igotu_data_all_sf <- st_as_sf(igotu_data_all,
                              coords = c("Longitude", "Latitude"),
                              crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs",
                              remove = FALSE)
igotu_data_all_sf_cropped <- igotu_data_all_sf[st_intersects(igotu_data_all_sf, hrec_boundary) %>% lengths > 0,]

igotu_data_all <- st_drop_geometry(igotu_data_all_sf_cropped)

# remove points before start or after end, export cleaned data
for(i in unique(igotu_data_all$ID)) {
    
    if(file.exists(paste0("Data/Hunting/igotu_cleaned/", i, ".csv"))) {
        next
    }
    
    # create temporary dataframe for each individual
    temp_df <- filter(igotu_data_all, ID == i)
    temp_df$Keep <- "NO"
    
    # filter to points within start and end
    for(j in 1:nrow(temp_df)) {
        if(as_hms(temp_df$Time[j]) > as_hms(temp_df$Start_time[j]) &&
           as_hms(temp_df$Time[j]) < as_hms(temp_df$End_time[j])) {
            temp_df$Keep[j] <- "YES"
        }
    }
    
    temp_df <- temp_df %>% 
        # remove dropped points
        filter(Keep == "YES") %>% 
        # only take columns of interest
        select(ID, Latitude, Longitude, Date, Time, DateTime, Party_ID, Harvest)
    
    write.csv(temp_df, paste0("Data/Hunting/igotu_cleaned/", i, ".csv"), row.names = F)
    
    
}
