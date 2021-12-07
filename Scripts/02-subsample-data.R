library(amt)
library(adehabitatLT)

# import all data and join into single dataframe
igotu_cleaned <- list.files(path = "Data/Hunting/igotu_cleaned/",
                               pattern = "*.csv", full.names = TRUE, recursive = TRUE) %>% 
    lapply(read.csv) %>% 
    bind_rows()

# make tracks
hunter_tracks <- mk_track(tbl = igotu_cleaned, 
                          .x = Longitude, .y = Latitude,
                          id = ID, 
                          .t = DateTime,
                          crs = CRS("+proj=longlat +ellps=WGS84")) 

# subsample to 3 minute fix rate
hunter_resampled <- hunter_tracks %>%
    nest(data = -"id") %>%
    mutate(resampled_data = map(data, function(x)
        x %>% track_resample(rate = minutes(3),
                             tolerance = seconds(0)))) %>%
    select(id, resampled_data) %>%
    unnest(cols = resampled_data) %>%
    
    # change column names to match originals
    rename(
        Longitude = x_,
        Latitude = y_,
        DateTime = t_,
        ID = id
    )

# join back the rest of the metadata to the resampled data
igotu_cleaned_resample <- left_join(hunter_resampled, igotu_cleaned) 

# export cleaned data
write.csv(igotu_cleaned_resample, 
          "Data/igotu_data_3min.csv", 
          row.names=F)