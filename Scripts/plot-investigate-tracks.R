# plot and export all tracks to make sure that it's all making sense

library(ggplot2)
library(sf)
library(dplyr)
library(readr)
library(stringr)

# RAW TRACKS ----------------------------------------------------------

igotu_file_names <- list.files(path = "Data/Hunting/igotu_raw/",
                               pattern = "*.csv", full.names = TRUE, recursive = TRUE) 
igotu_raw_dfs <- lapply(igotu_file_names, read.csv) 
names(igotu_raw_dfs) <- igotu_file_names
igotu_raw <- bind_rows(igotu_raw_dfs, .id = "FileName") %>% 
    mutate(DateTime = as.POSIXct(paste(Date, Time, sep = ""), 
                                 "%m/%d/%y %H:%M:%S",
                                 tz = "America/Los_Angeles"),
           ID = str_replace(FileName, "Data/Hunting/igotu_raw//", ""),
           ID = str_replace(ID, ".csv", "")) %>% 
    select(-c(FileName, Altitude, Speed, Distance, Essential, Track, Course, Type))
igotu_raw_sf <- st_as_sf(igotu_raw,
                     coords = c("Longitude", "Latitude"),
                     crs = "+proj=longlat +ellps=WGS84")

# bring in shape file
hrec_shp <- st_read("Data/Spatial data/Raw from Alex/HREC_boundary.shp") 

## Plots by individual animal
igotu_raw$DateTime <- as.POSIXct(igotu_raw$DateTime)

igotu_raw_plots <- lapply(sort(unique(igotu_raw_sf$ID)), function(i) {
    
    data_to_plot <- igotu_raw_sf[igotu_raw_sf$ID==i,]
    date_breaks <- diff(range(data_to_plot$DateTime)) * 0:4 / 4 + min(data_to_plot$DateTime)
    date_labels <- format(date_breaks, "%Y-%m-%d %H:%M:%S")
    
    ggplot() + 
        geom_sf(data = hrec_shp) +
        geom_sf(data = data_to_plot, aes(col = as.numeric(DateTime))) +
        theme_bw() +
        ggtitle(i) +
        scale_colour_gradientn(colours=rev(rainbow(6)),
                               breaks = as.numeric(date_breaks),
                               labels = date_labels)
})

# print all plots to pdf
pdf("igotu_raw-plots.pdf")
for (i in 1:length(igotu_raw_plots)) {
    print(igotu_raw_plots[[i]])
}
dev.off()

# CLEANED TRACKS ----------------------------------------------------------

igotu_cleaned <- list.files(path = "Data/Hunting/igotu_cleaned/",
                            pattern = "*.csv", full.names = TRUE, recursive = TRUE) %>% 
    lapply(read_csv) %>% 
    bind_rows()

igotu_sf <- st_as_sf(igotu_cleaned,
                     coords = c("Longitude", "Latitude"),
                     crs = "+proj=longlat +ellps=WGS84")

# bring in shape file
hrec_shp <- st_read("Data/Spatial data/Raw from Alex/HREC_boundary.shp") 

## Plots by individual animal
igotu_cleaned$DateTime <- as.POSIXct(igotu_cleaned$DateTime)

igotu_plots <- lapply(sort(unique(igotu_sf$ID)), function(i) {
    
    data_to_plot <- igotu_sf[igotu_sf$ID==i,]
    date_breaks <- diff(range(data_to_plot$DateTime)) * 0:4 / 4 + min(data_to_plot$DateTime)
    date_labels <- format(date_breaks, "%Y-%m-%d %H:%M:%S")
    
    ggplot() + 
        geom_sf(data = hrec_shp) +
        geom_sf(data = data_to_plot, aes(col = as.numeric(DateTime))) +
        theme_bw() +
        ggtitle(i) +
        scale_colour_gradientn(colours=rev(rainbow(6)),
                               breaks = as.numeric(date_breaks),
                               labels = date_labels)
})

# print all plots to pdf
pdf("igotu-plots.pdf")
for (i in 1:length(igotu_plots)) {
    print(igotu_plots[[i]])
}
dev.off()



# CLEANED FOR HMM 3 MIN ---------------------------------------------------

igotu_cleaned <- read.csv("Data/igotu_data_3min_covariates_for_hmm.csv")

igotu_sf <- st_as_sf(igotu_cleaned,
                     coords = c("x", "y"),
                     crs = "+proj=longlat +ellps=WGS84")

# bring in shape file
hrec_shp <- st_read("Data/Spatial data/Raw from Alex/HREC_boundary.shp") 

## Plots by individual animal
igotu_cleaned$DateTime <- as.POSIXct(igotu_cleaned$DateTime)

igotu_plots <- lapply(sort(unique(igotu_sf$ID)), function(i) {
    
    data_to_plot <- igotu_sf[igotu_sf$ID==i,]
    date_breaks <- diff(range(data_to_plot$DateTime)) * 0:4 / 4 + min(data_to_plot$DateTime)
    date_labels <- format(date_breaks, "%Y-%m-%d %H:%M:%S")
    
    ggplot() + 
        geom_sf(data = hrec_shp) +
        geom_sf(data = data_to_plot, aes(col = as.numeric(DateTime))) +
        theme_bw() +
        ggtitle(i) +
        scale_colour_gradientn(colours=rev(rainbow(6)),
                               breaks = as.numeric(date_breaks),
                               labels = date_labels)
})

# print all plots to pdf
pdf("igotu-plots-3min-hmm.pdf")
for (i in 1:length(igotu_plots)) {
    print(igotu_plots[[i]])
}
dev.off()

