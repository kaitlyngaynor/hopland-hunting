# plot and export all tracks to make sure that it's all making sense

library(ggplot2)
library(sf)
library(dplyr)
library(readr)

# RAW TRACKS ----------------------------------------------------------

igotu_raw <- list.files(path = "Data/Hunting/igotu_raw/",
                            pattern = "*.csv", full.names = TRUE, recursive = TRUE) %>% 
    lapply(read_csv) %>% 
    bind_rows()

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
