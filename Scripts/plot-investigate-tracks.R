# plot and export all tracks to make sure that it's all making sense

library(ggplot2)
library(sf)
library(dplyr)

igotu_cleaned <- list.files(path = "Data/Hunting/igotu_cleaned/",
                            pattern = "*.csv", full.names = TRUE, recursive = TRUE) %>% 
    lapply(read_csv) %>% 
    bind_rows()

igotu_sf <- st_as_sf(igotu_cleaned,
                     coords = c("Longitude", "Latitude"),
                     crs = "+proj=longlat +ellps=WGS84")

## Plots by individual animal
igotu_cleaned$DateTime <- as.POSIXct(igotu_cleaned$DateTime)

igotu_plots <- lapply(sort(unique(igotu_sf$ID)), function(i) {
    
    data_to_plot <- igotu_sf[igotu_sf$ID==i,]
    date_breaks <- diff(range(data_to_plot$DateTime)) * 0:4 / 4 + min(data_to_plot$DateTime)
    date_labels <- format(date_breaks, "%Y-%m-%d %H:%M:%S")
    
    ggplot() + 
        geom_sf(data = alaska) +
        geom_sf(data = alaska_shp) +
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
```