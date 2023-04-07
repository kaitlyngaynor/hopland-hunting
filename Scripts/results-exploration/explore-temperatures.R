library(dplyr)
library(ggplot2)

# bring in weather data
weather <- read.csv("Data/hopland_daily_weather.csv")

# bring in success and metadata
success <- read.csv("Results/hunter_cluster_success_long.csv") %>%
    dplyr::mutate(Harvest01 = ifelse(Harvest == "N",0,1)) %>% 
    tidyr::pivot_wider(names_from = "State_4state", values_from = "Percentage")
metadata1 <- read.csv("Data/Hunting/igotu_metadata_times_cleaned_28Nov2022.csv") %>% 
    mutate(Hunt_type = "Single_day")
metadata2 <- read.csv("Data/Hunting/igotu_metadata_2019_2022_29Jan2023.csv")
metadata <- bind_rows(metadata1, metadata2)
success <- left_join(success, metadata)


# join in weather
success <- left_join(success, weather)

# just look at weather on hunting days
weather_hunting <- weather %>% 
    dplyr::filter(Date %in% metadata$Date)
hist(weather_hunting$max_temp_f)
hist(weather_hunting$avg_temp_f)

ggplot(success, aes(x = Cluster4, y = avg_temp_f)) +
    geom_boxplot() +
    theme_bw()

ggplot(success, aes(x = Harvest, y = avg_temp_f)) +
    geom_boxplot() +
    theme_bw()

ggplot(success, aes(x = Cluster4, y = avg_temp_f, fill = Harvest)) +
    geom_boxplot() +
    theme_bw()








