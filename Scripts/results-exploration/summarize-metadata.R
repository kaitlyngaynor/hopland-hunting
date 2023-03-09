library(dplyr)
library(tidyr)
library(ggplot2)

metadata1 <- read.csv("Data/Hunting/igotu_metadata_times_cleaned_28Nov2022.csv") %>% 
    mutate(Hunt_type = "Single_day")
metadata2 <- read.csv("Data/Hunting/igotu_metadata_2019_2022_29Jan2023.csv")

metadata <- bind_rows(metadata1, metadata2)

hunters <- metadata %>% 
    dplyr::filter(Hunter_nonhunter == "H")

count(hunters, Harvest)

# 89 YES HARVEST
# 497 NO HARVEST

# How many to use?
count(hunters, Good_track)
# 586 total tracks
# 462 good tracks, 124 tracks not good

# Figure of tracks by year (usable vs not)
count(hunters, Good_track, Year) %>% 
    #pivot_wider(names_from = Good_track, values_from = n) %>% 
    ggplot(aes(x = Year, y = n, fill = Good_track)) +
    geom_bar(stat = "identity") +
    theme_bw() +
    scale_fill_manual(values = c("#FE7C00", "#4169E1")) +
    ylab("Number of Hunters")

# Figure of single vs. multi-day hunters
hunters %>% 
    filter(Good_track == "Y") %>% 
    count(Year, Hunt_type) %>% 
    ggplot(aes(x = Year, y = n, fill = Hunt_type)) +
    geom_bar(stat = "identity") +
    theme_bw() +
    scale_fill_manual(values = c("#4169E1", "#808080")) +
    ylab("Number of Hunter Tracks")

# How many successful/unsuccessful used?
hunters %>% 
    count(Good_track, Harvest) %>% 
    ggplot(aes(x = Good_track, y = n, fill = Harvest)) +
    geom_bar(stat = "identity") +
    theme_bw() +
    scale_fill_manual(values = c("#FE7C00", "#4169E1")) +
    ylab("Number of Hunters")

# 462 good tracks = 62 harvest, 400 no harvest (13%)
# 124 bad tracks = 27 harvest, 97 no harvest (22%)

# How many successful/unsuccessful by hunt type?
hunters %>% 
    count(Hunt_type, Harvest) %>% 
    ggplot(aes(x = Hunt_type, y = n, fill = Harvest)) +
    geom_bar(stat = "identity") +
    theme_bw() +
    scale_fill_manual(values = c("#4169E1", "#808080")) +
    ylab("Number of Hunters")

# 75 multiday hunters = 28 harvest, 47 no harvest (37%)
# 511 single day hunters = 61 harvest, 450 no harvest (12%)