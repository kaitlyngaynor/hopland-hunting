library(dplyr)

metadata1 <- read.csv("Data/Hunting/igotu_metadata_times_cleaned_28Nov2022.csv")
metadata2 <- read.csv("Data/Hunting/igotu_metadata_2019_2020_28Nov2022.csv")

metadata <- bind_rows(metadata1, metadata2)

hunters <- metadata %>% 
    dplyr::filter(Hunter_nonhunter == "H")

count(hunters, Harvest)

# 45 yes, 350 no, 15 unknown
# But we know that of the 15 unknown, 2 were yes, and 13 were no

# So, final count:

# 47 YES HARVEST
# 363 NO HARVEST

# How many to use?
count(hunters, Good_track)
# 410 total tracks
# 386 good tracks, 24 tracks not good

# How many successful/unsuccessful used?
hunters %>% 
    group_by(Good_track) %>%
    count(Harvest)

# 386 good tracks = 41 harvest, 330 no harvest, 15 unknown
# 24 bad tracks = 4 harvest, 20 no harvest
