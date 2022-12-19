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