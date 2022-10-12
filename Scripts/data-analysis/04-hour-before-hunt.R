# Take a closer look at the hour before a successful hunt


# Import and prepare data -------------------------------------------------------------------

library(hms)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
`%notin%` <- Negate(`%in%`)

# Bring in model results
data_hmm <- read.csv("Results/hmm-data-with-model-predictions-2022-10-11.csv") 

# Separate time into its own row
data_hmm <- separate(data = data_hmm, 
                     col = DateTime, 
                     into = c("Date", "Time"), 
                     sep = " ", 
                     remove = FALSE) 

# Identify harvest tracks with associated time
metadata_raw <- read.csv("Data/Hunting/igotu_metadata_times_cleaned_11Oct2022.csv") 
metadata <- metadata_raw %>% 
    filter(Harvest == "Y") %>% 
    dplyr::select(ID, Harvest_time)

# Filter tracks to just those with harvest times, join harvest times
data_hmm_harvest <- data_hmm %>% 
    filter(ID %in% metadata$ID) %>% 
    left_join(metadata)

# Calculate time from harvest
data_hmm_harvest$Time_to_Harvest <- NA
for(j in 1:nrow(data_hmm_harvest)) {
    data_hmm_harvest$Time_to_Harvest[j] <- as.numeric(difftime(as_hms(data_hmm_harvest$Time[j]),
                                                        as_hms(strptime(data_hmm_harvest$Harvest_time[j], "%H:%M")),
                                                        units = "hours"))
}

# Take just the hour before harvest
harvest_hour <- data_hmm_harvest %>% 
    filter(Time_to_Harvest < 0) %>% 
    filter(Time_to_Harvest > -1)

count(harvest_hour, state)

# Join with cluster associated with each hunter
hunting_cluster_all <- read.csv("Results/hunters_by_cluster_with_success.csv") 
hunting_cluster <- hunting_cluster_all %>% 
    filter(Harvest == "Y")
harvest_hour <- left_join(harvest_hour, hunting_cluster)


# Looking into sample size discrepancies ----------------------------------

count(hunting_cluster, Cluster)
# 22 hunters harvested deer - 8 drivers, 8 waiters, 6 walkers

# Two individuals are missing hour-before-hunt data but have cluster data
# these are both waiters (081416_10 and 082116_09), and both have "tbd" harvest times
hunting_cluster %>% 
    filter(ID %notin% harvest_hour$ID)

unique(harvest_hour$ID) # 20 harvesters with behavioral data & harvest times
unique(metadata$ID) # 31 harvesters in total with harvest times (with & without behavioral data)

# harvesters with recovered data and harvest - 33 of them
metadata_yes <- metadata_raw %>% filter(Harvest == "Y") %>% filter(Use_track == "Y")
missing <- metadata_yes %>% filter(ID %notin% harvest_hour$ID)


# Investigate characteristics of pre-hunt hour ----------------------------


# Look at habitat for hunting modes, independent of harvest
habitat_all <- data_hmm %>% 
    dplyr::select(ID, Habitat, DateTime, Harvest) %>% 
    left_join(dplyr::select(hunting_cluster_all, ID, Cluster)) %>% 
    mutate(Habitat = fct_recode(as.factor(Habitat), 
                                Shrubland = "1",
                                Woodland = "2",
                                Grassland = "3")) %>% 
    mutate(ID_DateTime = paste(ID, DateTime, sep = " "))

# Inelegant way to determine whether each observation was within the hour before a hunt
harvest_hour_ID_DateTime <- paste(harvest_hour$ID, harvest_hour$DateTime, sep = " ")
habitat_all$HourHarvest <- NA
for(i in 1:nrow(habitat_all)) {
    if(habitat_all$ID_DateTime[i] %in% harvest_hour_ID_DateTime) {
        habitat_all$HourHarvest[i] <- "HourBefore"
    } else {
        habitat_all$HourHarvest[i] <- "OtherTime"
    }
}

# Summarize by ID, HourHarvest
habitat_summary <- habitat_all %>% 
    count(ID, Habitat, HourHarvest, Cluster, Harvest) %>% 
    pivot_wider(names_from = Habitat, values_from = n) %>% 
    # fill NAs with 0s
    replace_na(list(Shrubland = 0, Woodland = 0, Grassland = 0)) %>% 
    mutate(Total = Shrubland + Woodland + Grassland) %>% 
    mutate(Shrubland_pct = Shrubland/Total, Woodland_pct = Woodland/Total, Grassland_pct = Grassland/Total)

head(habitat_summary)

habitat_summary_long <- habitat_summary %>% 
    dplyr::select(ID, HourHarvest, Harvest, Cluster, Shrubland, Woodland, Grassland) %>% 
    pivot_longer(cols = c(Shrubland, Woodland, Grassland),
                 names_to = "Habitat",
                 values_to = "Count")
habitat_summary_long_pct <- habitat_summary %>% 
    dplyr::select(ID, HourHarvest, Harvest, Cluster, Shrubland_pct, Woodland_pct, Grassland_pct) %>% 
    rename("Shrubland" = Shrubland_pct, "Woodland" = Woodland_pct, "Grassland" = Grassland_pct) %>% 
    pivot_longer(cols = c(Shrubland, Woodland, Grassland),
                 names_to = "Habitat",
                 values_to = "Percent")
habitat_summary_long <- left_join(habitat_summary_long, habitat_summary_long_pct) %>% 
    mutate(HourHarvest = paste(HourHarvest, Harvest, sep = "_"))

# Plot percent of time in each habitat, by cluster & hunting time/success
key <- data.frame(HourHarvest = c("HourBefore_Y", "OtherTime_N", "OtherTime_Y"),
                  HourHarvest2 = c("Hour Before Harvest", "Other Times (Unsuccessful)", "Other Times (Successful"))
habitat_summary_long <- left_join(habitat_summary_long, key)
ggplot(habitat_summary_long, aes(x = Cluster, y = Percent, fill = HourHarvest2)) +
    facet_grid(~Habitat) +
    geom_boxplot() + 
    theme_bw() +
    xlab("") +
    ggtitle("Habitat use by hunting mode, in hour before harvest",
            subtitle = "Compared to other times for both successful and unsuccessful hunters") +
    ylab("Percent Time Spent in Habitat") +
    scale_fill_manual(values = c("#6D8B74", "#D0C9C0", "#EFEAD8")) 
ggsave("Figures/habitat-hour-before.pdf", width = 8, height = 4)

# Cluster by success ONLY in hour before hunt
habitat_summary_long %>% 
    filter(HourHarvest == "HourBefore_Y") %>% 
    ggplot(aes(x = Habitat, y = Percent, fill = Cluster)) +
    geom_boxplot() + 
    theme_bw() +
    ggtitle("What were successful hunters doing in the hour before harvest?") +
    ylab("Percent Time Spent in Habitat")


# Just look at success vs. not, but ignore hour before hunt

habitat_summary2 <- habitat_all %>% 
    count(ID, Habitat, Cluster, Harvest) %>% 
    pivot_wider(names_from = Habitat, values_from = n) %>% 
    # fill NAs with 0s
    replace_na(list(Shrubland = 0, Woodland = 0, Grassland = 0)) %>% 
    mutate(Total = Shrubland + Woodland + Grassland) %>% 
    mutate(Shrubland_pct = Shrubland/Total, Woodland_pct = Woodland/Total, Grassland_pct = Grassland/Total)

habitat_summary_long2 <- habitat_summary2 %>% 
    dplyr::select(ID, Harvest, Cluster, Shrubland, Woodland, Grassland) %>% 
    pivot_longer(cols = c(Shrubland, Woodland, Grassland),
                 names_to = "Habitat",
                 values_to = "Count")
habitat_summary_long_pct2 <- habitat_summary %>% 
    dplyr::select(ID, Harvest, Cluster, Shrubland_pct, Woodland_pct, Grassland_pct) %>% 
    rename("Shrubland" = Shrubland_pct, "Woodland" = Woodland_pct, "Grassland" = Grassland_pct) %>% 
    pivot_longer(cols = c(Shrubland, Woodland, Grassland),
                 names_to = "Habitat",
                 values_to = "Percent")
habitat_summary_long2 <- left_join(habitat_summary_long2, habitat_summary_long_pct2)

# Cluster by habitat
ggplot(habitat_summary_long, aes(x = Habitat, y = Percent, fill = Cluster)) +
    geom_boxplot() + 
    theme_bw() +
    ggtitle("How did habitat use vary across hunting modes?") +
    ylab("Percent Time Spent in Habitat")

# Cluster success by habitat x success
ggplot(habitat_summary_long, aes(x = Cluster, y = Percent, fill = Harvest)) +
    facet_grid(~Habitat) +
    geom_boxplot() + 
    theme_bw() +
    ggtitle("How did habitat use vary across hunting modes & success?") +
    ylab("Percent Time Spent in Habitat")

library(ggridges)
# Cluster success by habitat x success
ggplot(habitat_summary_long, aes(x = Percent, y = Cluster, fill = Harvest)) +
    facet_grid(~Habitat) +
    geom_density_ridges() + 
    theme_ridges() +
    ggtitle("How did habitat use vary across hunting modes & success?") +
    ylab("Percent Time Spent in Habitat")
