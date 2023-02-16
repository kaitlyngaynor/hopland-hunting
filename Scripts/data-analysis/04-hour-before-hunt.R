# Take a closer look at the hour before a successful hunt


# Import and prepare data -------------------------------------------------------------------

library(hms)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
`%notin%` <- Negate(`%in%`)

# Bring in model results
data_hmm <- read.csv("Results/hmm-data-with-model-predictions-annotated-2023-02-15.csv") 

# Separate time into its own row
data_hmm <- separate(data = data_hmm, 
                     col = DateTime, 
                     into = c("Date", "Time"), 
                     sep = " ", 
                     remove = FALSE) 

# Identify harvest tracks with associated time
metadata_raw <- read.csv("Data/Hunting/igotu_metadata_times_cleaned_28Nov2022.csv") 
metadata <- metadata_raw %>% 
    filter(Harvest == "Y") %>% 
    dplyr::select(ID, Harvest_time)
metadata_raw2 <- read.csv("Data/Hunting/igotu_metadata_2019_2022_29Jan2023.csv") 
metadata2 <- metadata_raw2 %>% 
    filter(Harvest == "Y") %>% 
    dplyr::select(ID, Harvest_time)
metadata <- bind_rows(metadata, metadata2)

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

count(hunting_cluster, Cluster4)
# 56 hunters harvested deer - 31 drivers, 15 waiters, 10 walkers
## 2022-12-23 - now I'm seeing 48? 28 drivers, 11 waiters, 9 walkers

hunting_cluster %>% 
    filter(ID %notin% harvest_hour$ID)

# Two individuals are missing hour-before-hunt data but have cluster data
# 081416_19 (waiter) - missing data from around the harvest time
# 081819_01 (driver) - not sure of harvest time

length(unique(harvest_hour$ID)) # 46 harvesters with behavioral data & harvest times


# Investigate characteristics of pre-hunt hour ----------------------------

# Join cluster with movement data
data_hmm <- data_hmm %>% 
    left_join(dplyr::select(hunting_cluster_all, ID, Cluster4)) %>% 
    mutate(ID_DateTime = paste(ID, DateTime, sep = " "),
           Used = 1)

# Inelegant way to determine whether each observation was within the hour before a hunt
harvest_hour_ID_DateTime <- paste(harvest_hour$ID, harvest_hour$DateTime, sep = " ")
data_hmm$HourHarvest <- NA
for(i in 1:nrow(data_hmm)) {
    if(data_hmm$ID_DateTime[i] %in% harvest_hour_ID_DateTime) {
        data_hmm$HourHarvest[i] <- "HourBefore"
    } else {
        data_hmm$HourHarvest[i] <- "OtherTime"
    }
}


# RSF in hour before ------------------------------------------------------

# Bring in available points
available <- read.csv("Data/all-available-point-cov.csv") %>% 
    mutate(Used = 0)

# Join used and available together, scale covariates
used_avail <- dplyr::bind_rows(available, data_hmm) %>% 
    dplyr::select(ID, Ruggedness, Viewshed, Road_Distance, Chaparral_120m, Grassland_120m, HQ_Distance, Habitat,
                  Used, Cluster4, Harvest, HourHarvest, state, state_2stationary) %>% 
    dplyr::mutate(Habitat = fct_recode(as.factor(Habitat), 
                                Shrubland = "1",
                                Grassland = "2",
                                Grassland = "3"),
                  Ruggedness_scale = scale(Ruggedness),
                  Viewshed_scale = scale(Viewshed),
                  Road_Distance_scale = scale(Road_Distance),
                  Chaparral_120m_scale = scale(Chaparral_120m),
                  Grassland_120m_scale = scale(Grassland_120m),
                  HQ_Distance_scale = scale(HQ_Distance))

# Subset by hunting mode & time in relation to harvest
used_avail_drivers_hourbefore <- used_avail %>% 
    dplyr::filter((Cluster4 != "Walkers" & Cluster4 != "Waiters") | Used == 0) %>% 
    dplyr::filter(HourHarvest == "HourBefore" | Used == 0)
used_avail_walkers_hourbefore <- used_avail %>% 
    dplyr::filter((Cluster4 != "Drivers" & Cluster4 != "Waiters") | Used == 0) %>% 
    dplyr::filter(HourHarvest == "HourBefore" | Used == 0)
used_avail_waiters_hourbefore <- used_avail %>% 
    dplyr::filter((Cluster4 != "Walkers" & Cluster4 != "Drivers") | Used == 0) %>% 
    dplyr::filter(HourHarvest == "HourBefore" | Used == 0)
used_avail_drivers_othertime <- used_avail %>% 
    dplyr::filter((Cluster4 != "Walkers" & Cluster4 != "Waiters") | Used == 0) %>% 
    dplyr::filter(HourHarvest == "OtherTime" | Used == 0)
used_avail_walkers_othertime <- used_avail %>% 
    dplyr::filter((Cluster4 != "Drivers" & Cluster4 != "Waiters") | Used == 0) %>% 
    dplyr::filter(HourHarvest == "OtherTime" | Used == 0)
used_avail_waiters_othertime <- used_avail %>% 
    dplyr::filter((Cluster4 != "Walkers" & Cluster4 != "Drivers") | Used == 0) %>% 
    dplyr::filter(HourHarvest == "OtherTime" | Used == 0)

fit_drivers_hourbefore <- glm(Used ~ Ruggedness_scale + Viewshed_scale + Chaparral_120m_scale + Grassland_120m_scale,
                           data = used_avail_drivers_hourbefore,
                           family = binomial) # causing problems with convergence when road is included
fit_walkers_hourbefore <- glm(Used ~ Ruggedness_scale + Viewshed_scale + Chaparral_120m_scale + Grassland_120m_scale + Road_Distance_scale,
                           data = used_avail_walkers_hourbefore,
                           family = binomial) 
fit_waiters_hourbefore <- glm(Used ~ Ruggedness_scale + Viewshed_scale + Chaparral_120m_scale + Grassland_120m_scale + Road_Distance_scale,
                           data = used_avail_waiters_hourbefore,
                           family = binomial) 
fit_drivers_othertime <- glm(Used ~ Ruggedness_scale + Viewshed_scale + Chaparral_120m_scale + Grassland_120m_scale + Road_Distance_scale,
                             data = used_avail_drivers_othertime,
                             family = binomial) 
fit_walkers_othertime <- glm(Used ~ Ruggedness_scale + Viewshed_scale + Chaparral_120m_scale + Grassland_120m_scale + Road_Distance_scale,
                             data = used_avail_walkers_othertime,
                             family = binomial) 
fit_waiters_othertime <- glm(Used ~ Ruggedness_scale + Viewshed_scale + Chaparral_120m_scale + Grassland_120m_scale + Road_Distance_scale,
                             data = used_avail_waiters_othertime,
                             family = binomial) 

jtools::plot_summs(fit_drivers_hourbefore, fit_drivers_othertime, 
                   fit_walkers_hourbefore, fit_walkers_othertime, 
                   fit_waiters_hourbefore, fit_waiters_othertime,
                   model.names = c("Drivers - Hour Before", "Drivers - Other Time",
                                   "Walkers - Hour Before", "Walkers - Other Time",
                                   "Waiters - Hour Before", "Waiters - Other Time"))

fit_drivers_hourbefore2 <- glm(Used ~ Habitat, data = used_avail_drivers_hourbefore, family = binomial) 
fit_walkers_hourbefore2 <- glm(Used ~ Habitat, data = used_avail_walkers_hourbefore, family = binomial) 
fit_waiters_hourbefore2 <- glm(Used ~ Habitat, data = used_avail_waiters_hourbefore, family = binomial) 
fit_drivers_othertime2 <- glm(Used ~ Habitat, data = used_avail_drivers_othertime, family = binomial) 
fit_walkers_othertime2 <- glm(Used ~ Habitat, data = used_avail_walkers_othertime, family = binomial) 
fit_waiters_othertime2 <- glm(Used ~ Habitat, data = used_avail_waiters_othertime, family = binomial) 

jtools::plot_summs(fit_drivers_hourbefore2, fit_drivers_othertime2, 
                   #fit_walkers_hourbefore2, fit_walkers_othertime2, 
                   fit_waiters_hourbefore2, fit_waiters_othertime2,
                   model.names = c("Drivers - Hour Before", "Drivers - Other Time",
                                   #"Walkers - Hour Before", "Walkers - Other Time",
                                   "Waiters - Hour Before", "Waiters - Other Time"))




# OLD CODE ----------------------------------------------------------------

# Summarize by ID, HourHarvest
habitat_summary <- data_hmm %>% 
    count(ID, Habitat, HourHarvest, Cluster4, Harvest) %>% 
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
                  HourHarvest2 = c("Hour Before Harvest", "Other Times (Unsuccessful)", "Other Times (Successful)"))
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

habitat_summary_long %>% filter(Harvest == "Y") %>% 
ggplot(aes(x = ID, y = Percent, fill = Habitat)) +
    geom_bar(position = "stack", stat = "identity") +
    facet_grid(Cluster~HourHarvest, scales = "free", space = "free") +
    theme_minimal() +
    ylab("Percent of Time in Habitat") +
    xlab("Successful Hunters") +
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text.y = element_text(size = 12)) +
    coord_flip()

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
