# Makes the plot of the 3 behavioral states for each successful hunter

library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)

data_hmm <- read.csv("Results/hmm-data-with-model-predictions-annotated-2023-03-04.csv")

# Get number of points per individual
n_indiv <- data_hmm %>% 
    count(ID)

# Add up total time in each state
summary_state <- data_hmm %>% 
    count(ID, state_2stationary)

# Join with cluster associated with each hunter and filter to success
successful <- read.csv("Results/hunter_cluster_success_long.csv") %>% 
    filter(Harvest == "Y") %>% 
    dplyr::select(ID, Cluster4) %>%
    unique()

# Join in the times in state
successful <- left_join(successful, summary_state)

# Format for plotting
successful_long <- successful %>% 
    tidyr::pivot_wider(names_from = "state_2stationary", values_from = "n") %>% 
    mutate(Total = Driving + Stationary_offroad + Stationary_road + Walking,
           Driving_pct = Driving/Total,
           Stationary_offroad_pct = Stationary_offroad/Total,
           Stationary_road_pct = Stationary_road/Total,
           Walking_pct = Walking/Total) %>% 
    mutate(ID = fct_reorder(ID, Driving_pct)) %>% 
    pivot_longer(cols = c(Driving_pct, Stationary_offroad_pct, Stationary_road_pct, Walking_pct),
                 names_to = "State", values_to = "Percent")

# Make plot
ggplot(data = successful_long, aes(x = ID, y = Percent, fill = State)) +
    geom_bar(position = "stack", stat = "identity") +
    facet_grid(~Cluster4, scales = "free", space = "free") +
    theme_minimal() +
    ylab("Percent of Time") +
    xlab("Successful Hunters") +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text.x = element_text(size = 12)) +
    scale_fill_manual(values = c("#614124", "gray", "#CC704B", "#E8C07D"),
                      labels = c("Driving", "Stationary (Off-road)", "Stationary (On-road)", "Walking")) 
ggsave("Figures/successful-behavior.pdf", width = 6, height = 3.5)