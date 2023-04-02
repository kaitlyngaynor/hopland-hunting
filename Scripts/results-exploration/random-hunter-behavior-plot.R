# Makes the plot of the 3 behavioral states for 50 random hunters in each mode

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
    count(ID, state_2stationary) %>% 
    tidyr::pivot_wider(values_from = n,
                       names_from = state_2stationary) %>% 
    tidyr::pivot_longer(cols = c(Driving, Stationary_offroad, Stationary_road, Walking),
                        values_to = "n",
                        names_to = "state_2stationary")
summary_state$n <- replace_na(summary_state$n, 0)

# Join with cluster associated with each hunter
set.seed(123)
random_sample <- read.csv("Results/hunter_cluster_success_long.csv") %>% 
    dplyr::select(ID, Cluster4) %>%
    unique() %>% 
    dplyr::group_by(Cluster4) %>% 
    sample_n(size = 75)


# Join in the times in state
random_sample <- left_join(random_sample, summary_state)

# Format for plotting
random_sample_long <- random_sample %>% 
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
ggplot(data = random_sample_long, aes(x = ID, y = Percent, fill = State)) +
    geom_bar(position = "stack", stat = "identity") +
    facet_wrap(~Cluster4, scales = "free", ncol = 2) +
    theme_minimal() +
    ylab("Percent of Time") +
    xlab("") +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text.x = element_text(size = 12),
          legend.position = c(1, 0),
          legend.justification = c(1, 0)) +
    scale_fill_manual(values = c("#614124", "#E8C07D", "gray", "#CC704B"),
                      labels = c("Driving", "Stationary (Off-road)", "Stationary (On-road)", "Walking")) 
ggsave("Figures/random-75-cluster-behavior.pdf", width = 6, height = 6)
