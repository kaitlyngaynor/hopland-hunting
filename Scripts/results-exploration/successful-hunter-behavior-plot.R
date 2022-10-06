library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)

data_hmm <- read.csv("Results/hmm-data-with-model-predictions.csv")

# Get number of points per individual
n_indiv <- data_hmm %>% 
    count(ID)

# Add up total time in each state (using probabilities)
summary_prob <- data_hmm %>% 
    group_by(ID, state) %>% 
    summarise(Driving = sum(Driving_Prob),
              Stationary = sum(Stationary_Prob),
              Walking = sum(Walking_Prob))


# Add up total time in each state (based on most probable per point)
summary_point <- data_hmm %>% 
    count(ID, state) %>% 
    pivot_wider(names_from = state, values_from = n) %>% 
    left_join(n_indiv) %>% 
    mutate(Driving_Prob = Driving/n,
           Stationary_Prob = Stationary/n,
           Walking_Prob = Walking/n) %>% 
    ungroup()

# Join with cluster associated with each hunter and filter to success
hunting_cluster_all <- read.csv("Results/hunters_by_cluster_with_success.csv") 
successful <- hunting_cluster_all %>% 
    filter(Harvest == "Y") %>% 
    dplyr::select(ID, Cluster, Harvest)

# Join in the times in state
successful <- left_join(successful, summary_point)

# Make in long form
successful_long <- successful %>% 
    dplyr::select(ID, Cluster, Driving_Prob, Stationary_Prob, Walking_Prob) %>% 
    mutate(ID = fct_reorder(ID, Driving_Prob)) %>% 
    rename(Driving = Driving_Prob, Stationary = Stationary_Prob, Walking = Walking_Prob) %>% 
    pivot_longer(cols = c(Driving, Stationary, Walking),
                 names_to = "State", values_to = "Percent")

# Make plot
ggplot(data = successful_long, aes(x = ID, y = Percent, fill = State)) +
    geom_bar(position = "stack", stat = "identity") +
    facet_grid(~Cluster, scales = "free", space = "free") +
    theme_minimal() +
    ylab("Percent of Time") +
    xlab("Successful Hunters") +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text.x = element_text(size = 12)) +
    scale_fill_manual(values = c("#614124", "#CC704B", "#E8C07D")) 
    #scale_fill_manual(values = c("#557B83", "#39AEA9", "#A2D5AB"))
ggsave("Figures/successful-behavior.pdf", width = 6, height = 3.5)




# Make same plot for ALL individuals
all_indiv_long <- left_join(hunting_cluster_all, summary_point) %>% 
    dplyr::select(ID, Harvest, Cluster, Driving_Prob, Stationary_Prob, Walking_Prob) %>% 
    mutate(ID = fct_reorder(ID, Driving_Prob)) %>% 
    rename(Driving = Driving_Prob, Stationary = Stationary_Prob, Walking = Walking_Prob) %>% 
    pivot_longer(cols = c(Driving, Stationary, Walking),
                 names_to = "State", values_to = "Percent")
ggplot(data = all_indiv_long, aes(x = ID, y = Percent, fill = State)) +
    geom_bar(position = "stack", stat = "identity") +
    facet_grid(~Cluster, scales = "free", space = "free") +
    theme_minimal() +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())