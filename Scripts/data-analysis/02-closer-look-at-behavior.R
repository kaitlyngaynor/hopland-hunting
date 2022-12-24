library(dplyr)
library(ggplot2)

data_hmm <- read.csv("Results/hmm-data-with-model-predictions-2022-12-05.csv")
head(data_hmm)


# Stationary on vs off road -----------------------------------------------

# Split stationary on road vs. stationary off road
stationary <- data_hmm %>% dplyr::filter(state == "Stationary")
hist(stationary$Road_Distance) # if you look closely, big peak between 5-10 meters, then drops off
# Consider within 10 meters to be "on road"

# Create new column with 4 categories
data_hmm <- data_hmm %>% 
    dplyr::mutate(state_2stationary = if_else((state == "Stationary" & Road_Distance < 10),
                                              "Stationary_road", state)) %>% 
    dplyr::mutate(state_2stationary = if_else((state_2stationary == "Stationary"),
                                              "Stationary_offroad", state_2stationary))

# Calculate duration of bouts ---------------------------------------------

# For each row, determine what order it is in the sequence of consecutive steps of the same activity
data_hmm$sequence <- NA
data_hmm$sequence[1] <- 1 # set value of first row to 1
data_hmm$bout_end <- NA
for(i in 2:nrow(data_hmm)) {
    
    # If the same individual and same behavioral state
    if(data_hmm$ID[i] == data_hmm$ID[i-1] & 
       data_hmm$state[i] == data_hmm$state[i-1]) {
        
        # Add 1 to previous row to determine place in sequence
        data_hmm$sequence[i] <- data_hmm$sequence[i-1] + 1
    
    } else {
        # Otherwise set the place in sequence to 1
        data_hmm$sequence[i] <- 1
        data_hmm$bout_end[i-1] <- "yes"
    }
}

# Only take "End" rows to determine length of each bout (in steps & minutes)
hmm_bouts <- data_hmm %>% 
    dplyr::filter(bout_end == "yes") %>% 
    dplyr::select(ID, DateTime, state, sequence) %>% 
    dplyr::rename(bout_length = sequence) %>% 
    dplyr::mutate(bout_length_min = bout_length * 3)

# Plot histogram of bout lengths by behavior
ggplot(hmm_bouts, aes(x = bout_length_min)) +
    geom_histogram() +
    facet_wrap(~state, nrow=3) +
    theme_bw()

# Plot boxplot
ggplot(hmm_bouts, aes(x = state, y = bout_length_min)) +
    geom_boxplot() +
    theme_bw()

# calculate mean bout length per individual for each state
mean_bouts <- hmm_bouts %>% 
    group_by(ID, state) %>% 
    summarize(mean_bout = mean(bout_length_min))

mean_bouts_wide <- mean_bouts %>% 
    tidyr::pivot_wider(names_from = state, values_from = mean_bout) %>% 
    dplyr::rename(Driv_bout_mean = Driving,
                  Stat_bout_mean = Stationary,
                  Walk_bout_mean = Walking)

# and for all states
mean_bouts_all <- hmm_bouts %>% 
    group_by(ID) %>% 
    summarize(All_bout_mean = mean(bout_length_min))
mean_bouts_wide <- dplyr::left_join(mean_bouts_wide, mean_bouts_all)

# and calculate number of unique bouts
bout_counts <- hmm_bouts %>% 
    count(ID, state) %>% 
    tidyr::pivot_wider(names_from = state, values_from = n) %>% 
    dplyr::rename(Driv_bout_n = Driving,
                  Stat_bout_n = Stationary,
                  Walk_bout_n = Walking) %>% 
    dplyr::mutate(All_bout_n = Driv_bout_n + Stat_bout_n + Walk_bout_n)
mean_bouts_wide <- dplyr::left_join(mean_bouts_wide, bout_counts)

# Export behavioral states with extra column, bout length
write.csv(data_hmm, "Results/hmm-data-with-model-predictions-annotated-2022-12-19.csv", row.names = FALSE)

# Export bouts
write.csv(hmm_bouts, "Results/hmm-modeled-activity-bouts-2022-12-19.csv")

# Export long bounts
write.csv(mean_bouts_wide, "Results/ID-mean-bout-length.csv", row.names = F)
