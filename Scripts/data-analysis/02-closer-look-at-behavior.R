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
                                              "Stationary_road", state))

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
ggplot(hmm_bouts, aes(x = bout_length)) +
    geom_histogram() +
    facet_wrap(~state, nrow=3) +
    theme_bw()

# Plot boxplot
ggplot(hmm_bouts, aes(x = state, y = bout_length)) +
    geom_boxplot() +
    theme_bw()

# calculate mean bout length per individual for each state
hmm_bouts %>% 
    group_by(ID, state) %>% 
    summarize(mean_bout = mean(bout_length))



# Export behavioral states with extra column, bout length
write.csv(data_hmm, "Results/hmm-data-with-model-predictions-annotated-2022-12-19.csv", row.names = FALSE)

# Export bouts
write.csv(hmm_bouts, "Results/hmm-modeled-activity-bouts-2022-12-19.csv")

