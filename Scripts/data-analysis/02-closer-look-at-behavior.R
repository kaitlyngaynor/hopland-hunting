library(dplyr)
library(ggplot2)

data_hmm <- read.csv("Results/hmm-top-model-2023-03-04.csv")
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

# Export behavioral states with extra column
write.csv(data_hmm, "Results/hmm-data-with-model-predictions-annotated-2023-03-04.csv", row.names = FALSE)