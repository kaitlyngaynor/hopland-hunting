# See what's up with the step lengths. Is this why some tracks are really wonky looking?

library(dplyr)
library(readr)

igotu_data <- read_csv("Data/igotu_data_3min.csv")

# calculate time difference
igotu_data <- igotu_data[order(igotu_data$ID, igotu_data$DateTime),]

igotu_data$tdiff <- unlist(tapply(igotu_data$DateTime, INDEX = igotu_data$ID,
                          FUN = function(x) c(0, `units<-`(diff(x), "mins"))))
head(igotu_data)
hist(igotu_data$tdiff)

# just look at ones that are more than 10 minutes apart
long_steps <- igotu_data %>% 
    filter(tdiff > 10)
long_steps

# by individual
count(long_steps, ID)

# what about more than 6 min
long_steps6 <- igotu_data %>% 
    filter(tdiff > 6)
count(long_steps6, ID)
