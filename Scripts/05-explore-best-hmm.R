# Explore the top HMM model

library(dplyr)
library(moveHMM)
library(tidyr)
library(ggplot2)
library(sf)
library(raster)
library(SpatialKDE)
library(viridis)

# Set-up ------------------------------------------------------------------

# Load cleaned data
igotu_data <- read.csv("Data/igotu_data_3min_covariates.csv")

# Select columns of interest
igotu_data_fewer <- igotu_data %>% 
    dplyr::select(ID, Party_ID, Longitude, Latitude, DateTime,
                  rugged49.clean, rugged25.clean, rugged9.clean,
                  vegetation.coarser.clean2, view_for_kg_proj,
                  veg.edges.dist.clean, road.dist.clean,
                  grass_120m, chap_120m, wood_120m,
                  Elapsed_Time_Sunrise, Harvest)

# prep data for HMM
data_hmm <- moveHMM::prepData(igotu_data_fewer, 
                              type="LL", 
                              coordNames=c("Longitude","Latitude"))

# remove 225 step lengths of 'NA'
data_hmm <- data_hmm %>% 
    drop_na(step) 

# filter out all steps > 15mph
data_hmm <- data_hmm %>% 
    filter(step < 1.207008)

# scale covariates
data_hmm$rugged49_scale <- scale(data_hmm$rugged49.clean)
data_hmm$rugged25_scale <- scale(data_hmm$rugged25.clean)
data_hmm$rugged9_scale <- scale(data_hmm$rugged9.clean)
data_hmm$view_scale <- scale(data_hmm$view_for_kg_proj)
data_hmm$vegedge_scale <- scale(data_hmm$veg.edges.dist.clean)
data_hmm$road_scale <- scale(data_hmm$road.dist.clean)
data_hmm$grass_scale <- scale(data_hmm$grass_120m)
data_hmm$chap_scale <- scale(data_hmm$chap_120m)
data_hmm$wood_scale <- scale(data_hmm$wood_120m)
data_hmm$sunrise_scale <- scale(data_hmm$Elapsed_Time_Sunrise)

# set initial parameters (determined by 04b-hmm-3state-parameter-select.Rmd)
mu0_3state <- c(0.01, 0.07, 0.41)
sigma0_3state <- c(0.010, 0.045, 0.204)
zeromass0_3state <- c(0.469, 0.047, 0.0028) 
stepPar0_3state <- c(mu0_3state,sigma0_3state,zeromass0_3state)
angleMean0_3state <- c(pi,0,0)
kappa0_3state <- c(0.61,1.20,1.14)
anglePar0_3state <- c(angleMean0_3state,kappa0_3state)


# Run & explore model -----------------------------------------------------------------

# see vignette https://cran.r-project.org/web/packages/moveHMM/vignettes/moveHMM-guide.pdf

# explore top model (which was the full model), as determined in 04-hmm-3state.Rmd
# takes about 2.5 hours to run
#m <- fitHMM(data=data_hmm, nbStates=3, stepPar0=stepPar0_3state, anglePar0=anglePar0_3state,
#                   formula = ~road_scale + view_scale + wood_scale + rugged9_scale + chap_scale + sunrise_scale)
#saveRDS(m, "hmm-top-model-2022-05-09.Rds")
m <- readRDS("hmm-top-model-2022-05-09.Rds")

# model summary
m 

# proportion of time spent in each state
states <- viterbi(m)
prop.table(table(states)) 

# add most likely state to data
data_hmm$state <- factor(viterbi(m))
levels(data_hmm$state) <- c("Stationary", "Walking", "Driving") # change factor level names

# plot model results
plot(m, plotCI = TRUE)

# look at CIs of parameters
CI(m)

# plot stationary state probabilities
plotStationary(m, plotCI=TRUE)

# compute the pseudo-residuals
pr <- pseudoRes(m)
hist(pr$stepRes)
ks.test(x=pr$stepRes,y='pnorm',alternative='two.sided')
hist(pr$angleRes)
shapiro.test(pr$angleRes)
ks.test(x=pr$angleRes,y='pnorm',alternative='two.sided')
# From K-S test, residuals are NOT normally distributed, BUT we have like 55,000 points so it's going to be significantly different from normal. Good enough?

# time series, qq-plots, and ACF of the pseudo-residuals
plotPR(m)


# Map three states --------------------------------------------------------

# Join with coordinates
data_hmm_sf <- left_join(data_hmm,
                         dplyr::select(igotu_data, ID, DateTime, Latitude, Longitude)) |> 
    st_as_sf(coords = c("Longitude", "Latitude"),
             crs = "+proj=longlat +datum=WGS84", 
             remove = FALSE)

# Split observations into the three behavioral states
walking <- data_hmm_sf |> 
    dplyr::filter(state == "Walking") |> 
    st_transform(crs = "+proj=utm +zone=10 +datum=WGS84")
stationary <- data_hmm_sf |> 
    dplyr::filter(state == "Stationary") |> 
    st_transform(crs = "+proj=utm +zone=10 +datum=WGS84")
driving <- data_hmm_sf |> 
    dplyr::filter(state == "Driving") |> 
    st_transform(crs = "+proj=utm +zone=10 +datum=WGS84")

# Create 100m resolution reference raster to use for kernel density estimation
sf::sf_use_s2(FALSE)
huntable <- read_sf("Data/Spatial data/huntable.shp") |> 
     st_transform(crs = "+proj=utm +zone=10 +datum=WGS84")
reference <- create_raster(huntable, cell_size = 100) 
 
# alternatively, use 10m x 10m raster
# reference <- raster("Data/Spatial data/Cleaned rasters/road.dist.clean.tif")

# Calculate kernel density for walking
walking_dens <- SpatialKDE::kde(walking, 
                                band_width = 400,
                                grid = reference)
plot(walking_dens, 
     col = viridis(1e3), 
     zlim=c(0,500),
     main = "Walking")

# Calculate kernel density for driving
driving_dens <- SpatialKDE::kde(driving, 
                                band_width = 400,
                                grid = reference)
plot(driving_dens, 
     col = viridis(1e3), 
     zlim=c(0,500),
     main = "Driving")

# Calculate kernel density for stationary
stationary_dens <- SpatialKDE::kde(stationary, 
                                band_width = 400,
                                grid = reference)
plot(stationary_dens, 
     col = viridis(1e3), 
     zlim=c(0,500),
     main = "Stationary")

# Write all rasters to file
writeRaster(walking_dens, "Data/Spatial data/Hunter_KDE/walking_dens.tif")
writeRaster(driving_dens, "Data/Spatial data/Hunter_KDE/driving_dens.tif")
writeRaster(stationary_dens, "Data/Spatial data/Hunter_KDE/stationary_dens.tif")


# Explore time spent in states across hunters -----------------------------

# For each hunter, calculate number of points in each state
hunter_summary <- data_hmm |> 
    group_by(ID) |> 
    count(state) 

# Calculate percent of time spent in each state for each hunter
hunter_percentages <- hunter_summary |> 
    pivot_wider(names_from = state, values_from = n) |> 
    replace_na(list(Stationary = 0, Walking = 0, Driving = 0)) |> 
    mutate(Total = sum(Stationary, Walking, Driving),
           Stationary_pct = Stationary/Total,
           Walking_pct = Walking/Total,
           Driving_pct = Driving/Total) |> 
    dplyr::select(-c(Stationary, Walking, Driving, Total)) |> 
    ungroup()

# Create long version of datafrae
hunter_percentages_long <- hunter_percentages |> 
    pivot_longer(cols = c(Stationary_pct, Walking_pct, Driving_pct),
                 names_to = "State",
                 values_to = "Percentage")

# Histogram of percentage of time spent in each state
ggplot(hunter_percentages_long, aes(x = Percentage)) +
    facet_wrap(~State, nrow = 3) +
    geom_histogram() +
    theme_bw() +
    xlab("Percentage of Time Spent in Behavioral State")


# K-means clustering----------------------------------------------

# Prep for K-means cluster analysis
hunter_percentages_noID <- hunter_percentages |> 
    dplyr::select(-ID) 
set.seed(123)

# Elbow Method for finding the optimal number of clusters (k=2 to k=15)
# THREE CLUSTERS IS OPTIMAL
k.max <- 15
data <- hunter_percentages_noID
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# Run K-means cluster analysis with k=3 clusters
k3 <- kmeans(as.matrix(hunter_percentages_noID), centers = 3, nstart = 25)
k3

# RESULTS:
# K-means clustering with 3 clusters of sizes 75, 85, 65
# 
# Cluster means:
#     Stationary_pct Walking_pct Driving_pct
# 1      0.4939986   0.1879199   0.3180815
# 2      0.2073040   0.2034379   0.5892581
# 3      0.2695095   0.4529574   0.2775332    

# Assign cluster to each point
hunter_percentages$Cluster = factor(k3$cluster)
levels(hunter_percentages$Cluster) <- c("Waiters", "Drivers", "Walkers") # change factor level names

# Join assigned clusters with long data also
hunter_percentages_long <- left_join(hunter_percentages_long,
                                     dplyr::select(hunter_percentages, ID, Cluster))

# Join cluster with hunting success
hunter_success <- data_hmm |> 
    dplyr::select(ID, Harvest) |> 
    unique()
hunter_cluster_success <- left_join(hunter_percentages, hunter_success)
hunter_cluster_success_long <- left_join(hunter_percentages_long, hunter_success)

# Make histogram of time spent in each state across clusters
ggplot(hunter_percentages_long, aes(x = Percentage, fill = State)) +
    facet_grid(State~Cluster) +
    geom_histogram() +
    theme_bw() +
    xlab("Percentage of Time Spent in Behavioral State")

# Boxplot of time spent in each state BY Cluster
ggplot(hunter_cluster_success_long, aes(y = Percentage,
                                        x = Cluster,
                                        fill = State)) +
    geom_boxplot() +
    theme_bw()

# Boxplot of time spent in each state BY success
ggplot(hunter_cluster_success_long, aes(y = Percentage,
                                        x = State,
                                        fill = Harvest)) +
    geom_boxplot() +
    theme_bw()

# Plot success by cluster
ggplot(hunter_cluster_success, aes(x = Cluster, fill = Harvest)) +
    geom_bar(stat = "count") + 
    theme_bw() +
    xlab("Hunting Mode")

# Calculate success rate by cluster
success_rate <- hunter_cluster_success |>
    count(Cluster, Harvest) |> 
    pivot_wider(id_cols = Cluster,
                names_from = Harvest,
                values_from = n) |> 
    mutate(Total = N + Y,
           Success_Rate = Y/Total,
           Failure_rate = N/Total)
    

# Model success rate as function of dominant mode
hunter_cluster_success <- hunter_cluster_success %>%
    mutate(Harvest01 = ifelse(Harvest == "N",0,1))
fit <- glm(Harvest01 ~ Cluster, data = hunter_cluster_success,
           family = binomial)
summary(fit)
library(sjPlot)
plot_model(fit)



# Explore states in the hour before successful hunt ----------------------------------------

library(hms)

# Identify 31 harvest tracks with associated time
metadata <- read.csv("Data/Hunting/igotu_metadata_times_cleaned_03Feb2022.csv") |> 
    filter(Harvest == "Y") |> 
    filter(Harvest_time != "tbd") |> 
    filter(Harvest_time != "TBD")

# Filter tracks to just the hour before harvest
harvest_hour <- igotu_data |> 
    filter(ID %in% metadata$ID) |> 
    left_join(metadata)

# Calculate time from harvest
harvest_hour$Time_to_Hunt <- NA
for(j in 1:nrow(harvest_hour)) {
    harvest_hour$Time_to_Hunt[j] <- as.numeric(difftime(as_hms(harvest_hour$Time[j]),
                                                       as_hms(strptime(harvest_hour$Harvest_time[j], "%H:%M")),
                                                       units = "hours"))
}

# Take just the hour before harvest
harvest_hour <- harvest_hour |> 
    filter(Time_to_Hunt < 0) |> 
    filter(Time_to_Hunt > -1)

# Join with state
harvest_hour <- left_join(harvest_hour,
                          dplyr::select(data_hmm, ID, DateTime, state))

count(harvest_hour, state)
