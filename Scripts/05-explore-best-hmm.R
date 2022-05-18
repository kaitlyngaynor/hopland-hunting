# Explore the top HMM model

library(dplyr)
library(moveHMM)
library(tidyr)
library(ggplot2)
library(sf)
library(raster)
library(SpatialKDE)

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


# Model -------------------------------------------------------------------

# see vignette https://cran.r-project.org/web/packages/moveHMM/vignettes/moveHMM-guide.pdf

# explore top model (which was the full model), as determined in 04-hmm-3state.Rmd
# takes about 2.5 hours to run
#m <- fitHMM(data=data_hmm, nbStates=3, stepPar0=stepPar0_3state, anglePar0=anglePar0_3state,
#                   formula = ~road_scale + view_scale + wood_scale + rugged9_scale + chap_scale + sunrise_scale)
#saveRDS(m, "hmm-top-model-2022-05-09.Rds")
m <- readRDS("hmm-top-model-2022-05-09.Rds")

# make a version without roads
#m2 <- fitHMM(data=data_hmm, nbStates=3, stepPar0=stepPar0_3state, anglePar0=anglePar0_3state,
#            formula = ~ view_scale + wood_scale + rugged9_scale + chap_scale + sunrise_scale)
#saveRDS(m2, "hmm-top-model-no-road-2022-05-09.Rds")

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

# bring in locations
data_hmm_sf <- left_join(data_hmm,
                         dplyr::select(igotu_data, ID, DateTime, Latitude, Longitude)) |> 
    st_as_sf(coords = c("Longitude", "Latitude"),
             crs = "+proj=longlat +datum=WGS84", 
             remove = FALSE)

walking <- data_hmm_sf |> 
    dplyr::filter(state == "Walking") |> 
    st_transform(crs = "+proj=utm +zone=10 +datum=WGS84")
stationary <- data_hmm_sf |> 
    dplyr::filter(state == "Stationary") |> 
    st_transform(crs = "+proj=utm +zone=10 +datum=WGS84")
driving <- data_hmm_sf |> 
    dplyr::filter(state == "Driving") |> 
    st_transform(crs = "+proj=utm +zone=10 +datum=WGS84")

# bring in reference raster
reference <- raster("Data/Spatial data/Cleaned rasters/blm_dist.tif")
sf::sf_use_s2(FALSE)
huntable <- read_sf("Data/Spatial data/huntable.shp") |> 
    st_transform(crs = "+proj=utm +zone=10 +datum=WGS84")
reference2 <- create_raster(huntable, cell_size = 100) 

library(viridis)
# kernel density for walking
walking_dens <- SpatialKDE::kde(walking, 
                                band_width = 400,
                                grid = reference2)
plot(walking_dens, 
     col = viridis(1e3), 
     zlim=c(0,500),
     main = "Walking")

# kernel density for driving
driving_dens <- SpatialKDE::kde(driving, 
                                band_width = 400,
                                grid = reference2)
plot(driving_dens, 
     col = viridis(1e3), 
     zlim=c(0,500),
     main = "Driving")

# kernel density for stationary
stationary_dens <- SpatialKDE::kde(stationary, 
                                band_width = 400,
                                grid = reference2)
plot(stationary_dens, 
     col = viridis(1e3), 
     zlim=c(0,500),
     main = "Stationary")

# K-means clustering----------------------------------------------

# for each hunter, calculate number of points in each state
hunter_summary <- data_hmm |> 
    group_by(ID) |> 
    count(state) 

hunter_percentages <- hunter_summary |> 
    pivot_wider(names_from = state, values_from = n) |> 
    replace_na(list(Stationary = 0, Walking = 0, Driving = 0)) |> 
    mutate(Total = sum(Stationary, Walking, Driving),
           Stationary_pct = Stationary/Total,
           Walking_pct = Walking/Total,
           Driving_pct = Driving/Total) |> 
    dplyr::select(-c(Stationary, Walking, Driving, Total)) |> 
    ungroup()

hunter_percentages_long <- hunter_percentages |> 
    pivot_longer(cols = c(Stationary_pct, Walking_pct, Driving_pct),
                 names_to = "State",
                 values_to = "Percentage")

ggplot(hunter_percentages_long, aes(x = Percentage)) +
    facet_wrap(~State, nrow = 3) +
    geom_histogram() +
    theme_bw()

# K-means cluster analysis
hunter_percentages_noID <- hunter_percentages |> 
    dplyr::select(-ID) 
k3 <- kmeans(as.matrix(hunter_percentages_noID), centers = 3, nstart = 25)
k3

# K-means clustering with 3 clusters of sizes 85, 67, 73
# 
# Cluster means:
#     Stationary_pct Walking_pct Driving_pct
# 1      0.2073040   0.2034379   0.5892581 
# 2      0.2732064   0.4491096   0.2776839 
# 3      0.4967559   0.1841900   0.3190541      

# assign cluster to each point
hunter_percentages$Cluster = factor(k3$cluster)
levels(hunter_percentages$Cluster) <- c("Drivers", "Walkers", "Sitters") # change factor level names

# funny 3d plot
library(plotly)
p <- plot_ly(hunter_percentages_noID, 
             x=~Stationary_pct, 
             y=~Walking_pct, 
             z=~Driving_pct, 
             color=~Cluster) %>%
    add_markers(size=1.5)
print(p)

#Elbow Method for finding the optimal number of clusters - THREE CLUSTERS IS OPTIMAL
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
data <- hunter_percentages_noID
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


# join in with success
hunter_success <- data_hmm |> 
    dplyr::select(ID, Harvest) |> 
    unique()
hunter_cluster_success <- left_join(hunter_percentages, hunter_success)

# plot success by cluster
ggplot(hunter_cluster_success, aes(x = Cluster, fill = Harvest)) +
    geom_bar(stat = "count") + 
    theme_bw() +
    xlab("Hunting Mode")

# calculate success rate by mode
success_rate <- hunter_cluster_success |>
    count(Cluster, Harvest) |> 
    pivot_wider(id_cols = Cluster,
                names_from = Harvest,
                values_from = n) |> 
    mutate(Total = N + Y,
           Success_Rate = Y/Total,
           Failure_rate = N/Total)
    

# model of success rate as function of dominant mode
hunter_cluster_success <- hunter_cluster_success %>%
    mutate(Harvest01 = ifelse(Harvest == "N",0,1))
fit <- glm(Harvest01 ~ Cluster, data = hunter_cluster_success,
           family = binomial)
summary(fit)
library(sjPlot)
plot_model(fit)





# Successful vs unsuccessful hunters --------------------------------------

# split by successful & unsuccessful
data_hmm_success <- data_hmm |> 
    filter(Harvest == "Y")
data_hmm_unsuccess <- data_hmm |> 
    filter(Harvest == "N")

# remove any individuals from successful parties from "unsuccess" to be more conservative
`%notin%` <- Negate(`%in%`)
data_hmm_unsuccess_cons <- data_hmm_unsuccess |> 
    filter(Party_ID %notin% unique(data_hmm_success$Party_ID))

# run models
m_success <- fitHMM(data=data_hmm_success, nbStates=3, stepPar0=stepPar0_3state, anglePar0=anglePar0_3state,
                    formula = ~road_scale + view_scale + wood_scale + rugged9_scale + chap_scale + sunrise_scale)
saveRDS(m_success, "hmm-top-model-successfulharvest-2022-05-09.Rds")
m_unsuccess <- fitHMM(data=data_hmm_unsuccess, nbStates=3, stepPar0=stepPar0_3state, anglePar0=anglePar0_3state,
                      formula = ~road_scale + view_scale + wood_scale + rugged9_scale + chap_scale + sunrise_scale)
saveRDS(m_unsuccess, "hmm-top-model-unsuccessfulharvest-2022-05-09.Rds")
m_unsuccess_cons <- fitHMM(data=data_hmm_unsuccess_cons, nbStates=3, stepPar0=stepPar0_3state, anglePar0=anglePar0_3state,
                      formula = ~road_scale + view_scale + wood_scale + rugged9_scale + chap_scale + sunrise_scale)
saveRDS(m_unsuccess_cons, "hmm-top-model-unsuccessfulharvest-cons-2022-05-09.Rds")

# explore results

# proportion of time spent in each state
states_s <- viterbi(m_success)
prop.table(table(states_s)) 

states_u <- viterbi(m_unsuccess)
prop.table(table(states_u)) 

states_uc <- viterbi(m_unsuccess_cons)
prop.table(table(states_uc)) 

# plot stationary state probabilities
pdf("Figures/stationary-state-successful.pdf")
plotStationary(m_success, plotCI=TRUE)
dev.off()

pdf("Figures/stationary-state-unsuccessful.pdf")
plotStationary(m_unsuccess, plotCI=TRUE)
dev.off()

pdf("Figures/stationary-state-unsuccessful-cons.pdf")
plotStationary(m_unsuccess_cons, plotCI=TRUE)
dev.off()



# Only hour before successful hunt ----------------------------------------

# look at 31 harvest tracks
metadata <- read.csv("Data/Hunting/igotu_metadata_times_cleaned_03Feb2022.csv") |> 
    filter(Harvest == "Y") |> 
    filter(Harvest_time != "tbd") |> 
    filter(Harvest_time != "TBD")

# filter tracks to just the hour before harvest
harvest_hour <- igotu_data |> 
    filter(ID %in% metadata$ID) |> 
    left_join(metadata)

# calculate time from harvest
library(hms)
harvest_hour$Time_to_Hunt <- NA
for(j in 1:nrow(harvest_hour)) {
    harvest_hour$Time_to_Hunt[j] <- as.numeric(difftime(as_hms(harvest_hour$Time[j]),
                                                       as_hms(strptime(harvest_hour$Harvest_time[j], "%H:%M")),
                                                       units = "hours"))
}

# take just the hour before harvest
harvest_hour2 <- harvest_hour |> 
    filter(Time_to_Hunt < 0) |> 
    filter(Time_to_Hunt > -1)

# Select columns of interest & attached scaled values
harvest_hour_fewer <- harvest_hour2 %>% 
    dplyr::select(ID, Party_ID, Longitude, Latitude, DateTime,
                  rugged49.clean, rugged25.clean, rugged9.clean,
                  vegetation.coarser.clean2, view_for_kg_proj,
                  veg.edges.dist.clean, road.dist.clean,
                  grass_120m, chap_120m, wood_120m,
                  Elapsed_Time_Sunrise, Harvest) |> 
    left_join(dplyr::select(data_hmm, -c(step, angle, x, y)))

# prep data for HMM
data_hmm_hour <- moveHMM::prepData(harvest_hour_fewer, 
                              type="LL", 
                              coordNames=c("Longitude","Latitude"))

# remove step lengths of 'NA'
data_hmm_hour <- data_hmm_hour %>% 
    drop_na(step) 

# filter out all steps > 15mph - results in 278 observations
data_hmm_hour <- data_hmm_hour %>% 
    filter(step < 1.207008)

# NOT WORKING - rror in nlm(nLogLike, wpar, nbStates, bounds, parSize, data, stepDist,  :  non-finite value supplied by 'nlm'
m_hour <- fitHMM(data=data_hmm_hour, nbStates=3, stepPar0=stepPar0_3state, anglePar0=anglePar0_3state,
                   formula = ~road_scale + view_scale + wood_scale + rugged9_scale + chap_scale + sunrise_scale)
saveRDS(m_hour, "hmm-top-model-hour-2022-05-17.Rds")

plot(m_hour)

# plot stationary state probabilities
plotStationary(m, plotCI=TRUE)
