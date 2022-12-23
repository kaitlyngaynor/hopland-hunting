library(dplyr)
library(jtools)

# Bring in datasets
available <- read.csv("Data/all-available-point-cov.csv")
available$Used <- 0
used <- read.csv("Results/hmm-data-with-model-predictions-annotated-2022-12-19.csv")
used$Used <- 1

head(available)
head(used)

# Join into single dataframe and scale covariates
used_avail <- dplyr::bind_rows(used, available) %>% 
    dplyr::select(Ruggedness, Viewshed, Habitat, Road_Distance, Chaparral_120m, Woodland_120m, 
                  Used, state, state_2stationary) %>% 
    dplyr::mutate(Ruggedness_scale = scale(Ruggedness),
                  Viewshed_scale = scale(Viewshed),
                  Road_Distance_scale = scale(Road_Distance),
                  Chaparral_120m_scale = scale(Chaparral_120m),
                  Woodland_120m_scale = scale(Woodland_120m),
                  Habitat = as.factor(Habitat))

# Split used by behavior (retain all available)
used_avail_stationary <- used_avail %>% 
    dplyr::filter((state != "Walking" & state != "Driving") | Used == 0)
used_avail_walking <- used_avail %>% 
    dplyr::filter((state != "Stationary" & state != "Driving") | Used == 0)
used_avail_driving <- used_avail %>% 
    dplyr::filter((state != "Stationary" & state != "Walking") | Used == 0)

# Run separate RSF for each behavioral state
# Explored dredging full model - the full model is best
fit_stat <- glm(Used ~ Ruggedness_scale + Viewshed_scale + Road_Distance_scale + Chaparral_120m_scale + Woodland_120m_scale,
                    data = used_avail_stationary,
                    family = binomial) 
fit_walk <- glm(Used ~ Ruggedness_scale + Viewshed_scale + Road_Distance_scale + Chaparral_120m_scale + Woodland_120m_scale,
                    data = used_avail_walking,
                    family = binomial) 
fit_driv <- glm(Used ~ Ruggedness_scale + Viewshed_scale + Road_Distance_scale + Chaparral_120m_scale + Woodland_120m_scale,
                    data = used_avail_driving,
                    family = binomial) 
# getting message that fitted probabilities equal to 0 or 1 occurred - likely because driving is SO tied to roads. model not converging
fit_driv <- glm(Used ~ Ruggedness_scale + Viewshed_scale + Chaparral_120m_scale + Woodland_120m_scale,
                data = used_avail_driving,
                family = binomial) # version without road distance in it - but doesn't make much sense

# Run one RSF for all combined
fit_all <- glm(Used ~ Ruggedness_scale + Viewshed_scale + Road_Distance_scale + Chaparral_120m_scale + Woodland_120m_scale,
                data = used_avail,
                family = binomial) 

summary(fit_stat)
summary(fit_walk)
summary(fit_driv)
summary(fit_all)

# Plot the estimates
jtools::plot_summs(fit_stat, fit_walk)
jtools::plot_summs(fit_stat, fit_walk, fit_driv,
                   model.names = c("Stationary", "Walking", "Driving"))
jtools::plot_summs(fit_stat, fit_walk, fit_all,
                   model.names = c("Stationary", "Walking", "All states"))

