library(dplyr)
library(glmulti)
library(MuMIn)

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
fit_stat <- glm(Used ~ Ruggedness_scale + Viewshed_scale + Habitat + Road_Distance_scale + Chaparral_120m_scale + Woodland_120m_scale,
                    data = used_avail_stationary,
                    family = binomial) 
fit_walk <- glm(Used ~ Ruggedness_scale + Viewshed_scale + Habitat + Road_Distance_scale + Chaparral_120m_scale + Woodland_120m_scale,
                    data = used_avail_walking,
                    family = binomial) 
fit_driv <- glm(Used ~ Ruggedness_scale + Viewshed_scale + Habitat + Road_Distance_scale + Chaparral_120m_scale + Woodland_120m_scale,
                    data = used_avail_driving,
                    family = binomial) 
summary(fit_stat)
summary(fit_walk)
summary(fit_driv)

library(sjPlot)
