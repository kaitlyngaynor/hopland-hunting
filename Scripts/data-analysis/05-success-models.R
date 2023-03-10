library(dplyr)
library(ggplot2)
library(effects)
library(egg)

success <- read.csv("Results/hunter_cluster_success_long.csv") %>%
    dplyr::mutate(Harvest01 = ifelse(Harvest == "N",0,1)) %>% 
    tidyr::pivot_wider(names_from = "State_4state", values_from = "Percentage")

# Bring in metadata
metadata1 <- read.csv("Data/Hunting/igotu_metadata_times_cleaned_28Nov2022.csv") %>% 
    mutate(Hunt_type = "Single_day")
metadata2 <- read.csv("Data/Hunting/igotu_metadata_2019_2022_29Jan2023.csv")
metadata <- bind_rows(metadata1, metadata2)
success <- left_join(success, metadata)

# Take quick look at cluster vs multiday
success %>% 
    count(Cluster4, Hunt_type) %>% 
    ggplot(aes(x = Cluster4, y = n, fill = Hunt_type)) +
    geom_bar(stat = "identity") +
    theme_bw()
chisq.test(x = success$Hunt_type, y = success$Cluster4) # no difference in hunting mode between trophy & public hunts


# Models ------------------------------------------------------------------

# Hunting cluster
fit1 <- glm(Harvest01 ~ Cluster4, data = success, family = binomial) # AIC = 370.98
summary(fit1) 

# Year
fit2 <- glm(Harvest01 ~ Year, data = success, family = binomial) # AIC = 370.05
summary(fit2)

# Year (as factor)
success$Year_factor <- as.factor(success$Year)
fit2.1 <- glm(Harvest01 ~ Year_factor, data = success, family = binomial) # AIC = 374.52
summary(fit2.1)

# Year * single vs multiday
fit2.2 <- glm(Harvest01 ~ Year * Hunt_type, data = success, family = binomial) # AIC = 367.48
summary(fit2.2)

# Single vs multiday
fit3 <- glm(Harvest01 ~ Hunt_type, data = success, family = binomial) # AIC = 365.65
summary(fit3)

# Single vs multiday by cluster
fit4 <- glm(Harvest01 ~ Hunt_type * Cluster4, data = success, family = binomial) # AIC = 369.05
summary(fit4)

# Year by cluster
fit5 <- glm(Harvest01 ~ Year * Cluster4, data = success, family = binomial) # AIC = 372.99
summary(fit5)


# Plot model predictions --------------------------------------------------

# Hunting success by cluster
effects::predictorEffect("Cluster4", fit1) %>% 
    as_tibble() %>% 
    ggplot(aes(x = Cluster4, y = fit))+
    geom_point(aes(colour = Cluster4), 
               position = position_dodge(width = 0.5),
               size = 3)+
    geom_errorbar(aes(ymin = lower, ymax = upper, colour = Cluster4), 
                  linewidth = 0.75,
                  width = 0) +
    labs(y = "Harvest probability", x = "Cluster") +
    scale_fill_manual(values = c("#d8b365", "#969696", "#4d938a")) +
    scale_color_manual(values = c("#d8b365", "#969696", "#4d938a")) +
    theme_bw() +
    theme(panel.grid.major.x = element_blank(),
          legend.position = "none") +
    ylim(c(0, 0.25))

# Hunting success by year
effects::predictorEffect("Year", fit2) %>% 
    as_tibble() %>% 
    ggplot(aes(x = Year, y = fit))+
    geom_line()+
    geom_ribbon(aes(ymin = lower, ymax = upper), 
                alpha = 0.2) +
    labs(y = "Harvest probability", x = "Year") +
    theme_bw()

# Hunting success by year (factor)
effects::predictorEffect("Year_factor", fit2.1) %>% 
    as_tibble() %>% 
    ggplot(aes(x = Year_factor, y = fit))+
    geom_point(aes(colour = Year_factor), 
               position = position_dodge(width = 0.5),
               size = 3)+
    geom_errorbar(aes(ymin = lower, ymax = upper, colour = Year_factor), 
                  linewidth = 0.75,
                  width = 0) +
    labs(y = "Harvest probability", x = "Year") +
    theme_bw() +
    theme(panel.grid.major.x = element_blank(),
          legend.position = "none") +
    ylim(c(0, 0.75))

# Hunting success by year * type
effects::predictorEffect("Year", fit2.2) %>% 
    as_tibble() %>% 
    ggplot(aes(x = Year, y = fit, group = Hunt_type))+
    geom_line(aes(colour = Hunt_type))+
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = Hunt_type), 
                alpha = 0.2) +
    labs(y = "Harvest probability", x = "Year") +
    theme_bw()

# Hunting success by type * cluster
effects::predictorEffect("Hunt_type", fit4) %>% 
    as_tibble() %>% 
    ggplot(aes(x = Hunt_type, y = fit, group = Cluster4))+
    geom_point(aes(colour = Cluster4), 
               position = position_dodge(width = 0.5),
               size = 3)+
    geom_errorbar(aes(ymin = lower, ymax = upper, colour = Cluster4), 
                width = 0, 
                position = position_dodge(width = 0.5), 
                linewidth = 0.75) +
    labs(y = "Harvest probability", x = "Hunt duration") +
    scale_fill_manual(values = c("#d8b365", "#969696", "#4d938a")) +
    scale_color_manual(values = c("#d8b365", "#969696", "#4d938a")) +
    theme_bw() +
    theme(panel.grid.major.x = element_blank()) 

