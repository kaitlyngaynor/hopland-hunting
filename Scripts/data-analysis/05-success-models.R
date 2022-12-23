library(dplyr)
library(ggplot2)
library(effects)
library(egg)

success <- read.csv("Results/hunters_by_cluster_with_success.csv") %>%
    dplyr::filter(Harvest != "unknown") %>% 
    dplyr::mutate(Harvest01 = ifelse(Harvest == "N",0,1))

# Bring in metadata
metadata1 <- read.csv("Data/Hunting/igotu_metadata_times_cleaned_28Nov2022.csv")
metadata2 <- read.csv("Data/Hunting/igotu_metadata_2019_2020_28Nov2022.csv")
metadata <- bind_rows(metadata1, metadata2)
success <- left_join(success, metadata)

# Models ------------------------------------------------------------------

# Hunting cluster
fit <- glm(Harvest01 ~ Cluster4, data = success, family = binomial)
summary(fit) # AIC = 300.68

sjPlot::plot_model(fit)

# Hunting weekend/day
fit2 <- glm(Harvest01 ~ Year, data = success, family = binomial) # AIC = 300.2
fit3 <- glm(Harvest01 ~ Hunt_weekend, data = success, family = binomial) # AIC = 290.5
fit4 <- glm(Harvest01 ~ Hunt_day, data = success, family = binomial) # AIC = 292.1

# Cluster by weekend
fit5 <- glm(Harvest01 ~ Hunt_weekend * Cluster4, data = success, family = binomial) # 290.2


weekend <- data.frame(Hunt_weekend = seq(min(success$Hunt_weekend, na.rm=T), max(success$Hunt_weekend, na.rm=T), by = .1))
weekend$relative_risk <- predict(fit3, weekend, type = "response")


# Plot model predictions --------------------------------------------------

# Hunting success by year
effects::predictorEffect("Year", fit2) %>% 
    as_tibble() %>% 
    ggplot(aes(x = Year, y = fit))+
    geom_line()+
    geom_ribbon(aes(ymin = lower, ymax = upper), 
                alpha = 0.2) +
    labs(y = "Harvest probability", x = "Year") +
    theme_bw()

# Hunting success by weekend
effects::predictorEffect("Hunt_weekend", fit3) %>% 
    as_tibble() %>% 
    ggplot(aes(x = Hunt_weekend, y = fit))+
    geom_line()+
    geom_ribbon(aes(ymin = lower, ymax = upper), 
                alpha = 0.2) +
    labs(y = "Harvest probability", x = "Hunt weekend") +
    theme_bw()

# Hunting success by day - not as good of a fit as weekend
effects::predictorEffect("Hunt_day", fit4) %>% 
    as_tibble() %>% 
    ggplot(aes(x = Hunt_day, y = fit))+
    geom_line()+
    geom_ribbon(aes(ymin = lower, ymax = upper), 
                alpha = 0.2) +
    labs(y = "Harvest probability", x = "Hunt day") +
    theme_bw()

# Hunting success by weekend * cluster
effects::predictorEffect("Hunt_weekend", fit5) %>% 
    as_tibble() %>% 
    ggplot(aes(x = Hunt_weekend, y = fit, group = Cluster4))+
    geom_line(aes(colour = Cluster4))+
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = Cluster4), 
                alpha = 0.2) +
    labs(y = "Harvest probability", x = "Hunt weekend") +
    scale_fill_manual(values = c("#d8b365", "#969696", "#4d938a")) +
    scale_color_manual(values = c("#d8b365", "#969696", "#4d938a")) +
    theme_bw()

# Same plot as above, but facet-wrapped
effects::predictorEffect("Hunt_weekend", fit5) %>% 
    as_tibble() %>% 
    ggplot(aes(x = Hunt_weekend, y = fit)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), 
                alpha = 0.2) +
    facet_wrap(~Cluster4) +
    labs(y = "Harvest probability", x = "Hunt weekend") +
    theme_bw()
