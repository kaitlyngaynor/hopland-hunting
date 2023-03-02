library(dplyr)
library(ggplot2)
library(effects)
library(egg)

success <- read.csv("Results/hunters_by_cluster_with_success.csv") %>%
    dplyr::mutate(Harvest01 = ifelse(Harvest == "N",0,1))

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
cluster_multiday <- chisq.test(x = success$Hunt_type, y = success$Cluster4)

# Models ------------------------------------------------------------------

# Hunting cluster
fit <- glm(Harvest01 ~ Cluster4, data = success, family = binomial)
summary(fit) # AIC = 370.98

sjPlot::plot_model(fit)

# Hunting weekend/day
fit2 <- glm(Harvest01 ~ Year, data = success, family = binomial) # AIC = 370.05
fit3 <- glm(Harvest01 ~ Hunt_weekend, data = success, family = binomial) # AIC = 362.6

# Cluster by weekend
fit4 <- glm(Harvest01 ~ Hunt_weekend * Cluster4, data = success, family = binomial) # 364.02

# Single vs multiday
fit5 <- glm(Harvest01 ~ Hunt_type, data = success, family = binomial) # 365.65

# Single vs multiday by cluster
fit6 <- glm(Harvest01 ~ Hunt_type * Cluster4, data = success, family = binomial) # 369.05


weekend <- data.frame(Hunt_weekend = seq(min(success$Hunt_weekend, na.rm=T), max(success$Hunt_weekend, na.rm=T), by = .1))
weekend$relative_risk <- predict(fit3, weekend, type = "response")


# Plot model predictions --------------------------------------------------

# Hunting success by cluster
effects::predictorEffect("Cluster4", fit) %>% 
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

# Hunting success by duration * cluster
effects::predictorEffect("Hunt_type", fit6) %>% 
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

