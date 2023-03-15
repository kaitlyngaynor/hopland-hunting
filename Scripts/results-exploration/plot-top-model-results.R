# Explore results of the movement model

library(moveHMM)
library(ggplot2)
library(dplyr)

# Import model and explore results ----------------------------------------

m <- readRDS("Results/hmm-top-model-2023-03-10.Rds")

# model summary
m 

# proportion of time spent in each state
states <- viterbi(m)
prop.table(table(states)) 

# plot model results
plot(m, plotCI = TRUE)

# Save step length & turn angle plots manually 6"x4"
# step-length-raw.pdf and turn-angle-raw.pdf

# look at CIs of parameters
ci_hmm <- CI(m)

# export parameters
mean_betas <- as.data.frame(m$mle$beta)
mean_betas <- tibble::rownames_to_column(mean_betas, "VALUE")
names(mean_betas) <- c("Covariate", "stat_to_walk", "stat_to_driv", "walk_to_stat", "walk_to_driv", "driv_to_stat", "driv_to_walk")
mean_betas <- tidyr::pivot_longer(mean_betas, -Covariate, names_to = "Transitions", values_to = "Mean")
ci_lower <- as.data.frame(ci_hmm$beta$lower)
ci_lower <- tibble::rownames_to_column(ci_lower, "VALUE")
names(ci_lower) <- c("Covariate", "stat_to_walk", "stat_to_driv", "walk_to_stat", "walk_to_driv", "driv_to_stat", "driv_to_walk")
ci_lower <- tidyr::pivot_longer(ci_lower, -Covariate, names_to = "Transitions", values_to = "LCI")
ci_upper <- as.data.frame(ci_hmm$beta$upper)
ci_upper <- tibble::rownames_to_column(ci_upper, "VALUE")
names(ci_upper) <- c("Covariate", "stat_to_walk", "stat_to_driv", "walk_to_stat", "walk_to_driv", "driv_to_stat", "driv_to_walk")
ci_upper <- tidyr::pivot_longer(ci_upper, -Covariate, names_to = "Transitions", values_to = "UCI")
transition_parameters <- dplyr::left_join(mean_betas, ci_lower) %>% dplyr::left_join(ci_upper)

write.csv(transition_parameters, "Results/hmm-transition-probabilities.csv", row.names = F)

# plot stationary state probabilities
plotStationary(m, plotCI=TRUE)

# compute the pseudo-residuals
pr <- pseudoRes(m)
hist(pr$stepRes)
ks.test(x=pr$stepRes,y='pnorm',alternative='two.sided')
hist(pr$angleRes)
shapiro.test(pr$angleRes)
ks.test(x=pr$angleRes,y='pnorm',alternative='two.sided')
# From K-S test, residuals are NOT normally distributed, BUT we have many points so it's going to be significantly different from normal. Likely good enough.

# time series, qq-plots, and ACF of the pseudo-residuals
plotPR(m)


# plot transition probabilities
transition_parameters %>% 
    dplyr::filter(Covariate %in% c("Chaparral_120m_scale", "Ruggedness_scale", "Sunrise_Scale", "Viewshed_scale", "Woodland_120m_scale")) %>% 
    ggplot(aes(x = Covariate, y = Mean, col = Transitions)) + 
    geom_hline(aes(yintercept = 0), linetype="dashed", size = 0.5, color = "darkgrey") +
    geom_errorbar(aes(ymin = LCI, ymax = UCI), width=0, position = position_dodge(width = 1), alpha=.75) +
    geom_point(position = position_dodge(width = 1), size = 2) +
    theme(axis.title.y = element_blank(),
          legend.title = element_blank(),
          legend.key = element_rect(fill = alpha("white", 0.0)),
          strip.background = element_blank(),
          strip.text = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_text(size=rel(1.25)),
          panel.border = element_rect(color = "black", fill = NA, size = .5),
          legend.text=element_text(size=rel(1))
    ) +
    coord_flip() + # switch x and y coordinates
    ylab("Beta Coefficient") +
    scale_color_manual(values = c("darkblue", "#1e9b56", "#99a82b", "#f8be1d", "#f27229", "#ea3633"),
                       guide = guide_legend(reverse = TRUE)) +
    facet_grid(Covariate~., scales = "free")

# plot just road transition prob
transition_parameters %>% 
    dplyr::filter(Covariate %in% c("Road_Distance_scale")) %>% 
    ggplot(aes(x = Covariate, y = Mean, col = Transitions)) + 
    geom_hline(aes(yintercept = 0), linetype="dashed", size = 0.5, color = "darkgrey") +
    geom_errorbar(aes(ymin = LCI, ymax = UCI), width=0, position = position_dodge(width = 1), alpha=.75) +
    geom_point(position = position_dodge(width = 1), size = 2) +
    theme(axis.title.y = element_blank(),
          legend.title = element_blank(),
          legend.key = element_rect(fill = alpha("white", 0.0)),
          strip.background = element_blank(),
          strip.text = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_text(size=rel(1.25)),
          panel.border = element_rect(color = "black", fill = NA, size = .5),
          legend.text=element_text(size=rel(1))
    ) +
    coord_flip() + # switch x and y coordinates
    ylab("Beta Coefficient") +
    scale_color_manual(values = c("darkblue", "#1e9b56", "#99a82b", "#f8be1d", "#f27229", "#ea3633"),
                       guide = guide_legend(reverse = TRUE)) +
    facet_grid(Covariate~., scales = "free")

