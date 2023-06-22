library(ggplot2)
library(dplyr)
library(viridis)
library(gtable)
library(lemon)

all_rsf_results <- read.csv("Results/rsf-results-by-mode-success-30min.csv") %>% 
    dplyr::filter(Predictor != "Intercept") %>% 
    dplyr::rename("Hunting Mode" = "Hunting.Mode")
head(all_rsf_results)

# without road distance
all_rsf_results %>% 
    dplyr::filter(Predictor != "Road Distance") %>% 
    #ggplot(aes(x = Predictor, y = Coefficient, col = `Hunting Mode`, shape = Harvest, group = `Hunting Mode`)) + 
    ggplot(aes(x = Predictor, y = Coefficient, col = `Hunting Mode`, shape = Harvest)) + 
    geom_hline(aes(yintercept = 0), linetype="dashed", size = 0.5, color = "darkgrey") +
    geom_errorbar(aes(ymin = LCI, ymax = UCI), width=0, position = position_dodge(width = 1), alpha=.5) +
    geom_point(position = position_dodge(width = 1), size = 2, stroke = 1, alpha = .75) +
    theme(axis.title.y = element_blank(),
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
    scale_y_continuous(breaks=c(-1, -.75, -.5, -.25, 0, .25)) +
    coord_flip() + # switch x and y coordinates
    ylab("Coefficient") +
    scale_shape_manual(values=c(1, 19))+
    scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3"),
                       guide = guide_legend(reverse = TRUE)) +
    facet_grid(Predictor~., scales = "free")
ggsave("Figures/rsf-coefficients-1-30min.pdf", width = 8, height = 4)

# only road distance
all_rsf_results %>% 
    dplyr::filter(Predictor == "Road Distance") %>% 
    #ggplot(aes(x = Predictor, y = Coefficient, col = `Hunting Mode`, shape = Harvest, group = `Hunting Mode`)) + 
    ggplot(aes(x = Predictor, y = Coefficient, col = `Hunting Mode`, shape = Harvest)) + 
    geom_hline(aes(yintercept = 0), linetype="dashed", size = 0.5, color = "darkgrey") +
    geom_errorbar(aes(ymin = LCI, ymax = UCI), width=0, position = position_dodge(width = 1), alpha=.5) +
    geom_point(position = position_dodge(width = 1), size = 2, stroke = 1, alpha = .75) +
    theme(axis.title.y = element_blank(),
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
    ylab("Coefficient") +
    ylim(-5, 0.5) +
    scale_shape_manual(values=c(1, 19))+
    scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3"),
                       guide = guide_legend(reverse = TRUE)) +
    facet_grid(Predictor~., scales = "free") 
ggsave("Figures/rsf-coefficients-2-30min.pdf", width = 8.15, height = 1.3)
