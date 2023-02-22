library(ggplot2)
library(dplyr)
library(viridis)
library(gtable)
library(lemon)

all_rsf_results <- read.csv("Results/rsf-results-by-mode-success.csv") %>% 
    dplyr::filter(Predictor != "Intercept") %>% 
    dplyr::rename("Hunting Mode" = "Hunting.Mode")
head(all_rsf_results)

# without road distance
all_rsf_results %>% 
    dplyr::filter(Predictor != "Road Distance") %>% 
    ggplot(aes(x = Predictor, y = Coefficient, col = `Hunting Mode`, shape = Harvest, group = `Hunting Mode`)) + 
    geom_hline(aes(yintercept = 0), linetype="dashed", size = 0.5, color = "darkgrey") +
    geom_errorbar(aes(ymin = LCI, ymax = UCI), width=0, position = position_dodge(width = 1), alpha=.8) +
    geom_point(position = position_dodge(width = 1), size = 3, stroke = 1, alpha = .75) +
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
    ylim(-0.75, 0.3) +
    scale_shape_manual(values=c(1, 19))+
    scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3"),
                       guide = guide_legend(reverse = TRUE)) +
    facet_grid(Predictor~., scales = "free")
ggsave("Figures/rsf-coefficients-1.pdf", width = 8, height = 4)

# only road distance
all_rsf_results %>% 
    dplyr::filter(Predictor == "Road Distance") %>% 
    ggplot(aes(x = Predictor, y = Coefficient, col = `Hunting Mode`, shape = Harvest, group = `Hunting Mode`)) + 
    geom_hline(aes(yintercept = 0), linetype="dashed", size = 0.5, color = "darkgrey") +
    geom_errorbar(aes(ymin = LCI, ymax = UCI), width=0, position = position_dodge(width = 1), alpha=.8) +
    geom_point(position = position_dodge(width = 1), size = 3, stroke = 1, alpha = .75) +
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
    ylim(-4, 0.5) +
    scale_shape_manual(values=c(1, 19))+
    scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3"),
                       guide = guide_legend(reverse = TRUE)) +
    facet_grid(Predictor~., scales = "free") 
ggsave("Figures/rsf-coefficients-2.pdf", width = 8.15, height = 1.3)


  # Facet wrap version ------------------------------------------------------

# Create function to move legend into facet
shift_legend2 <- function(p) {
    # ...
    # to grob
    gp <- ggplotGrob(p)
    facet.panels <- grep("^panel", gp[["layout"]][["name"]])
    empty.facet.panels <- sapply(facet.panels, function(i) "zeroGrob" %in% class(gp[["grobs"]][[i]]))
    empty.facet.panels <- facet.panels[empty.facet.panels]
    
    # establish name of empty panels
    empty.facet.panels <- gp[["layout"]][empty.facet.panels, ]
    names <- empty.facet.panels$name
    # example of names:
    #[1] "panel-3-2" "panel-3-3"
    
    # now we just need a simple call to reposition the legend
    reposition_legend(p, 'center', panel=names)
}

plot <- all_rsf_results %>% 
    ggplot(aes(x = Predictor, y = Coefficient, col = `Hunting Mode`, shape = Harvest, group = Hunting_Mode)) + 
    geom_hline(aes(yintercept = 0), linetype="dashed", size = 0.5, color = "darkgrey") +
    geom_errorbar(aes(ymin = LCI, ymax = UCI), width=0, position = position_dodge(width = 1), alpha=.4) +
    geom_point(position = position_dodge(width = 1), size = 3) +
    theme(axis.title.y = element_blank(),
          legend.title = element_blank(),
          legend.key = element_rect(fill = alpha("white", 0.0)),
          strip.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          strip.text = element_text(size=rel(1)),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          panel.border = element_rect(color = "black", fill = NA, size = .5),
          legend.text=element_text(size=rel(1)),
          legend.position = c(1, 0),
          legend.justification = c(1, 0)
    ) +
    coord_flip() + # switch x and y coordinates
    ylab("Coefficient") +
    scale_shape_manual(values=c(1, 19))+
    scale_color_manual(values = c("#52573f", "#6281a7", "#8e8974"),
                       guide = guide_legend(reverse = TRUE)) +
    facet_wrap(~Predictor, scales = "free")
shift_legend2(plot)    
