library(ggplot2)

count.scale.df <- data.frame(set   = rep(c("a", "b", "c", "d", "e"), 2),
                             scale = rep(c("Single-species", "Multi-species", "Sector-specific", "Fishery-wide", "Ecosystem-wide"), 2),
                             class = rep(c("assessment", "management"), each = 5),
                             count = c(22, 1, 0, 4, 13, 
                                       16, 3, 8, 9, 4))

scale.count.plot <- ggplot(count.scale.df) +
  geom_bar(aes(x = set, y = count, fill = class), stat = "identity", position='dodge', width = 0.8, alpha = 0.9) +
  scale_fill_manual(values = c("tomato1", "steelblue2")) +
  labs(y = "", x = "") +
  theme_classic()  +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), limits = c(0,22)) +
  theme(panel.background = element_blank(),
        legend.position = "none")

png("plot/raw figures/management/management.scale.png", width = 7, height = 2, units = 'in', res = 800)
print(scale.count.plot)# Make plot
dev.off()