library(ggplot2)
library(RColorBrewer)

div.df <- read.csv("Data/for plot/ArcGis csv/3. diversity index.csv")

div.df$case <- factor(div.df$case, levels = rev(c(div.df$case))) # cases as code based on regions
# div.df$col <- rep(c(1,2),12)

type.df <- read.csv("Data/for plot/ArcGis csv/1. fishery type.csv")
div.df <- merge(div.df, type.df)
div.df <- div.df[order(div.df$code),]

### ------------------------------------------------------ ###
### ------------------- plot separately ------------------ ###
### ------------------------------------------------------ ###

# number of species
s.plot <- ggplot(div.df) +
  geom_bar(aes(x = S, y = case, fill = type), stat = "identity", alpha = 0.8) +
  scale_x_continuous(breaks = seq(0, 30, by = 10)) +
  labs(x = "number of species", y = "") +
  # scale_fill_manual(values = cbp1) +
  scale_fill_brewer(palette = "Set1") +
  theme_classic()  +
  theme(panel.background = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank()) 

png("plot/diversity/number of species.png", width = 2.5, height = 6, units = 'in', res = 800)
print(s.plot)# Make plot
dev.off()

# Margalef index

D.plot <- ggplot(div.df) +
  geom_bar(aes(x = D, y = case, fill = type), stat = "identity", alpha = 0.8) +
  labs(x = "Margalef index", y = "") +
  scale_fill_brewer(palette = "Set1") +
  theme_classic()  +
  theme(panel.background = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank())

png("plot/raw figures/diversity/Margalef.png", width = 2.5, height = 6, units = 'in', res = 800)
print(D.plot)# Make plot
dev.off()

# Shannon-Wiener index

H.plot <- ggplot(div.df) +
  geom_bar(aes(x = H, y = case, fill = type), stat = "identity", alpha = 0.8) +
  labs(x = "Shannon-Wiener index", y = "") +
  scale_fill_brewer(palette = "Set1") +
  theme_classic()  +
  theme(panel.background = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank())

png("plot/raw figures/diversity/Shannon-Wiener.png", width = 2.5, height = 6, units = 'in', res = 800)
print(H.plot)# Make plot
dev.off()

# Pielou index

J.plot <- ggplot(div.df) +
  geom_bar(aes(x = J, y = case, fill = type), stat = "identity", alpha = 0.8) +
  labs(x = "Pielou index", y = "") +
  scale_fill_brewer(palette = "Set1") +
  theme_classic()  +
  theme(panel.background = element_blank(),
        legend.position = "none",
        axis.text.y = element_blank())

png("plot/raw figures/diversity/Pielou.png", width = 2.5, height = 6, units = 'in', res = 800)
print(J.plot)# Make plot
dev.off()

### ------------------------------------------------------ ###
### ------------------- diversity by category ------------------ ###
### ------------------------------------------------------ ###

library(plyr)
library(tidyr)

### ------------------------------------------------------ ###
#  by type

div.type.df <- ddply(div.df, .(type), summarize,
                     median.S = median(S),
                     median.D = median(D),
                     median.H = median(H),
                     median.J = median(J))

div.type.df <- gather(div.type.df[,c(1,3,4,5)], index, value,
                      median.D:median.J,factor_key=TRUE)

div.quantile.df <- gather(div.df[,c(2, 3, 4, 5, 7, 9:11)], index, value,
                          c(D, H, J),factor_key=TRUE)

# standardize to the maximal value
div.quantile.df <- ddply(div.quantile.df, .(index), mutate,
                         value.relative = value/max(value))

by.type.plot <- ggplot(div.quantile.df) +
  geom_boxplot(aes(x = type, y = value.relative, color = index, fill = index), 
               width = 0.4, position = position_dodge(0.7)) +
  # geom_point(data = div.type.df, aes(x = type, y = value, group = index), fill = "black",
  #            position=position_dodge(width=0.7)) +
  # geom_bar(aes(x = type, y = value, fill = index), stat = "identity", alpha = 0.8, position = "dodge", width = 0.7) +
  labs(x = "", y = "") +
  scale_fill_brewer(palette = "Set1") +
  scale_color_manual(values = c("grey35", "grey35", "grey35")) +
  theme_classic()  +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), limits = c(0, 1)) +
  theme(panel.background = element_blank(),
        legend.position = "none")

png("plot/raw figures/diversity/by.type.png", width = 6, height = 5, units = 'in', res = 800)
print(by.type.plot)# Make plot
dev.off()


### ------------------------------------------------------ ###
#  by region

div.region.df <- ddply(div.df, .(region), summarize,
                     median.S = median(S),
                     median.D = median(D),
                     median.H = median(H),
                     median.J = median(J))

div.region.df <- gather(div.region.df[,c(1,3,4,5)], index, value,
                      median.D:median.J,factor_key=TRUE)

# div.quantile.df <- gather(div.df[,c(1, 2, 6, 7, 9)], index, value,
#                           c(D, H, J),factor_key=TRUE)

by.region.plot <- ggplot(div.quantile.df) +
  geom_boxplot(aes(x = region, y = value.relative, color = index, fill = index), 
               width = 0.4, position = position_dodge(0.7)) +
  # geom_bar(aes(x = region, y = value, fill = index), stat = "identity", alpha = 0.8, position = "dodge", width = 0.7) +
  labs(x = "", y = "") +
  scale_fill_brewer(palette = "Set1") +
  scale_color_manual(values = c("grey35", "grey35", "grey35")) +
  theme_classic()  +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), limits = c(0,1)) +
  theme(panel.background = element_blank(),
        legend.position = "none")

png("plot/raw figures/diversity/by.region.png", width = 6, height = 5, units = 'in', res = 800)
print(by.region.plot)# Make plot
dev.off()

### ------------------------------------------------------ ###
#  by size

size.df <- read.csv("Data/for plot/ArcGis csv/2. fishery size.csv")
div.size.df <- merge(div.df, size.df)

lm.D <- lm(div.size.df$D ~ log(div.size.df$total.catch))
summary(lm.D) # 0.666

lm.H <- lm(div.size.df$H ~ log(div.size.df$total.catch))
summary(lm.H) # 0.995

lm.J <- lm(div.size.df$J ~ log(div.size.df$total.catch))
summary(lm.J) # 0.0185


div.size.df <- gather(div.size.df[,c(-8)], index, value,
                        D:J,factor_key=TRUE)

by.size.plot <- ggplot(div.size.df) +
  geom_point(aes(x = log(total.catch), y = value, color = index), shape = 1, size = 2) +
  # geom_line(aes(x = log(total.catch), y = value, color = index)) +
  geom_smooth(aes(color = index, x = log(total.catch), y = value), method = 'lm', formula = y ~ x) +
  labs(x = "", y = "") +
  scale_color_brewer(palette = "Set1") +
  theme_classic()  +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), limits = c(0,3)) +
  theme(panel.background = element_blank(),
        legend.position = "none")

png("plot/raw figures/diversity/by.size.png", width = 6, height = 5, units = 'in', res = 800)
print(by.size.plot)# Make plot
dev.off()

### ------------------------------------------------------ ###
#  by latitude

div.lat.df <- merge(div.df, size.df)

lm.D <- lm(div.lat.df$D ~ abs(div.lat.df$y))
summary(lm.D) # 0.1571

lm.H <- lm(div.lat.df$H ~ abs(div.lat.df$y))
summary(lm.H) # 0.402

lm.J <- lm(div.lat.df$J ~ abs(div.lat.df$y))
summary(lm.J) # 0.2846


div.lat.df <- gather(div.lat.df[,c(-8)], index, value, D:J,factor_key=TRUE)

by.lat.plot <- ggplot(div.lat.df) +
  geom_point(aes(x = abs(y), y = value, color = index), shape = 1, size = 2) +
  # geom_line(aes(x = abs(y), y = value, color = index)) +
  geom_smooth(aes(color = index, x = abs(y), y = value), method = 'lm', formula = y ~ x) +
  labs(x = "", y = "") +
  scale_fill_brewer(palette = "Set1") +
  theme_classic()  +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), limits = c(0,3)) +
  theme(panel.background = element_blank(),
        legend.position = "none")

png("plot/raw figures/diversity/by.lat.png", width = 6, height = 5, units = 'in', res = 800)
print(by.lat.plot)# Make plot
dev.off()


### ------------------------------------------------------ ###
#  by latitude and fisheries type

div.lat.df <- merge(div.df, size.df)

# Margelef index

by.lat.by.type.plot <- ggplot(div.lat.df) +
  geom_point(aes(x = abs(y), y = D, color = type, shape = type), size = 4) +
  labs(x = "absolute latitude [degree]", y = "Margelef Index") +
  scale_fill_brewer(palette = "Set1") +
  theme_classic()  +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), limits = c(0,3.3)) +
  theme(panel.background = element_blank())

png("plot/raw figures/diversity/D.by.lat.by.type.png", width = 6, height = 5, units = 'in', res = 800)
print(by.lat.by.type.plot)# Make plot
dev.off()

# Shannon-Wiener index

by.lat.by.type.plot <- ggplot(div.lat.df) +
  geom_point(aes(x = abs(y), y = H, color = type, shape = type), size = 4) +
  labs(x = "absolute latitude [degree]", y = "Shannon-Wiener Index") +
  scale_fill_brewer(palette = "Set1") +
  theme_classic()  +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)), limits = c(0,3.3)) +
  theme(panel.background = element_blank())

png("plot/raw figures/diversity/H.by.lat.by.type.png", width = 6, height = 5, units = 'in', res = 800)
print(by.lat.by.type.plot)# Make plot
dev.off()


