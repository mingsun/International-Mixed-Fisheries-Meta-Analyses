library(plyr)

# load the species composition file
comp <- read.csv(file = "Data/csv/catch composition.csv")
# comp <- read.csv(file = "Data/csv/catch composition.csv")
comp.df <- comp[,c(1:4,8)] 

comp.df <- subset(comp.df, !is.na(catch) & code != 7)

### ---------------------------------------------------- ### 
### --------------- Calculate Diversity ---------------- ###
### ---------------------------------------------------- ###

# Margalef index
# D = (S-1)/ln(W), where S is the number of species, W is the total weight

comp.df.D <- ddply(comp.df, .(code, region, case), summarize,
                 D = (length(species)-1)/log(sum(catch)))

# Shannon-Wiener index
# H = -sum(Pi * ln(Pi)), where Pi is the weight proportion of ith species to total weight

comp.df.H <- ddply(comp.df, .(code, region, case), mutate,
                   Pi = catch/sum(catch))

comp.df.H <- ddply(comp.df.H, .(code, region, case), summarize,
                   H = -sum(Pi*log(Pi)))

# Pielou index
# J = H/ln(S), where H is the SW index and S is the number of species

comp.df.J <- ddply(comp.df, .(code, region, case), summarize,
                   S = length(species))

comp.df.J <- merge(comp.df.H, comp.df.J)
comp.df.J$J <- comp.df.J$H/log(comp.df.J$S)

# combine
div.df <- merge(comp.df.D, comp.df.J) 
remove(comp.df.D, comp.df.H, comp.df.J, comp.df)

div.df <- div.df[order(div.df$code),]

write.csv(div.df, "Data/for plot/ArcGis csv/3. diversity index.csv")

### ---------------------------------------------------- ### 
### --------------- connect with latitude -------------- ###
### ---------------------------------------------------- ###

# coord <- read.csv("Data/csv/case coordinates.csv")
# 
# full.df <- merge(div.df, coord[,c(-5,-6,-10)])
# write.csv(full.df, "Data/for plot/ArcGis csv/3. diversity index.csv")

library(ggplot2)

ggplot(full.df) +
  geom_segment(aes(x = D, xend = D, y = abs(lat.y1), yend = abs(lat.y2)))

ggplot(full.df) +
  geom_segment(aes(x = H, xend = H, y = abs(lat.y1), yend = abs(lat.y2)))

ggplot(full.df) +
  geom_segment(aes(x = J, xend = J, y = abs(lat.y1), yend = abs(lat.y2)))


### ---------------------------------------------------- ### 
### --------------- violin plot -------------- ###
### ---------------------------------------------------- ###

div.df <- read.csv("Data/for plot/ArcGis csv/3. diversity index.csv")
div.df$group <- "div"

# Margalef index D

D.violin <- ggplot(div.df) +
  geom_violin(aes(y = group, x = D), fill = "grey", alpha = 0.6, color = "grey") +
  geom_vline(xintercept = mean(div.df$D), linetype = 2, color = "black") +
  geom_point(aes(y = group, x = median(D)), size = 2) +
  xlim(c(0, 3)) +
  theme_classic() +
  theme(axis.title.y = element_blank(), axis.line.y = element_blank(), 
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title.x= element_blank())

png("plot/raw figures/diversity/D.violin.png",  width = 3, height = 1, units = 'in', res = 800)
print(D.violin)
dev.off()

# Shannon-Wiener index H 

H.violin <- ggplot(div.df) +
  geom_violin(aes(y = group, x = H), fill = "grey", alpha = 0.6, color = "grey") +
  geom_vline(xintercept = mean(div.df$H), linetype = 2, color = "black") +
  geom_point(aes(y = group, x = median(H)), size = 2) +
  xlim(c(0, 2.5)) +
  theme_classic() +
  theme(axis.title.y = element_blank(), axis.line.y = element_blank(), 
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title.x= element_blank())

png("plot/raw figures/diversity/H.violin.png",  width = 3, height = 1, units = 'in', res = 800)
print(H.violin)
dev.off()


# Pielou index J

J.violin <- ggplot(div.df) +
  geom_violin(aes(y = group, x = J), fill = "grey", alpha = 0.6, color = "grey") +
  geom_vline(xintercept = mean(div.df$J), linetype = 2, color = "black") +
  geom_point(aes(y = group, x = median(J)), size = 2) +
  xlim(c(0, 1)) +
  theme_classic() +
  theme(axis.title.y = element_blank(), axis.line.y = element_blank(), 
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        axis.title.x= element_blank())

png("plot/raw figures/diversity/J.violin.png",  width = 3, height = 1, units = 'in', res = 800)
print(J.violin)
dev.off()
