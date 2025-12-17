library(ggplot2)
library(plyr)

keywords <- read.csv("Data/for plot/R csv/9. WOS research progress.csv")

### ------------------------------------------------ ###
### -------------------- heat map ------------------ ###
### ------------------------------------------------ ###

keywords.hm <- keywords[,c(1,3,7)]

keywords.hm <- ddply(keywords.hm, .(code, case, topic), summarize, freq = length(topic))

# add empty entry to code 18 and 19 for plotting purpose
keywords.hm$topic[which(keywords.hm$code %in% c(17, 18))] <- "modeling"
keywords.hm$freq[which(keywords.hm$code %in% c(17, 18))] <- 0

empty.df.1 <- read.csv("Data/for plot/ArcGis csv/1. fishery type.csv")[,c(1,3)]
empty.df.2 <- expand.grid(topic = unique(keywords.hm$topic), code = 1:24, freq.0 = 0)[,c(2,1,3)]
empty.df <- merge(empty.df.1, empty.df.2)

keywords.hm <- merge(x = empty.df, y = keywords.hm,  all = TRUE)
keywords.hm$freq.0[!is.na(keywords.hm$freq)] <- keywords.hm$freq[!is.na(keywords.hm$freq)]
keywords.hm <- keywords.hm[,c(1:4)]; colnames(keywords.hm)[4] <- "freq"

# adjust the order of variables

keywords.hm$case <- factor(keywords.hm$case, levels = rev(c(empty.df.1$case))) # cases as code based on regions
keywords.hm$topic <- factor(keywords.hm$topic, levels = sort(as.character(unique(keywords.hm$topic)))) # topics following alphabetic

# plot

heatmap <- ggplot(keywords.hm[-c(73:84),]) +
  geom_tile(aes(x = topic, y = case, fill = freq), color = "white") +
  scale_fill_gradient2(low = "white", midpoint = 8.3, mid = "firebrick", high = "black") +
  theme_grey(base_size = 12) +
  labs(x = "", y = "") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0), position = "right") +
  theme(legend.position = "none",
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = 320, hjust = -0))
  # + coord_equal()

png("plot/raw figures/topic heatmap/topic heatmap.png", width = 10, height = 8, units = 'in', res = 800)
print(heatmap)# Make plot
dev.off()

### ------------------------------------------------ ###
### --------------------- barplot ------------------ ###
### ------------------------------------------------ ###

# keywords frequency by fishery
fishery <- ddply(keywords.hm, .(code, case), summarize, freq = sum(freq))
write.csv(fishery, "Data/for plot/R csv/keywords by fishery.csv")

fishery.barplot <- ggplot(fishery[-7,]) +
  geom_bar(aes(x = case, y = freq), stat = "identity", fill = "grey", width = 0.95)+
  scale_x_discrete(limits = rev(c(empty.df.1$case[-7]))) +
  scale_y_continuous(breaks = seq(0, 100, by = 20)) +
  labs(x = "", y = "") +
  theme_classic()  +
  theme(panel.background = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank()) +
  coord_flip(ylim = c(100,0))
 
png("plot/raw figures/topic heatmap/fishery barplot.png", width = 3, height = 6, units = 'in', res = 800)
print(fishery.barplot)# Make plot
dev.off()


# keywords frequency by topic
topic <- ddply(keywords.hm[-c(73:84),], .(topic), summarize, freq = sum(freq))

topic.barplot <- ggplot(topic) +
  geom_bar(aes(x = topic, y = freq), stat = "identity", fill = "grey", width = 0.95) +
  # scale_x_discrete(limits = rev(c(empty.df.1$case))) +
  scale_y_continuous(breaks = seq(0, 75, by = 25)) +
  labs(x = "", y = "") +
  theme_classic()  +
  theme(panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank())

png("plot/raw figures/topic heatmap/topic barplot.png", width = 6, height = 2, units = 'in', res = 800)
print(topic.barplot)# Make plot
dev.off()
