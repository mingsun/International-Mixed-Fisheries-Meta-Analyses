

# load the plot packages
library(ggplot2)
theme_set(theme_bw())
library(sf)

# load the country map packages
library(rnaturalearth)
library(rnaturalearthdata)

# extract the regional data
world <- ne_countries(scale = "medium", returnclass = "sf")
world_points<- st_centroid(world)


### ----------------------------------------------------------- ###
### -------------------------- type --------------------------- ###
### ----------------------------------------------------------- ###

type <- read.csv("Data/for plot/ArcGis csv/1. fishery type.csv")

type.map <- ggplot(data = world) +
  geom_sf(color = "white", fill = "grey85") +
  coord_sf(ylim = c(-75, 80)) +
  geom_point(data = type[c(-7),], aes(x = x, y = y, shape = type, color = type), size = 4) +
  scale_color_manual(values = c("springgreen4", "darkorange", "darkorchid4", "steelblue2", "brown3")) +
  scale_shape_manual(values = c(16, 15, 17, 19, 18)) +
  theme(panel.grid.major = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(), 
        axis.title = element_blank(), panel.border = element_blank(),
        legend.position = "none")

png("plot/raw figures/mapping/type.map.png",  width = 8, height = 8 *7/15, units = 'in', res = 800)
# png("plot/type.map.png",  width = 8, height = 8 *7/15, units = 'in', res = 800)
print(type.map)
dev.off()



### ----------------------------------------------------------- ###
### -------------------------- size --------------------------- ###
### ----------------------------------------------------------- ###

size <- read.csv("Data/for plot/ArcGis csv/2. fishery size.csv")
size$size <- ((size$total.catch)/10000)^2

size.map <- ggplot(data = world) +
  geom_sf(color = "white", fill = "grey85") +
  coord_sf(ylim = c(-75, 80)) +
  geom_point(data = size[c(-7),], aes(x = x, y = y, size = size), color = "dodgerblue4") +
  scale_size(range = c(1, 15)) +
  theme(panel.grid.major = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(), 
        axis.title = element_blank(), panel.border = element_blank(),
        legend.position = "none")

png("plot/raw figures/mapping/size.map.png",  width = 8, height = 8 *7/15, units = 'in', res = 800)
# png("plot/size.map.png",  width = 8, height = 8 *7/15, units = 'in', res = 800)
print(size.map)
dev.off()

# a violin plot
size$group <- "a"

size.violin <- ggplot(size[c(-7),]) +
  geom_violin(aes(x = group, y = total.catch), fill = "dodgerblue4", alpha = 0.6, color = "dodgerblue4") +
  ylim(c(90000, 2000000)) +
  ylab("catch[mt]") +
  theme_classic() +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), 
        axis.title.x = element_blank(), axis.line.x = element_blank() )

png("plot/raw figures/mapping/size.violin.png",  width = 2, height = 3.6, units = 'in', res = 800)
# png("plot/size.violin.png",  width = 2, height = 3.6, units = 'in', res = 800)
print(size.violin)
dev.off()

### ----------------------------------------------------------- ###
### -------------------------- gears --------------------------- ###
### ----------------------------------------------------------- ###

gear.map <- ggplot(data = world) +
  geom_sf(color = "white", fill = "grey85") +
  coord_sf(ylim = c(-75, 80)) +
  geom_point(data = size[c(-7),], aes(x = x, y = y), color = "dodgerblue4", size = 1) +
  scale_size(range = c(1, 15)) +
  theme(panel.grid.major = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(), 
        axis.title = element_blank(), panel.border = element_blank(),
        legend.position = "none")

png("plot/raw figures/mapping/gear.map.png",  width = 8, height = 8 *7/15, units = 'in', res = 800)
# png("plot/gear.map.png",  width = 8, height = 8 *7/15, units = 'in', res = 800)
print(gear.map)
dev.off()


# a frequency plot
gear.df <- data.frame(gear = c("T", "H", "G", "R", "S", "M"),
                      fre  = c(17, 15, 11, 9, 8, 8))

gear.df$gear  <- factor(gear.df$gear , levels = rev(c("T", "H", "G", "R", "S", "M")))

gear.fre <- ggplot(gear.df) +
  geom_bar(aes(x = gear, y = fre), stat = "identity", color = "white", fill = "dodgerblue4", alpha = 0.2, width = 0.8) +
  ylab("Frequency") +
  coord_flip(ylim = c(18, 0)) +
  theme_classic() +
  theme(panel.background = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title.y = element_blank())

png("plot/raw figures/mapping/gear.fre.png",  width = 1.3, height = 4, units = 'in', res = 800)
# png("plot/gear.fre.png",  width = 1.3, height = 4, units = 'in', res = 800)
print(gear.fre)
dev.off()



### ----------------------------------------------------------- ###
### ----------------------- status B -------------------------- ###
### ----------------------------------------------------------- ###
library(reshape2)

status.b <- read.csv("Data/for plot/ArcGis csv/5. stock status B.csv")[1:24,]
status.b <- melt(status.b, id.vars = c("code", "region", "case", "abb.", "x", "y"))

status.b.plot <- ggplot(data = status.b) +
  geom_bar(aes(x = "", y = value, fill = variable), color = "white", stat = "identity") +
  coord_polar("y", start = 0) +
  facet_wrap(.~case) +
  theme_bw()
  coord_sf(ylim = c(-75, 80)) +
  geom_point(data = size, aes(x = x, y = y, size = size), color = "dodgerblue4") +
  scale_size(range = c(1, 15)) +
  theme(panel.grid.major = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(), 
        axis.title = element_blank(), panel.border = element_blank(),
        legend.position = "none")

png("plot/raw figures/mapping/status.b.pie.png",  width = 8, height = 8 *7/15, units = 'in', res = 800)
print(status.b.plot)
dev.off()
