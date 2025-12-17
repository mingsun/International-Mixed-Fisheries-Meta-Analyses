# load the plot packages
library(ggplot2)
theme_set(theme_bw())
library(sf)
library(scatterpie)

# load the country map packages
library(rnaturalearth)
library(rnaturalearthdata)

# extract the regional data
world <- ne_countries(scale = "medium", returnclass = "sf")
world_points<- st_centroid(world)


base <- ggplot(data = world) +
  geom_sf(color = "white", fill = "grey85") +
  coord_sf(ylim = c(-75, 80)) +
  # geom_point(data = type[-7,], aes(x = x, y = y), color = "dodgerblue4", size = 1) +
  scale_size(range = c(1, 15)) +
  theme(panel.grid.major = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(), 
        axis.title = element_blank(), panel.border = element_blank(),
        legend.position = "none")

### -------------------------- F status --------------------------- ###

F.status <- read.csv("Data/for plot/ArcGis csv/6. stock status F.csv")[c(1:6, 8:24),]

F.pie <- base +
  geom_scatterpie(aes(x = x, y = y),
                  data = F.status, cols = colnames(F.status)[7:9]) +
  scale_fill_manual(values = c("brown2", "green3", "grey80"))
# scale_fill_manual(values = c("brown2", "green3", "gold2"))

png("plot/raw figures/mapping/F.status.png",  width = 8, height = 8 *7/15, units = 'in', res = 800)
print(F.pie)
dev.off()


### -------------------------- B status --------------------------- ###

B.status <- read.csv("Data/for plot/ArcGis csv/5. stock status B.csv")[c(1:6, 8:24),]

B.pie <- base +
  geom_scatterpie(aes(x = x, y = y),
                  data = B.status, cols = colnames(B.status)[7:9]) +
  scale_fill_manual(values = c("brown2", "green3", "grey80"))

png("plot/raw figures/mapping/B.status.png",  width = 8, height = 8 *7/15, units = 'in', res = 800)
print(B.pie)
dev.off()


