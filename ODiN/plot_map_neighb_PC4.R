library("dplyr")
library("sf")
library("rgdal")
library('ggplot2')
library("this.path")
library('ggmap')
library(readr)
setwd(this.path::this.dir())

################################################################################
# Focus datasets on DHZW and save them

# load DHZW area PC4
DHZW_PC4_codes <- read.csv("~/DHZW_synthetic-population/ODiN/data/DHZW_PC4_codes.csv", sep = ";" ,header=F)$V1

setwd('~/DHZW_synthetic-population/ODiN/data/map')
shp_nl_PC4 <- st_read('CBS-PC4-2019-v2')
shp_nl_PC4 <- shp_nl_PC4 %>%
  select(PC4, geometry)
shp_DHZW_PC4 = shp_nl_PC4[shp_nl_PC4$PC4 %in% DHZW_PC4_codes,]

# Save DHZW PC4
st_write(shp_DHZW_PC4, 'DHZW_PC4_shapefiles', driver = "ESRI Shapefile")

# load DHZW area neighrbouhoods
DHZW_neighborhood_codes <- read.csv("~/DHZW_synthetic-population/ODiN/data/DHZW_neighbourhoods_codes.csv", sep = ";" ,header=F)$V1

shp_nl_neighbs <- st_read('WijkBuurtkaart_2019_v3')
shp_nl_neighbs <- shp_nl_neighbs %>%
  select(BU_CODE, geometry)
shp_DHZW_neighbs = shp_nl_neighbs[shp_nl_neighbs$BU_CODE %in% DHZW_neighborhood_codes,]

# Save DHZW neighrbouhoods
st_write(shp_DHZW_neighbs, 'DHZW_neighbs_shapefiles', driver = "ESRI Shapefile")

################################################################################
# Plot PC4 and neighbourhood areas together to see the overlap
setwd('~/DHZW_synthetic-population/ODiN/data/map')
shp_DHZW_neighbs <- st_read('DHZW_neighbs_shapefiles')
shp_DHZW_PC4 <- st_read('DHZW_PC4_shapefiles')

plot(shp_DHZW_PC4)

colors <- c("Neighbourhood" = "blue", "PC4" = "red")
ggplot()+
  geom_sf(data = shp_DHZW_neighbs,
          aes(color = 'Neighbourhood'),
          inherit.aes = FALSE,
          fill=NA)+
  geom_sf(data = shp_DHZW_PC4,
          aes(color = 'PC4'),
          inherit.aes = FALSE,
          fill=NA)+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))+
  scale_color_manual(values = colors)+
  labs(color = "Type of area")


################################################################################
# With map
google_key <- read_file("data/google_key.txt")
register_google(key = google_key, write = TRUE)

map <- get_map(c(4.23, 52.02, 4.32, 52.07), source = "google", zoom=14)


# Transform nc to EPSG 3857 (Pseudo-Mercator, what Google uses)
PC4_3857 <- st_transform(shp_DHZW_PC4, 3857)
neighbs_3857 <- st_transform(shp_DHZW_neighbs, 3857)


colors <- c("Neighbourhood" = "blue", "PC4" = "red")
ggmap(map) +
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(
    data = PC4_3857,
    aes(color = 'PC4', fill = PC4),
    inherit.aes = FALSE,
    alpha = 0.7
  ) +
  geom_sf(
    data = neighbs_3857,
    aes(color = 'Neighbourhood'),
    inherit.aes = FALSE,
    fill = NA
  ) +
  scale_color_manual(values = colors) +
  labs(color = "Type of area") +
  scale_fill_distiller(palette = "OrRd",
                       direction = 1,
                       na.value = "grey50")