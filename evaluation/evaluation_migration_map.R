library(ggplot2)
library(dplyr)
library(tidyr)
library(ggmap)
library(readr)
library(sf)
library(shadowtext)

library("this.path")
setwd(this.path::this.dir())
source('../src/utils-evaluation-synthetic-population.R')

# Load datasets
setwd(this.path::this.dir())
setwd("../data/processed/individuals")
df_marginal_dist = read.csv("marginal_distributions_84583NED-formatted.csv", sep = ",")

# Load synthetic population
setwd(this.path::this.dir())
setwd(paste("../output/synthetic-population-households", sep = ""))
df_synth_pop = read.csv("synthetic_population_DHZW_2019.csv", sep = ",")

################################################################################
# With marginal distribution
df_gender <- get_proportions_over_marginal(df_marginal_dist = df_marginal_dist,
                                                       df_synth_pop = df_synth_pop,
                                                       aggregation_var = 'neighb_code',
                                                       cols_marginal = c('gender_male', 'gender_female'),
                                                       var_str = 'gender',
                                                       values = c('male', 'female')
)
df_gender <- df_gender %>%
  pivot_wider(names_from = 'dataset', values_from = 'proportion')

################################################################################
# # Retrieve Google Maps background map
# 
# setwd(this.path::this.dir())
# google_key <- read_file("../../keys/google_key.txt")
# register_google(key = google_key, write = TRUE)
# map <-
#   get_map(c(4.23, 52.02, 4.32, 52.07), source = "google", zoom = 14)
# ggmap_bbox <- function(map) {
#   if (!inherits(map, "ggmap"))
#     stop("map must be a ggmap object")
#   
#   map_bbox <- setNames(unlist(attr(map, "bb")),
#                        c("ymin", "xmin", "ymax", "xmax"))
#   bbox_3857 <-
#     st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
#   
#   attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
#   attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
#   attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
#   attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
#   map
# }
# map <- ggmap_bbox(map)

################################################################################
# add geometry

setwd(this.path::this.dir())
setwd('../../DHZW_shapefiles/data/processed/shapefiles/')
shp_DHZW_neighb <- st_read('DHZW_neighbs_shapefiles')
shp_DHZW_neighb <- shp_DHZW_neighb %>%
  rename(neighb_code = BU_CODE) %>%
  select(neighb_code, geometry)

# Merge geometry
df_gender <- merge(df_gender, shp_DHZW_neighb, by = 'neighb_code')

# Transform dataset to EPSG 3857 (Pseudo-Mercator, what Google uses)
df_gender$geometry <- st_transform(df_gender$geometry, 3857)

################################################################################
# centroids

df_gender$centroid <- st_centroid(df_gender$geometry)

centroids_marginal <-
  as.data.frame(st_coordinates(df_gender$centroid))
colnames(centroids_marginal) <- c('centroid_X', 'centroid_Y')
df_gender <- cbind(df_gender, centroids_marginal)

################################################################################

# Add the density of neighbourhoods
df_marginal_dist <- df_marginal_dist %>%
  select(neighb_code, tot_pop)
df_gender <- right_join(df_gender, df_marginal_dist, by='neighb_code')
#df_gender$tot_pop <- df_gender$tot_pop/max(df_gender$tot_pop)

df_gender$difference <- round((df_gender$`marginal distribution` - df_gender$`synthetic population`)*100, 1)
max_value <- max(df_gender$difference)

df_gender <- st_as_sf(df_gender)

df_gender_male <- df_gender[df_gender$gender == 'male',]
plot_male <- ggplot(data = df_gender_male) +
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(aes(fill = difference
              )) +
  scale_fill_distiller(
    palette = "PuBu",
    direction = 1,
    na.value = "grey50",
    limits=c(0, max_value)
  ) +
  scale_x_continuous(breaks = seq(4.24, 4.31, by = 0.02)) +
  geom_shadowtext(data = df_gender_male,
                  aes(
                    x = centroid_X,
                    y = centroid_Y,
                    label = paste0(difference, '%')
                  ),
                  size = 5)+
 # theme(legend.position = "none") +
  xlab('Longitude')+
  ylab('Latitude')+
  labs(fill = "% difference between \n synthetic population and \n marginal distribution per \n neighbourhood",
       alpha = "number of individuals")+
  ggtitle('Gender male') +
  theme(panel.background = element_blank(),
        text = element_text(size = 15))
plot_male

# df_gender_female <- df_gender[df_gender$gender == 'female',]
#
# plot_female <- ggplot(data = df_gender_female) +
#   coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
#   geom_sf(aes(fill = difference,
#               alpha = tot_pop)) +
#   scale_fill_distiller(
#     palette = "PuBu",
#     direction = 1,
#     na.value = "grey50",
#     limits=c(0, max_value)
#   ) +
#   scale_x_continuous(breaks = seq(4.24, 4.31, by = 0.02)) +
#   geom_shadowtext(data = df_gender_female,
#                   aes(
#                     x = centroid_X,
#                     y = centroid_Y,
#                     label = paste0(difference, '%')
#                   ),
#                   size = 4)+
#   theme(axis.text.y = element_blank(),
#         axis.title.y = element_blank())+
#   xlab('Longitude')+
#   ylab('Latitude')+
#   labs(fill = "% difference between \n synthetic population and \n marginal distribution per \n neighbourhood",
#        alpha = "number of individuals")+
#   ggtitle('Gender female') +
#   theme(panel.background = element_blank())
# 
# merged_plot <- egg::ggarrange(plot_male, plot_female, nrow = 1)

setwd(this.dir())
png("gender_male.png", width = 2560, height = 1500, units='px', res = 300)
plot_male
dev.off()