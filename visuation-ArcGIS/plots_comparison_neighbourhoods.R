library(readr)
library(dplyr)
library(sf)
library(ggmap)
library(egg)
library(shadowtext)
library(ggpubr)
library("this.path")
setwd(this.path::this.dir())
source('../config/config.R')

################################################################################
# Load datasets to plot

setwd('../output/ArcGIS')
df_marginal <- st_read('Neighb_marginals_ArcGIS')
df_synthetic <- st_read('Neighb_summary_ArcGIS')

# Transform dataset to EPSG 3857 (Pseudo-Mercator, what Google uses)
df_marginal <- st_transform(df_marginal, 3857)
df_synthetic <- st_transform(df_synthetic, 3857)

centroids_marginal <- df_marginal %>%
  st_centroid() %>%
  st_geometry()
centroids_marginal <-
  as.data.frame(st_coordinates(centroids_marginal))
colnames(centroids_marginal) <- c('centroid_X', 'centroid_Y')
df_marginal <- cbind(df_marginal, centroids_marginal)

centroids_synthetic <- df_synthetic %>%
  st_centroid() %>%
  st_geometry()
centroids_synthetic <-
  as.data.frame(st_coordinates(centroids_synthetic))
colnames(centroids_synthetic) <- c('centroid_X', 'centroid_Y')
df_synthetic <- cbind(df_synthetic, centroids_synthetic)

################################################################################
# Retrieve Google Maps background map

setwd(this.path::this.dir())
google_key <- read_file("../keys/google_key.txt")
register_google(key = google_key, write = TRUE)
map <-
  get_map(c(4.23, 52.02, 4.32, 52.07), source = "google", zoom = 14)
ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap"))
    stop("map must be a ggmap object")
  
  map_bbox <- setNames(unlist(attr(map, "bb")),
                       c("ymin", "xmin", "ymax", "xmax"))
  bbox_3857 <-
    st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
  
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}
map <- ggmap_bbox(map)

################################################################################
# Plots
setwd(this.path::this.dir())
setwd('../output/plots/neighbourhoods_comparison')

# ------------------------------------------------------------------------------
# Gender: male

plot_real <- ggmap(map) +
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = df_marginal,
          aes(fill = ((gndr_ml / (gndr_ml + gndr_fm)) * 100)),
          inherit.aes = FALSE) +
  scale_fill_distiller(
    palette = "PuBu",
    direction = 1,
    na.value = "grey50"
  ) +
  geom_shadowtext(data = df_marginal,
                  aes(
                    x = centroid_X,
                    y = centroid_Y,
                    label = paste0(round((gndr_ml / (gndr_ml + gndr_fm)) * 100, 1), '%')
                  ),
                  size = 3)+
  xlab('Longitude')+
  ylab('Latitude')+
  labs(fill = "% of male individuals \n per neighbourhood")+
  ggtitle('Real marginal distribution CBS 2019')
plot_real

df_marginal$diff = abs(round(((df_marginal$gndr_ml/(df_marginal$gndr_ml + df_marginal$gndr_fm)) - (df_synthetic$male/df_synthetic$n_ndvdl))*100, 2))

plot_diff <- ggmap(map) +
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = df_marginal,
          aes(fill = diff),
          inherit.aes = FALSE) +
  scale_fill_distiller(
    palette = "OrRd",
    direction = 1,
    na.value = "grey50",
    limits=c(0,5)
  ) +
  geom_shadowtext(data = df_marginal,
                  aes(
                    x = centroid_X,
                    y = centroid_Y,
                    label = paste0(diff, '%')
                  ),
                  size = 3)+
  xlab('Longitude')+
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())+
  labs(fill = "% difference between \n synthetic population \n and real data")+
  ggtitle('Comparison of synthetic population with real data')
plot_diff

# Put the two plots side-by-side
merged_plot <- egg::ggarrange(plot_real, plot_diff, nrow = 1)
#merged_plot <- annotate_figure(merged_plot, top = text_grob("Gender male", face = "bold", size = 14))

png("gender_male.png", width = 2560, height = 1000, units='px', res = 250)
merged_plot
dev.off()


# ------------------------------------------------------------------------------
# Gender: female

plot_real <- ggmap(map) +
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = df_marginal,
          aes(fill = ((gndr_fm / (gndr_ml + gndr_fm)) * 100)),
          inherit.aes = FALSE) +
  scale_fill_distiller(
    palette = "PuBu",
    direction = 1,
    na.value = "grey50"
  ) +
  geom_shadowtext(data = df_marginal,
                  aes(
                    x = centroid_X,
                    y = centroid_Y,
                    label = paste0(round((gndr_fm / (gndr_ml + gndr_fm)) * 100, 1), '%')
                  ),
                  size = 3)+
  xlab('Longitude')+
  ylab('Latitude')+
  labs(fill = "% of female individuals \n per neighbourhood")+
  ggtitle('Real marginal distribution CBS 2019')
plot_real

df_marginal$diff = abs(round(((df_marginal$gndr_fm/(df_marginal$gndr_ml + df_marginal$gndr_fm)) - (df_synthetic$female/df_synthetic$n_ndvdl))*100, 2))

plot_diff <- ggmap(map) +
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = df_marginal,
          aes(fill = diff),
          inherit.aes = FALSE) +
  scale_fill_distiller(
    palette = "OrRd",
    direction = 1,
    na.value = "grey50",
    limits=c(0,5)
  ) +
  geom_shadowtext(data = df_marginal,
                  aes(
                    x = centroid_X,
                    y = centroid_Y,
                    label = paste0(diff, '%')
                  ),
                  size = 3)+
  xlab('Longitude')+
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())+
  labs(fill = "% difference between \n synthetic population \n and real data")+
  ggtitle('Comparison of synthetic population with real data')
plot_diff

# Put the two plots side-by-side
merged_plot <- egg::ggarrange(plot_real, plot_diff, nrow = 1)
#merged_plot <- annotate_figure(merged_plot, top = text_grob("Gender male", face = "bold", size = 14))

png("gender_female.png", width = 2560, height = 1000, units='px', res = 250)
merged_plot
dev.off()

# ------------------------------------------------------------------------------
# Migration: Dutch

plot_real <- ggmap(map) +
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = df_marginal,
          aes(fill = ((mgrtn_D / (mgrtn_D +mgrtn_w + mgrtn__)) * 100)),
          inherit.aes = FALSE) +
  scale_fill_distiller(
    palette = "PuBu",
    direction = 1,
    na.value = "grey50"
  ) +
  geom_shadowtext(data = df_marginal,
                  aes(
                    x = centroid_X,
                    y = centroid_Y,
                    label = paste0(round((mgrtn_D / (mgrtn_D +mgrtn_w + mgrtn__)) * 100, 1), '%')
                  ),
                  size = 3)+
  xlab('Longitude')+
  ylab('Latitude')+
  labs(fill = "% of individuals with \n Dutch migration background \n per neighbourhood")+
  ggtitle('Real marginal distribution CBS 2019')
plot_real

df_marginal$diff = abs(round(((df_marginal$mgrtn_D/(df_marginal$mgrtn_D + df_marginal$mgrtn_w + df_marginal$mgrtn__)) - (df_synthetic$Dutch/df_synthetic$n_ndvdl))*100, 2))

plot_diff <- ggmap(map) +
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = df_marginal,
          aes(fill = diff),
          inherit.aes = FALSE) +
  scale_fill_distiller(
    palette = "OrRd",
    direction = 1,
    na.value = "grey50",
    limits=c(0,5)
  ) +
  geom_shadowtext(data = df_marginal,
                  aes(
                    x = centroid_X,
                    y = centroid_Y,
                    label = paste0(diff, '%')
                  ),
                  size = 3)+
  xlab('Longitude')+
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())+
  labs(fill = "% difference between \n synthetic population \n and real data")+
  ggtitle('Comparison of synthetic population with real data')
plot_diff

# Put the two plots side-by-side
merged_plot <- egg::ggarrange(plot_real, plot_diff, nrow = 1)
#merged_plot <- annotate_figure(merged_plot, top = text_grob("Gender male", face = "bold", size = 14))

png("migration_Dutch.png", width = 2560, height = 1000, units='px', res = 250)
merged_plot
dev.off()

# ------------------------------------------------------------------------------
# Migration: Western

plot_real <- ggmap(map) +
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = df_marginal,
          aes(fill = ((mgrtn_w / (mgrtn_D + mgrtn_w + mgrtn__)) * 100)),
          inherit.aes = FALSE) +
  scale_fill_distiller(
    palette = "PuBu",
    direction = 1,
    na.value = "grey50"
  ) +
  geom_shadowtext(data = df_marginal,
                  aes(
                    x = centroid_X,
                    y = centroid_Y,
                    label = paste0(round((mgrtn_w / (mgrtn_D + mgrtn_w + mgrtn__)) * 100, 1), '%')
                  ),
                  size = 3)+
  xlab('Longitude')+
  ylab('Latitude')+
  labs(fill = "% of individuals with \n Western migration background \n per neighbourhood")+
  ggtitle('Real marginal distribution CBS 2019')
plot_real

df_marginal$diff = abs(round(((df_marginal$mgrtn_w/(df_marginal$mgrtn_D + df_marginal$mgrtn_w + df_marginal$mgrtn__)) - (df_synthetic$Western/df_synthetic$n_ndvdl))*100, 2))

plot_diff <- ggmap(map) +
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = df_marginal,
          aes(fill = diff),
          inherit.aes = FALSE) +
  scale_fill_distiller(
    palette = "OrRd",
    direction = 1,
    na.value = "grey50",
    limits=c(0,5)
  ) +
  geom_shadowtext(data = df_marginal,
                  aes(
                    x = centroid_X,
                    y = centroid_Y,
                    label = paste0(diff, '%')
                  ),
                  size = 3)+
  xlab('Longitude')+
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())+
  labs(fill = "% difference between \n synthetic population \n and real data")+
  ggtitle('Comparison of synthetic population with real data')
plot_diff

# Put the two plots side-by-side
merged_plot <- egg::ggarrange(plot_real, plot_diff, nrow = 1)
#merged_plot <- annotate_figure(merged_plot, top = text_grob("Gender male", face = "bold", size = 14))

png("migration_Western.png", width = 2560, height = 1000, units='px', res = 250)
merged_plot
dev.off()

# ------------------------------------------------------------------------------
# Migration: Non-Western

plot_real <- ggmap(map) +
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = df_marginal,
          aes(fill = ((mgrtn__ / (mgrtn_D + mgrtn_w + mgrtn__)) * 100)),
          inherit.aes = FALSE) +
  scale_fill_distiller(
    palette = "PuBu",
    direction = 1,
    na.value = "grey50"
  ) +
  geom_shadowtext(data = df_marginal,
                  aes(
                    x = centroid_X,
                    y = centroid_Y,
                    label = paste0(round((mgrtn__ / (mgrtn_D + mgrtn_w + mgrtn__)) * 100, 1), '%')
                  ),
                  size = 3)+
  xlab('Longitude')+
  ylab('Latitude')+
  labs(fill = "% of individuals with \n Non-western migration background \n per neighbourhood")+
  ggtitle('Real marginal distribution CBS 2019')
plot_real

df_marginal$diff = abs(round(((df_marginal$mgrtn__/(df_marginal$mgrtn_D + df_marginal$mgrtn_w + df_marginal$mgrtn__)) - (df_synthetic$Nn_Wstr/df_synthetic$n_ndvdl))*100, 2))

plot_diff <- ggmap(map) +
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = df_marginal,
          aes(fill = diff),
          inherit.aes = FALSE) +
  scale_fill_distiller(
    palette = "OrRd",
    direction = 1,
    na.value = "grey50",
    limits=c(0,5)
  ) +
  geom_shadowtext(data = df_marginal,
                  aes(
                    x = centroid_X,
                    y = centroid_Y,
                    label = paste0(diff, '%')
                  ),
                  size = 3)+
  xlab('Longitude')+
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())+
  labs(fill = "% difference between \n synthetic population \n and real data")+
  ggtitle('Comparison of synthetic population with real data')
plot_diff

# Put the two plots side-by-side
merged_plot <- egg::ggarrange(plot_real, plot_diff, nrow = 1)
#merged_plot <- annotate_figure(merged_plot, top = text_grob("Gender male", face = "bold", size = 14))

png("migration_NonWestern.png", width = 2560, height = 1000, units='px', res = 250)
merged_plot
dev.off()

# ------------------------------------------------------------------------------
# Education attainment: low

plot_real <- ggmap(map) +
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = df_marginal,
          aes(fill = ((edctn_l / (edctn_l + edctn_m + edctn_h)) * 100)),
          inherit.aes = FALSE) +
  scale_fill_distiller(
    palette = "PuBu",
    direction = 1,
    na.value = "grey50"
  ) +
  geom_shadowtext(data = df_marginal,
                  aes(
                    x = centroid_X,
                    y = centroid_Y,
                    label = paste0(round((edctn_l / (edctn_l + edctn_m + edctn_h)) * 100, 1), '%')
                  ),
                  size = 3)+
  xlab('Longitude')+
  ylab('Latitude')+
  labs(fill = "% of Low-educated individuals \n per neighbourhood")+
  ggtitle('Real marginal distribution CBS 2019')
plot_real

df_marginal$diff = abs(round(((df_marginal$edctn_l/(df_marginal$edctn_l + df_marginal$edctn_m + df_marginal$edctn_h)) - (df_synthetic$edctn_l/df_synthetic$n_ndvdl))*100, 2))

plot_diff <- ggmap(map) +
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = df_marginal,
          aes(fill = diff),
          inherit.aes = FALSE) +
  scale_fill_distiller(
    palette = "OrRd",
    direction = 1,
    na.value = "grey50",
    limits=c(0,5)
  ) +
  geom_shadowtext(data = df_marginal,
                  aes(
                    x = centroid_X,
                    y = centroid_Y,
                    label = paste0(diff, '%')
                  ),
                  size = 3)+
  xlab('Longitude')+
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())+
  labs(fill = "% difference between \n synthetic population \n and real data")+
  ggtitle('Comparison of synthetic population with real data')
plot_diff

# Put the two plots side-by-side
merged_plot <- egg::ggarrange(plot_real, plot_diff, nrow = 1)
#merged_plot <- annotate_figure(merged_plot, top = text_grob("Gender male", face = "bold", size = 14))

png("education_attainment_low.png", width = 2560, height = 1000, units='px', res = 250)
merged_plot
dev.off()

# ------------------------------------------------------------------------------
# Education attainment: middle

plot_real <- ggmap(map) +
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = df_marginal,
          aes(fill = ((edctn_m / (edctn_l + edctn_m + edctn_h)) * 100)),
          inherit.aes = FALSE) +
  scale_fill_distiller(
    palette = "PuBu",
    direction = 1,
    na.value = "grey50"
  ) +
  geom_shadowtext(data = df_marginal,
                  aes(
                    x = centroid_X,
                    y = centroid_Y,
                    label = paste0(round((edctn_m / (edctn_l + edctn_m + edctn_h)) * 100, 1), '%')
                  ),
                  size = 3)+
  xlab('Longitude')+
  ylab('Latitude')+
  labs(fill = "% of Middle-educated individuals \n per neighbourhood")+
  ggtitle('Real marginal distribution CBS 2019')
plot_real

df_marginal$diff = abs(round(((df_marginal$edctn_m/(df_marginal$edctn_l + df_marginal$edctn_m + df_marginal$edctn_h)) - (df_synthetic$edctn_m/df_synthetic$n_ndvdl))*100, 2))

plot_diff <- ggmap(map) +
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = df_marginal,
          aes(fill = diff),
          inherit.aes = FALSE) +
  scale_fill_distiller(
    palette = "OrRd",
    direction = 1,
    na.value = "grey50",
    limits=c(0,5)
  ) +
  geom_shadowtext(data = df_marginal,
                  aes(
                    x = centroid_X,
                    y = centroid_Y,
                    label = paste0(diff, '%')
                  ),
                  size = 3)+
  xlab('Longitude')+
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())+
  labs(fill = "% difference between \n synthetic population \n and real data")+
  ggtitle('Comparison of synthetic population with real data')
plot_diff

# Put the two plots side-by-side
merged_plot <- egg::ggarrange(plot_real, plot_diff, nrow = 1)
#merged_plot <- annotate_figure(merged_plot, top = text_grob("Gender male", face = "bold", size = 14))

png("education_attainment_middle.png", width = 2560, height = 1000, units='px', res = 250)
merged_plot
dev.off()

# ------------------------------------------------------------------------------
# Education attainment: high

plot_real <- ggmap(map) +
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = df_marginal,
          aes(fill = ((edctn_h / (edctn_l + edctn_m + edctn_h)) * 100)),
          inherit.aes = FALSE) +
  scale_fill_distiller(
    palette = "PuBu",
    direction = 1,
    na.value = "grey50"
  ) +
  geom_shadowtext(data = df_marginal,
                  aes(
                    x = centroid_X,
                    y = centroid_Y,
                    label = paste0(round((edctn_h / (edctn_l + edctn_m + edctn_h)) * 100, 1), '%')
                  ),
                  size = 3)+
  xlab('Longitude')+
  ylab('Latitude')+
  labs(fill = "% of high-educated individuals \n per neighbourhood")+
  ggtitle('Real marginal distribution CBS 2019')
plot_real

df_marginal$diff = abs(round(((df_marginal$edctn_h/(df_marginal$edctn_l + df_marginal$edctn_m + df_marginal$edctn_h)) - (df_synthetic$edctn_h/df_synthetic$n_ndvdl))*100, 2))

plot_diff <- ggmap(map) +
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = df_marginal,
          aes(fill = diff),
          inherit.aes = FALSE) +
  scale_fill_distiller(
    palette = "OrRd",
    direction = 1,
    na.value = "grey50",
    limits=c(0,5)
  ) +
  geom_shadowtext(data = df_marginal,
                  aes(
                    x = centroid_X,
                    y = centroid_Y,
                    label = paste0(diff, '%')
                  ),
                  size = 3)+
  xlab('Longitude')+
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())+
  labs(fill = "% difference between \n synthetic population \n and real data")+
  ggtitle('Comparison of synthetic population with real data')
plot_diff

# Put the two plots side-by-side
merged_plot <- egg::ggarrange(plot_real, plot_diff, nrow = 1)
#merged_plot <- annotate_figure(merged_plot, top = text_grob("Gender male", face = "bold", size = 14))

png("education_attainment_high.png", width = 2560, height = 1000, units='px', res = 250)
merged_plot
dev.off()