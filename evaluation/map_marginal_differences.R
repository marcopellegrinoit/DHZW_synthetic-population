library(readr)
library(dplyr)
library(this.path)
library(sf)
library(ggplot2)
library(shadowtext)

################################################################################
# Read neighbourhood differences

setwd(this.dir())
setwd('data_comparison')
df <- read.csv('marginal_differences.csv')

df <- df %>% 
  mutate_if(is.numeric, round, digits=2)

################################################################################
# Add the density of neighbourhoods

setwd(this.dir())
setwd("../data/processed/individuals")
df_marginal_dist = read.csv("marginal_distributions_84583NED-formatted.csv", sep = ",")

df <- right_join(df, df_marginal_dist, by='neighb_code')

################################################################################
# add geometries

setwd(this.path::this.dir())
setwd('../../DHZW_shapefiles/data/processed/shapefiles')
shp_DHZW_neighb <- st_read('DHZW_neighbs_shapefiles')
shp_DHZW_neighb <- shp_DHZW_neighb %>%
  rename(neighb_code = BU_CODE) %>%
  select(neighb_code, geometry)

# Merge geometry
df <- merge(df, shp_DHZW_neighb, by = 'neighb_code')

# Transform dataset to EPSG 3857 (Pseudo-Mercator, what Google uses)
df$geometry <- st_transform(df$geometry, 3857)

# centroids

df$centroid <- st_centroid(df$geometry)

centroids_marginal <-
  as.data.frame(st_coordinates(df$centroid))
colnames(centroids_marginal) <- c('centroid_X', 'centroid_Y')
df <- cbind(df, centroids_marginal)

df <- st_as_sf(df)

library(units)
units::set_units(df) <- 'km^2'
set_units(df, m^2)

df$area <- st_area(df$geometry)
df$area <- as.numeric(df$area / 1000000)

################################################################################
# plot

################################################################################
# Age groups

df_max <- as.data.frame(df)
df_max <- df_max %>%
  select(diff_age_0_15,
         diff_age_15_25,
         diff_age_25_45,
         diff_age_45_65,
         diff_age_over65)
max_diff <- max(df_max)

# 0-14
df$density <- df$age_0_15 / df$area
title_exp <-expression(atop("Age group 0-14 \n density in marginal data", paste("[(Age group 0-14)/"~km^2~"]")))
plot <- ggplot(data = df) +
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(aes(fill = density
  )) +
  scale_fill_distiller(
    palette = "PuBu",
    direction = 1,
    na.value = "grey50",
    limits=c(min(df$density), max(df$density))
  ) +
  geom_point(aes(
               x = centroid_X,
               y = centroid_Y,
               size = diff_age_0_15
             ),
             color = 'red') +
  scale_size(limits = c(0, max_diff)) +
  scale_x_continuous(breaks = seq(4.24, 4.31, by = 0.02)) +
  xlab('Longitude')+
  ylab('Latitude')+
  labs(fill = title_exp,
       size = "Percent point difference \n between synthetic population \n and marginal data")+
  ggtitle(bold('Age group:')~' between 0 and 14 years old') +
  theme(panel.background = element_blank(),
        text = element_text(size = 15))

setwd(this.dir())
setwd('plots/marginal/maps/age_group')
png(paste0("age_group_0_14.png"), width = 1500, height = 1000, units='px', res = 170)
plot
dev.off()

# 15-24
df$density <- df$age_15_25 / df$area
title_exp <-expression(atop("Age group 15-24 \n density in marginal data", paste("[(Age group 15-24)/"~km^2~"]")))
plot <- ggplot(data = df) +
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(aes(fill = density
  )) +
  scale_fill_distiller(
    palette = "PuBu",
    direction = 1,
    na.value = "grey50",
    limits=c(min(df$density), max(df$density))
  ) +
  geom_point(aes(
    x = centroid_X,
    y = centroid_Y,
    size = diff_age_15_25
  ),
  color = 'red') +
  scale_size(limits = c(0, max_diff)) +
  scale_x_continuous(breaks = seq(4.24, 4.31, by = 0.02)) +
  xlab('Longitude')+
  ylab('Latitude')+
  labs(fill = title_exp,
       size = "Percent point difference \n between synthetic population \n and marginal data")+
  ggtitle(bold('Age group:')~' between 15 and 24 years old') +
  theme(panel.background = element_blank(),
        text = element_text(size = 15))

setwd(this.dir())
setwd('plots/marginal/maps/age_group')
png(paste0("age_group_15_24.png"), width = 1500, height = 1000, units='px', res = 170)
plot
dev.off()

# 25-44
df$density <- df$age_25_45 / df$area
title_exp <-expression(atop("Age group 25-44 \n density in marginal data", paste("[(Age group 25-44)/"~km^2~"]")))
plot <- ggplot(data = df) +
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(aes(fill = density
  )) +
  scale_fill_distiller(
    palette = "PuBu",
    direction = 1,
    na.value = "grey50",
    limits=c(min(df$density), max(df$density))
  ) +
  geom_point(aes(
    x = centroid_X,
    y = centroid_Y,
    size = diff_age_25_45
  ),
  color = 'red') +
  scale_size(limits = c(0, max_diff)) +
  scale_x_continuous(breaks = seq(4.24, 4.31, by = 0.02)) +
  xlab('Longitude')+
  ylab('Latitude')+
  labs(fill = title_exp,
       size = "Percent point difference \n between synthetic population \n and marginal data")+
  ggtitle(bold('Age group:')~' between 25 and 44 years old') +
  theme(panel.background = element_blank(),
        text = element_text(size = 15))

setwd(this.dir())
setwd('plots/marginal/maps/age_group')
png(paste0("age_group_25_44.png"), width = 1500, height = 1000, units='px', res = 170)
plot
dev.off()

# 45-64
df$density <- df$age_45_65 / df$area
title_exp <-expression(atop("Age group 45-64 \n density in marginal data", paste("[(Age group 45-64)/"~km^2~"]")))
plot <- ggplot(data = df) +
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(aes(fill = density
  )) +
  scale_fill_distiller(
    palette = "PuBu",
    direction = 1,
    na.value = "grey50",
    limits=c(min(df$density), max(df$density))
  ) +
  geom_point(aes(
    x = centroid_X,
    y = centroid_Y,
    size = diff_age_45_65
  ),
  color = 'red') +
  scale_size(limits = c(0, max_diff)) +
  scale_x_continuous(breaks = seq(4.24, 4.31, by = 0.02)) +
  xlab('Longitude')+
  ylab('Latitude')+
  labs(fill = title_exp,
       size = "Percent point difference \n between synthetic population \n and marginal data")+
  ggtitle(bold('Age group:')~' between 45 and 64 years old') +
  theme(panel.background = element_blank(),
        text = element_text(size = 15))

setwd(this.dir())
setwd('plots/marginal/maps/age_group')
png(paste0("age_group_45_64.png"), width = 1500, height = 1000, units='px', res = 170)
plot
dev.off()

# over 65
df$density <- df$age_over65 / df$area
title_exp <-expression(atop("Age group over 65 \n density in marginal data", paste("[(Age group over 65)/"~km^2~"]")))
plot <- ggplot(data = df) +
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(aes(fill = density
  )) +
  scale_fill_distiller(
    palette = "PuBu",
    direction = 1,
    na.value = "grey50",
    limits=c(min(df$density), max(df$density))
  ) +
  geom_point(aes(
    x = centroid_X,
    y = centroid_Y,
    size = diff_age_over65
  ),
  color = 'red') +
  scale_size(limits = c(0, max_diff)) +
  scale_x_continuous(breaks = seq(4.24, 4.31, by = 0.02)) +
  xlab('Longitude')+
  ylab('Latitude')+
  labs(fill = title_exp,
       size = "Percent point difference \n between synthetic population \n and marginal data")+
  ggtitle(bold('Age group:')~' between over 65 years old') +
  theme(panel.background = element_blank(),
        text = element_text(size = 15))

setwd(this.dir())
setwd('plots/marginal/maps/age_group')
png(paste0("age_group_over_65.png"), width = 1500, height = 1000, units='px', res = 170)
plot
dev.off()

################################################################################
# Gender

df_max <- as.data.frame(df)
df_max <- df_max %>%
  select(diff_male,
         diff_female)
max_diff <- max(df_max)

# male
df$density <- df$gender_male / df$area
title_exp <-expression(atop("Male density in marginal data", paste("[Male/"~km^2~"]")))
plot <- ggplot(data = df) +
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(aes(fill = density
  )) +
  scale_fill_distiller(
    palette = "PuBu",
    direction = 1,
    na.value = "grey50",
    limits=c(min(df$density), max(df$density))
  ) +
  geom_point(aes(
    x = centroid_X,
    y = centroid_Y,
    size = diff_male
  ),
  color = 'red') +
  scale_size(limits = c(0, max_diff)) +
  scale_x_continuous(breaks = seq(4.24, 4.31, by = 0.02)) +
  xlab('Longitude')+
  ylab('Latitude')+
  labs(fill = title_exp,
       size = "Percent point difference \n between synthetic population \n and marginal data")+
  ggtitle(bold('Gender:')~' male') +
  theme(panel.background = element_blank(),
        text = element_text(size = 15))

setwd(this.dir())
setwd('plots/marginal/maps/gender')
png(paste0("gender_male.png"), width = 1500, height = 1000, units='px', res = 170)
plot
dev.off()

# female
df$density <- df$gender_female / df$area
title_exp <-expression(atop("Female density in marginal data", paste("[Female/"~km^2~"]")))
plot <- ggplot(data = df) +
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(aes(fill = density
  )) +
  scale_fill_distiller(
    palette = "PuBu",
    direction = 1,
    na.value = "grey50",
    limits=c(min(df$density), max(df$density))
  ) +
  geom_point(aes(
    x = centroid_X,
    y = centroid_Y,
    size = diff_female
  ),
  color = 'red') +
  scale_size(limits = c(0, max_diff)) +
  scale_x_continuous(breaks = seq(4.24, 4.31, by = 0.02)) +
  xlab('Longitude')+
  ylab('Latitude')+
  labs(fill = title_exp,
       size = "Percent point difference \n between synthetic population \n and marginal data")+
  ggtitle(bold('Gender:')~' female') +
  theme(panel.background = element_blank(),
        text = element_text(size = 15))

setwd(this.dir())
setwd('plots/marginal/maps/gender')
png(paste0("gender_female.png"), width = 1500, height = 1000, units='px', res = 170)
plot
dev.off()

################################################################################
# Migration background

# Dutch
df$density <- df$migration_Dutch / df$area
title_exp <-expression(atop("Dutch migration background \n density in marginal data", paste("[Dutch/"~km^2~"]")))
plot <- ggplot(data = df) +
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(aes(fill = density
  )) +
  scale_fill_distiller(
    palette = "PuBu",
    direction = 1,
    na.value = "grey50",
    limits=c(min(df$density), max(df$density))
  ) +
  geom_point(aes(
    x = centroid_X,
    y = centroid_Y,
    size = diff_Dutch
  ),
  color = 'red') +
  scale_size(limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(4.24, 4.31, by = 0.02)) +
  xlab('Longitude')+
  ylab('Latitude')+
  labs(fill = title_exp,
       size = "Percent point difference \n between synthetic population \n and marginal data")+
  ggtitle(bold('Migration background:')~' Dutch') +
  theme(panel.background = element_blank(),
        text = element_text(size = 15))

setwd(this.dir())
setwd('plots/marginal/maps/migration_background')
png(paste0("migration_Dutch.png"), width = 1500, height = 1000, units='px', res = 170)
plot
dev.off()

# Western
df$density <- df$migration_west / df$area
title_exp <-expression(atop("Western migration background \n density in marginal data", paste("[Western/"~km^2~"]")))
plot <- ggplot(data = df) +
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(aes(fill = density
  )) +
  scale_fill_distiller(
    palette = "PuBu",
    direction = 1,
    na.value = "grey50",
    limits=c(min(df$density), max(df$density))
  ) +
  geom_point(aes(
    x = centroid_X,
    y = centroid_Y,
    size = diff_Western
  ),
  color = 'red') +
  scale_size(limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(4.24, 4.31, by = 0.02)) +
  xlab('Longitude')+
  ylab('Latitude')+
  labs(fill = title_exp,
       size = "Percent point difference \n between synthetic population \n and marginal data")+
  ggtitle(bold('Migration background:')~' Western') +
  theme(panel.background = element_blank(),
        text = element_text(size = 15))

setwd(this.dir())
setwd('plots/marginal/maps/migration_background')
png(paste0("migration_Western.png"), width = 1500, height = 1000, units='px', res = 170)
plot
dev.off()

# Non-Western
df$density <- df$migration_non_west / df$area
title_exp <-expression(atop("Non-Western migration background \n density in marginal data", paste("[Non-Western/"~km^2~"]")))
plot <- ggplot(data = df) +
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(aes(fill = density
  )) +
  scale_fill_distiller(
    palette = "PuBu",
    direction = 1,
    na.value = "grey50",
    limits=c(min(df$density), max(df$density))
  ) +
  geom_point(aes(
    x = centroid_X,
    y = centroid_Y,
    size = diff_Non_Western
  ),
  color = 'red') +
  scale_size(limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(4.24, 4.31, by = 0.02)) +
  xlab('Longitude')+
  ylab('Latitude')+
  labs(fill = title_exp,
       size = "Percent point difference \n between synthetic population \n and marginal data")+
  ggtitle(bold('Migration background:')~' Non-Western') +
  theme(panel.background = element_blank(),
        text = element_text(size = 15))

setwd(this.dir())
setwd('plots/marginal/maps/migration_background')
png(paste0("migration_Non_Western.png"), width = 1500, height = 1000, units='px', res = 170)
plot
dev.off()

################################################################################
# Education attainment

df_max <- as.data.frame(df)
df_max <- df_max %>%
  select(diff_edu_attainment_low,
         diff_edu_attainment_middle,
         diff_edu_attainment_high)
max_diff <- max(df_max)

# low
df$density <- df$education_absolved_low / df$area
title_exp <-expression(atop("Low education attainment \n density in marginal data", paste("[(Low education attainment)/"~km^2~"]")))
plot <- ggplot(data = df) +
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(aes(fill = density
  )) +
  scale_fill_distiller(
    palette = "PuBu",
    direction = 1,
    na.value = "grey50",
    limits=c(min(df$density), max(df$density))
  ) +
  geom_point(aes(
    x = centroid_X,
    y = centroid_Y,
    size = diff_edu_attainment_low
  ),
  color = 'red') +
  scale_size(limits = c(0, max_diff)) +
  scale_x_continuous(breaks = seq(4.24, 4.31, by = 0.02)) +
  xlab('Longitude')+
  ylab('Latitude')+
  labs(fill = title_exp,
       size = "Percent point difference \n between synthetic population \n and marginal data")+
  ggtitle(bold('Education attainment:')~' low') +
  theme(panel.background = element_blank(),
        text = element_text(size = 15))

setwd(this.dir())
setwd('plots/marginal/maps/education_attainment')
png(paste0("education_attainment_low.png"), width = 1500, height = 1000, units='px', res = 170)
plot
dev.off()

# middle
df$density <- df$education_absolved_middle / df$area
title_exp <-expression(atop("Middle education attainment \n density in marginal data", paste("[(Middle education attainment)/"~km^2~"]")))
plot <- ggplot(data = df) +
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(aes(fill = density
  )) +
  scale_fill_distiller(
    palette = "PuBu",
    direction = 1,
    na.value = "grey50",
    limits=c(min(df$density), max(df$density))
  ) +
  geom_point(aes(
    x = centroid_X,
    y = centroid_Y,
    size = diff_edu_attainment_middle
  ),
  color = 'red') +
  scale_size(limits = c(0, max_diff)) +
  scale_x_continuous(breaks = seq(4.24, 4.31, by = 0.02)) +
  xlab('Longitude')+
  ylab('Latitude')+
  labs(fill = title_exp,
       size = "Percent point difference \n between synthetic population \n and marginal data")+
  ggtitle(bold('Education attainment:')~' middle') +
  theme(panel.background = element_blank(),
        text = element_text(size = 15))

setwd(this.dir())
setwd('plots/marginal/maps/education_attainment')
png(paste0("education_attainment_middle.png"), width = 1500, height = 1000, units='px', res = 170)
plot
dev.off()

# high
df$density <- df$education_absolved_high / df$area
title_exp <-expression(atop("High education attainment \n density in marginal data", paste("[(High education attainment)/"~km^2~"]")))
plot <- ggplot(data = df) +
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(aes(fill = density
  )) +
  scale_fill_distiller(
    palette = "PuBu",
    direction = 1,
    na.value = "grey50",
    limits=c(min(df$density), max(df$density))
  ) +
  geom_point(aes(
    x = centroid_X,
    y = centroid_Y,
    size = diff_edu_attainment_high
  ),
  color = 'red') +
  scale_size(limits = c(0, max_diff)) +
  scale_x_continuous(breaks = seq(4.24, 4.31, by = 0.02)) +
  xlab('Longitude')+
  ylab('Latitude')+
  labs(fill = title_exp,
       size = "Percent point difference \n between synthetic population \n and marginal data")+
  ggtitle(bold('Education attainment:')~' high') +
  theme(panel.background = element_blank(),
        text = element_text(size = 15))

setwd(this.dir())
setwd('plots/marginal/maps/education_attainment')
png(paste0("education_attainment_high.png"), width = 1500, height = 1000, units='px', res = 170)
plot
dev.off()

################################################################################
# Household type

df_max <- as.data.frame(df)
df_max <- df_max %>%
  select(diff_hh_single,
         diff_hh_with_children,
         diff_hh_no_children)
max_diff <- max(df_max)

# single
df$density <- df$hh_single / df$area
title_exp <-expression(atop("Single household \n density in marginal data", paste("[(Single households)/"~km^2~"]")))
plot <- ggplot(data = df) +
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(aes(fill = density
  )) +
  scale_fill_distiller(
    palette = "PuBu",
    direction = 1,
    na.value = "grey50",
    limits=c(min(df$density), max(df$density))
  ) +
  geom_point(aes(
    x = centroid_X,
    y = centroid_Y,
    size = diff_hh_single
  ),
  color = 'red') +
  scale_size(limits = c(0, max_diff)) +
  scale_x_continuous(breaks = seq(4.24, 4.31, by = 0.02)) +
  xlab('Longitude')+
  ylab('Latitude')+
  labs(fill = title_exp,
       size = "Percent point difference \n between synthetic population \n and marginal data")+
  ggtitle(bold('Household type:')~' single') +
  theme(panel.background = element_blank(),
        text = element_text(size = 15))

setwd(this.dir())
setwd('plots/marginal/maps/household_type')
png(paste0("household_type_single.png"), width = 1500, height = 1000, units='px', res = 170)
plot
dev.off()

# with children
df$density <- df$hh_with_children / df$area
title_exp <-expression(atop("Household with children \n density in marginal data", paste("[(Households with children)/"~km^2~"]")))
plot <- ggplot(data = df) +
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(aes(fill = density
  )) +
  scale_fill_distiller(
    palette = "PuBu",
    direction = 1,
    na.value = "grey50",
    limits=c(min(df$density), max(df$density))
  ) +
  geom_point(aes(
    x = centroid_X,
    y = centroid_Y,
    size = diff_hh_with_children
  ),
  color = 'red') +
  scale_size(limits = c(0, max_diff)) +
  scale_x_continuous(breaks = seq(4.24, 4.31, by = 0.02)) +
  xlab('Longitude')+
  ylab('Latitude')+
  labs(fill = title_exp,
       size = "Percent point difference \n between synthetic population \n and marginal data")+
  ggtitle(bold('Household type:')~' with children') +
  theme(panel.background = element_blank(),
        text = element_text(size = 15))

setwd(this.dir())
setwd('plots/marginal/maps/household_type')
png(paste0("household_type_with_children.png"), width = 1500, height = 1000, units='px', res = 170)
plot
dev.off()

# without children
df$density <- df$hh_no_children / df$area
title_exp <-expression(atop("Household without children \n density in marginal data", paste("[(Households without children)/"~km^2~"]")))
plot <- ggplot(data = df) +
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(aes(fill = density
  )) +
  scale_fill_distiller(
    palette = "PuBu",
    direction = 1,
    na.value = "grey50",
    limits=c(min(df$density), max(df$density))
  ) +
  geom_point(aes(
    x = centroid_X,
    y = centroid_Y,
    size = diff_hh_no_children
  ),
  color = 'red') +
  scale_size(limits = c(0, max_diff)) +
  scale_x_continuous(breaks = seq(4.24, 4.31, by = 0.02)) +
  xlab('Longitude')+
  ylab('Latitude')+
  labs(fill = title_exp,
       size = "Percent point difference \n between synthetic population \n and marginal data")+
  ggtitle(bold('Household type:')~' without children') +
  theme(panel.background = element_blank(),
        text = element_text(size = 15))

setwd(this.dir())
setwd('plots/marginal/maps/household_type')
png(paste0("household_type_without_children.png"), width = 1500, height = 1000, units='px', res = 170)
plot
dev.off()
