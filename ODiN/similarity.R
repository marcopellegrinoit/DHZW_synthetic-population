library(dplyr)
library(tidyr)
library(ggplot2)
library(AMR)
library("this.path")
library(readr)

setwd(paste0(this.path::this.dir(), "/data/Formatted"))
df_ODiN <- read_csv("df_DHZW.csv")
df_synth_pop <- read_csv("~/DHZW_synthetic-population/synthetic-populations/synthetic_population_DHZW_2019_with_hh.csv")

library("sf")
library("rgeos")
shp_DHZW_PC4 <- st_read('~/DHZW_synthetic-population/ODiN/data/map/DHZW_PC4_shapefiles')

centroids <- 
  gCentroid( 
    spgeom = methods::as( object = shp_DHZW_PC4, Class = "Spatial" )
    , byid = TRUE 
  )
shp_DHZW_PC4 <- cbind(shp_DHZW_PC4, centroids@coords)
shp_DHZW_PC4 <- shp_DHZW_PC4 %>%
  rename('hh_PC4' = 'PC4',
         'hh_PC4_x' = 'x',
         'hh_PC4_y' = 'y')
# normalise coordinates
normalise <- function(x){(x-min(x))/(max(x)-min(x))}
shp_DHZW_PC4$hh_PC4_x <- normalise(shp_DHZW_PC4$hh_PC4_x)
shp_DHZW_PC4$hh_PC4_y <- normalise(shp_DHZW_PC4$hh_PC4_y)

################################################################################
# I make each ODiN agents unique, representing the centroid of clusters
df_unique_ODiN_agents <- df_ODiN %>%
  select(agent_ID, hh_PC4, age, gender, migration_background) %>%
  distinct(across(-agent_ID), .keep_all = TRUE)

df_unique_ODiN_agents$gender_value <- recode(df_unique_ODiN_agents$gender,
                                       'male' = 0,
                                       'female' = 1)

df_unique_ODiN_agents$migration_background_value <- recode(df_unique_ODiN_agents$migration_background,
                                                     'Dutch' = 1.0,
                                                     'Western' = 0.5,
                                                     'Non_Western' = 0.5)

df_unique_ODiN_agents$age_value <- df_unique_ODiN_agents$age/max(df_synth_pop$age)

df_unique_ODiN_agents = merge(df_unique_ODiN_agents, shp_DHZW_PC4, by='hh_PC4')

df_unique_ODiN_agents <- df_unique_ODiN_agents %>%
  select(agent_ID, hh_PC4, hh_PC4_x, hh_PC4_y, gender, gender_value, migration_background, migration_background_value, age, age_value)

################################################################################

df_synth_pop$gender_value <- recode(df_synth_pop$gender,
                                             'male' = 0,
                                             'female' = 1)

df_synth_pop$migration_background_value <- recode(df_synth_pop$migration_background,
                                                           'Dutch' = 1.0,
                                                           'Western' = 0.5,
                                                           'Non_Western' = 0.5)

df_synth_pop$age_value <- df_synth_pop$age/max(df_synth_pop$age)

df_synth_pop = merge(df_synth_pop, shp_DHZW_PC4, by='hh_PC4')

df_synth_pop <- df_synth_pop %>%
  select(agent_ID, hh_PC4, hh_PC4_x, hh_PC4_y, gender, gender_value, migration_background, migration_background_value, age, age_value)

################################################################################

df_synth_pop$ODiN_ID = NA
df_synth_pop$perfect_match = NA

df_synth_pop <- df_synth_pop [1:10,]
for (i in 1:nrow(df_synth_pop)) {
  
  print(i)

  df_unique_ODiN_agents$dist = sqrt(
    (df_unique_ODiN_agents$hh_PC4_x-df_synth_pop[i,]$hh_PC4_x)^2 +
    (df_unique_ODiN_agents$hh_PC4_y-df_synth_pop[i,]$hh_PC4_y)^2 +
    (df_unique_ODiN_agents$gender_value-df_synth_pop[i,]$gender_value)^2 +
    (df_unique_ODiN_agents$migration_background_value-df_synth_pop[i,]$migration_background_value)^2 +
    (df_unique_ODiN_agents$age_value-df_synth_pop[i,]$age_value)^2
    )
  
  df_synth_pop[i,]$ODiN_ID <- df_unique_ODiN_agents[which.min(df_unique_ODiN_agents$dist),]$agent_ID
  
  agent_ODiN <- df_unique_ODiN_agents[df_unique_ODiN_agents$agent_ID==df_synth_pop[i,]$ODiN_ID,]
  if(df_synth_pop[i,]$hh_PC4==agent_ODiN$hh_PC4 &
     df_synth_pop[i,]$gender==agent_ODiN$gender &
     df_synth_pop[i,]$migration_background==agent_ODiN$migration_background &
     df_synth_pop[i,]$age==agent_ODiN$age) {
    df_synth_pop[i,]$perfect_match = TRUE
  } else {
    df_synth_pop[i,]$perfect_match = FALSE
  }
}
