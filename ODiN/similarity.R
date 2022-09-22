library(dplyr)
library(tidyr)
library(ggplot2)
library(AMR)
library("this.path")
library(readr)

setwd(paste0(this.path::this.dir(), "/data/Formatted"))
df_ODiN <- read_csv("df_DHZW.csv")
df_synth_pop <- read_csv("~/DHZW_synthetic-population/synthetic-populations/synthetic_population_DHZW_2019_with_hh.csv")

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


df_unique_ODiN_agents <- df_unique_ODiN_agents %>%
  select(agent_ID, hh_PC4, gender, gender_value, migration_background, migration_background_value, age, age_value)

################################################################################

df_synth_pop$gender_value <- recode(df_synth_pop$gender,
                                             'male' = 0,
                                             'female' = 1)

df_synth_pop$migration_background_value <- recode(df_synth_pop$migration_background,
                                                           'Dutch' = 1.0,
                                                           'Western' = 0.5,
                                                           'Non_Western' = 0.5)

df_synth_pop$age_value <- df_synth_pop$age/max(df_synth_pop$age)

df_synth_pop <- df_synth_pop %>%
  select(agent_ID, hh_PC4, gender, gender_value, migration_background, migration_background_value, age, age_value)

################################################################################

df_synth_pop$ODiN_ID = NA
df_synth_pop$perfect_match = NA

#df_synth_pop <- df_synth_pop [1:10,]
for (i in 1:nrow(df_synth_pop)) {
  print(i)
  dist_table <- df_unique_ODiN_agents[df_unique_ODiN_agents$hh_PC4==df_synth_pop[i,]$hh_PC4,]
  
  dist_table$dist = sqrt(
    (dist_table$gender_value-df_synth_pop[i,]$gender_value)^2 +
    (dist_table$migration_background_value-df_synth_pop[i,]$migration_background_value)^2 +
    (dist_table$age_value-df_synth_pop[i,]$age_value)^2
    )
  
  df_synth_pop[i,]$ODiN_ID <- dist_table[which.min(dist_table$dist),]$agent_ID
  
  agent_ODiN <- dist_table[dist_table$agent_ID==df_synth_pop[i,]$ODiN_ID,]
  if(df_synth_pop[i,]$hh_PC4==agent_ODiN$hh_PC4 &
     df_synth_pop[i,]$gender==agent_ODiN$gender &
     df_synth_pop[i,]$migration_background==agent_ODiN$migration_background &
     df_synth_pop[i,]$age==agent_ODiN$age) {
    df_synth_pop[i,]$perfect_match = TRUE
  } else {
    df_synth_pop[i,]$perfect_match = FALSE
  }
}