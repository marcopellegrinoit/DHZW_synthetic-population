library(ggplot2)
library(dplyr)
library(tidyr)
library("this.path")
setwd(this.path::this.dir())
source('../../src/utils-evaluation-synthetic-population.R')

# Load datasets
setwd(this.path::this.dir())
setwd("../data/processed/households")

df_hh_type = read.csv("household_gender_age-71488NED-formatted.csv", sep = ",")

# Load synthetic population
setwd(this.path::this.dir())
setwd(paste("../../output/synthetic-population-households", sep = ""))
df_synth_pop = read.csv("synthetic_population_DHZW_2019.csv", sep = ",")

################################################################################
# Validation with stratified dataset
df_hh_type = df_hh_type %>%
  select(gender, age_group, single, couple, single_parent) %>%
  pivot_longer(cols = -c(gender, age_group), names_to = "hh_type", values_to = "real")

df_synth_pop$age_group = ""
df_synth_pop$age_group[df_synth_pop$age %in% 0:4] = "age_0_5"
df_synth_pop$age_group[df_synth_pop$age %in% 5:9] = "age_5_10"
df_synth_pop$age_group[df_synth_pop$age %in% 10:14] =  "age_10_15"
df_synth_pop$age_group[df_synth_pop$age %in% 15:19] = "age_15_20"
df_synth_pop$age_group[df_synth_pop$age %in% 20:24] =  "age_20_25" 
df_synth_pop$age_group[df_synth_pop$age %in% 25:29] = "age_25_30"
df_synth_pop$age_group[df_synth_pop$age %in% 30:34] =  "age_30_35"
df_synth_pop$age_group[df_synth_pop$age %in% 35:39] =  "age_35_40" 
df_synth_pop$age_group[df_synth_pop$age %in% 40:44] = "age_40_45"
df_synth_pop$age_group[df_synth_pop$age %in% 45:49] = "age_45_50"
df_synth_pop$age_group[df_synth_pop$age %in% 50:54] =  "age_50_55"
df_synth_pop$age_group[df_synth_pop$age %in% 55:59] = "age_55_60"
df_synth_pop$age_group[df_synth_pop$age %in% 60:64] =  "age_60_65" 
df_synth_pop$age_group[df_synth_pop$age %in% 65:69] = "age_65_70"
df_synth_pop$age_group[df_synth_pop$age %in% 70:74] =  "age_70_75"
df_synth_pop$age_group[df_synth_pop$age %in% 75:79] =  "age_75_80" 
df_synth_pop$age_group[df_synth_pop$age %in% 80:84] =  "age_80_85" 
df_synth_pop$age_group[df_synth_pop$age %in% 85:89] = "age_85_90"
df_synth_pop$age_group[df_synth_pop$age %in% 90:94] =  "age_90_95"
df_synth_pop$age_group[df_synth_pop$age %in% 95:104] =  "age_over_95"

df_synth_pop[df_synth_pop$hh_type == 'couple_children_straight',]$hh_type <- 'couple'
df_synth_pop[df_synth_pop$hh_type == 'couple_children_gay',]$hh_type <- 'couple'
df_synth_pop[df_synth_pop$hh_type == 'couple_children_lesbian',]$hh_type <- 'couple'
df_synth_pop[df_synth_pop$hh_type == 'couple_no_children_straight',]$hh_type <- 'couple'
df_synth_pop[df_synth_pop$hh_type == 'couple_no_children_lesbian',]$hh_type <- 'couple'
df_synth_pop[df_synth_pop$hh_type == 'couple_no_children_gay',]$hh_type <- 'couple'

df_hh_type$pred = 0
for (i in (1:nrow(df_hh_type))) {
  df_hh_type[i, 'pred',] = nrow(df_synth_pop[df_synth_pop$age_group == df_hh_type[i, 'age_group']$age_group &
                                                       df_synth_pop$gender == df_hh_type[i, 'gender']$gender &
                                                       df_synth_pop$hh_type == df_hh_type[i, 'hh_type']$hh_type,])
}
df_synth_pop = subset(df_synth_pop, select=-c(age_group))

df_hh_type <- df_hh_type %>%
  pivot_longer(cols = c(real, pred), names_to = 'dataset', values_to = 'proportion')

# rename values
df_hh_type$dataset <- recode(df_hh_type$dataset,
                                     'real' = 'stratified dataset',
                                     'pred' = 'synthetic population')

setwd(this.dir())
setwd('data_comparison')
write.csv(df_hh_type, 'hh_type.csv', row.names = FALSE)