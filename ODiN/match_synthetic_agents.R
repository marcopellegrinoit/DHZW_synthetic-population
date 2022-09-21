library(readr)
library("dplyr")
library("this.path")
setwd(this.path::this.dir())

#Load synthetic population
df_synth_pop <- read_csv("~/DHZW_synthetic-population/synthetic-populations/synthetic_population_DHZW_2019_with_hh.csv")

setwd(paste0(this.path::this.dir(), "/data/CBS PC neighbourhoods"))
df_PC4_neighb <- read_csv('PC4_neighb.csv')

################################################################################
# Recreate PC4 home address based on PC4 - neighbourhood CBS distribution matching

df_synth_pop$hh_PC4 = NA
for (neighb_code in unique(df_synth_pop$neighb_code)) {
  n_agents_area = length(unique(df_synth_pop[df_synth_pop$neighb_code==neighb_code,]$agent_ID))
  
  df_neighb_area = df_PC4_neighb[df_PC4_neighb$neighb_code==neighb_code,]
  
  # Next, we can just use those probabilities as fractions of the number of agents in each neighbourhood
  df_neighb_area$num_agents = df_neighb_area$prop * n_agents_area
  df_neighb_area = subset(df_neighb_area, select=-c(prop))
  
  df_neighb_area <- df_neighb_area[order(df_neighb_area$num_agents, decreasing = TRUE),]
  
  # round to integers
  for (i in (nrow(df_neighb_area):2)) {
    remainder = df_neighb_area[i,]$num_agents %% 2
    if (remainder > 0) {
      df_neighb_area[i, 'num_agents'] = df_neighb_area[i, 'num_agents'] - remainder
      df_neighb_area[i-1, 'num_agents'] = df_neighb_area[i-1, 'num_agents'] - remainder
    }
  }
  # I do the the last one manually
  df_neighb_area[1,]$num_agents = n_agents_area - sum(df_neighb_area[(2:nrow(df_neighb_area)),]$num_agents)
  
  # Sample agents accordingly
  for(i in 1:nrow(df_neighb_area)){
    agents_to_assign <- sample(df_synth_pop[df_synth_pop$neighb_code==neighb_code & is.na(df_synth_pop$hh_PC4),]$agent_ID, df_neighb_area[i,]$num_agents)
    df_synth_pop[df_synth_pop$agent_ID %in% agents_to_assign,]$hh_PC4 = df_neighb_area[i,]$PC4
  }
}

#write.csv(df_synth_pop, "~/DHZW_synthetic-population/synthetic-populations/synthetic_population_DHZW_2019_with_hh.csv", row.names = FALSE)

################################################################################

merge_ODiN_synthetic <- function(df_synth_pop, df_ODiN_agents, cols) {
  # Merge synthetic population with ODiN
  df_merged <- df_synth_pop %>%
    left_join(df_ODiN_agents, by = cols)
  
  # Randomly select one ODiN agent per synthetic agent
  df_merged <- df_merged %>% 
    group_by(agent_ID) %>%
    sample_n(1)
  
  return(df_merged)
}

# Load ODiN dataset
setwd(paste0(this.path::this.dir(), "/data/Formatted"))
df_ODiN <- read_csv("df_DHZW.csv")

df_ODiN_agents <- df_ODiN %>%
  select(agent_ID, hh_PC4, age, gender, migration_background) %>%
  distinct() %>%
  rename(ODiN_ID = agent_ID)

#############################################################################
# Age, gender, migration background, PC4 home
df_merged <- merge_ODiN_synthetic(df_synth_pop, df_ODiN_agents, cols = c('hh_PC4', 'age', 'gender', 'migration_background'))
prop_unassigned = nrow(df_merged[is.na(df_merged$ODiN_ID),])/nrow(df_synth_pop)

##############################################################################
# Groupages (5 years), gender, migration background, PC4 home
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

df_ODiN_agents$age_group = ""
df_ODiN_agents$age_group[df_ODiN_agents$age %in% 0:4] = "age_0_5"
df_ODiN_agents$age_group[df_ODiN_agents$age %in% 5:9] = "age_5_10"
df_ODiN_agents$age_group[df_ODiN_agents$age %in% 10:14] =  "age_10_15"
df_ODiN_agents$age_group[df_ODiN_agents$age %in% 15:19] = "age_15_20"
df_ODiN_agents$age_group[df_ODiN_agents$age %in% 20:24] =  "age_20_25" 
df_ODiN_agents$age_group[df_ODiN_agents$age %in% 25:29] = "age_25_30"
df_ODiN_agents$age_group[df_ODiN_agents$age %in% 30:34] =  "age_30_35"
df_ODiN_agents$age_group[df_ODiN_agents$age %in% 35:39] =  "age_35_40" 
df_ODiN_agents$age_group[df_ODiN_agents$age %in% 40:44] = "age_40_45"
df_ODiN_agents$age_group[df_ODiN_agents$age %in% 45:49] = "age_45_50"
df_ODiN_agents$age_group[df_ODiN_agents$age %in% 50:54] =  "age_50_55"
df_ODiN_agents$age_group[df_ODiN_agents$age %in% 55:59] = "age_55_60"
df_ODiN_agents$age_group[df_ODiN_agents$age %in% 60:64] =  "age_60_65" 
df_ODiN_agents$age_group[df_ODiN_agents$age %in% 65:69] = "age_65_70"
df_ODiN_agents$age_group[df_ODiN_agents$age %in% 70:74] =  "age_70_75"
df_ODiN_agents$age_group[df_ODiN_agents$age %in% 75:79] =  "age_75_80" 
df_ODiN_agents$age_group[df_ODiN_agents$age %in% 80:84] =  "age_80_85" 
df_ODiN_agents$age_group[df_ODiN_agents$age %in% 85:89] = "age_85_90"
df_ODiN_agents$age_group[df_ODiN_agents$age %in% 90:94] =  "age_90_95"
df_ODiN_agents$age_group[df_ODiN_agents$age %in% 95:104] =  "age_over_95"

df_merged <- merge_ODiN_synthetic(df_synth_pop, df_ODiN_agents, cols = c('hh_PC4', 'age_group', 'gender', 'migration_background'))
prop_unassigned = nrow(df_merged[is.na(df_merged$ODiN_ID),])/nrow(df_synth_pop)

##############################################################################
# Groupages (10 years), gender, migration background, PC4 home
df_synth_pop$age_group = ""
df_synth_pop$age_group[df_synth_pop$age %in% 0:9] = "age_0_10"
df_synth_pop$age_group[df_synth_pop$age %in% 10:19] =  "age_10_20"
df_synth_pop$age_group[df_synth_pop$age %in% 20:29] = "age_20_30"
df_synth_pop$age_group[df_synth_pop$age %in% 30:39] =  "age_30_40" 
df_synth_pop$age_group[df_synth_pop$age %in% 40:49] = "age_40_50"
df_synth_pop$age_group[df_synth_pop$age %in% 50:59] = "age_50_60"
df_synth_pop$age_group[df_synth_pop$age %in% 60:69] = "age_60_70"
df_synth_pop$age_group[df_synth_pop$age %in% 70:79] =  "age_70_80" 
df_synth_pop$age_group[df_synth_pop$age %in% 80:89] = "age_80_90"
df_synth_pop$age_group[df_synth_pop$age %in% 90:104] =  "age_over_90"

df_ODiN_agents$age_group = ""
df_ODiN_agents$age_group[df_ODiN_agents$age %in% 0:9] = "age_0_10"
df_ODiN_agents$age_group[df_ODiN_agents$age %in% 10:19] =  "age_10_20"
df_ODiN_agents$age_group[df_ODiN_agents$age %in% 20:29] = "age_20_30"
df_ODiN_agents$age_group[df_ODiN_agents$age %in% 30:39] =  "age_30_40" 
df_ODiN_agents$age_group[df_ODiN_agents$age %in% 40:49] = "age_40_50"
df_ODiN_agents$age_group[df_ODiN_agents$age %in% 50:59] = "age_50_60"
df_ODiN_agents$age_group[df_ODiN_agents$age %in% 60:69] = "age_60_70"
df_ODiN_agents$age_group[df_ODiN_agents$age %in% 70:79] =  "age_70_80" 
df_ODiN_agents$age_group[df_ODiN_agents$age %in% 80:89] = "age_80_90"
df_ODiN_agents$age_group[df_ODiN_agents$age %in% 90:104] =  "age_over_90"

df_merged <- merge_ODiN_synthetic(df_synth_pop, df_ODiN_agents, cols = c('hh_PC4', 'age_group', 'gender', 'migration_background'))
prop_unassigned = nrow(df_merged[is.na(df_merged$ODiN_ID),])/nrow(df_synth_pop)

##############################################################################
# Custom groupages, gender, migration background, PC4 home
df_synth_pop$age_group = ""
df_synth_pop$age_group[df_synth_pop$age %in% 0:9] = "age_0_9"
df_synth_pop$age_group[df_synth_pop$age %in% 10:14] =  "age_10_14"
df_synth_pop$age_group[df_synth_pop$age %in% 15:19] = "age_15_19"
df_synth_pop$age_group[df_synth_pop$age %in% 20:24] =  "age_20_24" 
df_synth_pop$age_group[df_synth_pop$age %in% 25:29] = "age_25_29"
df_synth_pop$age_group[df_synth_pop$age %in% 30:39] =  "age_30_39"
df_synth_pop$age_group[df_synth_pop$age %in% 40:49] =  "age_40_49" 
df_synth_pop$age_group[df_synth_pop$age %in% 50:69] = "age_50_69"
df_synth_pop$age_group[df_synth_pop$age >= 70] = "age_over_70"

df_ODiN_agents$age_group = ""
df_ODiN_agents$age_group[df_ODiN_agents$age %in% 0:9] = "age_0_9"
df_ODiN_agents$age_group[df_ODiN_agents$age %in% 10:14] =  "age_10_14"
df_ODiN_agents$age_group[df_ODiN_agents$age %in% 15:19] = "age_15_19"
df_ODiN_agents$age_group[df_ODiN_agents$age %in% 20:24] =  "age_20_24" 
df_ODiN_agents$age_group[df_ODiN_agents$age %in% 25:29] = "age_25_29"
df_ODiN_agents$age_group[df_ODiN_agents$age %in% 30:39] =  "age_30_39"
df_ODiN_agents$age_group[df_ODiN_agents$age %in% 40:49] =  "age_40_49" 
df_ODiN_agents$age_group[df_ODiN_agents$age %in% 50:69] = "age_50_69"
df_ODiN_agents$age_group[df_ODiN_agents$age >= 70] = "age_over_70"

df_merged <- merge_ODiN_synthetic(df_synth_pop, df_ODiN_agents, cols = c('hh_PC4', 'age_group', 'gender', 'migration_background'))
prop_unassigned = nrow(df_merged[is.na(df_merged$ODiN_ID),])/nrow(df_synth_pop)

unassigned = df_merged[is.na(df_merged$ODiN_ID),]

unassigned_unique <- unassigned %>%
  select(hh_PC4, age_group, gender, migration_background) %>%
  distinct() %>%
  group_by(hh_PC4, age_group, gender, migration_background) %>%
  summarise(frequency = n()) %>%
  ungroup

ggplot(unassigned_unique, aes(as.character(hh_PC4), y=age_group))+
  geom_point()