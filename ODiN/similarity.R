library(dplyr)
library(tidyr)
library(ggplot2)
library(AMR)
library("this.path")
library(readr)

reformat_attributes_similarity <- function (df) {
  df <- df %>%
    mutate(male = ifelse(gender == 'male', 1, 0)) %>%
    mutate(female = ifelse(gender == 'female', 1, 0)) %>%
    mutate(migration_Dutch = ifelse(migration_background == 'Dutch', 1, 0)) %>%
    mutate(migration_Western = ifelse(migration_background == 'Western', 1, 0)) %>%
    mutate(migration_Non_Western = ifelse(migration_background == 'Non_Western', 1, 0)) %>%
    mutate(age_0_9 = ifelse(age %in% c(0:9), 1, 0)) %>%
    mutate(age_10_14 = ifelse(age %in% c(10:14), 1, 0)) %>%
    mutate(age_15_19 = ifelse(age %in% c(15:19), 1, 0)) %>%
    mutate(age_20_24 = ifelse(age %in% c(20:24), 1, 0)) %>%
    mutate(age_25_29 = ifelse(age %in% c(25:29), 1, 0)) %>%
    mutate(age_30_39 = ifelse(age %in% c(30:39), 1, 0)) %>%
    mutate(age_40_49 = ifelse(age %in% c(40:49), 1, 0)) %>%
    mutate(age_50_69 = ifelse(age %in% c(50:69), 1, 0)) %>%
    mutate(age_over_70 = ifelse(age >= 70, 1, 0))
  
  return (df)
}

################################################################################
# Formatting of ODiN

df_ODiN <- read_csv("df_DHZW.csv")

# Select agents from ODiN
df_ODiN_agents <- df_ODiN %>%
  select(agent_ID, hh_PC4, age, gender, migration_background) %>%
  distinct()

# Add ID to link each agent to its unique prototype
df_ODiN_agents <- df_ODiN_agents %>%
  mutate(prototype_ID = group_indices(., hh_PC4, age, gender, migration_background))

# For each ODiN agent, count how many have the same prototype
df_ODiN_agents <- df_ODiN_agents %>%
  group_by(prototype_ID) %>%
  mutate(freq_same_prototype = n())

# Save dataset
setwd(paste0(this.path::this.dir(), "/data/Formatted"))
write.csv(df_ODiN_agents, 'df_ODiN_prototype.csv', row.names = FALSE)

# Select each unique prototype, to be centroid of the vectorial space
df_ODiN_prototypes <- df_ODiN_agents %>%
  select(prototype_ID, hh_PC4, age, gender, migration_background) %>%
  distinct()

df_ODiN_prototypes <-
  reformat_attributes_similarity(df_ODiN_prototypes)

################################################################################
# Formatting of the synthetic population

setwd(paste0(this.path::this.dir(), "/data/Formatted"))
df_ODiN <- read_csv("df_DHZW.csv")
df_synth_pop <-
  read_csv(
    "~/DHZW_synthetic-population/synthetic-populations/synthetic_population_DHZW_2019_with_hh.csv"
  )

df_synth_pop <- df_synth_pop %>%
  select(agent_ID, hh_PC4, gender, migration_background, age)

df_synth_pop <- reformat_attributes_similarity(df_synth_pop)

################################################################################
# Matching of synthetic agents to the closest ODiN prototype
df_match_synthetic_ODiN <- df_synth_pop

df_match_synthetic_ODiN$prototype_ID = NA
df_match_synthetic_ODiN$perfect_match = NA
df_match_synthetic_ODiN$dist = 0

start_time <- Sys.time()

for (i in 1:nrow(df_match_synthetic_ODiN)) {
  synthetic_agent <- df_match_synthetic_ODiN[i,]
  
  # Filter agents in the same postal code area
  dist_table <-
    df_ODiN_prototypes[df_ODiN_prototypes$hh_PC4 == synthetic_agent$hh_PC4,]
  
  # Calculate distance to each prototype
  dist_table$dist = sqrt(sum(
    (dist_table$male - synthetic_agent$male) ^ 2,
    (dist_table$female - synthetic_agent$female) ^ 2,
    (
      dist_table$migration_Dutch - synthetic_agent$migration_Dutch
    ) ^ 2,
    (
      dist_table$migration_Western - synthetic_agent$migration_Western
    ) ^ 2,
    (
      dist_table$migration_Non_Western - synthetic_agent$migration_Non_Western
    ) ^ 2,
    (dist_table$age_0_9 - synthetic_agent$age_0_9) ^ 2,
    (dist_table$age_10_14 - synthetic_agent$age_10_14) ^ 2,
    (dist_table$age_15_19 - synthetic_agent$age_15_19) ^ 2,
    (dist_table$age_20_24 - synthetic_agent$age_20_24) ^ 2,
    (dist_table$age_25_29 - synthetic_agent$age_25_29) ^ 2,
    (dist_table$age_30_39 - synthetic_agent$age_30_39) ^ 2,
    (dist_table$age_40_49 - synthetic_agent$age_40_49) ^ 2,
    (dist_table$age_50_69 - synthetic_agent$age_50_69) ^ 2,
    (dist_table$age_over_70 - synthetic_agent$age_over_70) ^ 2
  ))
  
  
  # Retrieve prototype ID of the closest match
  prototype_ID <-
    dist_table[which.min(dist_table$dist),]$prototype_ID
  
  # assign prototype ID to the synthetic agent
  df_match_synthetic_ODiN[i,]$prototype_ID <- prototype_ID
  
  df_match_synthetic_ODiN[i,]$dist <- min(dist_table$dist)
  
  # Retrieve the attributes of the ODiN prototype to check if the synthetic agent is exactly equal, or similar
  agent_ODiN <-
    dist_table[dist_table$prototype_ID == prototype_ID,]
  
  if (synthetic_agent$gender == agent_ODiN$gender &
      synthetic_agent$migration_background == agent_ODiN$migration_background &
      synthetic_agent$age == agent_ODiN$age) {
    df_match_synthetic_ODiN[i,]$perfect_match = TRUE
  } else {
    df_match_synthetic_ODiN[i,]$perfect_match = FALSE
  }
}

end_time <- Sys.time()
difftime(end_time, start_time, units = "secs")
difftime(end_time, start_time, units = "mins")

df_match_synthetic_ODiN <- df_match_synthetic_ODiN %>%
  select(agent_ID, prototype_ID, perfect_match, dist)

################################################################################
# Analysis

# Merge with ODiN prototypes demographics
df_match_synthetic_ODiN <-
  merge(df_match_synthetic_ODiN, df_ODiN_prototypes, by = 'prototype_ID')
df_match_synthetic_ODiN <- df_match_synthetic_ODiN %>%
  rename('ODiN_gender' = 'gender') %>%
  rename('ODiN_migration' = 'migration_background') %>%
  rename('ODiN_age' = 'age') %>%
  select(agent_ID,
         prototype_ID,
         ODiN_gender,
         ODiN_migration,
         ODiN_age,
         perfect_match,
         dist)

# merge with synthetic demographics
df_match_synthetic_ODiN <-
  merge(df_match_synthetic_ODiN, df_synth_pop, by = 'agent_ID')
df_match_synthetic_ODiN <- df_match_synthetic_ODiN %>%
  rename('synthetic_gender' = 'gender') %>%
  rename('synthetic_migration' = 'migration_background') %>%
  rename('synthetic_age' = 'age') %>%
  select(
    agent_ID,
    prototype_ID,
    ODiN_gender,
    synthetic_gender,
    ODiN_migration,
    synthetic_migration,
    ODiN_age,
    synthetic_age,
    perfect_match,
    dist
  )

df_match_synthetic_ODiN$age_diff = abs(df_match_synthetic_ODiN$synthetic_age - df_match_synthetic_ODiN$ODiN_age)

# Average age difference
mean(df_match_synthetic_ODiN$age_diff)

# % corresponding genders
nrow(df_match_synthetic_ODiN[df_match_synthetic_ODiN$synthetic_gender == df_match_synthetic_ODiN$ODiN_gender,]) /
  nrow(df_match_synthetic_ODiN)

# % corresponding migration backgrounds
nrow(df_match_synthetic_ODiN[df_match_synthetic_ODiN$synthetic_migration == df_match_synthetic_ODiN$ODiN_migration,]) /
  nrow(df_match_synthetic_ODiN)

# Save dataset
setwd(paste0(this.path::this.dir(), "/data/Formatted"))
write.csv(df_match_synthetic_ODiN,
          'df_match_synthetic_ODiN.csv',
          row.names = FALSE)


# Analysis of exact matches
table(df_match_synthetic_ODiN$perfect_match)

# Analysis of how many ODiN agents are assigned to synthetic agents
df_analysis <- df_ODiN_agents %>%
  select(prototype_ID, freq_same_prototype) %>%
  distinct()

df_analysis = merge(df_match_synthetic_ODiN, df_analysis, by = 'prototype_ID')

table(df_analysis$freq_same_prototype)