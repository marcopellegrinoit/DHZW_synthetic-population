library(dplyr)
library(tidyr)
library(ggplot2)
library(AMR)
library("this.path")
library(readr)

setwd(paste0(this.path::this.dir(), "/data/Formatted"))
df_ODiN <- read_csv("df_DHZW.csv")
df_synth_pop <-
  read_csv(
    "~/DHZW_synthetic-population/synthetic-populations/synthetic_population_DHZW_2019_with_hh.csv"
  )

################################################################################
# Formatting of ODiN

# Select agents from ODiN
df_ODiN_agents <- df_ODiN %>%
  select(agent_ID, hh_PC4, age, gender, migration_background) %>%
  distinct()

# Add ID to link each agent to its unique protoype
df_ODiN_agents <- df_ODiN_agents %>%
  mutate(prototype_ID = group_indices(., hh_PC4, age, gender, migration_background))

# For each ODiN agent, count how many have the same protoype
df_ODiN_agents <- df_ODiN_agents %>%
  group_by(prototype_ID) %>%
  mutate(freq_same_prototype = n())

# Save dataset
setwd(paste0(this.path::this.dir(), "/data/Formatted"))
write.csv(df_ODiN_agents, 'df_ODiN_prototype.csv', row.names=FALSE)

# Select each unique prototype, to be centroids of the vectorial space
df_ODiN_prototypes <- df_ODiN_agents %>%
  select(prototype_ID, hh_PC4, age, gender, migration_background) %>%
  distinct()

# Transform values into vector coordinates
df_ODiN_prototypes$gender <- recode(df_ODiN_prototypes$gender,
                                   'male' = 0,
                                   'female' = 1)

df_ODiN_prototypes$migration_background <-
  recode(
    df_ODiN_prototypes$migration_background,
    'Dutch' = 1.0,
    'Western' = 0.5,
    'Non_Western' = 0.0
  )

df_ODiN_prototypes$age <-
  df_ODiN_prototypes$age / max(df_synth_pop$age)

################################################################################
# Formatting of the synthetic population

# Transform values into vector coordinates
df_synth_pop$gender <- recode(df_synth_pop$gender,
                              'male' = 0,
                              'female' = 1)

df_synth_pop$migration_background <-
  recode(
    df_synth_pop$migration_background,
    'Dutch' = 1.0,
    'Western' = 0.5,
    'Non_Western' = 0.0
  )

df_synth_pop$age <- df_synth_pop$age / max(df_synth_pop$age)

df_synth_pop <- df_synth_pop %>%
  select(agent_ID, hh_PC4, gender, migration_background, age)

################################################################################
# Matching of synthetic agents to the closest ODiN prototype
df_match_synthetic_ODiN <- df_synth_pop

df_match_synthetic_ODiN$prototype_ID = NA
df_match_synthetic_ODiN$perfect_match = NA

#df_match_synthetic_ODiN <- df_match_synthetic_ODiN [1:10,]
for (i in 1:nrow(df_match_synthetic_ODiN)) {
  synthetic_agent <- df_match_synthetic_ODiN[i,]
  
  # Filter agents in the same postal code area
  dist_table <-
    df_ODiN_prototypes[df_ODiN_prototypes$hh_PC4 == synthetic_agent$hh_PC4,]
  
  # Calculate distance to each prototype
  dist_table$dist = sqrt(
    (dist_table$gender - synthetic_agent$gender) ^ 2 +
      (
        dist_table$migration_background - synthetic_agent$migration_background
      ) ^ 2 +
      (dist_table$age - synthetic_agent$age) ^ 2
  )
  
  # Retrieve prototype ID of the closest match
  prototype_ID <-
    dist_table[which.min(dist_table$dist),]$prototype_ID
  
  # assign prototype ID to the synthetic agent
  df_match_synthetic_ODiN[i,]$prototype_ID <- prototype_ID
  
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

df_match_synthetic_ODiN <- df_match_synthetic_ODiN %>%
  select(agent_ID, prototype_ID, perfect_match)

# Save dataset
setwd(paste0(this.path::this.dir(), "/data/Formatted"))
write.csv(df_match_synthetic_ODiN, 'df_match_synthetic_ODiN.csv', row.names=FALSE)

# Analysis of exact matches
table(df_match_synthetic_ODiN$perfect_match)

# Analysis of how many ODiN agents are assigned to synthetic agents
df_analysis <- df_ODiN_agents %>%
  select(prototype_ID, freq_same_prototype) %>%
  distinct()

df_analysis = merge(df_match_synthetic_ODiN, df_analysis, by='prototype_ID')

table(df_analysis$freq_same_prototype)