library(GenSynthPop)
library(dplyr)

library("this.path")
setwd(this.path::this.dir())
source('utils.R')

municipality = "den_haag_2019"

## Load marginal distributions

setwd(paste(this.path::this.dir(), "/data/", municipality, sep = ""))
df_MarginalDistr = read.csv("marginal_distributions_84583NED-formatted.csv", sep = ",")

# filter DHZW area
setwd(paste(this.path::this.dir(), "/data", sep = ""))
DHZW_neighborhood_codes <- read.csv("DHZW_neighbourhoods_codes.csv", sep = ";" ,header=F)$V1

df_MarginalDistr = df_MarginalDistr[df_MarginalDistr$neighb_code %in% DHZW_neighborhood_codes,]

################################################################################
## Initialise synthetic population with age groups withing neigbbourhoods
################################################################################

# create empty synthetic population dataframe with number of agents
# Note: CBS must have made a mistake, probably with the age grouping, because the total is larger than the general population.
# So, I initialise the synthetic population size with the sum of group ages, otherwise the script cannot work.

group_ages = c('age_0_15', 'age_15_25', 'age_25_45', 'age_45_65', 'age_over65')
population_size = sum(df_MarginalDistr[group_ages]) # 78655
df_SynthPop = gen_agent_df(population_size)

# Distribute the agents across the age groups and neighborhoods
df_SynthPop = distr_agent_neigh_age_group(neigh_df = df_MarginalDistr,
                                          agent_df = df_SynthPop,
                                          neigh_id = "neighb_code",
                                          age_colnames = group_ages)

################################################################################
## Translate age groups into interger age
################################################################################

# Note: since the dataset is municipality aggregated, I can only calculate each age proportion and then use it to sample

# Load dataset
setwd(paste(this.path::this.dir(), "/data/", municipality, "/stratified-datasets", sep = ""))
df_StratGender = read.csv("gender_age-03759NED-formatted.csv", sep = ",")

# for each group age of the synthetic population, sample the age from stratified dataset following the the frequency distribution
df_SynthPop$age=''
for(group_age in group_ages){
  sample <- sample(
    x = df_StratGender[df_StratGender$age_group==group_age,]$age,
    size = nrow(df_SynthPop[df_SynthPop$age_group==group_age,]),
    replace=TRUE,
    prob=df_StratGender[df_StratGender$age_group==group_age,]$group_propensity
  ) # sample from age frequency distribution
  
  df_SynthPop[df_SynthPop$age_group==group_age,]$age = sample # apply to synthetic population dataset
}
df_SynthPop$age = as.numeric(df_SynthPop$age)

################################################################################
## Gender generation based on age
################################################################################

# Compute conditional propensities
df_SynthPop = calc_propens_agents(dataframe = df_StratGender,
                                  variable = "female",
                                  total_population = "total",
                                  agent_df = df_SynthPop,
                                  list_conditional_var = c("age")
)

# Distribute attributes
df_SynthPop = distr_attr_strat_neigh_stats_binary(agent_df = df_SynthPop,
                                                  neigh_df = df_MarginalDistr,
                                                  neigh_ID = "neighb_code",
                                                  variable=  "gender",
                                                  list_var_classes_neigh_df = c("gender_female", "gender_male"),
                                                  list_agent_propens =  c("prop_female"),
                                                  list_class_names = c("female", "male")
)

# Remove extra columns
df_SynthPop = subset(df_SynthPop, select=-c(prop_female, random_scores, age_group))

################################################################################
## Migration background generation based on age and gender
################################################################################

# Load stratified dataset
setwd(paste(this.path::this.dir(), "/data/", municipality, "/stratified-datasets", sep = ""))
df_StratMigration = read.csv("gender_age_migration-84910NED-formatted.csv", sep = ",")

# Classify synthetic population ages into age groups to link to the stratified dataset
df_SynthPop$age_group = ""
df_SynthPop$age_group[df_SynthPop$age %in% 0:4] = "age_0_5"
df_SynthPop$age_group[df_SynthPop$age %in% 5:9] = "age_5_10"
df_SynthPop$age_group[df_SynthPop$age %in% 10:14] =  "age_10_15"
df_SynthPop$age_group[df_SynthPop$age %in% 15:19] = "age_15_20"
df_SynthPop$age_group[df_SynthPop$age %in% 20:24] =  "age_20_25" 
df_SynthPop$age_group[df_SynthPop$age %in% 25:29] = "age_25_30"
df_SynthPop$age_group[df_SynthPop$age %in% 30:34] =  "age_30_35"
df_SynthPop$age_group[df_SynthPop$age %in% 35:39] =  "age_35_40" 
df_SynthPop$age_group[df_SynthPop$age %in% 40:44] = "age_40_45"
df_SynthPop$age_group[df_SynthPop$age %in% 45:49] = "age_45_50"
df_SynthPop$age_group[df_SynthPop$age %in% 50:54] =  "age_50_55"
df_SynthPop$age_group[df_SynthPop$age %in% 55:59] = "age_55_60"
df_SynthPop$age_group[df_SynthPop$age %in% 60:64] =  "age_60_65" 
df_SynthPop$age_group[df_SynthPop$age %in% 65:69] = "age_65_70"
df_SynthPop$age_group[df_SynthPop$age %in% 70:74] =  "age_70_75"
df_SynthPop$age_group[df_SynthPop$age %in% 75:79] =  "age_75_80" 
df_SynthPop$age_group[df_SynthPop$age %in% 80:84] =  "age_80_85" 
df_SynthPop$age_group[df_SynthPop$age %in% 85:89] = "age_85_90"
df_SynthPop$age_group[df_SynthPop$age %in% 90:94] =  "age_90_95"
df_SynthPop$age_group[df_SynthPop$age %in% 95:104] =  "age_over_95"

## Conditional Propensities
df_SynthPop = calc_propens_agents(df_StratMigration, "Dutch", "total", df_SynthPop, c("age_group", "gender") )
df_SynthPop = calc_propens_agents(df_StratMigration, "Western", "total", df_SynthPop, c("age_group", "gender") )
df_SynthPop = calc_propens_agents(df_StratMigration, "Non_Western", "total", df_SynthPop, c("age_group", "gender") )

# Distribute values
df_SynthPop = distr_attr_strat_neigh_stats_3plus(agent_df =  df_SynthPop,
                                                 neigh_df =  df_MarginalDistr,
                                                 neigh_ID =  "neighb_code",
                                                 variable =  "migration_background", 
                                                 list_var_classes_neigh_df =  c("migration_Dutch", "migration_west", "migration_non_west"), 
                                                 list_agent_propens =  c("prop_Dutch", "prop_Western", "prop_Non_Western"),
                                                 list_class_names =  c("Dutch", "Western", "Non_Western"))

# Remove extra columns
df_SynthPop = subset(df_SynthPop, select=-c(prop_Dutch, prop_Western, prop_Non_Western, random_scores, age_group))

################################################################################
## Generate current education based on group age, gender and migration
################################################################################

# Load stratified dataset
setwd(paste(this.path::this.dir(), "/data/", municipality, "/stratified-datasets", sep = ""))
df_StratEduCurrent = read.csv("edu_current-71450NED-formatted.csv", sep = ",")

# Create groupages in the synthetic population to match the stratified dataset
df_SynthPop$age_group[df_SynthPop$age < 15] = NA
df_SynthPop$age_group[df_SynthPop$age %in% 15:19] = "age_15_20"
df_SynthPop$age_group[df_SynthPop$age %in% 20:24] = "age_20_25" 
df_SynthPop$age_group[df_SynthPop$age %in% 25:29] = "age_25_30" 
df_SynthPop$age_group[df_SynthPop$age %in% 30:34] = "age_30_35" 
df_SynthPop$age_group[df_SynthPop$age %in% 35:39] = "age_35_40" 
df_SynthPop$age_group[df_SynthPop$age %in% 40:44] = "age_40_45"
df_SynthPop$age_group[df_SynthPop$age %in% 45:49] = "age_45_50"
df_SynthPop$age_group[df_SynthPop$age >= 50] = "age_over_50"

# Calculate propensities
df_SynthPop = calc_propens_agents(dataframe =  df_StratEduCurrent, variable = "high", total_population =  "total", agent_df =  df_SynthPop, list_conditional_var = c("age_group", "gender", "migration_background") )
df_SynthPop = calc_propens_agents(dataframe =  df_StratEduCurrent, variable = "middle", total_population =  "total", agent_df =  df_SynthPop, list_conditional_var = c("age_group", "gender", "migration_background") )
df_SynthPop = calc_propens_agents(dataframe =  df_StratEduCurrent, variable = "low", total_population =  "total", agent_df =  df_SynthPop, list_conditional_var = c("age_group", "gender", "migration_background") )
df_SynthPop = calc_propens_agents(dataframe =  df_StratEduCurrent, variable = "no_current_edu", total_population =  "total", agent_df =  df_SynthPop, list_conditional_var = c("age_group", "gender", "migration_background") )

# Exclude agents that are younger than 15 years old. I will fix the education in the next step for them
df_SynthPop$current_edu_exclude = 0
df_SynthPop$current_edu_exclude[which(df_SynthPop$age<15)] = 1

df_SynthPop = distr_attr_cond_prop(agent_df = df_SynthPop,
                                   variable=  "current_education",
                                   list_agent_propens =  c("prop_low",  "prop_middle", "prop_high", "prop_no_edu"),
                                   list_class_names = c("low", "middle", "high", "no_current_edu"),
                                   agent_exclude = "current_edu_exclude"
)

# Remove extra columns
df_SynthPop = subset(df_SynthPop, select=-c(excluded, prop_low, prop_middle, prop_high, prop_no_current_edu, random_scores, age_group, current_edu_exclude))

# Fix values for people younger than 15 years old
df_SynthPop$current_education[which(df_SynthPop$age > 5 & df_SynthPop$age < 15) ] = "low" # students between 5 and 15 are obliged to low level schools
df_SynthPop$current_education[which(df_SynthPop$age <= 5) ] = "no_current_edu" # individuals younger than 5 do not go to school at all

# Save synthetic population
setwd(paste(this.path::this.dir(), "/synthetic-populations", sep = ""))
write.csv(df_SynthPop, 'synthetic_population_DHZW_2019.csv', row.names=FALSE)