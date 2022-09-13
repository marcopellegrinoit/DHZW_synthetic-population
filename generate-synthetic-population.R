library(GenSynthPop)
library(dplyr)

library("this.path")
setwd(this.path::this.dir())
source('utils.R')

municipality = "den_haag_2019"

## Load marginal distribution
setwd(paste(this.path::this.dir(), "/data/", municipality, sep = ""))
df_marginal_dist = read.csv("marginal_distributions_84583NED-formatted.csv", sep = ",")

# filter DHZW area
setwd(paste(this.path::this.dir(), "/data", sep = ""))
DHZW_neighborhood_codes <- read.csv("DHZW_neighbourhoods_codes.csv", sep = ";" ,header=F)$V1

df_marginal_dist = df_marginal_dist[df_marginal_dist$neighb_code %in% DHZW_neighborhood_codes,]
  
################################################################################
## Initialise synthetic population with age groups withing neigbbourhoods
################################################################################

# create empty synthetic population dataframe with number of agents
# Note: CBS must have made a mistake, probably with the age grouping, because the total is larger than the general population.
# So, I initialise the synthetic population size with the sum of group ages, otherwise the script cannot work.

group_ages = c('age_0_15', 'age_15_25', 'age_25_45', 'age_45_65', 'age_over65')
population_size = sum(df_marginal_dist[group_ages]) # 78655
df_synth_pop = gen_agent_df(population_size)

# Distribute the agents across the age groups and neighborhoods
df_synth_pop = distr_agent_neigh_age_group(neigh_df = df_marginal_dist,
                                          agent_df = df_synth_pop,
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
df_synth_pop$age=''
for(group_age in group_ages){
  sample <- sample(
    x = df_StratGender[df_StratGender$age_group==group_age,]$age,
    size = nrow(df_synth_pop[df_synth_pop$age_group==group_age,]),
    replace=TRUE,
    prob=df_StratGender[df_StratGender$age_group==group_age,]$group_propensity
  ) # sample from age frequency distribution
  
  df_synth_pop[df_synth_pop$age_group==group_age,]$age = sample # apply to synthetic population dataset
}
df_synth_pop$age = as.numeric(df_synth_pop$age)

################################################################################
## Gender generation based on age
################################################################################

# Compute conditional propensities
df_synth_pop = calc_propens_agents(dataframe = df_StratGender,
                                  variable = "female",
                                  total_population = "total",
                                  agent_df = df_synth_pop,
                                  list_conditional_var = c("age")
)

# Distribute attributes
df_synth_pop = distr_attr_strat_neigh_stats_binary(agent_df = df_synth_pop,
                                                  neigh_df = df_marginal_dist,
                                                  neigh_ID = "neighb_code",
                                                  variable=  "gender",
                                                  list_var_classes_neigh_df = c("gender_female", "gender_male"),
                                                  list_agent_propens =  c("prop_female"),
                                                  list_class_names = c("female", "male")
)

# Remove extra columns
df_synth_pop = subset(df_synth_pop, select=-c(prop_female, random_scores, age_group))

################################################################################
## Migration background generation based on age and gender
################################################################################

# Load stratified dataset
setwd(paste(this.path::this.dir(), "/data/", municipality, "/stratified-datasets", sep = ""))
df_StratMigration = read.csv("gender_age_migration-84910NED-formatted.csv", sep = ",")

# Classify synthetic population ages into age groups to link to the stratified dataset
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

## Conditional Propensities
df_synth_pop = calc_propens_agents(df_StratMigration, "Dutch", "total", df_synth_pop, c("age_group", "gender") )
df_synth_pop = calc_propens_agents(df_StratMigration, "Western", "total", df_synth_pop, c("age_group", "gender") )
df_synth_pop = calc_propens_agents(df_StratMigration, "Non_Western", "total", df_synth_pop, c("age_group", "gender") )

# Distribute values
df_synth_pop = distr_attr_strat_neigh_stats_3plus(agent_df =  df_synth_pop,
                                                 neigh_df =  df_marginal_dist,
                                                 neigh_ID =  "neighb_code",
                                                 variable =  "migration_background", 
                                                 list_var_classes_neigh_df =  c("migration_Dutch", "migration_west", "migration_non_west"), 
                                                 list_agent_propens =  c("prop_Dutch", "prop_Western", "prop_Non_Western"),
                                                 list_class_names =  c("Dutch", "Western", "Non_Western"))

# Remove extra columns
df_synth_pop = subset(df_synth_pop, select=-c(prop_Dutch, prop_Western, prop_Non_Western, random_scores, age_group))

################################################################################
# Attribute is_child

# Load formatted stratified dataset about household position, gender and groupages (municipality aggregated)
setwd(paste(this.path::this.dir(), "/data/", municipality, "/households/distributions", sep = ""))
df_StratHousehold = read.csv("household_gender_age-71488NED-formatted.csv", sep = ",", fileEncoding="UTF-8-BOM")

#setwd(paste(this.path::this.dir(), "/synthetic-populations", sep = ""))
#df_synth_pop = read.csv('synthetic_population_DHZW_2019.csv')

df_synth_pop$age_group = ""
df_synth_pop$age_group[df_synth_pop$age %in% 0:4] = "age_0_5"
df_synth_pop$age_group[df_synth_pop$age %in% 5:9] = "age_5_10"
df_synth_pop$age_group[df_synth_pop$age %in% 10:14] = "age_10_15"
df_synth_pop$age_group[df_synth_pop$age %in% 15:19] = "age_15_20"
df_synth_pop$age_group[df_synth_pop$age %in% 20:24] = "age_20_25"
df_synth_pop$age_group[df_synth_pop$age %in% 25:29] = "age_25_30"
df_synth_pop$age_group[df_synth_pop$age %in% 30:34] = "age_30_35"
df_synth_pop$age_group[df_synth_pop$age %in% 35:39] = "age_35_40"
df_synth_pop$age_group[df_synth_pop$age %in% 40:44] = "age_40_45"
df_synth_pop$age_group[df_synth_pop$age %in% 45:49] = "age_45_50"
df_synth_pop$age_group[df_synth_pop$age %in% 50:54] = "age_50_55"
df_synth_pop$age_group[df_synth_pop$age %in% 55:59] = "age_55_60"
df_synth_pop$age_group[df_synth_pop$age %in% 60:64] = "age_60_65"
df_synth_pop$age_group[df_synth_pop$age %in% 65:69] = "age_65_70"
df_synth_pop$age_group[df_synth_pop$age %in% 70:74] = "age_70_75"
df_synth_pop$age_group[df_synth_pop$age %in% 75:79] = "age_75_80"
df_synth_pop$age_group[df_synth_pop$age %in% 80:84] = "age_80_85"
df_synth_pop$age_group[df_synth_pop$age %in% 85:89] = "age_85_90"
df_synth_pop$age_group[df_synth_pop$age %in% 90:94] = "age_90_95"
df_synth_pop$age_group[df_synth_pop$age %in% 95:105] = "age_over_95"

df_StratHousehold$prob = df_StratHousehold$child / df_StratHousehold$total

df_synth_pop$is_child = 0
for (neighb_code in unique(df_synth_pop$neighb_code)) {
  for (i in 1:nrow(df_StratHousehold)) {
    agents_neighbourhood = df_synth_pop[df_synth_pop$neighb_code == neighb_code & df_synth_pop$gender == df_StratHousehold[i, 'gender'] & df_synth_pop$age_group == df_StratHousehold[i, 'age_group'],]
    n_children = round(nrow(agents_neighbourhood)*df_StratHousehold[i, 'prob'])
    if (n_children > 0) {
      children = sample_n(agents_neighbourhood, n_children)
      df_synth_pop[df_synth_pop$agent_ID %in% children$agent_ID,]$is_child=1
    }
  }
}

df_synth_pop = subset(df_synth_pop, select=-c(age_group))

setwd(paste(this.path::this.dir(), "/synthetic-populations", sep = ""))
write.csv(df_synth_pop, 'synthetic_population_DHZW_2019.csv', row.names=FALSE)

################################################################################
## Generate current education based on group age, gender and migration
################################################################################

setwd(paste(this.path::this.dir(), "/synthetic-populations", sep = ""))
df_synth_pop = read.csv('synthetic_population_DHZW_2019_with_hh.csv')

# Load stratified dataset
setwd(paste(this.path::this.dir(), "/data/", municipality, "/stratified-datasets", sep = ""))
df_StratEduCurrent = read.csv("edu_current-71450NED-formatted.csv", sep = ",")

# Create groupages in the synthetic population to match the stratified dataset
df_synth_pop$age_group[df_synth_pop$age < 10] = NA
df_synth_pop$age_group[df_synth_pop$age %in% 10:14] = "age_10_15"
df_synth_pop$age_group[df_synth_pop$age %in% 15:19] = "age_15_20"
df_synth_pop$age_group[df_synth_pop$age %in% 20:24] = "age_20_25" 
df_synth_pop$age_group[df_synth_pop$age %in% 25:29] = "age_25_30" 
df_synth_pop$age_group[df_synth_pop$age %in% 30:34] = "age_30_35" 
df_synth_pop$age_group[df_synth_pop$age %in% 35:39] = "age_35_40" 
df_synth_pop$age_group[df_synth_pop$age %in% 40:44] = "age_40_45"
df_synth_pop$age_group[df_synth_pop$age %in% 45:49] = "age_45_50"
df_synth_pop$age_group[df_synth_pop$age >= 50] = "age_over_50"

df_synth_pop$current_education = ''
for (neighb_code in unique(df_synth_pop$neighb_code)) {
  for (i in 1:nrow(df_StratEduCurrent)) {
    agents_neighbourhood = df_synth_pop[df_synth_pop$neighb_code == neighb_code &
                                          df_synth_pop$gender == df_StratEduCurrent[i, 'gender'] &
                                          !is.na(df_synth_pop$age_group) &
                                          df_synth_pop$age_group == df_StratEduCurrent[i, 'age_group'] &
                                          df_synth_pop$migration_background == df_StratEduCurrent[i, 'migration_background'],]
    n_agents = nrow(agents_neighbourhood)
    
    n_agents_low_edu = round(n_agents*df_StratEduCurrent[i, 'prob_low'])
    n_agents_middle_edu = round(n_agents*df_StratEduCurrent[i, 'prob_middle'])
    n_agents_high_edu = round(n_agents*df_StratEduCurrent[i, 'prob_high'])
    n_agents_no_edu = n_agents-(n_agents_low_edu+n_agents_middle_edu+n_agents_high_edu)
    
    if (n_agents_low_edu > 0) {
      agents_unassigned = df_synth_pop[df_synth_pop$neighb_code == neighb_code &
                                         df_synth_pop$gender == df_StratEduCurrent[i, 'gender'] &
                                         !is.na(df_synth_pop$age_group) &
                                         df_synth_pop$age_group == df_StratEduCurrent[i, 'age_group'] &
                                         df_synth_pop$migration_background == df_StratEduCurrent[i, 'migration_background'] &
                                         df_synth_pop$current_education=='',]
      
      agents_low_edu = sample_n(agents_unassigned, n_agents_low_edu)
      df_synth_pop[df_synth_pop$agent_ID %in% agents_low_edu$agent_ID,]$current_education='low'
    }
    if (n_agents_middle_edu > 0) {
      agents_unassigned = df_synth_pop[df_synth_pop$neighb_code == neighb_code &
                                         df_synth_pop$gender == df_StratEduCurrent[i, 'gender'] &
                                         !is.na(df_synth_pop$age_group) &
                                         df_synth_pop$age_group == df_StratEduCurrent[i, 'age_group'] &
                                         df_synth_pop$migration_background == df_StratEduCurrent[i, 'migration_background'] &
                                         df_synth_pop$current_education=='',]
      
      agents_middle_edu = sample_n(agents_unassigned, n_agents_middle_edu)
      df_synth_pop[df_synth_pop$agent_ID %in% agents_middle_edu$agent_ID,]$current_education='middle'
    }
    if (n_agents_high_edu > 0) {
      agents_unassigned = df_synth_pop[df_synth_pop$neighb_code == neighb_code &
                                         df_synth_pop$gender == df_StratEduCurrent[i, 'gender'] &
                                         !is.na(df_synth_pop$age_group) &
                                         df_synth_pop$age_group == df_StratEduCurrent[i, 'age_group'] &
                                         df_synth_pop$migration_background == df_StratEduCurrent[i, 'migration_background'] &
                                         df_synth_pop$current_education=='',]
      
      agents_high_edu = sample_n(agents_unassigned, n_agents_high_edu)
      df_synth_pop[df_synth_pop$agent_ID %in% agents_high_edu$agent_ID,]$current_education='high'
    }
    if (n_agents_no_edu > 0) {
      agents_unassigned = df_synth_pop[df_synth_pop$neighb_code == neighb_code &
                                         df_synth_pop$gender == df_StratEduCurrent[i, 'gender'] &
                                         !is.na(df_synth_pop$age_group) &
                                         df_synth_pop$age_group == df_StratEduCurrent[i, 'age_group'] &
                                         df_synth_pop$migration_background == df_StratEduCurrent[i, 'migration_background'] &
                                         df_synth_pop$current_education=='',]
      
      agents_no_edu = sample_n(agents_unassigned, n_agents_no_edu)
      df_synth_pop[df_synth_pop$agent_ID %in% agents_no_edu$agent_ID,]$current_education='no_current_edu'
    }
  }
}

# fix education for young kids
df_synth_pop[df_synth_pop$age < 10 & df_synth_pop$age > 5,]$current_education = 'low'
df_synth_pop[df_synth_pop$age <= 5,]$current_education = 'no_current_edu'


################################################################################
# Generate education attainment: not finished yet
################################################################################

setwd(paste(this.path::this.dir(), "/synthetic-populations", sep = ""))
df_synth_pop = read.csv('synthetic_population_DHZW_2019_with_hh.csv')

df_synth_pop$edu_attainment = NA

# Firstly, generate part of the attribute based on the current education

# young people can only have low education attainment
df_synth_pop[df_synth_pop$age < 10,]$edu_attainment = 'nothing'

# if currently in low education, the attainment cannot be higher than that
df_synth_pop[df_synth_pop$current_education=='low',]$edu_attainment = 'nothing'

# if currently in middle education, the attainment cannot be higher than low
df_synth_pop[df_synth_pop$current_education=='middle',]$edu_attainment = 'low'

# if currently in high education and younger than 22 yo, the attainment cannot be higher than middle, because the agent is currently in a bachelors
df_synth_pop[df_synth_pop$current_education=='high' & df_synth_pop$age <= 22,]$edu_attainment = 'middle'

# if currently in high education and older than 22 yo, the attainment must be high, because the agent is currently in a masters and already completed a bachelors.
df_synth_pop[df_synth_pop$current_education=='high' & df_synth_pop$age > 22,]$edu_attainment = 'high'

# now, I have to assign the education attainment to the remaining agents, hence the ones that are currently not in school. I can only use the marginal distribution.

# Prepare table from the marginals
df_EduAttainment = df_marginal_dist[c('neighb_code', 'education_absolved_low',
                                      'education_absolved_middle',
                                      'education_absolved_high')]

# Update marginal figures removing the individuals I already generated the attainment education for
for(i in 1:nrow(df_EduAttainment)){
  df_EduAttainment[i,c("education_absolved_low")] = df_EduAttainment[i,c("education_absolved_low")] - nrow(df_synth_pop[df_synth_pop$edu_attainment == "low" & df_synth_pop$neighb_code == df_EduAttainment$neighb_code[i] & df_synth_pop$age >= 15,])
  df_EduAttainment[i,c("education_absolved_middle" )] = df_EduAttainment[i,c("education_absolved_middle" )]- nrow(df_synth_pop[df_synth_pop$edu_attainment == "middle" & df_synth_pop$neighb_code == df_EduAttainment$neighb_code[i] & df_synth_pop$age >= 15,])
  df_EduAttainment[i,c("education_absolved_high")] = df_EduAttainment[i,c("education_absolved_high")] - nrow(df_synth_pop[df_synth_pop$edu_attainment == "high" & df_synth_pop$neighb_code == df_EduAttainment$neighb_code[i] & df_synth_pop$age >= 15,])
}
df_EduAttainment$education_absolved_low[df_EduAttainment$education_absolved_low < 0] = 0
df_EduAttainment$education_absolved_middle[df_EduAttainment$education_absolved_middle < 0] = 0
df_EduAttainment$education_absolved_high[df_EduAttainment$education_absolved_high < 0] = 0

df_EduAttainment = as.data.frame(t(df_EduAttainment))
df_EduAttainment = df_EduAttainment %>%
  `colnames<-`(.[1, ]) %>%
  .[-1, ]

df_EduAttainment <- cbind(edu_attainment = rownames(df_EduAttainment), df_EduAttainment)
rownames(df_EduAttainment) <- 1:nrow(df_EduAttainment)
df_EduAttainment[df_EduAttainment$edu_attainment=='education_absolved_low',]$edu_attainment='low'
df_EduAttainment[df_EduAttainment$edu_attainment=='education_absolved_middle',]$edu_attainment='middle'
df_EduAttainment[df_EduAttainment$edu_attainment=='education_absolved_high',]$edu_attainment='high'

df_synth_pop[is.na(df_synth_pop$edu_attainment),]$edu_attainment = sample(x = df_EduAttainment$edu_attainment,
                                                                          size = nrow(df_synth_pop[is.na(df_synth_pop$edu_attainment),]),
                                                                          replace = TRUE,  
                                                                          prob = df_EduAttainment$df_synth_pop[is.na(df_synth_pop$edu_attainment),]$neighb_code
)
# Save synthetic population
setwd(paste(this.path::this.dir(), "/synthetic-populations", sep = ""))
#write.csv(df_synth_pop, 'synthetic_population_DHZW_2019_with_hh.csv', row.names=FALSE)
