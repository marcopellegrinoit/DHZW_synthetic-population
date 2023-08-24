################################################################################
#
# Purpose of script: generate a synthetic population of the DHZW area. Takes into account several CBS aggregated demographics distributions.
#
# Author: Marco Pellegrino
#
# Year: 2022
#
################################################################################

library(GenSynthPop)
library(dplyr)
library(readr)
library("this.path")
setwd(this.path::this.dir())
source('src/utils-synthetic-population.R')

################################################################################
# Load marginal distribution

setwd(this.path::this.dir())
setwd('data/processed/individuals')
df_marginal_dist = read.csv("marginal_distributions_84583NED-formatted.csv", sep = ",")

# how many individuals in total
sum(df_marginal_dist['tot_pop'])

################################################################################

# Initialise the synthetic population with the given number of agents per neighbourhood
df_synth_pop <-
  initialise_with_neighbourhood_codes(df_marginal_dist, 'neighb_code' , 'tot_pop')


################################################################################

# Assign the age group attribute by using the age group marginal distribution of each neighbourhood
group_ages <-
  c('age_0_15',
    'age_15_25',
    'age_25_45',
    'age_45_65',
    'age_over65')
df_synth_pop <-
  distribute_attribute_marginal(df_synth_pop,
                                df_marginal_dist,
                                'age_group',
                                group_ages,
                                'neighb_code',
                                'tot_pop')

################################################################################
# Assign precise age (integer)

# Load dataset
setwd(this.path::this.dir())
setwd('data/processed/individuals')
df_joint_gender = read.csv("gender_age-03759NED-formatted.csv", sep = ",")

# for each group age of the synthetic population, sample the age from stratified dataset following the the frequency distribution
# future improvement: instead of sampling, it could be better to actually partition, but the table management is non-trivial
df_synth_pop$age = ''
for (group_age in group_ages) {
  sample <- sample(
    x = df_joint_gender[df_joint_gender$age_group == group_age, ]$age,
    size = nrow(df_synth_pop[df_synth_pop$age_group == group_age, ]),
    replace = TRUE,
    prob = df_joint_gender[df_joint_gender$age_group == group_age, ]$group_propensity
  ) # sample from age frequency distribution
  
  df_synth_pop[df_synth_pop$age_group == group_age, ]$age = sample # apply to synthetic population dataset
}
df_synth_pop$age = as.numeric(df_synth_pop$age)

################################################################################
# Gender generation based on age

# Compute proportions within joint distribution
df_synth_pop = calc_propens_agents(
  dataframe = df_joint_gender,
  variable = "female",
  total_population = "total",
  agent_df = df_synth_pop,
  list_conditional_var = c("age")
)

# For each neighborhood, update the joint distribution using its marginal distribution. Then, assign the attribute
# Future work: rewrite the function with partitioning instead

df_synth_pop = distr_attr_strat_neigh_stats_binary(
  agent_df = df_synth_pop,
  neigh_df = df_marginal_dist,
  neigh_ID = "neighb_code",
  variable =  "gender",
  list_var_classes_neigh_df = c("gender_female", "gender_male"),
  list_agent_propens =  c("prop_female"),
  list_class_names = c("female", "male")
)

# Remove extra columns
df_synth_pop = subset(df_synth_pop, select = -c(prop_female, random_scores, age_group))

################################################################################
# Migration background generation based on age and gender

# Load joint distribution between age groups, gender, and migration background
setwd(this.path::this.dir())
setwd('data/processed/individuals')
df_joint_migration = read.csv("gender_age_migration-84910NED-formatted.csv", sep = ",")

# Categorise the age groups of the synthetic population into the corresponding age groups of the joint distribution
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

# Compute proportions within joint distribution
df_synth_pop = calc_propens_agents(df_joint_migration,
                                   "Dutch",
                                   "total",
                                   df_synth_pop,
                                   c("age_group", "gender"))
df_synth_pop = calc_propens_agents(df_joint_migration,
                                   "Western",
                                   "total",
                                   df_synth_pop,
                                   c("age_group", "gender"))
df_synth_pop = calc_propens_agents(df_joint_migration,
                                   "Non_Western",
                                   "total",
                                   df_synth_pop,
                                   c("age_group", "gender"))

# For each neighborhood, update the joint distribution using its marginal distribution. Then, assign the attribute with sampling.
# Future work: rewrite the function with partitioning instead
df_synth_pop = distr_attr_strat_neigh_stats_3plus(
  agent_df =  df_synth_pop,
  neigh_df =  df_marginal_dist,
  neigh_ID =  "neighb_code",
  variable =  "migration_background",
  list_var_classes_neigh_df =  c("migration_Dutch", "migration_west", "migration_non_west"),
  list_agent_propens =  c("prop_Dutch", "prop_Western", "prop_Non_Western"),
  list_class_names =  c("Dutch", "Western", "Non_Western")
)

# Remove extra columns
df_synth_pop = subset(
  df_synth_pop,
  select = -c(
    prop_Dutch,
    prop_Western,
    prop_Non_Western,
    random_scores,
    age_group
  )
)

################################################################################
# Generate attribute is_child (if the agent is a child living with parents)

# Load formatted stratified dataset about household position, gender and groupages (municipality aggregated)
setwd(this.path::this.dir())
setwd('data/processed/households')

df_joint_household = read.csv(
  "household_gender_age-71488NED-formatted.csv",
  sep = ",",
  fileEncoding = "UTF-8-BOM"
)

# Categorise the age groups of the synthetic population into the corresponding age groups of the joint distribution
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

# Compute proportions within the joint distribution
df_joint_household$prob_child = df_joint_household$child / df_joint_household$total

# Assign the attribute based on the joint distribution proportions
df_synth_pop <-
  distribute_attribute_joint_dist(
    df_synth_pop = df_synth_pop,
    df_joint = df_joint_household,
    new_attribute = 'is_child',
    attributes_to_match = c('age_group', 'gender'),
    values_new_attribute = c(1, 0),
    probabilities = c('prob_child')
  )

# Remove extra columns
df_synth_pop = subset(df_synth_pop, select = -c(age_group))

################################################################################
# Generate current education based on group age, gender and migration

# Load stratified dataset
setwd(this.path::this.dir())
setwd('data/processed/individuals')
df_joint_edu_current = read.csv("edu_current-71450NED-formatted.csv", sep = ",")

# Categorise the age groups of the synthetic population into the corresponding age groups of the joint distribution
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

# Assign the attribute based on the joint distribution proportions
df_synth_pop <-
  distribute_attribute_joint_dist(
    df_synth_pop = df_synth_pop,
    df_joint = df_joint_edu_current,
    new_attribute = 'current_education',
    attributes_to_match = c('age_group', 'gender', 'migration_background'),
    values_new_attribute = c('low', 'middle', 'high', 'no_current_edu'),
    probabilities = c('prob_low', 'prob_middle', 'prob_high')
  )

# fix education for young kids
df_synth_pop[df_synth_pop$age < 10 &
               df_synth_pop$age > 5, ]$current_education = 'low'
df_synth_pop[df_synth_pop$age <= 5, ]$current_education = 'no_current_edu'

# Remove age group column
df_synth_pop = subset(df_synth_pop, select = -c(age_group))

################################################################################
# Generate education attainment

df_synth_pop$edu_attainment = NA

# Firstly, generate part of the attribute based on the current education

# young people can only have low education attainment
df_synth_pop[df_synth_pop$age < 10, ]$edu_attainment = 'nothing'

# if currently in low education, the attainment cannot be higher than that
df_synth_pop[df_synth_pop$current_education == 'low', ]$edu_attainment = 'nothing'

# if currently in middle education, the attainment cannot be higher than low
df_synth_pop[df_synth_pop$current_education == 'middle', ]$edu_attainment = 'low'

# if currently in high education and younger than 22 yo, the attainment cannot be higher than middle, because the agent is currently in a bachelors
df_synth_pop[df_synth_pop$current_education == 'high' &
               df_synth_pop$age <= 22, ]$edu_attainment = 'middle'

# if currently in high education and older than 22 yo, the attainment must be high, because the agent is currently in a masters and already completed a bachelors.
df_synth_pop[df_synth_pop$current_education == 'high' &
               df_synth_pop$age > 22, ]$edu_attainment = 'high'

# now, I have to assign the education attainment to the remaining agents, hence the ones that are currently not in school. I can only use the marginal distribution.

# Prepare table from the marginals
df_edu_attainment = df_marginal_dist[c(
  'neighb_code',
  'education_absolved_low',
  'education_absolved_middle',
  'education_absolved_high'
)]

# Update marginal figures removing the individuals I already generated the attainment education for
for (i in 1:nrow(df_edu_attainment)) {
  df_edu_attainment[i, c("education_absolved_low")] = df_edu_attainment[i, c("education_absolved_low")] - nrow(df_synth_pop[df_synth_pop$edu_attainment == "low" &
                                                                                                                              df_synth_pop$neighb_code == df_edu_attainment$neighb_code[i] &
                                                                                                                              df_synth_pop$age >= 15, ])
  df_edu_attainment[i, c("education_absolved_middle")] = df_edu_attainment[i, c("education_absolved_middle")] - nrow(df_synth_pop[df_synth_pop$edu_attainment == "middle" &
                                                                                                                                    df_synth_pop$neighb_code == df_edu_attainment$neighb_code[i] &
                                                                                                                                    df_synth_pop$age >= 15, ])
  df_edu_attainment[i, c("education_absolved_high")] = df_edu_attainment[i, c("education_absolved_high")] - nrow(df_synth_pop[df_synth_pop$edu_attainment == "high" &
                                                                                                                                df_synth_pop$neighb_code == df_edu_attainment$neighb_code[i] &
                                                                                                                                df_synth_pop$age >= 15, ])
}
df_edu_attainment$education_absolved_low[df_edu_attainment$education_absolved_low < 0] = 0
df_edu_attainment$education_absolved_middle[df_edu_attainment$education_absolved_middle < 0] = 0
df_edu_attainment$education_absolved_high[df_edu_attainment$education_absolved_high < 0] = 0

df_edu_attainment = as.data.frame(t(df_edu_attainment))
df_edu_attainment = df_edu_attainment %>%
  `colnames<-`(.[1,]) %>%
  .[-1,]

df_edu_attainment <-
  cbind(edu_attainment = rownames(df_edu_attainment), df_edu_attainment)
rownames(df_edu_attainment) <- 1:nrow(df_edu_attainment)
df_edu_attainment[df_edu_attainment$edu_attainment == 'education_absolved_low', ]$edu_attainment =
  'low'
df_edu_attainment[df_edu_attainment$edu_attainment == 'education_absolved_middle', ]$edu_attainment =
  'middle'
df_edu_attainment[df_edu_attainment$edu_attainment == 'education_absolved_high', ]$edu_attainment =
  'high'

# For each neighbourhood, sample the education attainment of the unassigned agents using the its neighbourhood marginal distribution
# future improvement: instead of sampling, use a partion approach
df_synth_pop[is.na(df_synth_pop$edu_attainment), ]$edu_attainment = sample(
  x = df_edu_attainment$edu_attainment,
  size = nrow(df_synth_pop[is.na(df_synth_pop$edu_attainment), ]),
  replace = TRUE,
  prob = df_edu_attainment$df_synth_pop[is.na(df_synth_pop$edu_attainment), ]$neighb_code
)

################################################################################
# Car and moped license ownership

# Load stratified dataset over age groups
setwd(this.path::this.dir())
setwd('data/processed')
df_joint_car_license = read.csv("car_license-83488NED-formatted.csv", sep = ",")

# Categorise the age groups of the synthetic population into the corresponding age groups of the joint distribution
df_synth_pop$age_group <- NA
df_synth_pop$age_group[df_synth_pop$age %in% 16:17] = "age_16_17"
df_synth_pop$age_group[df_synth_pop$age %in% 18:19] = "age_18_19"
df_synth_pop$age_group[df_synth_pop$age %in% 20:24] = "age_20_24"
df_synth_pop$age_group[df_synth_pop$age %in% 25:29] = "age_25_29"
df_synth_pop$age_group[df_synth_pop$age %in% 30:39] = "age_30_39"
df_synth_pop$age_group[df_synth_pop$age %in% 40:49] = "age_40_49"
df_synth_pop$age_group[df_synth_pop$age %in% 50:59] = "age_50_59"
df_synth_pop$age_group[df_synth_pop$age %in% 60:64] = "age_60_64"
df_synth_pop$age_group[df_synth_pop$age %in% 65:69] = "age_65_69"
df_synth_pop$age_group[df_synth_pop$age %in% 70:74] = "age_70_74"
df_synth_pop$age_group[df_synth_pop$age >= 75] = "age_over_75"

# Assign the car license attribute based on the joint distribution proportions
df_synth_pop <-
  distribute_attribute_joint_dist(
    df_synth_pop = df_synth_pop,
    df_joint = df_joint_car_license,
    new_attribute = 'car_license',
    attributes_to_match = c('age_group'),
    values_new_attribute = c(1, 0),
    probabilities = c('car')
  )

# Agents younger than 16 years old cannot have a license
df_synth_pop[df_synth_pop$age < 16,]$car_license <- 0

# Assign the moped license attribute based on the joint distribution proportions
df_synth_pop <-
  distribute_attribute_joint_dist(
    df_synth_pop = df_synth_pop,
    df_joint = df_joint_car_license,
    new_attribute = 'moped_license',
    attributes_to_match = c('age_group'),
    values_new_attribute = c(1, 0),
    probabilities = c('moped')
  )

# Agents younger than 16 years old cannot have a license
df_synth_pop[df_synth_pop$age < 16,]$moped_license <- 0

# Remove age group column
df_synth_pop = subset(df_synth_pop, select = -c(age_group))
  
################################################################################
# Save the output synthetic population
setwd(this.path::this.dir())
setwd('output/synthetic-population')
write.csv(df_synth_pop, 'synthetic_population_DHZW_2019.csv', row.names = FALSE)