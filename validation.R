library(GenSynthPop)
library(dplyr)
library(tidyr)

library("this.path")
setwd(this.path::this.dir())
source('utils.R')

municipality = "den_haag_2019"

setwd(paste(this.path::this.dir(), "/data/", municipality, sep = ""))
df_MarginalDistr = read.csv("marginal_distributions_84583NED-formatted.csv", sep = ",")

setwd(paste(this.path::this.dir(), "/data/", municipality, "/stratified-datasets", sep = ""))
df_StratGender = read.csv("gender_age-03759NED-formatted.csv", sep = ",")
df_StratMigration = read.csv("gender_age_migration-84910NED-formatted.csv", sep = ",")
df_StratEduCurrent = read.csv("edu_current-71450NED-formatted.csv", sep = ",")

#setwd(paste(this.path::this.dir(), "/data/", municipality, "/households/distributions", sep = ""))
#df_StratHousehold = read.csv("household_gender_age-71488NED-formatted.csv", sep = ",", fileEncoding="UTF-8-BOM")

setwd(paste(this.path::this.dir(), "/synthetic-populations", sep = ""))
df_synth_pop = read.csv("synthetic_population_DHZW_2019_with_hh.csv", sep = ",")

# filter DHZW area
setwd(paste(this.path::this.dir(), "/data", sep = ""))
DHZW_neighborhood_codes <- read.csv("DHZW_neighbourhoods_codes.csv", sep = ";" ,header=F)$V1

df_MarginalDistr = df_MarginalDistr[df_MarginalDistr$neighb_code %in% DHZW_neighborhood_codes,]

################################################################################
# Gender
################################################################################

# Validation with marginal distribution
df_GenderMarginalsValid = validation(df_real_distr = df_MarginalDistr,
                                 df_synt_pop = df_SynthPop,
                                 join_var = "neighb_code",
                                 list_real_df_var = c("gender_male", "gender_female"), 
                                 var_pred_df = "gender",
                                 list_values = c("male", "female"),
                                 age_limits = FALSE
)
R2_GenderMarginals = R_squared_manual(df_GenderMarginalsValid$real, df_GenderMarginalsValid$pred)

# Validation with stratified dataset
df_GenderStratValid = validation(df_real_distr = df_StratGender,
                                 df_synt_pop = df_SynthPop,
                                 join_var = "age",
                                 list_real_df_var = c("male", "female"), 
                                 var_pred_df = "gender",
                                 list_values = c("male", "female"),
                                 age_limits = FALSE
)
R2_GenderStrat = R_squared_manual(df_GenderStratValid$real, df_GenderStratValid$pred)

################################################################################
# Migration background
################################################################################

## Validation with marginal distribution
df_MigratMarginals = validation(df_real_distr = df_MarginalDistr,
                                     df_synt_pop = df_SynthPop,
                                     join_var = "neighb_code",
                                     list_real_df_var = c("migration_Dutch", "migration_west", "migration_non_west"), 
                                     var_pred_df = "migration_background",
                                     list_values = c("Dutch", "Western", "Non_Western"),
                                     age_limits = FALSE
)
R2_MigratMarginals = R_squared_manual(df_MigratMarginals$real, df_MigratMarginals$pred)

## Validation with stratified dataset: age_group and gender
df_MigratGenderAge = df_StratMigration %>%
  select(gender, age_group, Dutch, Non_Western, Western) %>%
  pivot_longer(cols = -c(gender, age_group), names_to = "migration_background", values_to = "real")
df_MigratGenderAge$pred = 0

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

for (i in (1:nrow(df_MigratGenderAge))) {
  df_MigratGenderAge[i, 'pred',] = nrow(df_SynthPop[df_SynthPop$age_group == df_MigratGenderAge[i, 'age_group']$age_group &
                                                    df_SynthPop$gender == df_MigratGenderAge[i, 'gender']$gender &
                                                    df_SynthPop$migration_background == df_MigratGenderAge[i, 'migration_background']$migration_background,])
}
R2_MigratGenderAge = R_squared_manual(df_MigratGenderAge$real, df_MigratGenderAge$pred)
df_SynthPop = subset(df_SynthPop, select=-c(age_group))

################################################################################
# Current education
################################################################################

df_StratEduCurrentValidation = df_StratEduCurrent %>%
  select(gender, age_group, migration_background, low, middle, high, no_current_edu) %>%
  pivot_longer(cols = -c(gender, age_group, migration_background), names_to = "current_education", values_to = "real")
df_StratEduCurrentValidation$pred = 0

df_synth_pop$age_group[df_synth_pop$age < 15] = NA
df_synth_pop$age_group[df_synth_pop$age %in% 10:14] = "age_10_15"
df_synth_pop$age_group[df_synth_pop$age %in% 15:19] = "age_15_20"
df_synth_pop$age_group[df_synth_pop$age %in% 20:24] = "age_20_25" 
df_synth_pop$age_group[df_synth_pop$age %in% 25:29] = "age_25_30" 
df_synth_pop$age_group[df_synth_pop$age %in% 30:34] = "age_30_35" 
df_synth_pop$age_group[df_synth_pop$age %in% 35:39] = "age_35_40" 
df_synth_pop$age_group[df_synth_pop$age %in% 40:44] = "age_40_45"
df_synth_pop$age_group[df_synth_pop$age %in% 45:49] = "age_45_50"
df_synth_pop$age_group[df_synth_pop$age >= 50] = "age_over_50"

for (i in (1:nrow(df_StratEduCurrentValidation))) {
  df_StratEduCurrentValidation[i, 'pred',] = nrow(df_synth_pop[!is.na(df_synth_pop$age_group) &
                                                               df_synth_pop$age_group == df_StratEduCurrentValidation[i, 'age_group']$age_group &
                                                               df_synth_pop$gender == df_StratEduCurrentValidation[i, 'gender']$gender &
                                                               df_synth_pop$migration_background == df_StratEduCurrentValidation[i, 'migration_background']$migration_background &
                                                               df_synth_pop$current_education == df_StratEduCurrentValidation[i, 'current_education']$current_education,])
}
R2_EduCurrent = R_squared_manual(df_StratEduCurrentValidation$real, df_StratEduCurrentValidation$pred)


df_synth_pop = subset(df_synth_pop, select=-c(age_group))

################################################################################
# Education attainment
################################################################################

df_EduAttainmentMarginals = validation(df_real_distr = df_MarginalDistr,
                                       df_synt_pop = df_synth_pop,
                                       join_var = "neighb_code",
                                       list_real_df_var = c("education_absolved_low", "education_absolved_middle", "education_absolved_high"),
                                       var_pred_df = "edu_attainment",
                                       list_values = c("low", "middle", "high"),
                                       age_limits = TRUE
)
R2_EduAttainment = R_squared_manual(df_EduAttainmentMarginals$real, df_EduAttainmentMarginals$pred)