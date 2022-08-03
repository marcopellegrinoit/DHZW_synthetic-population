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
df_StratMigration = read.csv("gender_age_migration-84910NED-formatted.csv", sep = ",") # count of people per lifeyear and gender in all of Amsterdam

setwd(paste(this.path::this.dir(), "/data/", municipality, "/synthetic-populations", sep = ""))
df_SynthPop = read.csv("synthetic_population.csv", sep = ",")

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
R2_GenderMarginals = R_squared(df_GenderMarginalsValid$real, df_GenderMarginalsValid$pred)

# Validation with stratified dataset
df_GenderStratValid = validation(df_real_distr = df_StratGender,
                                 df_synt_pop = df_SynthPop,
                                 join_var = "age",
                                 list_real_df_var = c("male", "female"), 
                                 var_pred_df = "gender",
                                 list_values = c("male", "female"),
                                 age_limits = FALSE
)
R2_GenderStrat = R_squared(df_GenderStratValid$real, df_GenderStratValid$pred)

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
R2_MigratMarginals = R_squared(df_MigratMarginals$real, df_MigratMarginals$pred)

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
R2_MigratGenderAge = R_squared(df_MigratGenderAge$real, df_MigratGenderAge$pred)

# Validation with stratified dataset: age_group
df_MigratAge = df_MigratGenderAge %>%
  select(age_group, migration_background, real) %>%
  group_by(age_group, migration_background) %>%
  summarise(real = sum(real))

for (i in (1:nrow(df_MigratAge))) {
  df_MigratAge[i, 'pred',] = nrow(df_SynthPop[df_SynthPop$age_group == df_MigratAge[i, 'age_group']$age_group &
                                              df_SynthPop$migration_background == df_MigratAge[i, 'migration_background']$migration_background,])
}
R2_MigratAge = R_squared(df_MigratAge$real, df_MigratAge$pred)

# Validation with stratified dataset: gender
df_MigratGender = df_MigratGenderAge %>%
  select(gender, migration_background, real) %>%
  group_by(gender, migration_background) %>%
  summarise(real = sum(real))

for (i in (1:nrow(df_MigratGender))) {
  df_MigratGender[i, 'pred',] = nrow(df_SynthPop[df_SynthPop$gender == df_MigratGender[i, 'gender']$gender &
                                                df_SynthPop$migration_background == df_MigratGender[i, 'migration_background']$migration_background,])
}
R2_MigratGender = R_squared(df_MigratGender$real, df_MigratGender$pred)

################################################################################
# Current education
################################################################################



