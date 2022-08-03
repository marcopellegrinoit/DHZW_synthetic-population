library(GenSynthPop)
library(dplyr)

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

# Validation with marginal distribution
df_MigratMarginals = validation(df_real_distr = df_MarginalDistr,
                                     df_synt_pop = df_SynthPop,
                                     join_var = "neighb_code",
                                     list_real_df_var = c("migration_Dutch", "migration_west", "migration_non_west"), 
                                     var_pred_df = "migration_background",
                                     list_values = c("Dutch", "Western", "Non_Western"),
                                     age_limits = FALSE
)
R2_MigratMarginals = R_squared(df_MigratMarginals$real, df_MigratMarginals$pred)

# Validation with stratified dataset: age_group and gender
df_MigratGender = validation(df_real_distr = df_StratMigration,
                             df_synt_pop = df_SynthPop,
                             join_var = c("age_group", "gender"),
                             list_real_df_var = c("Dutch", "Western", "Non_Western"), 
                             var_pred_df = "migration_background",
                             list_values = c("Dutch", "Western", "Non_Western"),
                             age_limits = FALSE
)
R2_MigratGender = R_squared(df_MigratGender$real, df_MigratGender$pred)

# Validation with stratified dataset: age_group
df_MigratAge = validation(df_real_distr = df_StratMigration,
                                 df_synt_pop = df_SynthPop,
                                 join_var = "age_group",
                                 list_real_df_var = c("Dutch", "Western", "Non_Western"), 
                                 var_pred_df = "migration_background",
                                 list_values = c("Dutch", "Western", "Non_Western"),
                                 age_limits = FALSE
)
R2_MigratAge = R_squared(df_MigratAge$real, df_MigratAge$pred)

# Validation with stratified dataset: gender
df_MigratGender = validation(df_real_distr = df_StratMigration,
                                 df_synt_pop = df_SynthPop,
                                 join_var = "gender",
                                 list_real_df_var = c("Dutch", "Western", "Non_Western"), 
                                 var_pred_df = "migration_background",
                                 list_values = c("Dutch", "Western", "Non_Western"),
                                 age_limits = FALSE
)
R2_MigratGender = R_squared(df_MigratGender$real, df_MigratGender$pred)

################################################################################
# Current education
################################################################################
