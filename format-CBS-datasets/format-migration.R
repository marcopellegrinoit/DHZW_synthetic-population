library(GenSynthPop)
library(dplyr)

library("this.path")

setwd(this.path::this.dir())
source('../utils.R')

municipality = "den_haag_2019"

# Load age conversion codes
setwd(this.path::this.dir())
setwd(paste("../data/codes", sep = ""))
codes_agegroup20 = read.csv("codes_agegroup20.csv", fileEncoding="UTF-8-BOM")

# Load dataset
setwd(this.path::this.dir())
setwd(paste("../data/", municipality, "/stratified-datasets", sep = ""))
df_StratMigration = read.csv("gender_age_migration-84910NED.csv", sep = ";")

# Select interesting attributes
df_StratMigration = df_StratMigration %>%
  select(Geslacht,
         Leeftijd,
         Migratieachtergrond,
         BevolkingOp1Januari_1
  ) %>%
  rename(gender = Geslacht,
         age_group_20_code = Leeftijd,
         migration_background_code = Migratieachtergrond,
         n_people = BevolkingOp1Januari_1
  )
df_StratMigration[is.na(df_StratMigration)] <- 0

# Refactor group age, gender and migration background
df_StratMigration = refactor_age_group_20(df_StratMigration, codes_agegroup20)
df_StratMigration = refactor_gender(df_StratMigration)
df_StratMigration = refactor_migration(df_StratMigration)

# Reformat stratified dataset, transforming the migration background column into a column for each value
df_StratMigration = restructure_one_var_marginal(df = df_StratMigration,
                                                 variable = 'migration_background',
                                                 countsname = 'n_people')

df_StratMigration = df_StratMigration %>%
  rename(age_group = age_group_20)

write.csv(df_StratMigration, 'gender_age_migration-84910NED-formatted.csv', row.names=FALSE)