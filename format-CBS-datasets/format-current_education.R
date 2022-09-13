library(GenSynthPop)
library(dplyr)

library("this.path")

setwd(this.path::this.dir())
source('../utils.R')

municipality = "den_haag_2019"

# Note: to know the total for each group, I use the migration background stratified dataset (which need to be formatted first).

# Load age conversion codes
setwd(this.path::this.dir())
setwd("../data/codes")
codes_ages_education = read.csv("codes_ages_current_education.csv", fileEncoding="UTF-8-BOM", sep=";")
codes_education = read.csv("codes_education.csv", sep=';', fileEncoding="UTF-8-BOM")

setwd(this.path::this.dir())
setwd(paste("../data/", municipality, "/stratified-datasets", sep = ""))
df_StratEduCurrent = read.csv("edu_current-71450NED.csv", sep = ";")
df_StratMigration = read.csv("gender_age_migration-84910NED-formatted.csv", sep = ",")

df_StratEduCurrent = df_StratEduCurrent %>%
  select(Geslacht,
         Leeftijd,
         Migratieachtergrond,
         Onderwijssoort,
         LeerlingenStudenten_1
         
  ) %>%
  rename(gender = Geslacht,
         age_code = Leeftijd,
         migration_background_code = Migratieachtergrond,
         education_code = Onderwijssoort,
         n_people = LeerlingenStudenten_1
  )

df_StratEduCurrent = refactor_gender(df_StratEduCurrent)
df_StratEduCurrent = refactor_ages_education(df_StratEduCurrent, codes_ages_education)
df_StratEduCurrent = refactor_migration(df_StratEduCurrent)
df_StratEduCurrent = refactor_education(df_StratEduCurrent, codes_education)

df_StratEduCurrent = df_StratEduCurrent[df_StratEduCurrent$education_level!='',]

# Create new group ages in the stratified dataset to match the migration background stratified
#df_StratEduCurrent = df_StratEduCurrent[!(df_StratEduCurrent$age_group_education %in% 12:14),]
df_StratEduCurrent$age_group[df_StratEduCurrent$age_group_education %in% 10:14] = "age_10_15"
df_StratEduCurrent$age_group[df_StratEduCurrent$age_group_education %in% 15:19] = "age_15_20"
df_StratEduCurrent$age_group[df_StratEduCurrent$age_group_education %in% 20:24] = "age_20_25" 
df_StratEduCurrent$age_group[df_StratEduCurrent$age_group_education %in% 25:29] = "age_25_30" 
df_StratEduCurrent$age_group[df_StratEduCurrent$age_group_education == 'age_30_35'] = "age_30_35" 
df_StratEduCurrent$age_group[df_StratEduCurrent$age_group_education == 'age_35_40'] = "age_35_40" 
df_StratEduCurrent$age_group[df_StratEduCurrent$age_group_education == 'age_40_45'] = "age_40_45"
df_StratEduCurrent$age_group[df_StratEduCurrent$age_group_education == 'age_45_50'] = "age_45_50"
df_StratEduCurrent$age_group[df_StratEduCurrent$age_group_education == 'age_over_50'] = "age_over_50"


# Group the young integer ages into groupages, to match the other dataset groupages
df_StratEduCurrent = df_StratEduCurrent %>%
  group_by(gender, age_group, migration_background, education_level) %>%
  summarise(n_people = sum(n_people))

# Transform the education level column into columns for each value
df_StratEduCurrent = df_StratEduCurrent %>%
  pivot_wider(names_from = 'education_level', values_from = 'n_people')

# Format the migration background stratified to match the same groupage
df_StratMigration$age_group[df_StratMigration$age_group=='age_50_55' |
                              df_StratMigration$age_group=='age_55_60' |
                              df_StratMigration$age_group=='age_60_65' |
                              df_StratMigration$age_group=='age_65_70' |
                              df_StratMigration$age_group=='age_70_75' |
                              df_StratMigration$age_group=='age_75_80' |
                              df_StratMigration$age_group=='age_80_85' |
                              df_StratMigration$age_group=='age_85_90' |
                              df_StratMigration$age_group=='age_90_95' |
                              df_StratMigration$age_group=='age_over_95'] = "age_over_50" 

# Transform the migration background column into columns for each value
df_StratMigration = df_StratMigration %>%
  select(age_group, gender, Dutch, Western, Non_Western) %>%
  pivot_longer(cols=c('Dutch', 'Western', 'Non_Western'), names_to = 'migration_background', values_to = 'n_people')

# Group old groupages together, to match the other dataset groupages
df_StratMigration = df_StratMigration %>%
  group_by(gender, age_group, migration_background) %>%
  summarise(n_people = sum(n_people))

# merge the migration background dataset, so  that I can get the totals for each group combination
df_StratEduCurrent = merge(df_StratEduCurrent, df_StratMigration, by=c('age_group', 'gender', 'migration_background'))

df_StratEduCurrent = df_StratEduCurrent %>%
  rename(total = n_people)

# Calculate the people that are not currently going to school
df_StratEduCurrent$no_current_edu = df_StratEduCurrent$total - (df_StratEduCurrent$low + df_StratEduCurrent$middle + df_StratEduCurrent$high)

# Calculate probabilities
df_StratEduCurrent$prob_low = df_StratEduCurrent$low/df_StratEduCurrent$total
df_StratEduCurrent$prob_middle = df_StratEduCurrent$middle/df_StratEduCurrent$total
df_StratEduCurrent$prob_high = df_StratEduCurrent$high/df_StratEduCurrent$total

# Save formatted dataset
write.csv(df_StratEduCurrent, 'edu_current-71450NED-formatted.csv', row.names=FALSE)