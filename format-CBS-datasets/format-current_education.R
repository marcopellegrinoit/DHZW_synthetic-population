library(GenSynthPop)
library(dplyr)

library("this.path")

setwd(this.path::this.dir())
source('../utils.R')

municipality = "den_haag_2019"

# Load age conversion codes
setwd(this.path::this.dir())
setwd("../data/codes")
codes_ages_education = read.csv("codes_ages_education.csv", fileEncoding="UTF-8-BOM", sep=";")
codes_education = read.csv("codes_education.csv", sep=';', fileEncoding="UTF-8-BOM")

setwd(this.path::this.dir())
setwd(paste("../data/", municipality, "/stratified-datasets", sep = ""))
df_StratEduCurrent = read.csv("edu_current-71450NED.csv", sep = ";")

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


write.csv(df_StratEduCurrent, 'edu_current-71450NED-formatted.csv', row.names=FALSE)