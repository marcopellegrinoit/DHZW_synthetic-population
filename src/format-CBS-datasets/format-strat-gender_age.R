library(GenSynthPop)
library(dplyr)
library("this.path")
setwd(this.path::this.dir())
source('../../config/config.R')
source('../utils-synthetic-population.R')

# Load age conversion codes
# Load age conversion codes
setwd(this.path::this.dir())
setwd("../../data/codes")
codes_age = read.csv("codes_age.csv", sep=",")

# Load dataset
setwd(this.path::this.dir())
setwd(
  paste(
    "../../data/raw",
    year,
    municipality,
    'individuals_demographics',
    sep = '/'
  )
)
df_StratGender = read.csv("gender_age-03759NED.csv", sep = ";") # count of people per lifeyear and gender in all of Amsterdam

# Select and translate useful attributes
df_StratGender = df_StratGender %>%
  select(Geslacht,
         Leeftijd,
         BevolkingOp1Januari_1
  ) %>%
  rename(gender = Geslacht,
         age_code = Leeftijd,
         n_people = BevolkingOp1Januari_1)

# Refactor gender, age
df_StratGender = refactor_gender(df_StratGender)
df_StratGender = refactor_age(df_StratGender, codes_age)
df_StratGender = df_StratGender %>% filter(!is.na(as.numeric(age))) # filter only numeric ages. Anyway, the highest age is already 104
df_StratGender$age = as.numeric(df_StratGender$age)

# Reformat stratified dataset, transforming the gender column into a column for each value
df_StratGender = restructure_one_var_marginal(df = df_StratGender,
                                              variable = 'gender',
                                              countsname = 'n_people')

# create group ages in the stratified dataset
df_StratGender$age_group = "age_over65" # default for non-numeric
df_StratGender$age_group[df_StratGender$age %in% 0:14] = "age_0_15"
df_StratGender$age_group[df_StratGender$age %in% 15:24] = "age_15_25"
df_StratGender$age_group[df_StratGender$age %in% 25:44] = "age_25_45"
df_StratGender$age_group[df_StratGender$age %in% 45:64] = "age_45_65"
df_StratGender$age_group[df_StratGender$age %in% 65:105] = "age_over65"

# for each individual age, calculate its proportion to the total of each age group
df_StratGender = df_StratGender %>%
  group_by(age_group) %>%
  mutate(group_propensity = total/sum(total))

setwd(this.path::this.dir())
setwd(
  paste(
    "../../data/processed",
    year,
    municipality,
    'individuals_demographics',
    sep = '/'
  )
)
write.csv(df_StratGender, 'gender_age-03759NED-formatted.csv', row.names=FALSE)