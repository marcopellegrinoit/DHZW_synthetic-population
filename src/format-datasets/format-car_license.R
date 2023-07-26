library(readr)
library(dplyr)
library(this.path)
library(stringr)
library(tidyr)
setwd(this.path::this.dir())

# Load dataset
setwd(this.path::this.dir())
setwd("../../data/raw")
df <- read.csv('car_license-83488NED.csv', sep=';', fileEncoding = "UTF-8-BOM")

df <- df %>%
  select(Rijbewijscategorie,
         Leeftijd.rijbewijshouder,
         Personen.met.rijbewijs..aantal.) %>%
  rename('category' = 'Rijbewijscategorie',
         'age_group' = 'Leeftijd.rijbewijshouder',
         'n_people' = 'Personen.met.rijbewijs..aantal.')

df$category <- recode(
  df$category,
  'Autorijbewijs excl. aanhangerrijbewijs' = 'car',
  'Bromfietsrijbewijs' = 'moped')

df$age_group <- recode(
  df$age_group,
  '16 tot 18 jaar' = 'age_16_17',
  '18 tot 20 jaar' = 'age_18_19',
  '20 tot 25 jaar' = 'age_20_24',
  '25 tot 30 jaar' = 'age_25_29',
  '30 tot 40 jaar' = 'age_30_39',
  '40 tot 50 jaar' = 'age_40_49',
  '50 tot 60 jaar' = 'age_50_59',
  '60 tot 65 jaar' = 'age_60_64',
  '65 tot 70 jaar' = 'age_65_69',
  '70 tot 75 jaar' = 'age_70_74',
  '75 jaar of ouder' = 'age_over_75')

df <- df %>%
  pivot_wider(names_from = 'category', values_from = 'n_people')

# Add total number of inhabitants per age group

# Load dataset
setwd(this.path::this.dir())
setwd("../../data/processed")
df_inhabitants <- read.csv('inhabintants_south_holland-03759NED-formatted.csv')

df <- merge(df, df_inhabitants, by = 'age_group')

df$car <- df$car/df$n_people
df$moped <- df$moped/df$n_people


write.csv(df, 'car_license-83488NED-formatted.csv', row.names = FALSE)