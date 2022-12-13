library(readr)
library(dplyr)
library(this.path)

setwd(this.dir())
setwd('../../data/raw')

df <- read.csv('car_ownership_NL_2015.csv', sep=';')

df <- df %>%
  select(Huishoudkenmerken,
         Huishoudens.in.bezit.van.auto...Huishoudens.in.bezit.van.auto.....) %>%
  rename('hh_type' = 'Huishoudkenmerken',
         'percentage' = 'Huishoudens.in.bezit.van.auto...Huishoudens.in.bezit.van.auto.....')

df$hh_type <- recode(
  df$hh_type,
  'Type: Eenpersoonshuishouden' = 'single',
  'Type: Eenoudergezin' = 'single-parent',
  'Type: Paar, met kind(eren)' = 'couple with children',
  'Type: Paar, zonder kind' = 'couple without children',
  'Gestandaardiseerd inkomen: 1e 20%-groep' = '1 income group 20%',
  'Gestandaardiseerd inkomen: 2e 20%-groep' = '2 income group 20%',
  'Gestandaardiseerd inkomen: 3e 20%-groep' = '3 income group 20%',
  'Gestandaardiseerd inkomen: 4e 20%-groep' = '4 incomegroup 20%',
  'Gestandaardiseerd inkomen: 5e 20%-groep' = '5 income group 20%')

df$percentage <- as.numeric(gsub(",", ".", df$percentage))
df$percentage <- df$percentage/100

setwd(this.dir())
setwd('../../data/processed')

write.csv(df, 'car_ownership_NL_2015-formatted.csv')