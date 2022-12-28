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
  'Gestandaardiseerd inkomen: 4e 20%-groep' = '4 income group 20%',
  'Gestandaardiseerd inkomen: 5e 20%-groep' = '5 income group 20%')

df$percentage <- as.numeric(gsub(",", ".", df$percentage))
df$percentage <- df$percentage/100

# save dataset
setwd(this.dir())
setwd('../../data/processed')

write.csv(df_combined, 'car_ownership_NL_2015-formatted.csv', row.names = FALSE)

# not used

df_combined <- expand.grid(c('single', 'single-parent', 'couple with children', 'couple without children'),
            c('1 income group 20%', '2 income group 20%', '3 income group 20%', '4 income group 20%', '5 income group 20%'))
colnames(df_combined) <- c('hh_type', 'hh_income')

df_combined <- merge(df_combined, df, by.x = 'hh_type', by.y = 'hh_type')
df_combined <- df_combined %>%
  rename('percentage_hh_type' = 'percentage')
df_combined$percentage_hh_type <- df_combined$percentage_hh_type * (1/5)
df_combined <- merge(df_combined, df, by.x = 'hh_income', by.y = 'hh_type')
df_combined <- df_combined %>%
  rename('percentage_hh_income' = 'percentage')
df_combined$percentage_hh_income <- df_combined$percentage_hh_income * (1/4)

df_combined$percentage_combined <-  df_combined$percentage_hh_type + df_combined$percentage_hh_income