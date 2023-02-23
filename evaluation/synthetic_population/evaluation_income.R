library(ggplot2)
library(dplyr)
library(tidyr)
library("this.path")
setwd(this.path::this.dir())
source('../../src/utils-evaluation-synthetic-population.R')
source('../../config/config.R')

# Load datasets


# Load synthetic population
setwd(this.path::this.dir())
setwd(paste("../../output/synthetic-population-households/4_car_2022-12-26_15-50", sep = ""))
df_households = read.csv("df_households_DHZW_2019.csv", sep = ",")

setwd(this.path::this.dir())
setwd(
  paste(
    "../../data/processed",
    year,
    municipality,
    'households',
    sep = '/'
  )
)
df_strat_income = read.csv("household_income_85064NED-formatted.csv")
df_strat_income <- df_strat_income[df_strat_income$type %in% unique(df_households$hh_type),]

################################################################################
# Validation with stratified dataset

df_strat_income = df_strat_income %>%
  rename(hh_type = type) %>%
  pivot_longer(cols = -c(hh_type), names_to = "income_group", values_to = "real")

df_gen = data.frame(table(df_households$hh_type, df_households$income_group))
colnames(df_gen) <- c('hh_type', 'income_group', 'pred')

df_strat_income <- left_join(df_strat_income, df_gen, by=c('hh_type', 'income_group'))


df_strat_income <- df_strat_income %>%
  pivot_longer(cols = c(real, pred), names_to = 'dataset', values_to = 'proportion')

# rename values
df_strat_income$dataset <- recode(df_strat_income$dataset,
                                   'real' = 'stratified dataset',
                                   'pred' = 'synthetic population')

setwd(this.dir())
setwd('data_comparison')
write.csv(df_strat_income, 'income_group.csv', row.names = FALSE)
