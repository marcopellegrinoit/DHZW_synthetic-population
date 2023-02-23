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
    sep = '/'
  )
)
df_car_ownership = read.csv("car_ownership_NL_2015-formatted.csv")
df_car_ownership <- df_car_ownership[df_car_ownership$hh_type %in% unique(df_households$hh_type),]

################################################################################
# Validation with stratified dataset
df_car_ownership <- df_car_ownership %>%
  rename(with_car = percentage)

df_car_ownership$no_car <- 1-df_car_ownership$with_car

df_car_ownership <- df_car_ownership %>%
  pivot_longer(cols=c('with_car', 'no_car'), names_to = 'car_ownership', values_to = 'real')

df_gen = data.frame(table(df_households$hh_type, df_households$car_ownership))

colnames(df_gen) <- c('hh_type', 'car_ownership', 'pred')

df_gen$car_ownership <- as.character(df_gen$car_ownership)
df_gen[df_gen$car_ownership == '0',]$car_ownership <- 'no_car'
df_gen[df_gen$car_ownership == '1',]$car_ownership <- 'with_car'

df_car_ownership <- left_join(df_car_ownership, df_gen, by=c('hh_type', 'car_ownership'))

df_car_ownership <- df_car_ownership %>%
  pivot_longer(cols = c(real, pred), names_to = 'dataset', values_to = 'proportion')

# rename values
df_car_ownership$dataset <- recode(df_car_ownership$dataset,
                                       'real' = 'stratified dataset',
                                       'pred' = 'synthetic population')

setwd(this.dir())
setwd('data_comparison')
write.csv(df_car_ownership, 'car_ownership.csv', row.names = FALSE)
