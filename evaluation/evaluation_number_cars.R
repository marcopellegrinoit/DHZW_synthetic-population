library(ggplot2)
library(dplyr)
library(tidyr)
library("this.path")
setwd(this.path::this.dir())
source('../../config/config.R')

# Load synthetic households
setwd(this.path::this.dir())
setwd(paste("../../output/synthetic-population-households/4_car_2022-12-26_15-50", sep = ""))
df_households = read.csv("df_households_DHZW_2019.csv", sep = ",")

# Load car datasets
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
df_marginal_dist = read.csv("marginal_distributions_84583NED-formatted.csv", sep = ",")

################################################################################

df_cars <- df_marginal_dist %>%
  select(neighb_code, n_cars, n_cars_per_hh)

df_cars$hh_cars <- 0
df_cars$gen_cars <- 0
df_cars$hh_gen <- 0

for (i in 1:nrow(df_cars)) {
  df_cars[i,]$hh_gen <- nrow(df_households[df_households$neighb_code == df_cars[i,]$neighb_code,])
  df_cars[i,]$hh_cars <- nrow(df_households[df_households$neighb_code == df_cars[i,]$neighb_code &
                                              df_households$car_ownership == 1,])
  df_cars[i,]$gen_cars <- round(df_cars[i,]$hh_gen * df_cars[i,]$n_cars_per_hh)
}

colnames(df_cars) <- c('neighb_code', 'n cars [real]', '% cars [real]' , 'hh >=1 cars [gen]', 'hh [gen] * % cars [real]', 'hh [gen]')