library(ggplot2)
library(dplyr)
library(tidyr)
library("this.path")
setwd(this.path::this.dir())
source('../../src/utils-evaluation-synthetic-population.R')
source('../../config/config.R')

# Load datasets
setwd(
  paste(
    "../../data/processed",
    year,
    sep = '/'
  )
)
df_strat_car_license = read.csv("car_license-83488NED-formatted.csv", sep = ",")

# Load synthetic population
setwd(this.path::this.dir())
setwd(paste("../../output/synthetic-population-households/4_car_2022-12-26_15-50", sep = ""))
df_synth_pop = read.csv("synthetic_population_DHZW_2019.csv", sep = ",")

################################################################################
# With stratified dataset

# Create group ages to match the stratified dataset
df_synth_pop$age_group <- NA
df_synth_pop$age_group[df_synth_pop$age < 16] = NA
df_synth_pop$age_group[df_synth_pop$age %in% 16:17] = "age_16_17"
df_synth_pop$age_group[df_synth_pop$age %in% 18:19] = "age_18_19"
df_synth_pop$age_group[df_synth_pop$age %in% 20:24] = "age_20_24"
df_synth_pop$age_group[df_synth_pop$age %in% 25:29] = "age_25_29"
df_synth_pop$age_group[df_synth_pop$age %in% 30:39] = "age_30_39"
df_synth_pop$age_group[df_synth_pop$age %in% 40:49] = "age_40_49"
df_synth_pop$age_group[df_synth_pop$age %in% 50:59] = "age_50_59"
df_synth_pop$age_group[df_synth_pop$age %in% 60:64] = "age_60_64"
df_synth_pop$age_group[df_synth_pop$age %in% 65:69] = "age_65_69"
df_synth_pop$age_group[df_synth_pop$age %in% 70:74] = "age_70_74"
df_synth_pop$age_group[df_synth_pop$age >= 75] = "age_over_75"

df_strat_car_license <- df_strat_car_license %>%
  select(age_group, car)
df_strat_car_license$no_car <- 1-df_strat_car_license$car

colnames(df_strat_car_license) <- c('age_group', 'with_car_license', 'no_car_license')

df_strat_car_license <- df_strat_car_license %>%
  pivot_longer(cols=c('with_car_license', 'no_car_license'), names_to = 'car', values_to = 'real')

df_gen = data.frame(table(df_synth_pop$age_group, df_synth_pop$car_license))
colnames(df_gen) <- c('age_group', 'car', 'pred')

df_gen$car <- as.character(df_gen$car)
df_gen[df_gen$car == '0',]$car <- 'no_car_license'
df_gen[df_gen$car == '1',]$car <- 'with_car_license'

df_strat_car_license <- left_join(df_strat_car_license, df_gen, by=c('age_group', 'car'))

df_strat_car_license <- df_strat_car_license %>%
  pivot_longer(cols = c(real, pred), names_to = 'dataset', values_to = 'proportion')

# rename values
df_strat_car_license$dataset <- recode(df_strat_car_license$dataset,
                                  'real' = 'stratified dataset',
                                  'pred' = 'synthetic population')

setwd(this.dir())
setwd('data_comparison')
write.csv(df_strat_car_license, 'car_license.csv', row.names = FALSE)

df_strat_car_license <- df_strat_car_license %>%
  group_by(age_group, gender, is_child, dataset) %>%
  mutate(proportion = proportion / sum(proportion))

# bar plot
ggplot(df_strat_car_license, aes(age_group, proportion)) +
  facet_grid(vars(gender))+
  geom_bar(aes(fill = dataset),
           position = "dodge",
           stat = "identity",
           width=0.4)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Proportions of ischild per agegroup adn gender")+
  ylab("Individuals (%)")+
  xlab("Gender and age groups")+
  labs(fill = "Population")+
  theme(legend.title=element_text(size=20),
        legend.text=element_text(size=15))