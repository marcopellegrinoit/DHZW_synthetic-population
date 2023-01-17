library(ggplot2)
library(dplyr)
library(tidyr)
library("this.path")
setwd(this.path::this.dir())
source('../src/utils-evaluation-synthetic-population.R')
source('../config/config.R')

# Load datasets
setwd(
  paste(
    "../data/processed",
    year,
    municipality,
    'individuals_demographics',
    sep = '/'
  )
)
df_marginal_dist = read.csv("marginal_distributions_84583NED-formatted.csv", sep = ",")
df_gender_strat = read.csv("gender_age-03759NED-formatted.csv", sep = ",")
df_migration_strat = read.csv("gender_age_migration-84910NED-formatted.csv", sep = ",")
df_edu_current_strat = read.csv("edu_current-71450NED-formatted.csv", sep = ",")

setwd(this.path::this.dir())
setwd(paste("../output/synthetic-population-households/4_car_2022-12-26_15-50", sep = ""))
df_synth_pop = read.csv("synthetic_population_DHZW_2019.csv", sep = ",")

################################################################################
# Gender
################################################################################

# With marginal distribution
df_gender_marginal <- get_proportions_over_marginal(df_marginal_dist = df_marginal_dist,
                                          df_synth_pop = df_synth_pop,
                                          aggregation_var = neighb_code,
                                          cols_marginal = c(gender_male, gender_female),
                                          var_str = 'gender',
                                          values = c('male', 'female')
)
ggplot(df_gender_marginal, aes(neighb_code, proportion)) +
  facet_grid(vars(gender))+
  geom_bar(aes(fill = dataset),
           position = "dodge",
           stat = "identity",
           width=0.4)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Proportions of gender per neighbourhood")+
  ylab("Individuals (%)")+
  xlab("Neighbourhood code")+
  labs(fill = "Population")+
  theme(legend.title=element_text(size=20),
        legend.text=element_text(size=15))

################################################################################
# Validation with stratified dataset
df_gender_strat = df_gender_strat %>%
  select(age, male, female) %>%
  pivot_longer(cols = -c(age), names_to = 'gender', values_to = "real")
df_gender_strat$pred = 0

for (i in (1:nrow(df_gender_strat))) {
  df_gender_strat[i, 'pred',] = nrow(df_synth_pop[df_synth_pop$age == df_gender_strat[i, 'age']$age &
                                                       df_synth_pop$gender == df_gender_strat[i, 'gender']$gender,])
}
df_gender_strat <- df_gender_strat %>%
  pivot_longer(cols = c(real, pred), names_to = 'dataset', values_to = 'proportion')

# rename values
df_gender_strat$dataset <- recode(df_gender_strat$dataset,
                                     'real' = 'stratified dataset',
                                     'pred' = 'synthetic population')

df_gender_strat <- df_gender_strat %>%
  group_by(age, dataset) %>%
  mutate(proportion = proportion / sum(proportion))

# bar plot
ggplot(df_gender_strat, aes(age, proportion)) +
  facet_grid(vars(gender))+
  geom_bar(aes(fill = dataset),
           position = "dodge",
           stat = "identity",
           width=0.4)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Proportions of gender per age")+
  ylab("Individuals (%)")+
  xlab("Gender and age groups")+
  labs(fill = "Population")+
  theme(legend.title=element_text(size=20),
        legend.text=element_text(size=15))

################################################################################
# Migration background
################################################################################

# With marginal distribution
df_migration_marginal <- get_proportions_over_marginal(df_marginal_dist = df_marginal_dist,
                                                    df_synth_pop = df_synth_pop,
                                                    aggregation_var = neighb_code,
                                                    cols_marginal = c(migration_Dutch, migration_west, migration_non_west),
                                                    var_str = 'migration_background',
                                                    values = c('Dutch', 'Western', 'Non_Western')
)

ggplot(df_migration_marginal, aes(neighb_code, proportion)) +
  facet_grid(vars(migration_background))+
  geom_bar(aes(fill = dataset),
           position = "dodge",
           stat = "identity",
           width=0.4)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Proportions of migration background per neighbourhood")+
  ylab("Individuals (%)")+
  xlab("Neighbourhood code")+
  labs(fill = "Population")+
  theme(legend.title=element_text(size=20),
        legend.text=element_text(size=15))

################################################################################
# Validation with stratified dataset
df_migration_strat = df_migration_strat %>%
  select(gender, age_group, Dutch, Non_Western, Western) %>%
  pivot_longer(cols = -c(gender, age_group), names_to = "migration_background", values_to = "real")
df_migration_strat$pred = 0

df_synth_pop$age_group = ""
df_synth_pop$age_group[df_synth_pop$age %in% 0:4] = "age_0_5"
df_synth_pop$age_group[df_synth_pop$age %in% 5:9] = "age_5_10"
df_synth_pop$age_group[df_synth_pop$age %in% 10:14] =  "age_10_15"
df_synth_pop$age_group[df_synth_pop$age %in% 15:19] = "age_15_20"
df_synth_pop$age_group[df_synth_pop$age %in% 20:24] =  "age_20_25" 
df_synth_pop$age_group[df_synth_pop$age %in% 25:29] = "age_25_30"
df_synth_pop$age_group[df_synth_pop$age %in% 30:34] =  "age_30_35"
df_synth_pop$age_group[df_synth_pop$age %in% 35:39] =  "age_35_40" 
df_synth_pop$age_group[df_synth_pop$age %in% 40:44] = "age_40_45"
df_synth_pop$age_group[df_synth_pop$age %in% 45:49] = "age_45_50"
df_synth_pop$age_group[df_synth_pop$age %in% 50:54] =  "age_50_55"
df_synth_pop$age_group[df_synth_pop$age %in% 55:59] = "age_55_60"
df_synth_pop$age_group[df_synth_pop$age %in% 60:64] =  "age_60_65" 
df_synth_pop$age_group[df_synth_pop$age %in% 65:69] = "age_65_70"
df_synth_pop$age_group[df_synth_pop$age %in% 70:74] =  "age_70_75"
df_synth_pop$age_group[df_synth_pop$age %in% 75:79] =  "age_75_80" 
df_synth_pop$age_group[df_synth_pop$age %in% 80:84] =  "age_80_85" 
df_synth_pop$age_group[df_synth_pop$age %in% 85:89] = "age_85_90"
df_synth_pop$age_group[df_synth_pop$age %in% 90:94] =  "age_90_95"
df_synth_pop$age_group[df_synth_pop$age %in% 95:104] =  "age_over_95"

for (i in (1:nrow(df_migration_strat))) {
  df_migration_strat[i, 'pred',] = nrow(df_synth_pop[df_synth_pop$age_group == df_migration_strat[i, 'age_group']$age_group &
                                                    df_synth_pop$gender == df_migration_strat[i, 'gender']$gender &
                                                    df_synth_pop$migration_background == df_migration_strat[i, 'migration_background']$migration_background,])
}
df_synth_pop = subset(df_synth_pop, select=-c(age_group))

df_migration_strat <- df_migration_strat %>%
  pivot_longer(cols = c(real, pred), names_to = 'dataset', values_to = 'proportion')

# rename values
df_migration_strat$dataset <- recode(df_migration_strat$dataset,
                     'real' = 'stratified dataset',
                     'pred' = 'synthetic population')
df_migration_strat$age_group <- recode(df_migration_strat$age_group,
                                     'age_0_5' = 'below 4',
                                     'age_5_10' = '[5, 9]',
                                     'age_10_15' = '[10, 14]',
                                     'age_15_20' = '[15, 19]',
                                     'age_20_25' = '[20, 24]',
                                     'age_25_30' = '[25, 29]',
                                     'age_30_35' = '[30, 34]',
                                     'age_35_40' = '[35, 39]',
                                     'age_40_45' = '[40, 44]',
                                     'age_45_50' = '[45, 49]',
                                     'age_50_55' = '[50, 54]',
                                     'age_55_60' = '[55, 59]',
                                     'age_60_65' = '[60, 64]',
                                     'age_65_70' = '[65, 69]',
                                     'age_70_75' = '[70, 74]',
                                     'age_75_80' = '[75, 79]',
                                     'age_80_85' = '[80, 84]',
                                     'age_85_90' = '[85, 89]',
                                     'age_90_95' = '[90, 94]',
                                     'age_over_95' = 'over 95'
                                     )
df_migration_strat$migration_background <- recode(df_migration_strat$migration_background,
                                       'Non_Western' = 'Non Western'
)

df_migration_strat <- df_migration_strat %>%
  group_by(gender, age_group, dataset) %>%
  mutate(proportion = proportion / sum(proportion))

# bar plot
ggplot(df_migration_strat, aes(gender, proportion)) +
  facet_grid(vars(migration_background), vars(age_group))+
  geom_bar(aes(fill = dataset),
           position = "dodge",
           stat = "identity",
           width=0.4)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Proportions of migration background per neighbourhood")+
  ylab("Individuals (%)")+
  xlab("Gender and age groups")+
  labs(fill = "Population")+
  theme(legend.title=element_text(size=20),
        legend.text=element_text(size=15))

df_migration_strat <- df_migration_strat %>%
  pivot_wider(names_from = 'dataset', values_from = 'proportion')
df_migration_strat$difference <- df_migration_strat$`marginal distribution` - df_migration_strat$`synthetic population`

# heatmap
ggplot(df_migration_strat, aes(migration_background, age_group, fill=difference)) + 
  facet_grid(vars(gender))+
  geom_tile()+
  ggtitle("Difference of proportions of individuals in each combination of migration-gender-agegroup")+
  ylab("Age group")+
  xlab("Migration background")+
  labs(fill = "marginal distribution - \n synthetic population")+
  theme(legend.title=element_text(size=20),
        legend.text=element_text(size=15))

################################################################################
# Current education (only stratified dataset)
################################################################################

df_edu_current_strat = df_edu_current_strat %>%
  select(gender, age_group, migration_background, low, middle, high, no_current_edu) %>%
  pivot_longer(cols = -c(gender, age_group, migration_background), names_to = "current_education", values_to = "real")
df_edu_current_strat$pred = 0

df_synth_pop$age_group[df_synth_pop$age < 15] = NA
df_synth_pop$age_group[df_synth_pop$age %in% 10:14] = "age_10_15"
df_synth_pop$age_group[df_synth_pop$age %in% 15:19] = "age_15_20"
df_synth_pop$age_group[df_synth_pop$age %in% 20:24] = "age_20_25" 
df_synth_pop$age_group[df_synth_pop$age %in% 25:29] = "age_25_30" 
df_synth_pop$age_group[df_synth_pop$age %in% 30:34] = "age_30_35" 
df_synth_pop$age_group[df_synth_pop$age %in% 35:39] = "age_35_40" 
df_synth_pop$age_group[df_synth_pop$age %in% 40:44] = "age_40_45"
df_synth_pop$age_group[df_synth_pop$age %in% 45:49] = "age_45_50"
df_synth_pop$age_group[df_synth_pop$age >= 50] = "age_over_50"

for (i in (1:nrow(df_edu_current_strat))) {
  df_edu_current_strat[i, 'pred',] = nrow(df_synth_pop[!is.na(df_synth_pop$age_group) &
                                                               df_synth_pop$age_group == df_edu_current_strat[i, 'age_group']$age_group &
                                                               df_synth_pop$gender == df_edu_current_strat[i, 'gender']$gender &
                                                               df_synth_pop$migration_background == df_edu_current_strat[i, 'migration_background']$migration_background &
                                                               df_synth_pop$current_education == df_edu_current_strat[i, 'current_education']$current_education,])
}
df_synth_pop = subset(df_synth_pop, select=-c(age_group))


df_edu_current_strat <- df_edu_current_strat %>%
  pivot_longer(cols = c(real, pred), names_to = 'dataset', values_to = 'proportion')

# rename values
df_edu_current_strat$dataset <- recode(df_edu_current_strat$dataset,
                                     'real' = 'stratified dataset',
                                     'pred' = 'synthetic population')
df_edu_current_strat$age_group <- recode(df_edu_current_strat$age_group,
                                       'age_10_15' = '[10, 14]',
                                       'age_15_20' = '[15, 19]',
                                       'age_20_25' = '[20, 24]',
                                       'age_25_30' = '[25, 29]',
                                       'age_30_35' = '[30, 34]',
                                       'age_35_40' = '[35, 39]',
                                       'age_40_45' = '[40, 44]',
                                       'age_over_50' = 'over 45'
)
df_edu_current_strat$current_education <- recode(df_edu_current_strat$current_education,
                                                  'no_current_edu' = 'no current education'
)
df_edu_current_strat$migration_background <- recode(df_edu_current_strat$migration_background,
                                                  'Non_Western' = 'Non Western'
)

df_edu_current_strat <- df_edu_current_strat %>%
  group_by(gender, age_group, migration_background, dataset) %>%
  mutate(proportion = proportion / sum(proportion))

# bar plot
ggplot(df_edu_current_strat, aes(age_group, proportion)) +
  facet_grid(vars(migration_background), vars(gender))+
  geom_bar(aes(fill = dataset),
           position = "dodge",
           stat = "identity",
           width=0.4)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Proportions of migration backgrounds per age group and gender")+
  ylab("Individuals (%)")+
  xlab("Gender and age groups")+
  labs(fill = "Population")+
  theme(legend.title=element_text(size=20),
        legend.text=element_text(size=15))

df_edu_current_strat <- df_edu_current_strat %>%
  pivot_wider(names_from = 'dataset', values_from = 'proportion')
df_edu_current_strat$difference <- df_edu_current_strat$`stratified dataset` - df_edu_current_strat$`synthetic population`

# heatmap
ggplot(df_edu_current_strat, aes(migration_background, current_education, fill=difference)) + 
  facet_grid(vars(gender), vars(age_group))+
  geom_tile()+
  ggtitle("Difference of proportions of individuals in each combination of current education-migration-gender-agegroup")+
  ylab("Age group")+
  xlab("Migration background")+
  labs(fill = "marginal distribution - \n synthetic population")+
  theme(legend.title=element_text(size=20),
        legend.text=element_text(size=15))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

################################################################################
# Education attainment
################################################################################

# With marginal distribution
df_edu_attainment_marginal <- get_proportions_over_marginal(df_marginal_dist = df_marginal_dist,
                                                       df_synth_pop = df_synth_pop,
                                                       aggregation_var = neighb_code,
                                                       cols_marginal = c(education_absolved_low, education_absolved_middle, education_absolved_high),
                                                       var_str = 'edu_attainment',
                                                       values = c('low', 'middle', 'high'),
                                                       age_limits = c(15, 75)
)

ggplot(df_edu_attainment_marginal, aes(neighb_code, proportion)) +
  facet_grid(vars(edu_attainment))+
  geom_bar(aes(fill = dataset),
           position = "dodge",
           stat = "identity",
           width=0.4)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Proportions of education attainment levels per neighbourhood")+
  ylab("Individuals (%)")+
  xlab("Neighbourhood code")+
  labs(fill = "Population")+
  theme(legend.title=element_text(size=20),
        legend.text=element_text(size=15))