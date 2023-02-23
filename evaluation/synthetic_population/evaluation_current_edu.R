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
    municipality,
    'individuals_demographics',
    sep = '/'
  )
)
df_edu_current_strat = read.csv("edu_current-71450NED-formatted.csv", sep = ",")

# Load synthetic population
setwd(this.path::this.dir())
setwd(paste("../../output/synthetic-population-households/4_car_2022-12-26_15-50", sep = ""))
df_synth_pop = read.csv("synthetic_population_DHZW_2019.csv", sep = ",")

################################################################################
# Only stratified dataset available

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
                                         'age_10_15' = '10 - 14',
                                         'age_15_20' = '15 - 19',
                                         'age_20_25' = '20 - 24',
                                         'age_25_30' = '25 - 29',
                                         'age_30_35' = '30 - 34',
                                         'age_35_40' = '35 - 39',
                                         'age_40_45' = '40 - 44',
                                         'age_over_50' = '>= 45'
)
df_edu_current_strat$current_education <- recode(df_edu_current_strat$current_education,
                                                 'no_current_edu' = 'no current education'
)
df_edu_current_strat$migration_background <- recode(df_edu_current_strat$migration_background,
                                                    'Non_Western' = 'Non-Western'
)

setwd(this.dir())
setwd('data_comparison')
write.csv(df_edu_current_strat, 'current_education.csv', row.names = FALSE)

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
  ggtitle("Proportions of individuals with current education levels per age group, gender and migration background")+
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
