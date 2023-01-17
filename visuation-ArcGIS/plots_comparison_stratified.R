library(readr)
library(dplyr)
library(tidyr)
library(sf)
library(ggmap)
library(egg)
library(shadowtext)
library(ggpubr)
library("this.path")
setwd(this.path::this.dir())
source('../config/config.R')

# Load synthetic population
setwd(this.path::this.dir())
df_synth_pop <-
  read_csv(
    "../output/synthetic-population-households/synthetic_population_DHZW_2019_with_hh.csv"
  )

################################################################################
# Age - gender

# Load stratified dataset
setwd(this.path::this.dir())
df_gender_age <- read_csv(
  paste(
    '../data/processed',
    year,
    municipality,
    'individuals_demographics/gender_age-03759NED-formatted.csv',
    sep = '/'
  )
)

# Calculate figures
df_gender_age = df_gender_age %>%
  select(age, male, female) %>%
  pivot_longer(cols = -c(age),
               names_to = "gender",
               values_to = "real")
df_gender_age$pred = 0
for (i in (1:nrow(df_gender_age))) {
  df_gender_age[i, 'pred', ] = nrow(df_synth_pop[df_synth_pop$age == df_gender_age[i, 'age']$age &
                                                   df_synth_pop$gender == df_gender_age[i, 'gender']$gender, ])
}

# Normalise and calculate difference
df_gender_age$real = df_gender_age$real / sum(df_gender_age$real)
df_gender_age$pred = df_gender_age$pred / sum(df_gender_age$pred)
df_gender_age$diff = abs(df_gender_age$pred - df_gender_age$real) * 100

# Plot heatmap
ggplot(df_gender_age, aes(age, gender, fill = diff), breaks = c(1:104)) +
  geom_tile() +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  ggtitle("Stratified information: age and gender") +
  ylab("Gender") +
  xlab("Age") +
  labs(fill = "% difference between \n synthetic population \n and real data") +
  theme(
    title = element_text(size = 20),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 15),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20)
  ) +
  scale_x_continuous(n.breaks = 10, limits = c(min(df_gender_age$age), max(df_gender_age$age)))


################################################################################
# Age - gender - migration background

# Load stratified dataset
setwd(this.path::this.dir())
df_migration_gender_age <- read_csv(
  paste(
    '../data/processed',
    year,
    municipality,
    'individuals_demographics/gender_age_migration-84910NED-formatted.csv',
    sep = '/'
  )
)

# Recreate age groups in the synthetic population
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

# Calculate figures
df_migration_gender_age = df_migration_gender_age %>%
  select(gender, age_group, Dutch, Non_Western, Western) %>%
  pivot_longer(
    cols = -c(gender, age_group),
    names_to = "migration_background",
    values_to = "real"
  )
df_migration_gender_age$pred = 0
for (i in (1:nrow(df_migration_gender_age))) {
  df_migration_gender_age[i, 'pred',] = nrow(df_synth_pop[df_synth_pop$age_group == df_migration_gender_age[i, 'age_group']$age_group &
                                                            df_synth_pop$gender == df_migration_gender_age[i, 'gender']$gender &
                                                            df_synth_pop$migration_background == df_migration_gender_age[i, 'migration_background']$migration_background,])
}

# Normalise and calculate difference
df_migration_gender_age$real = df_migration_gender_age$real / sum(df_migration_gender_age$real)
df_migration_gender_age$pred = df_migration_gender_age$pred / sum(df_migration_gender_age$pred)
df_migration_gender_age$diff = abs(df_migration_gender_age$pred - df_migration_gender_age$real) * 100

df_migration_gender_age <- df_migration_gender_age %>%
  mutate(
    age_group = recode(
      age_group,
      'age_0_5' = 'Between 0 and 4',
      'age_5_10' = 'Between 5 and 9',
      'age_10_15' = 'Between 10 and 14',
      'age_15_20' = 'Between 15 and 19',
      'age_20_25' = 'Between 20 and 24',
      'age_25_30' = 'Between 25 and 29',
      'age_30_35' = 'Between 30 and 34',
      'age_35_40' = 'Between 35 and 39',
      'age_40_45' = 'Between 40 and 44',
      'age_45_50' = 'Between 45 and 50',
      'age_50_55' = 'Between 50 and 54',
      'age_55_60' = 'Between 55 and 59',
      'age_60_65' = 'Between 60 and 64',
      'age_65_70' = 'Between 65 and 69',
      'age_70_75' = 'Between 70 and 74',
      'age_75_80' = 'Between 75 and 79',
      'age_80_85' = 'Between 80 and 84',
      'age_85_90' = 'Between 85 and 89',
      'age_90_95' = 'Between 90 and 94',
      'age_over_95' = 'Over 95'
    )
  )

df_migration_gender_age$age_group <-
  factor(
    df_migration_gender_age$age_group,
    levels = c(
      'Between 0 and 4',
      'Between 5 and 9',
      'Between 10 and 14',
      'Between 15 and 19',
      'Between 20 and 24',
      'Between 25 and 29',
      'Between 30 and 34',
      'Between 35 and 39',
      'Between 40 and 44',
      'Between 45 and 50',
      'Between 50 and 54',
      'Between 55 and 59',
      'Between 60 and 64',
      'Between 65 and 69',
      'Between 70 and 74',
      'Between 75 and 79',
      'Between 80 and 84',
      'Between 85 and 89',
      'Between 90 and 94',
      'Over 95'
    )
  )


df_migration_gender_age <- df_migration_gender_age %>%
  mutate(migration_background = recode(migration_background,
                                       'Non_Western' = 'Non-Western'))


ggplot(df_migration_gender_age,
       aes(
         x = as.character(age_group),
         y = as.character(gender),
         fill = diff
       )) +
  geom_tile() +
  facet_grid(vars(migration_background)) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  theme(
    title = element_text(size = 20),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20),
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    )
  ) +
  xlab("Age group") +
  ylab("Gender") +
  labs(fill = '% difference between \n synthetic population \n and real data')