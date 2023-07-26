library(readr)
library(dplyr)
library(this.path)
library(stringr)
library(tidyr)

# Load dataset
setwd(this.path::this.dir())
setwd("../../data/raw")
df <- read.csv('inhabintants_south_holland-03759NED.csv', sep=';')

df <- df %>%
  select(Leeftijd,
         Bevolking.op.1.januari..aantal.) %>%
  rename('age' = 'Leeftijd',
         'n_people' = 'Bevolking.op.1.januari..aantal.')

df$age <- recode(
  df$age,
  '16 jaar' = 16,
  '17 jaar' = 17,
  '18 jaar' = 18,
  '19 jaar' = 19,
  '20 jaar' = 20,
  '21 jaar' = 21,
  '22 jaar' = 22,
  '23 jaar' = 23,
  '24 jaar' = 24,
  '25 jaar' = 25,
  '26 jaar' = 26,
  '27 jaar' = 27,
  '28 jaar' = 28,
  '29 jaar' = 29,
  '30 jaar' = 30,
  '31 jaar' = 31,
  '32 jaar' = 32,
  '33 jaar' = 33,
  '34 jaar' = 34,
  '35 jaar' = 35,
  '36 jaar' = 36,
  '37 jaar' = 37,
  '38 jaar' = 38,
  '39 jaar' = 39,
  '40 jaar' = 40,
  '41 jaar' = 41,
  '42 jaar' = 42,
  '43 jaar' = 43,
  '44 jaar' = 44,
  '45 jaar' = 45,
  '46 jaar' = 46,
  '47 jaar' = 47,
  '48 jaar' = 48,
  '49 jaar' = 49,
  '50 jaar' = 50,
  '51 jaar' = 51,
  '52 jaar' = 52,
  '53 jaar' = 53,
  '54 jaar' = 54,
  '55 jaar' = 55,
  '56 jaar' = 56,
  '57 jaar' = 57,
  '58 jaar' = 58,
  '59 jaar' = 59,
  '60 jaar' = 60,
  '61 jaar' = 61,
  '62 jaar' = 62,
  '63 jaar' = 63,
  '64 jaar' = 64,
  '65 jaar' = 65,
  '66 jaar' = 66,
  '67 jaar' = 67,
  '68 jaar' = 68,
  '69 jaar' = 69,
  '70 jaar' = 70,
  '71 jaar' = 71,
  '72 jaar' = 72,
  '73 jaar' = 73,
  '74 jaar' = 74,
  '75 jaar' = 75,
  '76 jaar' = 76,
  '77 jaar' = 77,
  '78 jaar' = 78,
  '79 jaar' = 79,
  '80 jaar' = 80,
  '81 jaar' = 81,
  '82 jaar' = 82,
  '83 jaar' = 83,
  '84 jaar' = 84,
  '85 jaar' = 85,
  '86 jaar' = 86,
  '87 jaar' = 87,
  '88 jaar' = 88,
  '89 jaar' = 89,
  '90 jaar' = 90,
  '91 jaar' = 91,
  '92 jaar' = 92,
  '93 jaar' = 93,
  '94 jaar' = 94,
  '95 jaar of ouder' = 95)

age_group <- c("age_16_17", "age_18_19", "age_20_24", "age_25_29", "age_30_39", "age_40_49", "age_50_59", "age_60_64", "age_65_69", "age_70_74", "age_over_75")
df_groups <- data.frame(age_group)

df_groups$n_people <- 0
df_groups[df_groups$age_group == 'age_16_17',]$n_people <- sum(df[df$age %in% 16:17,]$n_people)
df_groups[df_groups$age_group == 'age_18_19',]$n_people <- sum(df[df$age %in% 18:19,]$n_people)
df_groups[df_groups$age_group == 'age_20_24',]$n_people <- sum(df[df$age %in% 20:24,]$n_people)
df_groups[df_groups$age_group == 'age_25_29',]$n_people <- sum(df[df$age %in% 25:29,]$n_people)
df_groups[df_groups$age_group == 'age_30_39',]$n_people <- sum(df[df$age %in% 30:39,]$n_people)
df_groups[df_groups$age_group == 'age_40_49',]$n_people <- sum(df[df$age %in% 40:49,]$n_people)
df_groups[df_groups$age_group == 'age_50_59',]$n_people <- sum(df[df$age %in% 50:59,]$n_people)
df_groups[df_groups$age_group == 'age_60_64',]$n_people <- sum(df[df$age %in% 60:64,]$n_people)
df_groups[df_groups$age_group == 'age_65_69',]$n_people <- sum(df[df$age %in% 65:69,]$n_people)
df_groups[df_groups$age_group == 'age_70_74',]$n_people <- sum(df[df$age %in% 70:74,]$n_people)
df_groups[df_groups$age_group == 'age_over_75',]$n_people <- sum(df[df$age %in% 75:95,]$n_people)

# Save dataset
setwd(this.path::this.dir())
setwd("../../data/processed")
write.csv(df_groups, 'inhabintants_south_holland-03759NED-formatted.csv', row.names = FALSE)