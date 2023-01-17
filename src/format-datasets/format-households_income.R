library(readr)
library(dplyr)
library(this.path)
setwd(this.path::this.dir())
source('../../config/config.R')

setwd(this.dir())
setwd(
  paste(
    "../../data/raw",
    year,
    municipality,
    'households',
    sep = '/'
  )
)

df <- read.csv('household_income_85064NED.csv', sep = ';')

df <- df %>%
  select(Kenmerken.van.huishoudens,
         Verdeling.inkomen.Gestandaardiseerd.inkomen..1e.10..groep....,
         Verdeling.inkomen.Gestandaardiseerd.inkomen..2e.10..groep....,
         Verdeling.inkomen.Gestandaardiseerd.inkomen..3e.10..groep....,
         Verdeling.inkomen.Gestandaardiseerd.inkomen..4e.10..groep....,
         Verdeling.inkomen.Gestandaardiseerd.inkomen..5e.10..groep....,
         Verdeling.inkomen.Gestandaardiseerd.inkomen..6e.10..groep....,
         Verdeling.inkomen.Gestandaardiseerd.inkomen..7e.10..groep....,
         Verdeling.inkomen.Gestandaardiseerd.inkomen..8e.10..groep....,
         Verdeling.inkomen.Gestandaardiseerd.inkomen..9e.10..groep....,
         Verdeling.inkomen.Gestandaardiseerd.inkomen..10e.10..groep....) %>%
  rename('type' = 'Kenmerken.van.huishoudens',
         'income_1_10' = 'Verdeling.inkomen.Gestandaardiseerd.inkomen..1e.10..groep....',
         'income_2_10' = 'Verdeling.inkomen.Gestandaardiseerd.inkomen..2e.10..groep....',
         'income_3_10' = 'Verdeling.inkomen.Gestandaardiseerd.inkomen..3e.10..groep....',
         'income_4_10' = 'Verdeling.inkomen.Gestandaardiseerd.inkomen..4e.10..groep....',
         'income_5_10' = 'Verdeling.inkomen.Gestandaardiseerd.inkomen..5e.10..groep....',
         'income_6_10' = 'Verdeling.inkomen.Gestandaardiseerd.inkomen..6e.10..groep....',
         'income_7_10' = 'Verdeling.inkomen.Gestandaardiseerd.inkomen..7e.10..groep....',
         'income_8_10' = 'Verdeling.inkomen.Gestandaardiseerd.inkomen..8e.10..groep....',
         'income_9_10' = 'Verdeling.inkomen.Gestandaardiseerd.inkomen..9e.10..groep....',
         'income_10_10' = 'Verdeling.inkomen.Gestandaardiseerd.inkomen..10e.10..groep....')

df$type <- recode(
  df$type,
  'Type: Eenpersoonshuishouden' = 'single',
  'Type: Eenoudergezin' = 'single-parent',
  'Type: Paar, met kind(eren)' = 'couple_with_children',
  'Type: Paar, zonder kind' = 'couple_without_children',
  'Hoofdkostwinner: Nederland' = 'breadwinner_migration_Dutch',
  'Hoofdkostwinner: westers' = 'breadwinner_migration_Western',
  'Hoofdkostwinner: niet-westers' = 'breadwinner_migration_Non_Western',
  'Hoofdkostwinner: tot 25 jaar' = 'breadwinner_age_0_24',
  'Hoofdkostwinner: 25 tot 45 jaar' = 'breadwinner_age_25_44',
  'Hoofdkostwinner: 45 tot 65 jaar' = 'breadwinner_age_45_64',
  'Hoofdkostwinner: 65 jaar of ouder' = 'breadwinner_age_over_65')

df[c('income_1_10', 'income_2_10', 'income_3_10', 'income_4_10', 'income_5_10', 'income_6_10', 'income_7_10', 'income_8_10', 'income_9_10', 'income_10_10')] <- df[c('income_1_10', 'income_2_10', 'income_3_10', 'income_4_10', 'income_5_10', 'income_6_10', 'income_7_10', 'income_8_10', 'income_9_10', 'income_10_10')]/100

# save
setwd(this.dir())
setwd(
  paste(
    "../../data/processed",
    year,
    municipality,
    'households',
    sep = '/'
  )
)
write.csv(df, 'household_income_85064NED-formatted.csv', row.names = FALSE)


# not used
df_combined <- expand.grid(c('single', 'single-parent', 'couple with children', 'couple without children'),
                           c('breadwinner_migration_Dutch', 'breadwinner_migration_Western', 'breadwinner_migration_Non_Western'))
colnames(df_combined) <- c('hh_type', 'hh_migration_background')


df_combined <- merge(df_combined, df, by.x = 'hh_type', by.y = 'type')
df_combined <- df_combined %>%
  rename('income_1_10_type' = 'income_1_10',
         'income_2_10_type' = 'income_2_10',
         'income_3_10_type' = 'income_3_10',
         'income_4_10_type' = 'income_4_10',
         'income_5_10_type' = 'income_5_10',
         'income_6_10_type' = 'income_6_10',
         'income_7_10_type' = 'income_7_10',
         'income_8_10_type' = 'income_8_10',
         'income_9_10_type' = 'income_9_10',
         'income_10_10_type' = 'income_10_10')

df_combined <- merge(df_combined, df, by.x = 'hh_migration_background', by.y = 'type')
df_combined <- df_combined %>%
  rename('income_1_10_migration' = 'income_1_10',
         'income_2_10_migration' = 'income_2_10',
         'income_3_10_migration' = 'income_3_10',
         'income_4_10_migration' = 'income_4_10',
         'income_5_10_migration' = 'income_5_10',
         'income_6_10_migration' = 'income_6_10',
         'income_7_10_migration' = 'income_7_10',
         'income_8_10_migration' = 'income_8_10',
         'income_9_10_migration' = 'income_9_10',
         'income_10_10_migration' = 'income_10_10')

df_combined$income_1_10 <- df_combined$income_1_10_type * df_combined$income_1_10_migration
df_combined$income_2_10 <- df_combined$income_2_10_type * df_combined$income_2_10_migration
df_combined$income_3_10 <- df_combined$income_3_10_type * df_combined$income_3_10_migration
df_combined$income_4_10 <- df_combined$income_4_10_type * df_combined$income_4_10_migration
df_combined$income_5_10 <- df_combined$income_5_10_type * df_combined$income_5_10_migration
df_combined$income_6_10 <- df_combined$income_6_10_type * df_combined$income_6_10_migration
df_combined$income_7_10 <- df_combined$income_7_10_type * df_combined$income_7_10_migration
df_combined$income_8_10 <- df_combined$income_8_10_type * df_combined$income_8_10_migration
df_combined$income_9_10 <- df_combined$income_9_10_type * df_combined$income_9_10_migration
df_combined$income_10_10 <- df_combined$income_10_10_type * df_combined$income_10_10_migration

df_combined <- df_combined %>%
  select(hh_type,
         hh_migration_background,
         income_1_10,
         income_2_10,
         income_3_10,
         income_4_10,
         income_5_10,
         income_6_10,
         income_7_10,
         income_8_10,
         income_9_10,
         income_10_10)