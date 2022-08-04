library(dplyr)
library("this.path")

municipality = 'den_haag_2019'

setwd(paste(this.path::this.dir(), "/data/", municipality, "/stratified-datasets", sep = ""))
df_StratHousehold = read.csv("household_gender_age-71488ned.csv", sep = ";", fileEncoding="UTF-8-BOM")

# Rename, translate and reorganise dataset
df_StratHousehold = df_StratHousehold %>%
  rename(gender = Geslacht,
         age_group = Leeftijd,
         child = Personen.in.particuliere.huishoudens.Thuiswonend.kind..aantal.,
         single = Personen.in.particuliere.huishoudens.Alleenstaand..aantal.,
         couple = Personen.in.particuliere.huishoudens.Samenwonend.Totaal.samenwonende.personen..aantal.,
         single_parent = Personen.in.particuliere.huishoudens.Ouder.in.eenouderhuishouden..aantal.) %>%
  select(
    gender,
    age_group,
    child,
    single,
    couple,
    single_parent
  ) %>%
  mutate(age_group = recode(age_group,
                            "0 tot 5 jaar" = 'age_0_5',
                            '5 tot 10 jaar' = 'age_5_10',
                            '10 tot 15 jaar' = 'age_10_15',
                            '15 tot 20 jaar' = 'age_15_20',
                            '20 tot 25 jaar' = 'age_20_25',
                            '25 tot 30 jaar' = 'age_25_30',
                            '30 tot 35 jaar' = 'age_30_35',
                            '35 tot 40 jaar' = 'age_35_40',
                            '40 tot 45 jaar' = 'age_40_45',
                            '45 tot 50 jaar' = 'age_45_50',
                            '50 tot 55 jaar' = 'age_50_55',
                            '55 tot 60 jaar' = 'age_55_60',
                            '60 tot 65 jaar' = 'age_60_65',
                            '65 tot 70 jaar' = 'age_65_70',
                            '70 tot 75 jaar' = 'age_70_75',
                            '75 tot 80 jaar' = 'age_75_80',
                            '80 tot 85 jaar' = 'age_80_85',
                            '85 tot 90 jaar' = 'age_85_90',
                            '90 tot 95 jaar' = 'age_90_95',
                            '95 jaar of ouder' = 'age_over_95')) %>%
  mutate(gender = recode(gender,
                         "Mannen" = 'male',
                         "Vrouwen" = 'female',
  ))

df_StratHousehold[is.na(df_StratHousehold )] <- 0

df_StratHousehold$total = df_StratHousehold$child + df_StratHousehold$single + df_StratHousehold$couple + df_StratHousehold$single_parent

# Save synthetic population
setwd(paste(this.path::this.dir(), "/data/", municipality, "/households/distributions", sep = ""))
write.csv(df_StratHousehold, 'household_gender_age-71488NED-formatted.csv', row.names=FALSE)