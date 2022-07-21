library(GenSynthPop)
library(dplyr)

library("this.path")
setwd(this.path::this.dir())
source('utils.R')

setwd(paste(this.path::this.dir(), "/data/synthetic-populations", sep = ""))
df_SynthPop = read.csv('synthetic_population_DHZW.csv')

setwd(paste(this.path::this.dir(), "/data/stratified-datasets", sep = ""))
df_StratHousehold = read.csv("household_gender_age-71488ned.csv", sep = ";", fileEncoding="UTF-8-BOM")

df_StratHousehold = df_StratHousehold %>%
  rename(gender = Geslacht,
         age_group = Leeftijd,
         children = Personen.in.particuliere.huishoudens.Thuiswonend.kind..aantal.,
         singles = Personen.in.particuliere.huishoudens.Alleenstaand..aantal.,
         couples = Personen.in.particuliere.huishoudens.Samenwonend.Totaal.samenwonende.personen..aantal.,
         single_parents = Personen.in.particuliere.huishoudens.Ouder.in.eenouderhuishouden..aantal.) %>%
  select(
    gender,
    age_group,
    children,
    singles,
    couples,
    single_parents
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
df_StratHousehold$total = df_StratHousehold$children + df_StratHousehold$singles + df_StratHousehold$couples + df_StratHousehold$single_parents


df_SynthPop$age_group = ""
df_SynthPop$age_group[df_SynthPop$age %in% 0:5] = "age_0_5"
df_SynthPop$age_group[df_SynthPop$age %in% 5:10] = "age_5_10"
df_SynthPop$age_group[df_SynthPop$age %in% 10:15] = "age_10_15"
df_SynthPop$age_group[df_SynthPop$age %in% 15:20] = "age_15_20"
df_SynthPop$age_group[df_SynthPop$age %in% 20:25] = "age_20_25"
df_SynthPop$age_group[df_SynthPop$age %in% 25:30] = "age_25_30"
df_SynthPop$age_group[df_SynthPop$age %in% 30:35] = "age_30_35"
df_SynthPop$age_group[df_SynthPop$age %in% 35:40] = "age_35_40"
df_SynthPop$age_group[df_SynthPop$age %in% 40:45] = "age_40_45"
df_SynthPop$age_group[df_SynthPop$age %in% 45:50] = "age_45_50"
df_SynthPop$age_group[df_SynthPop$age %in% 50:55] = "age_50_55"
df_SynthPop$age_group[df_SynthPop$age %in% 55:60] = "age_55_60"
df_SynthPop$age_group[df_SynthPop$age %in% 60:65] = "age_60_65"
df_SynthPop$age_group[df_SynthPop$age %in% 65:70] = "age_65_70"
df_SynthPop$age_group[df_SynthPop$age %in% 70:75] = "age_70_75"
df_SynthPop$age_group[df_SynthPop$age %in% 75:80] = "age_75_80"
df_SynthPop$age_group[df_SynthPop$age %in% 80:85] = "age_80_85"
df_SynthPop$age_group[df_SynthPop$age %in% 85:90] = "age_85_90"
df_SynthPop$age_group[df_SynthPop$age %in% 90:95] = "age_90_95"
df_SynthPop$age_group[df_SynthPop$age %in% 95:105] = "age_over_95"

df_SynthPop = calc_propens_agents(df_StratHousehold, "children", "total", df_SynthPop, c("age_group", "gender") )
df_SynthPop = calc_propens_agents(df_StratHousehold, "singles", "total", df_SynthPop, c("age_group", "gender") )
df_SynthPop = calc_propens_agents(df_StratHousehold, "couples", "total", df_SynthPop, c("age_group", "gender") )
df_SynthPop = calc_propens_agents(df_StratHousehold, "single_parents", "total", df_SynthPop, c("age_group", "gender") )

df_SynthPop = distr_attr_cond_prop(agent_df = df_SynthPop,
                                   variable = 'household_position',
                                   list_agent_propens = c('prop_children', 'prop_singles', 'prop_couples', 'prop_single_parents'),
                                   list_class_names = c('children', 'singles', 'couples', 'single_parents'))

df_SynthPop = subset(df_SynthPop, select=-c(prop_children, prop_singles, prop_couples, prop_single_parents, random_scores))

##############################################
##############################################
##############################################

# Load and reformat household size distributions

setwd(paste(this.path::this.dir(), "/data/", sep = ""))
df_HouseholdSize = read.csv("household_size_71486NED.csv", sep = ";", fileEncoding="UTF-8-BOM")
df_HouseholdSize = df_HouseholdSize %>%
  rename('1' = Eenpersoonshuishouden_22,
         '2' = k_2Personen_24,
         '3' = k_3Personen_25,
         '4' = k_4Personen_26,
         '5_or_more' = k_5OfMeerPersonen_27
         )%>%
  select('1', '2', '3', '4', '5_or_more')
df_HouseholdSize = as.data.frame(t(df_HouseholdSize))
colnames(df_HouseholdSize) = c('freq')
df_HouseholdSize <- cbind(size = rownames(df_HouseholdSize), df_HouseholdSize)
rownames(df_HouseholdSize) <- 1:nrow(df_HouseholdSize)


#################################################
# function to sample an agent from the synthetic population on age, gender and household type, using the household dataset as frequency distribution

sample_agent <- function(df_SynthPop, df_StratHousehold, type) {
  sample_age_gender <- df_StratHousehold[sample.int(nrow(df_StratHousehold),
                                                    1,
                                                    replace = TRUE,
                                                    prob = df_StratHousehold$singles),] 
  
  agent = df_SynthPop[which.max(df_SynthPop$age_group == sample_age_gender[1, 'age_group'] &
                                  df_SynthPop$gender == sample_age_gender[1, 'gender'] &
                                  df_SynthPop$household_position == type),]
  if (nrow(agent)==0){
    return -1
  } else {
    return (agent)
  }
}
############################################

df_Households <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(df_Households) <- c("hh_ID", "hh_size")

h_size <- sample(
  x = df_HouseholdSize$size,
  size = 1,
  replace=TRUE,
  prob=df_HouseholdSize$freq)
h_type = NA


if(h_size == 1) {
  h_type = 'single'
  agent = sample_single(df_SynthPop, df_StratHousehold, 'singles')
  if (agent!=-1){
    ## add new row to the main household dataframe
    hh_ID = nrow(df_Households) + 1
    df_Households[hh_ID] = c(hh_ID, h_size)
    ## create a new dataframe for this new household
    new_household = agent
    write.csv(new_household, gsub(" ", "", paste('hh_',hh_ID,'.csv')))
  }
}
if(h_size == 2) {
 
}