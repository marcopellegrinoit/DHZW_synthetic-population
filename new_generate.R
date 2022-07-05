library(GenSynthPop)
library(dplyr)

library("this.path")
setwd(this.path::this.dir())
source('utils.R')

flag_validation_plots = FALSE

########################## Conversion codes ####################################

setwd(paste(this.path::this.dir(), "/data/codes", sep = ""))
codes_age = read.csv("codes_age.csv", sep=",")
codes_agegroup20 = read.csv("codes_agegroup20.csv", fileEncoding="UTF-8-BOM")
codes_ages_education = read.csv("codes_ages_education.csv", fileEncoding="UTF-8-BOM", sep=";")
codes_education = read.csv("codes_education.csv", fileEncoding="UTF-8-BOM", sep=";")

# Neighborhood codes of DHZW
DHZW_neighborhood_codes <- c('BU05183284',
                             'BU05183536',
                             'BU05183638',
                             'BU05183620',
                             'BU05183639',
                             'BU05183488',
                             'BU05183489',
                             'BU05183488',
                             'BU05183480',
                             'BU05181785',
                             'BU05183387',
                             'BU05183396',
                             'BU05183398',
                             'BU05183399'
)

################################################################################
## Load marginal distributions

setwd(paste(this.path::this.dir(), "/data", sep = ""))

## Load The Hague neighborhood dataset: n people per age group and neighbourhood 
setwd(paste(this.path::this.dir(), "/data", sep = ""))
pop_marginal_distributions = read.csv("marginal_distributions_TheHague_2019_84583NED.csv", sep = ";")
marginal_distributions = pop_marginal_distributions[which(pop_marginal_distributions$SoortRegio_2 == "Buurt     "),] # select neighborhood data only
remove(pop_marginal_distributions)

# filter DHZW area
marginal_distributions = marginal_distributions[marginal_distributions$WijkenEnBuurten %in% DHZW_neighborhood_codes,]

# filter and rename only useful attributes
marginal_distributions = marginal_distributions %>%
  select(Codering_3,
         AantalInwoners_5,
         Mannen_6,
         Vrouwen_7,
         k_0Tot15Jaar_8,
         k_15Tot25Jaar_9,
         k_25Tot45Jaar_10,
         k_45Tot65Jaar_11,
         k_65JaarOfOuder_12,
         WestersTotaal_17,
         NietWestersTotaal_18
  )%>%
  rename(neighb_code = Codering_3,
         tot_pop = AantalInwoners_5,
         gender_male = Mannen_6,
         gender_female = Vrouwen_7,
         age_0_15 = k_0Tot15Jaar_8,
         age_15_25 = k_15Tot25Jaar_9,
         age_25_45 = k_25Tot45Jaar_10,
         age_45_65 = k_45Tot65Jaar_11,
         age_over65 = k_65JaarOfOuder_12,
         migration_west = WestersTotaal_17,
         migration_non_west = NietWestersTotaal_18
  )

################################################################################
## Initialise synthetic population with age groups withing neigbhbourhoods
################################################################################

# create empty synthetic population dataframe with number of agents
# Note: CBS must have made a mistake, probably with the age grouping, because the total is larger than the general population.
# So, I initialise the synthetic population size with the sum of group ages, otherwise the script cannot work.

group_ages = c('age_0_15', 'age_15_25', 'age_25_45', 'age_45_65', 'age_over65')
population_size = sum(marginal_distributions[group_ages]) # 78655
agent_df = gen_agent_df(population_size)

# Distribute the agents across the age groups and neighborhoods
agent_df = distr_agent_neigh_age_group(neigh_df = marginal_distributions,
                                       agent_df = agent_df,
                                       neigh_id = "neighb_code",
                                       age_colnames = group_ages)

################################################################################
## Translate age groups into interger age
################################################################################

# Note: since the dataset is municipality aggregated, I can only calculate each age proportion and then use it to sample

# Load dataset
setwd(paste(this.path::this.dir(), "/data/stratified-datasets", sep = ""))
strat.gender_age = read.csv("gender_age-03759NED.csv", sep = ";") # count of people per lifeyear and gender in all of Amsterdam

# Select and translate useful attributes
strat.gender_age = strat.gender_age %>%
  select(Geslacht,
         Leeftijd,
         BevolkingOp1Januari_1
  ) %>%
  rename(gender = Geslacht,
         age_code = Leeftijd,
         n_people = BevolkingOp1Januari_1)

# Refactor gender, age
strat.gender_age = refactor_gender(strat.gender_age)
strat.gender_age = refactor_age(strat.gender_age, codes_age)
strat.gender_age = strat.gender_age %>% filter(!is.na(as.numeric(age))) # filter only numeric ages. Anyway, the highest age is already 104
strat.gender_age$age = as.numeric(strat.gender_age$age)

# Reformat stratified dataset, transforming the gender column into a column for each value
strat.gender_age = restructure_one_var_marginal(df = strat.gender_age,
                                                variable = 'gender',
                                                countsname = 'n_people')

# create group ages in the stratified dataset
strat.gender_age$age_group = "age_over65" # default for non-numeric
strat.gender_age$age_group[strat.gender_age$age %in% 0:14] = "age_0_15"
strat.gender_age$age_group[strat.gender_age$age %in% 15:24] = "age_15_25"
strat.gender_age$age_group[strat.gender_age$age %in% 25:44] = "age_25_45"
strat.gender_age$age_group[strat.gender_age$age %in% 45:64] = "age_45_65"
strat.gender_age$age_group[strat.gender_age$age %in% 65:105] = "age_over65"

# for each individual age, calculate its proportion to the total of each age group
strat.gender_age = strat.gender_age %>%
  group_by(age_group) %>%
  mutate(group_propensity = total/sum(total))

# for each group age of the synthetic population, sample the age from stratified dataset following the the frequency distribution
agent_df$age=''
for(group_age in group_ages){
  sample <- sample(
    x = strat.gender_age[strat.gender_age$age_group==group_age,]$age,
    size = nrow(agent_df[agent_df$age_group==group_age,]),
    replace=TRUE,
    prob=strat.gender_age[strat.gender_age$age_group==group_age,]$group_propensity
  ) # sample from age frequency distribution
  
  agent_df[agent_df$age_group==group_age,]$age = sample # apply to synthetic population dataset
}
agent_df$age = as.numeric(agent_df$age)

################################################################################
## Gender generation based on age
################################################################################

# Compute conditional propensities
agent_df = calc_propens_agents(dataframe = strat.gender_age,
                               variable = "female",
                               total_population = "total",
                               agent_df = agent_df,
                               list_conditional_var = c("age")
                               )

# Distribute attributes
agent_df = distr_attr_strat_neigh_stats_binary(agent_df = agent_df,
                                               neigh_df = marginal_distributions,
                                               neigh_ID = "neighb_code",
                                               variable=  "gender",
                                               list_var_classes_neigh_df = c("gender_male", "gender_female"),
                                               list_agent_propens =  c("prop_female"),
                                               list_class_names = c("female", "male")
                                               )

# Remove extra columns
agent_df = subset(agent_df, select=-c(prop_female, random_scores))

################################################################################
## Validation and analysis

if (flag_validation_plots) {
  # calculate cross-validation neighb_code - gender, with neighborhood totals
  validation_neigh_gender = validation(df_real_distr = marginal_distributions,
                                       df_synt_pop = agent_df,
                                       join_var = "neighb_code",
                                       list_real_df_var = c("gender_male", "gender_female"), 
                                       var_pred_df = "gender",
                                       list_values = c("male", "female")
  )
  
  # plot accuracy heatmap
  plot_heatmap(df = validation_neigh_gender,
               join_var = 'neighb_code',
               var = 'gender')
  
  plot_syth_pop_age_density(agent_df)
  
  plot_syth_strat_age_density(agent_df, strat.gender_age)
  
  # calculate total R2 score
  validation_neigh_gender.R2 = R_squared(validation_neigh_gender$real, validation_neigh_gender$pred) 
}

################################################################################
## Migration background generation based on age and gender
################################################################################

# Load stratified dataset
setwd(paste(this.path::this.dir(), "/data/stratified-datasets", sep = ""))
strat.gender_age_migr = read.csv("gender_age_migration-84910NED.csv", sep = ";")

# Select interesting attributes
strat.gender_age_migr = strat.gender_age_migr %>%
  select(Geslacht,
         Leeftijd,
         Migratieachtergrond,
         BevolkingOp1Januari_1
  ) %>%
  rename(gender = Geslacht,
         age_group_20_code = Leeftijd,
         migration_background_code = Migratieachtergrond,
         n_people = BevolkingOp1Januari_1
  )

# Refactor group age, gender and migration background
strat.gender_age_migr = refactor_age_group_20(strat.gender_age_migr, codes_agegroup20)
strat.gender_age_migr = refactor_gender(strat.gender_age_migr)
strat.gender_age_migr = refactor_migration(strat.gender_age_migr)

# Reformat stratified dataset, transforming the gender column into a column for each value
strat.gender_age_migr = restructure_one_var_marginal(df = strat.gender_age_migr,
                                                     variable = 'migration_background',
                                                     countsname = 'n_people')

# Classify synthetic population ages into age groups to link to the stratified dataset
agent_df$age_group_20 = ""
agent_df$age_group_20[agent_df$age %in% 0:4] = "age_0_5"
agent_df$age_group_20[agent_df$age %in% 5:9] = "age_5_10"
agent_df$age_group_20[agent_df$age %in% 10:14] =  "age_10_15"
agent_df$age_group_20[agent_df$age %in% 15:19] = "age_15_20"
agent_df$age_group_20[agent_df$age %in% 20:24] =  "age_20_25" 
agent_df$age_group_20[agent_df$age %in% 25:29] = "age_25_30"
agent_df$age_group_20[agent_df$age %in% 30:34] =  "age_30_35"
agent_df$age_group_20[agent_df$age %in% 35:39] =  "age_35_40" 
agent_df$age_group_20[agent_df$age %in% 40:44] = "age_40_45"
agent_df$age_group_20[agent_df$age %in% 45:49] = "age_45_50"
agent_df$age_group_20[agent_df$age %in% 50:54] =  "age_50_55"
agent_df$age_group_20[agent_df$age %in% 55:59] = "age_55_60"
agent_df$age_group_20[agent_df$age %in% 60:64] =  "age_60_65" 
agent_df$age_group_20[agent_df$age %in% 65:69] = "age_65_70"
agent_df$age_group_20[agent_df$age %in% 70:74] =  "age_70_75"
agent_df$age_group_20[agent_df$age %in% 75:79] =  "age_75_80" 
agent_df$age_group_20[agent_df$age %in% 80:84] =  "age_80_85" 
agent_df$age_group_20[agent_df$age %in% 85:89] = "age_85_90"
agent_df$age_group_20[agent_df$age %in% 90:94] =  "age_90_95"
agent_df$age_group_20[agent_df$age %in% 95:104] =  "age_over_95"

# Calculate the missing Dutch migration background in the overall marginal distribution
marginal_distributions$migration_Dutch = marginal_distributions$tot_pop - (marginal_distributions$migration_west + marginal_distributions$migration_non_west)

## Conditional Propensities
agent_df = calc_propens_agents(strat.gender_age_migr, "Dutch", "total", agent_df, c("age_group_20", "gender") )
agent_df = calc_propens_agents(strat.gender_age_migr, "Western", "total", agent_df, c("age_group_20", "gender") )
agent_df = calc_propens_agents(strat.gender_age_migr, "Non_Western", "total", agent_df, c("age_group_20", "gender") )

# Distribute values
agent_df = distr_attr_strat_neigh_stats_3plus(agent_df =  agent_df,
                                              neigh_df =  marginal_distributions,
                                              neigh_ID =  "neighb_code",
                                              variable =  "migration_background", 
                                              list_var_classes_neigh_df =  c("migration_Dutch", "migration_west", "migration_non_west"), 
                                              list_agent_propens =  c("prop_Dutch", "prop_Western", "prop_Non_Western"),
                                              list_class_names =  c("Dutch", "Western", "Non_Western"))

# Remove extra columns
agent_df = subset(agent_df, select=-c(prop_Dutch, prop_Western, prop_Non_Western, random_scores, age_group_20))

################################################################################
## Validation and analysis

if (flag_validation_plots) {
  # calculate cross-validation neighb_code - gender, with neighborhood totals
  validation_neigh_migration = validation(df_real_distr = marginal_distributions,
                                          df_synt_pop = agent_df,
                                          join_var = "neighb_code",
                                          list_real_df_var = c("migration_Dutch", "migration_west", "migration_non_west"), 
                                          var_pred_df = "migration_background",
                                          list_values = c("Dutch", "Western", "Non_Western")
  )
  
  # plot accuracy heatmap
  plot_heatmap(df = validation_neigh_migration,
               join_var = 'neighb_code',
               var = 'migration_background')
  
  # calculate total R2 score
  validation_neigh_migration.R2 = R_squared(validation_neigh_migration$real, validation_neigh_migration$pred) 
}

################################################################################
## Generate education level
################################################################################

## Data Preparation

# Load stratified dataset
setwd(paste(this.path::this.dir(), "/data/stratified-datasets", sep = ""))
strat.gender_age_migr_edu = read.csv("absolved_edu-gender-age-migration-71493NED.csv", sep = ";")

# Select interesting attributes
strat.gender_age_migr_edu = strat.gender_age_migr_edu %>%
  select(Geslacht,
         Leeftijd,
         Migratieachtergrond,
         Onderwijssoort,
         Gediplomeerden_1
  ) %>%
  rename(gender = Geslacht,
         age_code = Leeftijd,
         migration_background_code = Migratieachtergrond,
         education_code = Onderwijssoort,
         n_people = Gediplomeerden_1
  )

strat.gender_age_migr_edu = refactor_gender(strat.gender_age_migr_edu)
strat.gender_age_migr_edu = refactor_ages_education(strat.gender_age_migr_edu, codes_ages_education)
strat.gender_age_migr_edu = refactor_migration(strat.gender_age_migr_edu)
strat.gender_age_migr_edu = refactor_education(strat.gender_age_migr_edu, codes_education)

# Reformat stratified dataset, transforming the gender column into a column for each value
strat.gender_age_migr_edu = strat.gender_age_migr_edu %>% 
  pivot_wider(names_from = "education_group", values_from = "n_people")

strat.gender_age_migr_edu = strat.gender_age_migr_edu[strat.gender_age_migr_edu$gender!='total',]
strat.gender_age_migr_edu = strat.gender_age_migr_edu[strat.gender_age_migr_edu$age_group_education!='total',]


strat.gender_age_migr_edu$total = strat.gender_age_migr_edu$low + strat.gender_age_migr_edu$middle  + strat.gender_age_migr_edu$high

agents$age_group_education = as.character(agents$age)
agents$age_group_education[agents$age %in% 30:34] = "age_30_35"
agents$age_group_education[agents$age %in% 35:39] = "age_35_40"
agents$age_group_education[agents$age %in% 40:44] = "age_40_45" 
agents$age_group_education[agents$age %in% 45:49] = "age_45_50" 
agents$age_group_education[agents$age >= 50] = "age_over_50"  


## Conditional Propensities
agents = calc_propens_agents(strat.gender_age_migr_edu, "low", "total", agents, c("age_group_education", "gender", "migration_background") )
agents = calc_propens_agents(strat.gender_age_migr_edu, "middle", "total", agents, c("age_group_education", "gender", "migration_background"))
agents = calc_propens_agents(strat.gender_age_migr_edu, "high", "total", agents, c("age_group_education", "gender", "migration_background") )



agents$absolved_education[which(agents$age < 15 & agents$age > 5) ] = "low"
agents$absolved_education[which( agents$age <= 5) ] = "no_current_edu"

agents$absolved = ""
agents$absolved[agents$current_education == "middle"] = "low"
agents$absolved[agents$current_education == "high" & agents$age <= 22] = "middle"
agents$absolved[agents$current_education == "high" & agents$age > 22] = "high"
agents$diplm_exclude = 0
agents$diplm_exclude[which(is.na(agents$prop_high)| agents$age < 15 | agents$absolved != "") ] = 1