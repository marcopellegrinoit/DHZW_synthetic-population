library(GenSynthPop)
library(dplyr)

library("this.path")
setwd(this.path::this.dir())
source('utils.R')

flag_validation_plots = TRUE

########################## Conversion codes ####################################

setwd(paste(this.path::this.dir(), "/data/codes", sep = ""))
codes_age = read.csv("codes_age.csv", sep=",")
codes_agegroup20 = read.csv("codes_agegroup20.csv", fileEncoding="UTF-8-BOM")
codes_ages_education = read.csv("codes_ages_education.csv", fileEncoding="UTF-8-BOM", sep=";")
codes_education = read.csv("codes_education.csv", fileEncoding="UTF-8-BOM")

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

## Load The Hague neighborhood dataset: n people per age group and neighbourhood 
setwd(paste(this.path::this.dir(), "/data", sep = ""))
pop_df_MarginalDistr = read.csv("marginal_distributions_TheHague_2019_84583NED.csv", sep = ";")
df_MarginalDistr = pop_df_MarginalDistr[which(pop_df_MarginalDistr$SoortRegio_2 == "Buurt     "),] # select neighborhood data only
remove(pop_df_MarginalDistr)

# filter DHZW area
df_MarginalDistr = df_MarginalDistr[df_MarginalDistr$WijkenEnBuurten %in% DHZW_neighborhood_codes,]

# filter and rename only useful attributes
df_MarginalDistr = df_MarginalDistr %>%
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
         NietWestersTotaal_18,
         OpleidingsniveauLaag_64,
         OpleidingsniveauMiddelbaar_65,
         OpleidingsniveauHoog_66
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
         migration_non_west = NietWestersTotaal_18,
         education_absolved_low = OpleidingsniveauLaag_64,
         education_absolved_middle = OpleidingsniveauMiddelbaar_65,
         education_absolved_high = OpleidingsniveauHoog_66
  )
df_MarginalDistr[df_MarginalDistr$education_absolved_low=='       .',]$education_absolved_low=0
df_MarginalDistr[df_MarginalDistr$education_absolved_middle=='       .',]$education_absolved_middle=0
df_MarginalDistr[df_MarginalDistr$education_absolved_high=='       .',]$education_absolved_high=0
df_MarginalDistr$education_absolved_low=as.numeric(df_MarginalDistr$education_absolved_low)
df_MarginalDistr$education_absolved_middle=as.numeric(df_MarginalDistr$education_absolved_middle)
df_MarginalDistr$education_absolved_high=as.numeric(df_MarginalDistr$education_absolved_high)

################################################################################
## Initialise synthetic population with age groups withing neigbbourhoods
################################################################################

# create empty synthetic population dataframe with number of agents
# Note: CBS must have made a mistake, probably with the age grouping, because the total is larger than the general population.
# So, I initialise the synthetic population size with the sum of group ages, otherwise the script cannot work.

group_ages = c('age_0_15', 'age_15_25', 'age_25_45', 'age_45_65', 'age_over65')
population_size = sum(df_MarginalDistr[group_ages]) # 78655
df_SynthPop = gen_agent_df(population_size)

# Distribute the agents across the age groups and neighborhoods
df_SynthPop = distr_agent_neigh_age_group(neigh_df = df_MarginalDistr,
                                       agent_df = df_SynthPop,
                                       neigh_id = "neighb_code",
                                       age_colnames = group_ages)

################################################################################
## Translate age groups into interger age
################################################################################

# Note: since the dataset is municipality aggregated, I can only calculate each age proportion and then use it to sample

# Load dataset
setwd(paste(this.path::this.dir(), "/data/stratified-datasets", sep = ""))
df_StratGender = read.csv("gender_age-03759NED.csv", sep = ";") # count of people per lifeyear and gender in all of Amsterdam

# Select and translate useful attributes
df_StratGender = df_StratGender %>%
  select(Geslacht,
         Leeftijd,
         BevolkingOp1Januari_1
  ) %>%
  rename(gender = Geslacht,
         age_code = Leeftijd,
         n_people = BevolkingOp1Januari_1)

# Refactor gender, age
df_StratGender = refactor_gender(df_StratGender)
df_StratGender = refactor_age(df_StratGender, codes_age)
df_StratGender = df_StratGender %>% filter(!is.na(as.numeric(age))) # filter only numeric ages. Anyway, the highest age is already 104
df_StratGender$age = as.numeric(df_StratGender$age)

# Reformat stratified dataset, transforming the gender column into a column for each value
df_StratGender = restructure_one_var_marginal(df = df_StratGender,
                                                variable = 'gender',
                                                countsname = 'n_people')

# create group ages in the stratified dataset
df_StratGender$age_group = "age_over65" # default for non-numeric
df_StratGender$age_group[df_StratGender$age %in% 0:14] = "age_0_15"
df_StratGender$age_group[df_StratGender$age %in% 15:24] = "age_15_25"
df_StratGender$age_group[df_StratGender$age %in% 25:44] = "age_25_45"
df_StratGender$age_group[df_StratGender$age %in% 45:64] = "age_45_65"
df_StratGender$age_group[df_StratGender$age %in% 65:105] = "age_over65"

# for each individual age, calculate its proportion to the total of each age group
df_StratGender = df_StratGender %>%
  group_by(age_group) %>%
  mutate(group_propensity = total/sum(total))

# for each group age of the synthetic population, sample the age from stratified dataset following the the frequency distribution
df_SynthPop$age=''
for(group_age in group_ages){
  sample <- sample(
    x = df_StratGender[df_StratGender$age_group==group_age,]$age,
    size = nrow(df_SynthPop[df_SynthPop$age_group==group_age,]),
    replace=TRUE,
    prob=df_StratGender[df_StratGender$age_group==group_age,]$group_propensity
  ) # sample from age frequency distribution
  
  df_SynthPop[df_SynthPop$age_group==group_age,]$age = sample # apply to synthetic population dataset
}
df_SynthPop$age = as.numeric(df_SynthPop$age)

################################################################################
## Gender generation based on age
################################################################################

# Compute conditional propensities
df_SynthPop = calc_propens_agents(dataframe = df_StratGender,
                               variable = "female",
                               total_population = "total",
                               agent_df = df_SynthPop,
                               list_conditional_var = c("age")
)

# Distribute attributes
df_SynthPop = distr_attr_strat_neigh_stats_binary(agent_df = df_SynthPop,
                                               neigh_df = df_MarginalDistr,
                                               neigh_ID = "neighb_code",
                                               variable=  "gender",
                                               list_var_classes_neigh_df = c("gender_male", "gender_female"),
                                               list_agent_propens =  c("prop_female"),
                                               list_class_names = c("female", "male")
)

# Remove extra columns
df_SynthPop = subset(df_SynthPop, select=-c(prop_female, random_scores))

################################################################################
## Validation and analysis

if (flag_validation_plots) {
  # calculate cross-validation neighb_code - gender, with neighborhood totals
  df_ValidationGender = validation(df_real_distr = df_MarginalDistr,
                                       df_synt_pop = df_SynthPop,
                                       join_var = "neighb_code",
                                       list_real_df_var = c("gender_male", "gender_female"), 
                                       var_pred_df = "gender",
                                       list_values = c("male", "female")
  )
  
  # plot accuracy heatmap
  plot_heatmap(df = df_ValidationGender,
               join_var = 'neighb_code',
               var = 'gender')
  
  plot_syth_pop_age_density(df_SynthPop)
  
  plot_syth_strat_age_density(df_SynthPop, df_StratGender)
  
  # calculate total R2 score
  df_ValidationGender.R2 = R_squared(df_ValidationGender$real, df_ValidationGender$pred) 
}

################################################################################
## Migration background generation based on age and gender
################################################################################

# Load stratified dataset
setwd(paste(this.path::this.dir(), "/data/stratified-datasets", sep = ""))
df_StratMigration = read.csv("gender_age_migration-84910NED.csv", sep = ";")

# Select interesting attributes
df_StratMigration = df_StratMigration %>%
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
df_StratMigration = refactor_age_group_20(df_StratMigration, codes_agegroup20)
df_StratMigration = refactor_gender(df_StratMigration)
df_StratMigration = refactor_migration(df_StratMigration)

# Reformat stratified dataset, transforming the migration background column into a column for each value
df_StratMigration = restructure_one_var_marginal(df = df_StratMigration,
                                                     variable = 'migration_background',
                                                     countsname = 'n_people')

# Classify synthetic population ages into age groups to link to the stratified dataset
df_SynthPop$age_group_20 = ""
df_SynthPop$age_group_20[df_SynthPop$age %in% 0:4] = "age_0_5"
df_SynthPop$age_group_20[df_SynthPop$age %in% 5:9] = "age_5_10"
df_SynthPop$age_group_20[df_SynthPop$age %in% 10:14] =  "age_10_15"
df_SynthPop$age_group_20[df_SynthPop$age %in% 15:19] = "age_15_20"
df_SynthPop$age_group_20[df_SynthPop$age %in% 20:24] =  "age_20_25" 
df_SynthPop$age_group_20[df_SynthPop$age %in% 25:29] = "age_25_30"
df_SynthPop$age_group_20[df_SynthPop$age %in% 30:34] =  "age_30_35"
df_SynthPop$age_group_20[df_SynthPop$age %in% 35:39] =  "age_35_40" 
df_SynthPop$age_group_20[df_SynthPop$age %in% 40:44] = "age_40_45"
df_SynthPop$age_group_20[df_SynthPop$age %in% 45:49] = "age_45_50"
df_SynthPop$age_group_20[df_SynthPop$age %in% 50:54] =  "age_50_55"
df_SynthPop$age_group_20[df_SynthPop$age %in% 55:59] = "age_55_60"
df_SynthPop$age_group_20[df_SynthPop$age %in% 60:64] =  "age_60_65" 
df_SynthPop$age_group_20[df_SynthPop$age %in% 65:69] = "age_65_70"
df_SynthPop$age_group_20[df_SynthPop$age %in% 70:74] =  "age_70_75"
df_SynthPop$age_group_20[df_SynthPop$age %in% 75:79] =  "age_75_80" 
df_SynthPop$age_group_20[df_SynthPop$age %in% 80:84] =  "age_80_85" 
df_SynthPop$age_group_20[df_SynthPop$age %in% 85:89] = "age_85_90"
df_SynthPop$age_group_20[df_SynthPop$age %in% 90:94] =  "age_90_95"
df_SynthPop$age_group_20[df_SynthPop$age %in% 95:104] =  "age_over_95"

# Calculate the missing Dutch migration background in the overall marginal distribution
df_MarginalDistr$migration_Dutch = df_MarginalDistr$tot_pop - (df_MarginalDistr$migration_west + df_MarginalDistr$migration_non_west)

## Conditional Propensities
df_SynthPop = calc_propens_agents(df_StratMigration, "Dutch", "total", df_SynthPop, c("age_group_20", "gender") )
df_SynthPop = calc_propens_agents(df_StratMigration, "Western", "total", df_SynthPop, c("age_group_20", "gender") )
df_SynthPop = calc_propens_agents(df_StratMigration, "Non_Western", "total", df_SynthPop, c("age_group_20", "gender") )

# Distribute values
df_SynthPop = distr_attr_strat_neigh_stats_3plus(agent_df =  df_SynthPop,
                                              neigh_df =  df_MarginalDistr,
                                              neigh_ID =  "neighb_code",
                                              variable =  "migration_background", 
                                              list_var_classes_neigh_df =  c("migration_Dutch", "migration_west", "migration_non_west"), 
                                              list_agent_propens =  c("prop_Dutch", "prop_Western", "prop_Non_Western"),
                                              list_class_names =  c("Dutch", "Western", "Non_Western"))

# Remove extra columns
df_SynthPop = subset(df_SynthPop, select=-c(prop_Dutch, prop_Western, prop_Non_Western, random_scores, age_group_20))

################################################################################
## Validation and analysis

if (flag_validation_plots) {
  # calculate cross-validation neighb_code - gender, with neighborhood totals
  df_ValidationMigration = validation(df_real_distr = df_MarginalDistr,
                                          df_synt_pop = df_SynthPop,
                                          join_var = "neighb_code",
                                          list_real_df_var = c("migration_Dutch", "migration_west", "migration_non_west"), 
                                          var_pred_df = "migration_background",
                                          list_values = c("Dutch", "Western", "Non_Western")
  )
  
  # plot accuracy heatmap
  plot_heatmap(df = df_ValidationMigration,
               join_var = 'neighb_code',
               var = 'migration_background')
  
  # calculate total R2 score
  df_ValidationMigration.R2 = R_squared(df_ValidationMigration$real, df_ValidationMigration$pred) 
}

################################################################################
## Generate education based on group age, gender and migration
################################################################################

# Load stratified dataset
setwd(paste(this.path::this.dir(), "/data/stratified-datasets", sep = ""))
df_StratEduAbsolved = read.csv("edu_absolved-71493NED.csv", sep = ";")
df_StratEduCurrent = read.csv("edu_current-71450NED.csv", sep = ";")

# Select interesting attributes
df_StratEduAbsolved = df_StratEduAbsolved %>%
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
df_StratEduCurrent = df_StratEduCurrent %>%
  select(Geslacht,
         Leeftijd,
         Migratieachtergrond,
         Onderwijssoort,
         LeerlingenDeelnemersStudenten_1
         
  ) %>%
  rename(gender = Geslacht,
         age_code = Leeftijd,
         migration_background_code = Migratieachtergrond,
         education_code = Onderwijssoort,
         n_people = LeerlingenDeelnemersStudenten_1
  )

df_StratEduAbsolved = refactor_gender(df_StratEduAbsolved)
df_StratEduAbsolved = refactor_ages_education(df_StratEduAbsolved, codes_ages_education)
df_StratEduAbsolved = refactor_migration(df_StratEduAbsolved)
df_StratEduAbsolved = refactor_education(df_StratEduAbsolved, codes_education)

df_StratEduCurrent = refactor_gender(df_StratEduCurrent)
df_StratEduCurrent = refactor_ages_education(df_StratEduCurrent, codes_ages_education)
df_StratEduCurrent = refactor_migration(df_StratEduCurrent)
df_StratEduCurrent = refactor_education(df_StratEduCurrent, codes_education)

# Create new group ages in the synthetic population
df_SynthPop$age_group_education = as.character(df_SynthPop$age)
df_SynthPop$age_group_education[df_SynthPop$age %in% 30:34] = "age_30_35"
df_SynthPop$age_group_education[df_SynthPop$age %in% 35:39] = "age_35_40"
df_SynthPop$age_group_education[df_SynthPop$age %in% 40:44] = "age_40_45" 
df_SynthPop$age_group_education[df_SynthPop$age %in% 45:49] = "age_45_50" 
df_SynthPop$age_group_education[df_SynthPop$age >= 50] = "age_over_50"

# Reformat and combine education stratified information
df_StratEducation = unique(df_StratEduAbsolved[, c("age_group_education" ,  "gender"  , "migration_background")])
for(n in 1:nrow(df_StratEducation)){
  df_StratEducation$absolved_high[n] = sum(df_StratEduAbsolved$n_people[which(df_StratEduAbsolved$age_group_education == df_StratEducation$age_group_education[n] & df_StratEduAbsolved$gender == df_StratEducation$gender[n] & df_StratEduAbsolved$migration_background == df_StratEducation$migration_background[n] & df_StratEduAbsolved$education_level == "high")])
  df_StratEducation$absolved_middle[n] = sum(df_StratEduAbsolved$n_people[which(df_StratEduAbsolved$age_group_education == df_StratEducation$age_group_education[n] & df_StratEduAbsolved$gender == df_StratEducation$gender[n] & df_StratEduAbsolved$migration_background == df_StratEducation$migration_background[n] & df_StratEduAbsolved$education_level == "middle")])
  df_StratEducation$absolved_low[n] = sum(df_StratEduAbsolved$n_people[which(df_StratEduAbsolved$age_group_education == df_StratEducation$age_group_education[n] & df_StratEduAbsolved$gender == df_StratEducation$gender[n] & df_StratEduAbsolved$migration_background == df_StratEducation$migration_background[n] & df_StratEduAbsolved$education_level == "low")])
  df_StratEducation$absolved_tot[n] = sum(df_StratEduAbsolved$n_people[which(df_StratEduAbsolved$age_group_education == df_StratEducation$age_group_education[n] & df_StratEduAbsolved$gender == df_StratEducation$gender[n] & df_StratEduAbsolved$migration_background == df_StratEducation$migration_background[n] & df_StratEduAbsolved$education_level != "")])
  
  df_StratEducation$current_high[n] = sum(df_StratEduCurrent$n_people[which(df_StratEduCurrent$age_group_education == df_StratEducation$age_group_education[n] & df_StratEduCurrent$gender == df_StratEducation$gender[n] & df_StratEduCurrent$migration_background == df_StratEducation$migration_background[n] & df_StratEduCurrent$education_level == "high")])
  df_StratEducation$current_middle[n] = sum(df_StratEduCurrent$n_people[which(df_StratEduCurrent$age_group_education == df_StratEducation$age_group_education[n] & df_StratEduCurrent$gender == df_StratEducation$gender[n] & df_StratEduCurrent$migration_background == df_StratEducation$migration_background[n] & df_StratEduCurrent$education_level == "middle")])
  df_StratEducation$current_low[n] = sum(df_StratEduCurrent$n_people[which(df_StratEduCurrent$age_group_education == df_StratEducation$age_group_education[n] & df_StratEduCurrent$gender == df_StratEducation$gender[n] & df_StratEduCurrent$migration_background == df_StratEducation$migration_background[n] & df_StratEduCurrent$education_level == "low")])
  
  df_StratEducation$current_total[n] = sum(df_StratEduCurrent$n_people[which(df_StratEduCurrent$age_group_education==df_StratEducation$age_group_education[n] & df_StratEduCurrent$gender == df_StratEducation$gender[n] & df_StratEduCurrent$migration_background == df_StratEducation$migration_background[n])])
  df_StratEducation$current_no_edu[n] = (df_StratEducation$current_total[n] - sum(df_StratEducation$current_low[n], df_StratEducation$current_middle[n], df_StratEducation$current_high[n]))
}

# Calculate propensities for both current and absolved education
df_SynthPop = calc_propens_agents(dataframe =  df_StratEducation, variable = "absolved_high", total_population =  "absolved_tot", agent_df =  df_SynthPop, list_conditional_var = c("age_group_education", "gender", "migration_background") )
df_SynthPop = calc_propens_agents(dataframe =  df_StratEducation, variable = "absolved_middle", total_population =  "absolved_tot", agent_df =  df_SynthPop, list_conditional_var = c("age_group_education", "gender", "migration_background") )
df_SynthPop = calc_propens_agents(dataframe =  df_StratEducation, variable = "absolved_low", total_population =  "absolved_tot", agent_df =  df_SynthPop, list_conditional_var = c("age_group_education", "gender", "migration_background") )
df_SynthPop = calc_propens_agents(dataframe =  df_StratEducation, variable = "current_high", total_population =  "current_total", agent_df =  df_SynthPop, list_conditional_var = c("age_group_education", "gender", "migration_background") )
df_SynthPop = calc_propens_agents(dataframe =  df_StratEducation, variable = "current_middle", total_population =  "current_total", agent_df =  df_SynthPop, list_conditional_var = c("age_group_education", "gender", "migration_background") )
df_SynthPop = calc_propens_agents(dataframe =  df_StratEducation, variable = "current_low", total_population =  "current_total", agent_df =  df_SynthPop, list_conditional_var = c("age_group_education", "gender", "migration_background") )
df_SynthPop = calc_propens_agents(dataframe =  df_StratEducation, variable = "current_no_edu", total_population =  "current_total", agent_df =  df_SynthPop, list_conditional_var = c("age_group_education", "gender", "migration_background") )

# Refine and distribute current education
df_SynthPop$current_edu_exclude = 0
df_SynthPop$current_edu_exclude[which(df_SynthPop$age<15)] = 1

df_SynthPop = distr_attr_cond_prop(agent_df = df_SynthPop,
                                variable=  "current_education",
                                list_agent_propens =  c("prop_current_low",  "prop_current_middle", "prop_current_high", "prop_current_no_edu"),
                                list_class_names = c("low", "middle", "high", "no_current_edu"),
                                agent_exclude = "current_edu_exclude"
                                )

# Remove extra columns
df_SynthPop = subset(df_SynthPop, select=-c(prop_current_low, prop_current_middle, prop_current_high, prop_current_no_edu, random_scores, age_group_education))
                                            
# Refine values
df_SynthPop$current_education[which(df_SynthPop$age > 5 & df_SynthPop$age < 15) ] = "low" # students between 5 and 15 are obliged to low level schools
df_SynthPop$current_education[which(df_SynthPop$age <= 5) ] = "no_current_edu" # individuals younger than 5 do not go to school at all
df_SynthPop[df_SynthPop$current_education == 0,]$current_education = 'no_current_edu' # reformat to string


################################################################################
# Absolved education

# Manually contruct the absolved education based on the previous created current education
df_SynthPop$absolved = ""
df_SynthPop$absolved[df_SynthPop$current_education == "middle"] = "low" # if the current education is middle, the absolved cannot be higher than low
df_SynthPop$absolved[df_SynthPop$current_education == "high" & df_SynthPop$age <= 22] = "middle" # if current is high and yonger than 22 it cannot have another high degree
df_SynthPop$absolved[df_SynthPop$current_education == "high" & df_SynthPop$age > 22] = "high" # if current is high and it is older than 22, it means it is doing a master degree and already achieve a bachelor

# re adjust the marginal, removing the people fow which I already manually generated the absolved education
df_MarginalDistr$LowerEdu = 0
df_MarginalDistr$MiddleEdu = 0
df_MarginalDistr$HigherEdu = 0
for(i in 1:nrow(df_MarginalDistr)){
  df_MarginalDistr[i,c("LowerEdu")] = df_MarginalDistr[i,c("education_absolved_low")] - nrow(df_SynthPop[df_SynthPop$absolved == "low" & df_SynthPop$neighb_code == df_MarginalDistr$neighb_code[i] & df_SynthPop$age >= 15,])
  df_MarginalDistr[i,c("MiddleEdu" )] = df_MarginalDistr[i,c("education_absolved_middle" )]- nrow(df_SynthPop[df_SynthPop$absolved == "middle" & df_SynthPop$neighb_code == df_MarginalDistr$neighb_code[i] & df_SynthPop$age >= 15,])
  df_MarginalDistr[i,c("HigherEdu")] = df_MarginalDistr[i,c("education_absolved_high")] - nrow(df_SynthPop[df_SynthPop$absolved == "high" & df_SynthPop$neighb_code == df_MarginalDistr$neighb_code[i] & df_SynthPop$age >= 15,])
}
df_MarginalDistr$LowerEdu[df_MarginalDistr$LowerEdu < 0] = 0
df_MarginalDistr$MiddleEdu[df_MarginalDistr$MiddleEdu < 0] = 0
df_MarginalDistr$HigherEdu[df_MarginalDistr$HigherEdu < 0] = 0

# exclude from the attribute distribution these agents that already have an absolved education based on the current education, or they are younger than 15 yo.
df_SynthPop$diplm_exclude = 0
df_SynthPop$diplm_exclude[which(is.na(df_SynthPop$prop_absolved_high)|df_SynthPop$age < 15 | df_SynthPop$absolved != "") ] = 1

df_SynthPop = distr_attr_strat_neigh_stats_3plus(agent_df = df_SynthPop,
                                              neigh_df = df_MarginalDistr,
                                              neigh_ID = "neighb_code",
                                              variable=  "absolved_education", 
                                              list_var_classes_neigh_df = c("LowerEdu" , "MiddleEdu" ,"HigherEdu"), 
                                              list_agent_propens =  c("prop_absolved_low",  "prop_absolved_middle", "prop_absolved_high" ), 
                                              list_class_names = c("low", "middle", "high"),
                                              agent_exclude = c("diplm_exclude"))

# Refine values
df_SynthPop$absolved_education[df_SynthPop$absolved != ""] = df_SynthPop$absolved[df_SynthPop$absolved != ""]
df_SynthPop[df_SynthPop$absolved_education == 0,]$absolved_education = 'no_absolved_edu'

# Remove extra columns
df_SynthPop = subset(df_SynthPop, select=-c(prop_absolved_low, prop_absolved_middle, prop_absolved_high, random_scores, absolved, diplm_exclude, excluded))

################################################################################
## Validation and analysis

if (flag_validation_plots) {
  # calculate cross-validation neighb_code - gender, with neighborhood totals
  df_ValidationEduAbsolved = validation(df_real_distr = df_MarginalDistr,
                                          df_synt_pop = df_SynthPop,
                                          join_var = "neighb_code",
                                          list_real_df_var = c("education_absolved_low" , "education_absolved_middle" ,"education_absolved_high"), 
                                          var_pred_df = "absolved_education",
                                          list_values = c("low", "middle", "high")
  )
  
  # plot accuracy heatmap
  plot_heatmap(df = df_ValidationEduAbsolved,
               join_var = 'neighb_code',
               var = 'absolved_education')
  
  # calculate total R2 score
  df_ValidationEduAbsolved.R2 = R_squared(df_ValidationEduAbsolved$real, df_ValidationEduAbsolved$pred) 
}

################################################################################
## Generate household position based on group age and gender
################################################################################

# Load stratified dataset
setwd(paste(this.path::this.dir(), "/data/stratified-datasets", sep = ""))
df_StratHousehold = read.csv("household_gender_age-71488ned.csv", sep = ";", fileEncoding="UTF-8-BOM")

# Rename, translate and reorganise dataset
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

# Save synthetic population
setwd(paste(this.path::this.dir(), "/data/households/distributions", sep = ""))
write.csv(df_StratHousehold, 'household_gender_age-71488NED-formatted.csv', row.names=FALSE)

# Create group ages in the synthetic population
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

# Calculate propensities
df_SynthPop = calc_propens_agents(df_StratHousehold, "children", "total", df_SynthPop, c("age_group", "gender") )
df_SynthPop = calc_propens_agents(df_StratHousehold, "singles", "total", df_SynthPop, c("age_group", "gender") )
df_SynthPop = calc_propens_agents(df_StratHousehold, "couples", "total", df_SynthPop, c("age_group", "gender") )
df_SynthPop = calc_propens_agents(df_StratHousehold, "single_parents", "total", df_SynthPop, c("age_group", "gender") )

# Distribute attributes
df_SynthPop = distr_attr_cond_prop(agent_df = df_SynthPop,
                                   variable = 'household_position',
                                   list_agent_propens = c('prop_children', 'prop_singles', 'prop_couples', 'prop_single_parents'),
                                   list_class_names = c('children', 'singles', 'couples', 'single_parents'))

# Remove extra columns
df_SynthPop = subset(df_SynthPop, select=-c(prop_children, prop_singles, prop_couples, prop_single_parents, random_scores))

# Save synthetic population
setwd(paste(this.path::this.dir(), "/data/synthetic-populations", sep = ""))
write.csv(df_SynthPop, 'synthetic_population_DHZW.csv', row.names=FALSE)