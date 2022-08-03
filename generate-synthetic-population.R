library(GenSynthPop)
library(dplyr)

library("this.path")
setwd(this.path::this.dir())
source('utils.R')

municipality = "den_haag_2019"

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

setwd(paste(this.path::this.dir(), "/data/", municipality, sep = ""))
df_MarginalDistr = read.csv("marginal_distributions_84583NED-formatted.csv", sep = ",")

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
setwd(paste(this.path::this.dir(), "/data/", municipality, "/stratified-datasets", sep = ""))
df_StratGender = read.csv("gender_age-03759NED-formatted.csv", sep = ",")

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
                                                  list_var_classes_neigh_df = c("gender_female", "gender_male"),
                                                  list_agent_propens =  c("prop_female"),
                                                  list_class_names = c("female", "male")
)

# Remove extra columns
df_SynthPop = subset(df_SynthPop, select=-c(prop_female, random_scores, age_group))

################################################################################
## Migration background generation based on age and gender
################################################################################

# Load stratified dataset
setwd(paste(this.path::this.dir(), "/data/", municipality, "/stratified-datasets", sep = ""))
df_StratMigration = read.csv("gender_age_migration-84910NED-formatted.csv", sep = ",")

# Classify synthetic population ages into age groups to link to the stratified dataset
df_SynthPop$age_group = ""
df_SynthPop$age_group[df_SynthPop$age %in% 0:4] = "age_0_5"
df_SynthPop$age_group[df_SynthPop$age %in% 5:9] = "age_5_10"
df_SynthPop$age_group[df_SynthPop$age %in% 10:14] =  "age_10_15"
df_SynthPop$age_group[df_SynthPop$age %in% 15:19] = "age_15_20"
df_SynthPop$age_group[df_SynthPop$age %in% 20:24] =  "age_20_25" 
df_SynthPop$age_group[df_SynthPop$age %in% 25:29] = "age_25_30"
df_SynthPop$age_group[df_SynthPop$age %in% 30:34] =  "age_30_35"
df_SynthPop$age_group[df_SynthPop$age %in% 35:39] =  "age_35_40" 
df_SynthPop$age_group[df_SynthPop$age %in% 40:44] = "age_40_45"
df_SynthPop$age_group[df_SynthPop$age %in% 45:49] = "age_45_50"
df_SynthPop$age_group[df_SynthPop$age %in% 50:54] =  "age_50_55"
df_SynthPop$age_group[df_SynthPop$age %in% 55:59] = "age_55_60"
df_SynthPop$age_group[df_SynthPop$age %in% 60:64] =  "age_60_65" 
df_SynthPop$age_group[df_SynthPop$age %in% 65:69] = "age_65_70"
df_SynthPop$age_group[df_SynthPop$age %in% 70:74] =  "age_70_75"
df_SynthPop$age_group[df_SynthPop$age %in% 75:79] =  "age_75_80" 
df_SynthPop$age_group[df_SynthPop$age %in% 80:84] =  "age_80_85" 
df_SynthPop$age_group[df_SynthPop$age %in% 85:89] = "age_85_90"
df_SynthPop$age_group[df_SynthPop$age %in% 90:94] =  "age_90_95"
df_SynthPop$age_group[df_SynthPop$age %in% 95:104] =  "age_over_95"

## Conditional Propensities
df_SynthPop = calc_propens_agents(df_StratMigration, "Dutch", "total", df_SynthPop, c("age_group", "gender") )
df_SynthPop = calc_propens_agents(df_StratMigration, "Western", "total", df_SynthPop, c("age_group", "gender") )
df_SynthPop = calc_propens_agents(df_StratMigration, "Non_Western", "total", df_SynthPop, c("age_group", "gender") )

# Distribute values
df_SynthPop = distr_attr_strat_neigh_stats_3plus(agent_df =  df_SynthPop,
                                                 neigh_df =  df_MarginalDistr,
                                                 neigh_ID =  "neighb_code",
                                                 variable =  "migration_background", 
                                                 list_var_classes_neigh_df =  c("migration_Dutch", "migration_west", "migration_non_west"), 
                                                 list_agent_propens =  c("prop_Dutch", "prop_Western", "prop_Non_Western"),
                                                 list_class_names =  c("Dutch", "Western", "Non_Western"))

# Remove extra columns
df_SynthPop = subset(df_SynthPop, select=-c(prop_Dutch, prop_Western, prop_Non_Western, random_scores, age_group))

################################################################################
## Generate education based on group age, gender and migration
################################################################################

# Load stratified dataset
setwd(paste(this.path::this.dir(), "/data/", municipality, "/stratified-datasets", sep = ""))
df_StratEduAbsolved = read.csv("edu_absolved-71493NED.csv", sep = ";")
df_StratEduCurrent = read.csv("edu_current-71450NED-formatted.csv", sep = ",")

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


df_StratEduAbsolved = refactor_gender(df_StratEduAbsolved)
df_StratEduAbsolved = refactor_ages_education(df_StratEduAbsolved, codes_ages_education)
df_StratEduAbsolved = refactor_migration(df_StratEduAbsolved)
df_StratEduAbsolved = refactor_education(df_StratEduAbsolved, codes_education)

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
df_SynthPop = subset(df_SynthPop, select=-c(prop_current_low, prop_current_middle, prop_current_high, prop_current_no_edu, random_scores, age_group_education, current_edu_exclude))

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
df_SynthPop$diplm_exclude[which(df_SynthPop$age < 15 | df_SynthPop$absolved != "") ] = 1

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
                                        list_values = c("low", "middle", "high"),
                                        age_limits = TRUE
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
df_SynthPop = calc_propens_agents(df_StratHousehold, "single", "total", df_SynthPop, c("age_group", "gender") )
df_SynthPop = calc_propens_agents(df_StratHousehold, "child", "total", df_SynthPop, c("age_group", "gender") )
df_SynthPop = calc_propens_agents(df_StratHousehold, "couple", "total", df_SynthPop, c("age_group", "gender") )
df_SynthPop = calc_propens_agents(df_StratHousehold, "single_parent", "total", df_SynthPop, c("age_group", "gender") )

## start by distributing single which is also on the marginal distributions
## note: a single household is also a single individual (which is what we have in the stratified dataset)

df_MarginalDistr$pp_no_single = df_MarginalDistr$tot_pop - df_MarginalDistr$hh_single
df_SynthPop = distr_attr_strat_neigh_stats_binary(agent_df = df_SynthPop,
                                                  neigh_df = df_MarginalDistr,
                                                  neigh_ID = "neighb_code",
                                                  variable=  "hh_position_single",
                                                  list_var_classes_neigh_df = c("hh_single", "pp_no_single"),
                                                  list_agent_propens =  c("prop_single"),
                                                  list_class_names = c("single", "no_single")
)

if (flag_validation_plots) {
  # calculate cross-validation neighb_code - hh_single, with neighborhood totals
  df_ValidationSingles = validation(df_real_distr = df_MarginalDistr,
                                        df_synt_pop = df_SynthPop,
                                        join_var = "neighb_code",
                                        list_real_df_var = c("hh_single", "pp_no_single"), 
                                        var_pred_df = "hh_position_single",
                                        list_values = c("single", "no_single"),
                                        age_limits = FALSE
  )

  # calculate total R2 score
  df_ValidationSingles.R2 = R_squared(df_ValidationSingles$real, df_ValidationSingles$pred) 
}

# Distribute remaining attributes
df_SynthPop$singles_exclude = 0
df_SynthPop$singles_exclude[which(df_SynthPop$hh_position_single=='single')] = 1
df_SynthPop = distr_attr_cond_prop(agent_df = df_SynthPop,
                                   variable = 'hh_position',
                                   list_agent_propens = c('prop_child', 'prop_couple', 'prop_single_parent'),
                                   list_class_names = c('child', 'couple', 'single_parent'),
                                   agent_exclude = "singles_exclude")
df_SynthPop$hh_position[which(df_SynthPop$hh_position_single=='single')] = 'single'


# Remove extra columns
df_SynthPop = subset(df_SynthPop, select=-c(hh_position_single, prop_child, prop_single, prop_couple, prop_single_parent, random_scores, singles_exclude, excluded))

# Save synthetic population
setwd(paste(this.path::this.dir(), "/data/", municipality, "/synthetic-populations", sep = ""))
write.csv(df_SynthPop, 'synthetic_population.csv', row.names=FALSE)