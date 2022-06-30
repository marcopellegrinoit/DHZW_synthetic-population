library("this.path")
library('dplyr')
setwd(this.path::this.dir())
source('utils.R')
source('tabea-functions.R')

################################################################################
######################## Data Preparation and Application ######################
################################################################################

flag_validation_plots = FALSE

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

########################## Conversion codes ####################################

setwd(paste(this.path::this.dir(), "/data/codes", sep = ""))
codes_age = read.csv("age_codes.csv", sep=",")
agegroup20_codes = read.csv("agegroup20_codes.csv", fileEncoding="UTF-8-BOM")


############################ Marginal distribution #############################

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
########### Start with a spatial reference and initial variable:  ##############
########################### agegroup within neighborhood #######################

# create empty synthetic population dataframe with number of agents
# Note: CBS must have made a mistake, probably with the age grouping, because the total is larger than the general population.
# So, I initialise the synthetic population size with the sum of group ages, otherwise the script cannot work.
group_ages = c('age_0_15', 'age_15_25', 'age_25_45', 'age_45_65', 'age_over65')

sum(marginal_distributions[group_ages]) # 78655
sum(marginal_distributions['tot_pop'])  # 78635

population_size = sum(marginal_distributions[group_ages]) # 78655
agents = gen_agent_df(population_size)

# initialise synthetic population with agegroup and neighbourhood code
agents$age_group = ""
agents$neighb_code = ""

n = 0 # to accumulate how many people have been added already
for(i in 1:nrow(marginal_distributions)){   # for each neighborhood
  for(group_age in group_ages) {            # for each group age
    nr_people = marginal_distributions[i, group_age] # how many people for this neighborhood and group age
    agents$age_group[(n+1):(n+nr_people)] = group_age # copy the group age
    agents$neighb_code[(n+1):(n+nr_people)] = marginal_distributions$neighb_code[i] # copy the neighborhood cod
    n = n + nr_people
  }
}

# shuffle order
agents = agents[sample(nrow(agents)),]

################################################################################
########### Translating age groups into interger age ###########################
################################################################################

# Note: since the dataset is municipality aggregated, I can only calculate each age proportion and then use it to sample

## Data preparation

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

# preparation of the table
strat.gender_age$n_people = as.numeric(strat.gender_age$n_people)
strat.gender_age$age = as.character(strat.gender_age$age)

males = strat.gender_age[strat.gender_age$gender == "male" ,c("age", "n_people")]
colnames(males) = c('age', 'male')
females = strat.gender_age[strat.gender_age$gender == "female" ,c("age", "n_people")]           
colnames(females) = c('age', 'female')
strat.gender_age = merge(females, males, by= "age")
remove(males)
remove(females)
strat.gender_age$total = strat.gender_age$male + strat.gender_age$female

strat.gender_age <- strat.gender_age[!is.na(as.numeric(as.character(strat.gender_age$age))),]
strat.gender_age$age <- as.numeric(strat.gender_age$age)


# create group ages in the stratified dataset
strat.gender_age$age_group = ""
strat.gender_age$age_group[strat.gender_age$age %in% 0:14] = "age_0_15"
strat.gender_age$age_group[strat.gender_age$age %in% 15:24] = "age_15_25"
strat.gender_age$age_group[strat.gender_age$age %in% 25:44] = "age_25_45"
strat.gender_age$age_group[strat.gender_age$age %in% 45:64] = "age_45_65"
strat.gender_age$age_group[strat.gender_age$age %in% 65:105] = "age_over65"
strat.gender_age = strat.gender_age[order(as.numeric(strat.gender_age$age)),]

# for each individual age, calculate its proportion to the total of each age group
strat.gender_age = strat.gender_age %>%
  group_by(age_group) %>%
  mutate(group_propensity = total/sum(total))

# for each group age of the synthetic population, sample the age from stratified dataset following the the frequency distribution
agents$age=''
for(group_age in group_ages){
  sample <- sample(
    x = strat.gender_age[strat.gender_age$age_group==group_age,]$age,
    size = nrow(agents[agents$age_group==group_age,]),
    replace=TRUE,
    prob=strat.gender_age[strat.gender_age$age_group==group_age,]$group_propensity
    ) # sample from age frequency distribution
  
  agents[agents$age_group==group_age,]$age = sample # apply to synthetic population dataset
}
agents$age = as.numeric(agents$age)


#setwd(paste(this.path::this.dir(), "/data/synthetic-populations", sep = ""))
#write.csv(agents, "Synthetic_population_neighg-age.csv")

################################################################################
## Gender generation based on age

# Compute conditional propensities
agents = calc_propens_agents(strat.gender_age, "female", "total", agents, c("age"))

# Distribute attributes
agents = distr_bin_attr_strat_n_neigh_stats(agent_df = agents,
                                            neigh_df = marginal_distributions,
                                            neigh_ID = "neighb_code",
                                            variable=  "gender",
                                            list_var_classes_neigh_df = c("gender_male", "gender_female"),
                                            list_agent_propens =  c("prop_female"),
                                            list_class_names = c("female", "male")
                                            )
# Remove extra columns
agents = subset(agents, select=-c(prop_female, random_scores))

################################################################################
## Validation and analysis

if (flag_validation_plots) {
  # calculate cross-validation neighb_code - gender, with neighborhood totals
  validation_neigh_gender = validation(df_real_distr = marginal_distributions,
                                  df_synt_pop = agents,
                                  join_var = "neighb_code",
                                  list_real_df_var = c("gender_male", "gender_female"), 
                                  var_pred_df = "gender",
                                  list_values = c("male", "female")
  )
  
  # plot accuracy heatmap
  plot_heatmap(df = validation_neigh_gender,
               join_var = 'neighb_code',
               var = 'gender')
  
  plot_syth_pop_age_density(agents)
  
  plot_syth_strat_age_density(agents, strat.gender_age)
  
  # calculate total R2 score
  validation_neigh_gender.R2 = R_squared(validation_neigh_gender$real, validation_neigh_gender$pred) 
}

# save current synthetic population
#setwd(paste(this.path::this.dir(), "/data/synthetic-populations", sep = ""))
#write.csv(agents, "Synthetic_population_neighg-age-gender.csv")

################################################################################
############################# migration background #############################
################################################################################

## Data Preparation

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
         migration_background = Migratieachtergrond,
         n_people = BevolkingOp1Januari_1
  )

# Refactor group age and gender
strat.gender_age_migr = refactor_age_group_20(strat.gender_age_migr, agegroup20_codes)
strat.gender_age_migr = refactor_gender(strat.gender_age_migr)

# Refactor and subselect interesting migration backgrounds
strat.gender_age_migr$migration_background = gsub("1012600", "Dutch", strat.gender_age_migr$migration_background)
strat.gender_age_migr$migration_background = gsub("2012655", "Western", strat.gender_age_migr$migration_background)
strat.gender_age_migr$migration_background = gsub("2012657", "Non_Western", strat.gender_age_migr$migration_background)
strat.gender_age_migr= strat.gender_age_migr[which(strat.gender_age_migr$migration_background == "Western" | strat.gender_age_migr$migration_background == "Non_Western"  | strat.gender_age_migr$migration_background == "Dutch"),]

################################################################################

# Reformat stratified dataset, transforming the migration background column into a column for each value
strat.gender_age_migr = strat.gender_age_migr %>% 
  pivot_wider(names_from = "migration_background", values_from = "n_people")
# Calculate total
strat.gender_age_migr$total = strat.gender_age_migr$Dutch + strat.gender_age_migr$Western + strat.gender_age_migr$Non_Western

# Classifying ages into age groups to link to the stratified dataset
agents$age_group_20 = ""
agents$age_group_20[agents$age %in% 0:4] = "age_0_5"
agents$age_group_20[agents$age %in% 5:9] = "age_5_10"
agents$age_group_20[agents$age %in% 10:14] =  "age_10_15"
agents$age_group_20[agents$age %in% 15:19] = "age_15_20"
agents$age_group_20[agents$age %in% 20:24] =  "age_20_25" 
agents$age_group_20[agents$age %in% 25:29] = "age_25_30"
agents$age_group_20[agents$age %in% 30:34] =  "age_30_35"
agents$age_group_20[agents$age %in% 35:39] =  "age_35_40" 
agents$age_group_20[agents$age %in% 40:44] = "age_40_45"
agents$age_group_20[agents$age %in% 45:49] = "age_45_50"
agents$age_group_20[agents$age %in% 50:54] =  "age_50_55"
agents$age_group_20[agents$age %in% 55:59] = "age_55_60"
agents$age_group_20[agents$age %in% 60:64] =  "age_60_65" 
agents$age_group_20[agents$age %in% 65:69] = "age_65_70"
agents$age_group_20[agents$age %in% 70:74] =  "age_70_75"
agents$age_group_20[agents$age %in% 75:79] =  "age_75_80" 
agents$age_group_20[agents$age %in% 80:84] =  "age_80_85" 
agents$age_group_20[agents$age %in% 85:89] = "age_85_90"
agents$age_group_20[agents$age %in% 90:94] =  "age_90_95"
agents$age_group_20[agents$age %in% 95:104] =  "age_over_95" 

# Calculate the missing Dutch migration background in the overall marginal distribution
marginal_distributions$migration_Dutch = marginal_distributions$tot_pop - (marginal_distributions$migration_west + marginal_distributions$migration_non_west)

## Conditional Propensities
agents = calc_propens_agents(strat.gender_age_migr, "Dutch", "total", agents, c("age_group_20", "gender") )
agents = calc_propens_agents(strat.gender_age_migr, "Western", "total", agents, c("age_group_20", "gender") )
agents = calc_propens_agents(strat.gender_age_migr, "Non_Western", "total", agents, c("age_group_20", "gender") )

# Distribute values
agents = distr_attr_strat_n_neigh_stats_3plus(agent_df =  agents,
                                              neigh_df =  marginal_distributions,
                                              neigh_ID =  "neighb_code",
                                              variable =  "migration_background", 
                                              list_var_classes_neigh_df =  c("migration_Dutch", "migration_west", "migration_non_west"), 
                                              list_agent_propens =  c("prop_Dutch", "prop_Western", "prop_Non_Western"),
                                              list_class_names =  c("Dutch", "Western", "Non-Western"))
# Remove extra columns
agents = subset(agents, select=-c(prop_Dutch, prop_Western, prop_Non_Western, random_scores, age_group_20))

# save current synthetic population
#setwd(paste(this.path::this.dir(), "/data/synthetic-populations", sep = ""))
#write.csv(agents, "Synthetic_population_neighg-age-gender-migration.csv")

################################################################################
## Validation and analysis

if (flag_validation_plots) {
  # calculate cross-validation neighb_code - gender, with neighborhood totals
  validation_neigh_migration = validation(df_real_distr = marginal_distributions,
                                  df_synt_pop = agents,
                                  join_var = "neighb_code",
                                  list_real_df_var = c("migration_Dutch", "migration_west", "migration_non_west"), 
                                  var_pred_df = "migration_background",
                                  list_values = c("Dutch", "Western", "Non-Western")
  )
  
  # plot accuracy heatmap
  plot_heatmap(df = validation_neigh_migration,
               join_var = 'neighb_code',
               var = 'migration_background')
  
  # calculate total R2 score
  validation_neigh_migration.R2 = R_squared(validation_neigh_migration$real, validation_neigh_migration$pred) 
}