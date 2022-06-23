library("this.path")
library('dplyr')
setwd(this.path::this.dir())
source('utils.R')
source('tabea-functions.R')

############################################################################################
######################## Data Preparation and Application ##################################
############################################################################################

# move to data folder
setwd(paste(this.path::this.dir(), "/data", sep = ""))

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

############################################################################################
################################ Marginal distribution #####################################
############################################################################################

## Load neighborhood dataset: n people per age group and neighbourhood 

pop_marginal_distributions = read.csv("marginal_distributions_TheHague_2019_84583NED.csv", sep = ";")
marginal_distributions = pop_marginal_distributions[which(pop_marginal_distributions$SoortRegio_2 == "Buurt     "),] # select neighborhood data only
remove(pop_marginal_distributions)

marginal_distributions = marginal_distributions[marginal_distributions$WijkenEnBuurten %in% DHZW_neighborhood_codes,]
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

############################################################################################
################################# Stratified datasets ######################################
############################################################################################

###################################### Conversion codes ####################################
setwd(paste(this.path::this.dir(), "/data/codes", sep = ""))
codes_age = read.csv("age_codes.csv")
codes_migration = read.csv("migration_backgrounds_codes.csv", sep=";")
codes_education = read.csv("education_codes.csv", sep=";")

###################################### Gender - age ########################################
setwd(paste(this.path::this.dir(), "/data/stratified-datasets", sep = ""))

strat.gender_age = read.csv("gender_age-03759NED.csv", sep = ";") # count of people per lifeyear and gender in all of Amsterdam

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

# remove totals
strat.gender_age = strat.gender_age[!strat.gender_age$age=='total',]
strat.gender_age = strat.gender_age[!strat.gender_age$gender=='total',]

# apply groups
strat.gender_age$age_group = ""
strat.gender_age$age_group[strat.gender_age$age %in% 0:14] = "age_0_15"
strat.gender_age$age_group[strat.gender_age$age %in% 15:24] = "age_15_25"
strat.gender_age$age_group[strat.gender_age$age %in% 25:44] = "age_25_45"
strat.gender_age$age_group[strat.gender_age$age %in% 45:64] = "age_45_65"
strat.gender_age$age_group[strat.gender_age$age %in% 65:105] = "age_over65"
strat.gender_age$age_group[strat.gender_age$age == 'over_95'] = "age_over65"
strat.gender_age$age_group[strat.gender_age$age == 'over_105'] = "age_over65"


tmp_strat.gender_agegroup = strat.gender_age
tmp_strat.gender_agegroup = subset(tmp_strat.gender_agegroup, select=-c(age))
tmp_strat.gender_agegroup = tmp_strat.gender_agegroup %>%
  group_by(age_group, gender) %>%
  mutate(n_people = sum(n_people)) %>%
  unique()

strat.gender_agegroup = data.frame()
males = tmp_strat.gender_agegroup[tmp_strat.gender_agegroup$gender=='male',]$n_people
females = tmp_strat.gender_agegroup[tmp_strat.gender_agegroup$gender=='female',]$n_people
strat.gender_agegroup = rbind(strat.gender_agegroup, males)
strat.gender_agegroup = rbind(strat.gender_agegroup, females)

colnames(strat.gender_agegroup) = unique(tmp_strat.gender_agegroup$age_group)
strat.gender_agegroup$total<-rowSums(strat.gender_agegroup[,])
gender = c('male', 'female')
strat.gender_agegroup=cbind(strat.gender_agegroup, gender)

remove(tmp_strat.gender_agegroup)

############################################################################################
# Age - gender - migration background
strat.gender_age_migr = read.csv("gender_age_migration-84910NED.csv", sep=";")

strat.gender_age_migr = strat.gender_age_migr %>%
  select(Geslacht,
         Leeftijd,
         Migratieachtergrond,
         BevolkingOp1Januari_1
  ) %>%
  rename(gender = Geslacht,
         age_code = Leeftijd,
         migration_background_code = Migratieachtergrond,
         n_people = BevolkingOp1Januari_1
  )

# Refactor gender, age, migration
strat.gender_age_migr = refactor_gender(strat.gender_age_migr)
strat.gender_age_migr = refactor_age(strat.gender_age_migr, codes_age)
strat.gender_age_migr = refactor_migration(strat.gender_age_migr, codes_migration)

############################################################################################

# Gender - age - migration background - education

strat.gender_age_edu_migr = read.csv("gender_age_education_migration-71493NED.csv", sep=";")

strat.gender_age_edu_migr = strat.gender_age_edu_migr %>%
  select(Geslacht,
         Leeftijd,
         Onderwijssoort,
         Migratieachtergrond,
         Gediplomeerden_1
  ) %>%
  rename(gender = Geslacht,
         age_code = Leeftijd,
         education_code = Onderwijssoort,
         migration_background_code = Migratieachtergrond,
         n_people = Gediplomeerden_1
  )

# Refactor gender, age, migration, education
strat.gender_age_edu_migr = refactor_gender(strat.gender_age_edu_migr)
strat.gender_age_edu_migr = refactor_age(strat.gender_age_edu_migr, codes_age)
strat.gender_age_edu_migr = refactor_migration(strat.gender_age_edu_migr, codes_migration)
strat.gender_age_edu_migr = refactor_education(strat.gender_age_edu_migr, codes_education)


#####################################################################################################
######################## Start with a spatial reference and initial variable:  ######################
################################ gender within neighborhood #######################################

# create empty synthetic population dataframe with number of agents
# Note: CBS must have made a mistake, probably with the age grouping, because the total is larger than the general population.
# So, I initialise the synthetic population size with the sum of genders, otherwise the script cannot work.

sum(sum(marginal_distributions['gender_male']), sum(marginal_distributions['gender_female'])) # 78615
sum(marginal_distributions['tot_pop'])  # 78635

population_size = sum(sum(marginal_distributions['gender_male']), sum(marginal_distributions['gender_female']))

agents = gen_agent_df(population_size)

# initialise synthetic population with gender and neighbourhood code
agents$gender = ""
agents$neighb_code = ""

# reformat column names
gender_neigh_frequencies = marginal_distributions %>%
  select(neighb_code,
         gender_male,
         gender_female) %>%
  rename(male = gender_male,
         female = gender_female)

# distribute genders on the synthetic population
genders = c('male', 'female')
n = 0 # to accumulate how many people have been added already
for(i in 1:nrow(gender_neigh_frequencies)){   # for each neighborhood
  for(gender in genders) {            # for each group age
    nr_people = gender_neigh_frequencies[i, gender] # how many people for this neighborhood and group age
    agents$gender[(n+1):(n+nr_people)] = gender # copy the group age
    agents$neighb_code[(n+1):(n+nr_people)] = gender_neigh_frequencies$neighb_code[i] # copy the neighborhood cod
    n = n + nr_people
  }
}
remove(gender_neigh_frequencies)
remove(genders)

# shuffle order
agents = agents[sample(nrow(agents)),]

############################################ #############################################################################
########## First Application of the conditional propensity & neighborhood constraint functions: ##########################
######################################## Variable: age_group #############################################################
##########################################################################################################################

group_ages = c('age_0_15', 'age_15_25', 'age_25_45', 'age_45_65', 'age_over65')

# calculate propensities for each age group, based on gender
for (group_age in unique(group_ages)) {
  agents = calc_propens_agents(strat.gender_agegroup, group_age, 'total', agents, c("gender"))
}

# distribute age groups on the synthetic population
agents = distr_attr_strat_n_neigh_stats_3plus(agent_df =  agents,
                                              neigh_df =  marginal_distributions,
                                              neigh_ID =  "neighb_code",
                                              variable =  "group_age", 
                                              list_var_classes_neigh_df =  group_ages, 
                                              list_agent_propens =  c("prop_age_0_15",
                                                                      "prop_age_15_25",
                                                                      "prop_age_25_45",
                                                                      "prop_age_45_65",
                                                                      "prop_age_over65"),
                                              list_class_names =  group_ages
                                              )

# remove useless columns
agents = subset(agents, select=-c(random_scores,
                                  prop_age_0_15,
                                  prop_age_15_25,
                                  prop_age_25_45,
                                  prop_age_45_65,
                                  prop_age_over65)
                )

# cross-validation of group ages
neigh_valid = crossvalid(valid_df = marginal_distributions,
                         agent_df = agents,
                         join_var = "neighb_code",
                         list_valid_var = group_ages, 
                         agent_var = "group_age",
                         list_agent_attr = group_ages
)

############################################################################################################################
################################ Translating age groups into interger age, based on age stratified dataset #############################
############################################################################################################################

# preparation of the frequency table
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
    size = nrow(agents[agents$group_age==group_age,]),
    replace=TRUE,
    prob=strat.gender_age[strat.gender_age$age_group==group_age,]$group_propensity
  ) # sample from age frequency distribution
  
  agents[agents$group_age==group_age,]$age = sample # apply to synthetic population dataset
}
agents$age = as.numeric(agents$age)