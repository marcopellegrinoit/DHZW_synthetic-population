library("this.path")
library('dplyr')
setwd(this.path::this.dir())
source('utils_marco.R')
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
setwd(paste(this.path::this.dir(), "/data/stratified", sep = ""))

strat.gender_age = read.csv("gender_age_2019_03759NED.csv", sep = ";") # count of people per lifeyear and gender in all of Amsterdam

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


############################################################################################
# Age - gender - migration background
strat.gender_age_migr = read.csv("gender_age_migration_84910NED.csv", sep=";")

strat.gender_age_migr = strat.gender_age_migr %>%
  select(Geslacht,
         Leeftijd,
         Migratieachtergrond,
         BevolkingOp1Januari_1,
         Perioden,
         Generatie
  ) %>%
  rename(gender = Geslacht,
         age_code = Leeftijd,
         migration_background_code = Migratieachtergrond,
         n_people = BevolkingOp1Januari_1,
         period = Perioden,
         generation = Generatie
  )


# Filter total generations
strat.gender_age_migr = strat.gender_age_migr[strat.gender_age_migr$generation == "T001040",]
strat.gender_age_migr = subset(strat.gender_age_migr, select=-c(generation))

# 2019 only
strat.gender_age_migr = strat.gender_age_migr[strat.gender_age_migr$period=='2019JJ00',]
strat.gender_age_migr = subset(strat.gender_age_migr, select=-c(period))

# Refactor gender, age, migration
strat.gender_age_migr = refactor_gender(strat.gender_age_migr)
strat.gender_age_migr = refactor_age(strat.gender_age_migr, codes_age)
strat.gender_age_migr = refactor_migration(strat.gender_age_migr, codes_migration)

############################################################################################

# Gender - age - migration background - education

strat.gender_age_edu_migr = read.csv("gender_age_education_migration_71493NED.csv", sep=";")

strat.gender_age_edu_migr = strat.gender_age_edu_migr %>%
  select(Geslacht,
         Leeftijd,
         Onderwijssoort,
         Migratieachtergrond,
         Perioden,
         Gediplomeerden_1
  ) %>%
  rename(gender = Geslacht,
         age_code = Leeftijd,
        education_code = Onderwijssoort,
         migration_background_code = Migratieachtergrond,
         period = Perioden,
         n_people = Gediplomeerden_1
  )


# 2019 only
strat.gender_age_edu_migr = strat.gender_age_edu_migr[strat.gender_age_edu_migr$period=='2019SJ00',]
strat.gender_age_edu_migr = subset(strat.gender_age_edu_migr, select=-c(period))

# Refactor gender, age, migration, education
strat.gender_age_edu_migr = refactor_gender(strat.gender_age_edu_migr)
strat.gender_age_edu_migr = refactor_age(strat.gender_age_edu_migr, codes_age)
strat.gender_age_edu_migr = refactor_migration(strat.gender_age_edu_migr, codes_migration)
strat.gender_age_edu_migr = refactor_education(strat.gender_age_edu_migr, codes_education)


#####################################################################################################
######################## Start with a spatial reference and initial variable:  ######################
################################ agegroup within neighborhood #######################################

# create empty synthetic population dataframe with number of agents
# Note: CBS must have made a mistake, probably with the age grouping, because the total is larger than the general population.
# So, I initialise the synthetic population size with the sum of group ages, otherwise the script cannot work.
group_ages = c('age_0_15', 'age_15_25', 'age_25_45', 'age_45_65', 'age_over65')

sum(marginal_distributions[group_ages]) # 78655
sum(marginal_distributions['tot_pop'])  # 78635

population_size = sum(marginal_distributions[group_ages])

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

############################################################################################################################
################################ Translating age groups into interger age, based on age stratified dataset #############################
############################################################################################################################
# since the dataset is municipality aggregated, I can only calculate each age proportion and then use it

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

agents$age = ''

new_agents = agents
new_agents = new_agents[0,]

for(group_age in group_ages){
  for (i in which(strat.gender_age$age_group == group_age)){
    age_row = strat.gender_age[i,]
    age_to_insert = age_row$age
    group_propensity = age_row$group_propensity
    
    agents_subset = agents[agents$age_group==group_age,] %>%
      sample_frac(size = age_row$group_propensity, replace = FALSE)%>%
      mutate(age = age_to_insert)
    
    new_agents = rbind(new_agents, agents_subset)
  }
}
agents = new_agents

############################################ #############################################################################
########## First Application of the conditional propensity & neighborhood constraint functions: ############################
######################################## Variable: Sex ###################################################################
##########################################################################################################################

############################### sex (based on sex per neighborhood and sex per age statistics) ######################################################################

## Compute conditional Propensities
agents = calc_propens_agents(strat.gender_age, "female", "total", agents, c("age"))

## assigning attributes to agents
agents = distr_bin_attr_strat_n_neigh_stats(agent_df = agents,
                                            neigh_df = marginal_distributions,
                                            neigh_ID = "neighb_code",
                                            variable=  "gender",
                                            list_var_classes_neigh_df = c("gender_male", "gender_female"),
                                            list_agent_propens =  c("prop_female"),
                                            list_class_names = c("female", "male")
                                            )
agents = subset(agents, select=-c(prop_female, random_scores))


## cross validating with neighborhood and stratified totals
neigh_valid = crossvalid(valid_df = marginal_distributions,
                         agent_df = agents,
                         join_var = "neighb_code",
                         list_valid_var = c("gender_male", "gender_female"), 
                         agent_var = "gender",
                         list_agent_attr = c("male", "female")
                         )

strat_valid = crossvalid(valid_df = strat.gender_age,
                         agent_df = agents,
                         join_var = "age",
                         list_valid_var = c("male", "female"), 
                         agent_var = "gender",
                         list_agent_attr = c("male", "female")
                         )

# save first step
write.csv(agents, "Synthetic_population_v1.csv")
agents = read.csv("Synthetic_population_v1.csv")

##################################################################
################## migration background ##########################
##################################################################

## Data Preparation
strat.gender_age_migr = strat.gender_age_migr[, c("age", "Geslacht" , "Migratieachtergrond" ,  "TotaalAantalPersonenInHuishoudens_1" , "ThuiswonendKind_2" , 
                                "Alleenstaand_3" , "TotaalSamenwonendePersonen_4" ,  "PartnerInNietGehuwdPaarZonderKi_5" ,
                                "PartnerInGehuwdPaarZonderKinderen_6" , "PartnerInNietGehuwdPaarMetKinderen_7",  "PartnerInGehuwdPaarMetKinderen_8",   
                                "OuderInEenouderhuishouden_9" )]

migrat_age = rbind(migrat_age, migrat_age)
migrat_age$sex =""
migrat_age$sex[0:21] = "female"
migrat_age$sex[22:42] = "male"
migrat_age$tot = 0
migrat_age$Western = 0
migrat_age$Dutch = 0
migrat_age$Non_Western = 0

for(i in 1:nrow(migrat_age)){
  migrat_age$tot[i] = sum(strat.gender_age_migr$TotaalAantalPersonenInHuishoudens_1[which(strat.gender_age_migr$age == migrat_age$age[i] & strat.gender_age_migr$Geslacht == migrat_age$sex[i])])
  migrat_age$Western[i] = strat.gender_age_migr$TotaalAantalPersonenInHuishoudens_1[which(strat.gender_age_migr$age == migrat_age$age[i] & strat.gender_age_migr$Geslacht == migrat_age$sex[i] & strat.gender_age_migr$Migratieachtergrond == "\"Westerse migratieachtergrond\"" )]
  migrat_age$Dutch[i] = strat.gender_age_migr$TotaalAantalPersonenInHuishoudens_1[which(strat.gender_age_migr$age == migrat_age$age[i] & strat.gender_age_migr$Geslacht == migrat_age$sex[i] & strat.gender_age_migr$Migratieachtergrond == "\"Nederlandse achtergrond\"" )]
  migrat_age$Non_Western[i] = strat.gender_age_migr$TotaalAantalPersonenInHuishoudens_1[which(strat.gender_age_migr$age == migrat_age$age[i] & strat.gender_age_migr$Geslacht == migrat_age$sex[i] & strat.gender_age_migr$Migratieachtergrond == "\"Niet-westerse migratieachtergrond\"")]
}


agents$age_group_20 = "" #classifying ages into age groups to link to migrant dataset
# "95 jaar of ouder" "0 tot 5 jaar"     "5 tot 10 jaar"    "10 tot 15 jaar"   "15 tot 20 jaar"   "20 tot 25 jaar"   "25 tot 30 jaar"  
# "30 tot 35 jaar"   "35 tot 40 jaar"   "40 tot 45 jaar"   "45 tot 50 jaar"   "50 tot 55 jaar"   "55 tot 60 jaar"   "60 tot 65 jaar"   "65 tot 70 jaar"  
# "70 tot 75 jaar"   "75 tot 80 jaar"   "80 tot 85 jaar"   "85 tot 90 jaar"   "90 tot 95 jaar"
agents$age_group_20[agents$age %in% 0:4] = "0 tot 5 jaar"
agents$age_group_20[agents$age %in% 5:9] = "5 tot 10 jaar"
agents$age_group_20[agents$age %in% 10:14] =  "10 tot 15 jaar"
agents$age_group_20[agents$age %in% 15:19] = "15 tot 20 jaar"
agents$age_group_20[agents$age %in% 20:24] =  "20 tot 25 jaar" 
agents$age_group_20[agents$age %in% 25:29] = "25 tot 30 jaar"
agents$age_group_20[agents$age %in% 30:34] =  "30 tot 35 jaar"
agents$age_group_20[agents$age %in% 35:39] =  "35 tot 40 jaar" 
agents$age_group_20[agents$age %in% 40:44] = "40 tot 45 jaar"
agents$age_group_20[agents$age %in% 45:49] = "45 tot 50 jaar"
agents$age_group_20[agents$age %in% 50:54] =  "50 tot 55 jaar"
agents$age_group_20[agents$age %in% 55:59] = "55 tot 60 jaar"
agents$age_group_20[agents$age %in% 60:64] =  "60 tot 65 jaar" 
agents$age_group_20[agents$age %in% 65:69] = "65 tot 70 jaar"
agents$age_group_20[agents$age %in% 70:74] =  "70 tot 75 jaar"
agents$age_group_20[agents$age %in% 75:79] =  "75 tot 80 jaar" 
agents$age_group_20[agents$age %in% 80:84] =  "80 tot 85 jaar" 
agents$age_group_20[agents$age %in% 85:89] = "85 tot 90 jaar"
agents$age_group_20[agents$age %in% 90:94] =  "90 tot 95 jaar"
agents$age_group_20[agents$age %in% 95:104] =  "95 jaar of ouder" 

colnames(migrat_age)[2] = "age_group_20"
marginal_2020$nr_Dutch = marginal_2020$AantalInwoners_5 - (marginal_2020$WestersTotaal_17 + marginal_2020$NietWestersTotaal_18)


## Conditional Propensities
agents = calc_propens_agents(migrat_age, "Dutch", "tot", agents, c("age_group_20", "sex") )
agents = calc_propens_agents(migrat_age, "Western", "tot", agents, c("age_group_20", "sex") )
agents = calc_propens_agents(migrat_age, "Non_Western", "tot", agents, c("age_group_20", "sex") )

## assigning attributes to agents
agents = distr_attr_strat_n_neigh_stats_3plus(agent_df =  agents, neigh_df =  marginal_2020, neigh_ID =  "neighb_code", variable =  "migrationbackground", 
                                   list_var_classes_neigh_df =  c("nr_Dutch", "WestersTotaal_17", "NietWestersTotaal_18"), 
                                   list_agent_propens =  c("prop_Dutch", "prop_Western", "prop_Non_Western"), list_class_names =  c("Dutch", "Western", "Non-Western"))

## cross validating with neighborhood and stratified totals
neigh_valid = crossvalid(valid_df = marginal_2020, agent_df = agents, join_var = "neighb_code", list_valid_var = c("nr_Dutch", "WestersTotaal_17", "NietWestersTotaal_18"), 
                         agent_var = "migrationbackground", list_agent_attr = c("Dutch", "Western", "Non-Western") )

strat_valid = crossvalid(valid_df = migrat_age, agent_df = agents, join_var = c("age_group_20", "sex"), list_valid_var =  c("Dutch", "Western", "Non_Western"), 
                         agent_var = "migrationbackground", list_agent_attr =  c("Dutch", "Western", "Non-Western") )


agents= agents[,c("agent_ID","neighb_code",  "age" , "sex", "age_group" , "age_group_20", "migrationbackground")]
write.csv(agents, "Agent_pop.csv")

######################################################################
################### household composition ############################
######################################################################
householdstats = read.csv("household position age gender.csv") ## count of people per age group per neighborhood
# and strat.gender_age_migr 

#### is a child

agents$ischild = 0
agents$ischild[which(agents$age %in%0:17)] = 1


### Single Household or not

## Data Preparation
strat.gender_age_migr$migrationbackground = ""
strat.gender_age_migr$migrationbackground[strat.gender_age_migr$Migratieachtergrond == "\"Westerse migratieachtergrond\""] = "Western"
strat.gender_age_migr$migrationbackground[strat.gender_age_migr$Migratieachtergrond == "\"Niet-westerse migratieachtergrond\""] = "Non-Western"
strat.gender_age_migr$migrationbackground[strat.gender_age_migr$Migratieachtergrond == "\"Nederlandse achtergrond\""] = "Dutch"
colnames(strat.gender_age_migr)[1:2] = c("age_group_20", "sex")
colnames(strat.gender_age_migr)[7] = c( "singlehh")

marginal_2020$nr_multiplehh = NA
for (i in 1:nrow(marginal_2020)) {
  marginal_2020$nr_multiplehh[i] = (nrow(agents[which(agents$neighb_code == marginal_2020$neighb_code[i] & agents$ischild != 1),])- as.numeric(marginal_2020$Eenpersoonshuishoudens_29[i]))
  if(marginal_2020$nr_multiplehh[i] < 0){
    marginal_2020$nr_multiplehh[i] = 0
  }
}


## Conditional Propensities
agents = calc_propens_agents(dataframe =  strat.gender_age_migr, variable =  "singlehh", total_population =  "TotaalAantalPersonenInHuishoudens_1", agent_df =  agents, list_conditional_var = c("age_group_20", "sex", "migrationbackground") )

## assigning attributes to agents
agents = distr_bin_attr_strat_n_neigh_stats(agent_df = agents, neigh_df = marginal_2020, neigh_ID = "neighb_code", agent_exclude = c("ischild"),
                                       variable=  "hh_single",  list_var_classes_neigh_df = c("Eenpersoonshuishoudens_29", "nr_multiplehh"), list_agent_propens =  c("prop_singlehh"), 
                                       list_class_names = c(1, 0))

## cross validating with neighborhood and stratified totals
neigh_valid = crossvalid(valid_df = marginal_2020, agent_df = agents, join_var = "neighb_code", list_valid_var = c("Eenpersoonshuishoudens_29", "nr_multiplehh"), 
                         agent_var = "hh_single", list_agent_attr = c(1, 0) )

strat_valid = crossvalid(valid_df = strat.gender_age_migr, agent_df = agents, join_var = c("age_group_20", "sex", "migrationbackground"), list_valid_var =  c("singlehh"), 
                         agent_var = "hh_single", list_agent_attr =  c(1, 0) )


### has a child

## Data Preparation
strat.gender_age_migr$have_kids = strat.gender_age_migr$PartnerInNietGehuwdPaarMetKinderen_7 + strat.gender_age_migr$PartnerInGehuwdPaarMetKinderen_8

marginal_2020$nr_kids = 0
marginal_2020$nr_ppl_with_kids = 0
marginal_2020$nr_ppl_without_kids = 0

for (i in 1:nrow(marginal_2020)){
  marginal_2020$nr_kids[i] = length(which(agents$neighb_code == marginal_2020$neighb_code[i] & agents$ischild == 1))
  marginal_2020$nr_ppl_with_kids[i] = marginal_2020$nr_kids[i] * 1.8 #20% live in single parent households
  marginal_2020$nr_ppl_without_kids[i] = (length(which(agents$neighb_code == marginal_2020$neighb_code[i] & agents$ischild != 1 & agents$hh_single != 1)))- marginal_2020$nr_ppl_with_kids[i]
}

## Conditional Propensities
agents = calc_propens_agents(dataframe =  strat.gender_age_migr, variable =  "have_kids", total_population =  "TotaalAantalPersonenInHuishoudens_1", agent_df =  agents, list_conditional_var = c("age_group_20", "sex", "migrationbackground") )

## assigning attributes to agents
agents = distr_bin_attr_strat_n_neigh_stats(agent_df = agents, neigh_df = marginal_2020, neigh_ID = "neighb_code", variable=  "havechild", list_var_classes_neigh_df = c("nr_ppl_with_kids", "nr_ppl_without_kids"), list_agent_propens =  c("prop_have_kids"), list_class_names = c(1, 0),  agent_exclude = c("ischild", "hh_single"))

## cross validating with neighborhood and stratified totals
neigh_valid = crossvalid(valid_df = marginal_2020, agent_df = agents, join_var = "neighb_code", list_valid_var = c("nr_ppl_with_kids", "nr_ppl_without_kids"), 
                         agent_var = "havechild", list_agent_attr = c(1, 0) )

strat_valid = crossvalid(valid_df = strat.gender_age_migr, agent_df = agents, join_var = c("age_group_20", "sex", "migrationbackground"), list_valid_var =  c("have_kids"), 
                         agent_var = "havechild", list_agent_attr =  c(1, 0) )


agents = agents[,c("agent_ID","neighb_code",  "age" , "sex", "age_group" , "age_group_20", "migrationbackground", "hh_single", "ischild", "havechild", "prop_female" ,"prop_Dutch", "prop_Western","prop_Non_Western", "prop_singlehh", "prop_have_kids")]
write.csv(agents, "Agent_pop_with_prop.csv")

setwd("C:/Dokumente/PhD EXPANSE/Data/Amsterdam/Population/CBS statistics")
agents = read.csv("Agent_pop_with_prop.csv")

############################# socioeconomics ################################################
# education level
x = marginal_distributions[,c("OpleidingsniveauLaag_64" , "OpleidingsniveauMiddelbaar_65" ,"OpleidingsniveauHoog_66")]
colnames(marginal_distributions)[5] = "neighb_code"
colnames(marginal_distributions)

## Data Preparation
setwd("C:/Dokumente/PhD EXPANSE/Data/Amsterdam/Population/CBS statistics/socioeconomics")
absolved_edu = read.csv("diplomaabsolvation conditioned by age sex and migrationbackground.csv")
current_edu = read.csv("students conditioned by age sex and migrationbackground.csv")
age_coding = read.csv("age_coding.csv")
edu_coding = read.csv("education_coding.csv")
colnames(age_coding)[1] = "Leeftijd"
colnames(edu_coding)[1:2] = c("Onderwijssoort", "education_level")
absolved_edu = merge(absolved_edu, age_coding, by= "Leeftijd", all.x = T, all.y = F)
current_edu = merge(current_edu, age_coding, by= "Leeftijd", all.x = T, all.y = F)
absolved_edu = merge(absolved_edu, edu_coding, by= "Onderwijssoort", all.x = T, all.y = F)
current_edu = merge(current_edu, edu_coding, by= "Onderwijssoort", all.x = T, all.y = F)

current_edu[, c("education_level" , "age" ,  "sex"  , "Migratieachtergrond"  , "LeerlingenDeelnemersStudenten_1")]           
absolved_edu[, c("education_level" , "age" ,  "sex"  , "Migratieachtergrond"  , "Gediplomeerden_1")]  

edu_stats = unique(absolved_edu[, c("age" ,  "sex"  , "Migratieachtergrond")])
for(n in 1:nrow(edu_stats)){
  edu_stats$absolved_high[n] = sum(absolved_edu$Gediplomeerden_1[which(absolved_edu$age == edu_stats$age[n] & absolved_edu$sex == edu_stats$sex[n] & absolved_edu$Migratieachtergrond == edu_stats$Migratieachtergrond[n] & absolved_edu$ranking == "high")])
  edu_stats$absolved_middle[n] = sum(absolved_edu$Gediplomeerden_1[which(absolved_edu$age == edu_stats$age[n] & absolved_edu$sex == edu_stats$sex[n] & absolved_edu$Migratieachtergrond == edu_stats$Migratieachtergrond[n] & absolved_edu$ranking == "middle")])
  edu_stats$absolved_low[n] = sum(absolved_edu$Gediplomeerden_1[which(absolved_edu$age == edu_stats$age[n] & absolved_edu$sex == edu_stats$sex[n] & absolved_edu$Migratieachtergrond == edu_stats$Migratieachtergrond[n] & absolved_edu$ranking == "low")])
  edu_stats$absolved_tot[n] = sum(absolved_edu$Gediplomeerden_1[which(absolved_edu$age == edu_stats$age[n] & absolved_edu$sex == edu_stats$sex[n] & absolved_edu$Migratieachtergrond == edu_stats$Migratieachtergrond[n] & absolved_edu$ranking != "")])
  edu_stats$current_high[n] = sum(current_edu$LeerlingenDeelnemersStudenten_1[which(current_edu$age == edu_stats$age[n] & current_edu$sex == edu_stats$sex[n] & current_edu$Migratieachtergrond == edu_stats$Migratieachtergrond[n] & current_edu$ranking == "high")])
  edu_stats$current_middle[n] = sum(current_edu$LeerlingenDeelnemersStudenten_1[which(current_edu$age == edu_stats$age[n] & current_edu$sex == edu_stats$sex[n] & current_edu$Migratieachtergrond == edu_stats$Migratieachtergrond[n] & current_edu$ranking == "middle")])
  edu_stats$current_low[n] = sum(current_edu$LeerlingenDeelnemersStudenten_1[which(current_edu$age == edu_stats$age[n] & current_edu$sex == edu_stats$sex[n] & current_edu$Migratieachtergrond == edu_stats$Migratieachtergrond[n] & current_edu$ranking == "low")])
  edu_stats$current_totalindf[n] = sum(current_edu$LeerlingenDeelnemersStudenten_1[which(current_edu$age == edu_stats$age[n] & current_edu$sex == edu_stats$sex[n] & current_edu$Migratieachtergrond == edu_stats$Migratieachtergrond[n] & current_edu$ranking != "")])
  edu_stats$current_total[n] = nrow(agents[ which(agents$age_group_new  == edu_stats$age[n] & agents$sex == edu_stats$sex[n] & agents$migrationbackground == edu_stats$Migratieachtergrond[n]), ])
  edu_stats$current_no_edu[n] = (edu_stats$current_total[n] - sum(edu_stats$current_low[n], edu_stats$current_middle[n], edu_stats$current_high[n]))
}

edu_stats = edu_stats[edu_stats$age_group_new != "Totaal" & edu_stats$sex != "Total" & edu_stats$migrationbackground != "Total",]
colnames(edu_stats)[1:3] = c("age_group_new", "sex", "migrationbackground")

agents$age_group_new = agents$age
agents$age_group_new[agents$age %in% 30:34] = "30 tot 35 jaar"
agents$age_group_new[agents$age %in% 35:39] = "35 tot 40 jaar"
agents$age_group_new[agents$age %in% 40:44] = "40 tot 45 jaar" 
agents$age_group_new[agents$age %in% 45:49] = "45 tot 50 jaar" 
agents$age_group_new[agents$age >= 50] = "50 jaar of ouder"  


marginal_distributions[,c("OpleidingsniveauLaag_64")] = as.numeric(marginal_distributions[,c("OpleidingsniveauLaag_64")])
marginal_distributions[,c("OpleidingsniveauMiddelbaar_65" )] = as.numeric(marginal_distributions[,c("OpleidingsniveauMiddelbaar_65" )])
marginal_distributions[,c("OpleidingsniveauHoog_66")] = as.numeric(marginal_distributions[,c("OpleidingsniveauHoog_66")])


## Conditional Propensities
agents = calc_propens_agents(dataframe =  edu_stats, variable = "absolved_high", total_population =  "absolved_tot", agent_df =  agents, list_conditional_var = c("age_group_new", "sex", "migrationbackground") )
agents = calc_propens_agents(dataframe =  edu_stats, variable = "absolved_middle", total_population =  "absolved_tot", agent_df =  agents, list_conditional_var = c("age_group_new", "sex", "migrationbackground") )
agents = calc_propens_agents(dataframe =  edu_stats, variable = "absolved_low", total_population =  "absolved_tot", agent_df =  agents, list_conditional_var = c("age_group_new", "sex", "migrationbackground") )
agents = calc_propens_agents(dataframe =  edu_stats, variable = "current_high", total_population =  "current_total", agent_df =  agents, list_conditional_var = c("age_group_new", "sex", "migrationbackground") )
agents = calc_propens_agents(dataframe =  edu_stats, variable = "current_middle", total_population =  "current_total", agent_df =  agents, list_conditional_var = c("age_group_new", "sex", "migrationbackground") )
agents = calc_propens_agents(dataframe =  edu_stats, variable = "current_low", total_population =  "current_total", agent_df =  agents, list_conditional_var = c("age_group_new", "sex", "migrationbackground") )
agents = calc_propens_agents(dataframe =  edu_stats, variable = "current_no_edu", total_population =  "current_total", agent_df =  agents, list_conditional_var = c("age_group_new", "sex", "migrationbackground") )


## assigning attributes to agents
agents$current_edu_exclude = 0
agents$current_edu_exclude[which(is.na(agents$prop_current_high))] = 1

agents = distr_attr_cond_prop(agent_df = agents, variable=  "current_education",   list_agent_propens =  c("prop_current_low",  "prop_current_middle", "prop_current_high", "prop_current_no_edu"), 
                              list_class_names = c("low", "middle", "high", "no_current_edu"), agent_exclude = "current_edu_exclude")

agents$current_education[which(agents$age < 15 & agents$age > 5) ] = "low"
agents$current_education[which( agents$age <= 5) ] = "no_current_edu"

agents$absolved = ""
agents$absolved[agents$current_education == "middle"] = "low"
agents$absolved[agents$current_education == "high" & agents$age <= 22] = "middle"
agents$absolved[agents$current_education == "high" & agents$age > 22] = "high"

marginal_distributions$LowerEdu = 0
marginal_distributions$MiddleEdu = 0
marginal_distributions$HigherEdu = 0

for(i in 1:nrow(marginal_distributions)){
  marginal_distributions[i,c("LowerEdu")] = marginal_distributions[i,c("OpleidingsniveauLaag_64")] - nrow(agents[agents$absolved == "low" & agents$neighb_code == marginal_distributions$neighb_code[i] & agents$age >= 15,])
  marginal_distributions[i,c("MiddleEdu" )] = marginal_distributions[i,c("OpleidingsniveauMiddelbaar_65" )]- nrow(agents[agents$absolved == "middle" & agents$neighb_code == marginal_distributions$neighb_code[i] & agents$age >= 15,])
  marginal_distributions[i,c("HigherEdu")] = marginal_distributions[i,c("OpleidingsniveauHoog_66")] - nrow(agents[agents$absolved == "high" & agents$neighb_code == marginal_distributions$neighb_code[i] & agents$age >= 15,])
}


marginal_distributions$LowerEdu[marginal_distributions$LowerEdu < 0] = 0
marginal_distributions$MiddleEdu[marginal_distributions$MiddleEdu < 0] = 0
marginal_distributions$HigherEdu[marginal_distributions$HigherEdu < 0] = 0

marginal_distributions[, c("OpleidingsniveauLaag_64" , "OpleidingsniveauMiddelbaar_65" ,"OpleidingsniveauHoog_66")]
marginal_distributions[, c("LowerEdu" , "MiddleEdu" ,"HigherEdu")]

agents$diplm_exclude = 0
agents$diplm_exclude[which(is.na(agents$prop_absolved_high)| agents$age < 15 | agents$absolved != "") ] = 1

agents = distr_attr_strat_n_neigh_stats_3plus(agent_df = agents, neigh_df = marginal_distributions, neigh_ID = "neighb_code", variable=  "absolved_education", 
                                            list_var_classes_neigh_df = c("LowerEdu" , "MiddleEdu" ,"HigherEdu"), 
                                            list_agent_propens =  c("prop_absolved_low",  "prop_absolved_middle", "prop_absolved_high" ), 
                                            list_class_names = c("low", "middle", "high"),  agent_exclude = c("diplm_exclude"))


agents$absolved_education[agents$absolved != ""] = agents$absolved[agents$absolved != ""]

## cross validating with neighborhood and stratified totals
neigh_valid = crossvalid(valid_df = marginal_distributions, agent_df = agents, join_var = "neighb_code", list_valid_var = c("OpleidingsniveauLaag_64" , "OpleidingsniveauMiddelbaar_65" ,"OpleidingsniveauHoog_66"), 
                         agent_var = "absolved_education", list_agent_attr = c("low", "middle", "high") )

strat_valid = crossvalid(valid_df = edu_stats, agent_df = agents, join_var = c("age_group_new", "sex", "migrationbackground"), list_valid_var =  c("absolved_low", "absolved_middle", "absolved_high"), 
                         agent_var = "absolved_education", list_agent_attr = c("low", "middle", "high") )



colnames(agents)
agents = agents[,c("agent_ID","neighb_code",  "age" , "sex", "age_group" , "age_group_20", "migrationbackground", "hh_single", "ischild", 
                   "havechild", "current_education", "absolved_education", "prop_female" ,"prop_Dutch", "prop_Western","prop_Non_Western",
                   "prop_singlehh", "prop_have_kids",  "prop_absolved_high", "prop_absolved_middle","prop_absolved_low" ,"prop_current_high",   
                   "prop_current_middle", "prop_current_low" ,"prop_current_no_edu" )]

setwd("C:/Dokumente/PhD EXPANSE/Data/Amsterdam/Population/CBS statistics")
write.csv(agents, "Agent_pop_with_prop.csv", row.names = FALSE)
agents = read.csv("Agent_pop_with_prop.csv")

setwd("C:/Dokumente/PhD EXPANSE/Data/Amsterdam/Population")
agents_clean = agents[,c("agent_ID","neighb_code",  "age" , "sex", "age_group" , "age_group_20", "migrationbackground", "hh_single", "ischild", 
                   "havechild", "current_education", "absolved_education" )]
write.csv(agents_clean, "Agent_pop.csv", row.names = FALSE)


# employment
c("NettoArbeidsparticipatie_67" , "PercentageWerknemers_68", "PercentageZelfstandigen_69" )

#income
x = marginal_2018[,c("AantalInkomensontvangers_67"  , "k_40PersonenMetLaagsteInkomen_70" , "k_20PersonenMetHoogsteInkomen_71",  
                    "k_40HuishoudensMetLaagsteInkomen_73",  "k_20HuishoudensMetHoogsteInkomen_74",  "HuishoudensMetEenLaagInkomen_75",
                    "HuishOnderOfRondSociaalMinimum_76")]

# k_40PersonenMetLaagsteInkomen_70 Share of persons in private households that belong to national 40% people with the lowest personal income.       
# k_20PersonenMetHoogsteInkomen_71 Share of persons in private households that belong to national 20% people with the highest personal income


colnames(marginal_distributions)

setwd("C:/Dokumente/PhD EXPANSE/Data/Amsterdam/Population/CBS statistics/socioeconomics")
income_stats = read.csv("income by gender and age.csv")
personal_attributes = read.csv("personal_attributes.csv")
colnames(personal_attributes)[1] = "Persoonskenmerken"
income_stats = merge(income_stats, personal_attributes, by= "Persoonskenmerken", all.x = T, all.y = F)
income_stats$Personen_1
personal_attributes
c("Migratieachtergrond: Nederland" , "Migratieachtergrond: westers", "Migratieachtergrond: niet-westers",  "Totaal personen"  , "Leeftijd: 0 tot 15 jaar" ,
  "Leeftijd: 15 tot 25 jaar", "Leeftijd: 25 tot 45 jaar" , "Leeftijd: 45 tot 65 jaar" , "Leeftijd: 65 jaar of ouder", "Positie hh: hoofdkostw. zonder partner",
  "Positie hh: hoofdkostw. met partner" , "Positie hh: hoofdkostwinner", "Positie hh: partner van hoofdkostwinner", "Positie hh: kind < 18 jaar" ,
  "Positie hh: kind >=18 jaar" ,"Positie hh: overig lid"  , "SEC: zelfstandige"  ,"SEC: uitkerings- en pensioenontvanger" , 
  "SEC: ontvanger werkloosheidsuitkering", "SEC: ontvanger van sociale voorziening", "SEC: arbeidsongeschikte", "SEC: overige (zonder inkomen)",
  "SEC: werknemer" , "SEC: uitkeringsontvanger",  "SEC: pensioenontvanger" , "SEC:(school)kind of student"  )  

# in English
c("Migration background: the Netherlands", "migration background: western", "migration background: non-western", "Total persons", "age: 0 to 15 years old",
"Age: 15 to 25 years old", "age: 25 to 45 years old", "age: 45 to 65 years old", "age: 65 years or older", "position HH: main kostw. Without partner",
"HH position: main kostw. With partner", "HH position: main kostwinner", "position HH: partner of main kostwinner", "position HH: child <18 years old",
"Position HH: child> = 18 years", "position HH: other member", "sec: self-employed", "sec: benefit and pension receiver",
"Sec: recipient unemployment benefit", "SEC: Social Provision Recipient", "SEC: Incapacitated", "SEC: Other (without income)",
"SEC: employee", "SEC: Benefit receiver", "SEC: Pension Recipient", "SEC: (School) Child or Student")


Inc_stats = create_stratified_prob_table(nested_cond_attr_list = list(c("Leeftijd: 0 tot 15 jaar" ,"Leeftijd: 15 tot 25 jaar", "Leeftijd: 25 tot 45 jaar" , 
                                                                        "Leeftijd: 45 tot 65 jaar" , "Leeftijd: 65 jaar of ouder"),
                                                                      c("Migratieachtergrond: Nederland" , "Migratieachtergrond: westers", "Migratieachtergrond: niet-westers")),
                                         column_names = c("age_group", "migrationbackground"), var_for_pred = c("PersonenMetPersoonlijkInkomen_2"), total_population = "Personen_1",
                                         orig_df = income_stats, strat_var = "Title")



#social support
c("HuishOnderOfRondSociaalMinimum_79" , "HuishoudensTot110VanSociaalMinimum_80", "HuishoudensTot120VanSociaalMinimum_81" ,"MediaanVermogenVanParticuliereHuish_82", 
  "PersonenPerSoortUitkeringBijstand_83" , "PersonenPerSoortUitkeringAO_84", "PersonenPerSoortUitkeringWW_85" , "PersonenPerSoortUitkeringAOW_86", "JongerenMetJeugdzorgInNatura_87",
  "PercentageJongerenMetJeugdzorg_88")


################### Health ###################################
setwd("C:/Dokumente/PhD EXPANSE/Data/Amsterdam/Population/CBS statistics/Health")
neigh_health = read.csv("Gezondheid_per_wijk_en_buurt_2016_09062021_151732.csv")
colnames(neigh_health)
neigh_health = neigh_health[which(neigh_health$Soort.Regio == "Buurt"),]


c("ï..Wijken.en.buurten", "Gemeentenaam" ,"Soort.Regio",   "neighb_code", "Drankgebruik.Voldoet.aan.alcoholrichtlijn" , "Overgewicht.Obesitas",
  "Roker" , "Lichamelijke.gezondheid.Langdurige.ziekte.of.aandoening", "Psychische.gezondheid.Hoog.risico.op.angst.of.depressie",
  "Eenzaamheid.Ernstig.zeer.ernstig.eenzaam",   "Goed.zeer.goed.ervaren.gezondheid",  "Sporten.en.bewegen.Voldoet.aan.beweegrichtlijn",
  "Mantelzorg.Mantelzorger"  , "Mantelzorg.Mantelzorg.ontvangen.nu..65.." )

# percentage of people 19 years or older  
# (1)Drankgebruik.Voldoet.aan.alcoholrichtlijn = people adhering to alcohol guideline (0-1 glas of alcohol per day)
# (2) Overgewicht.Obesitas = people met een BMI van 30,0 kg/m2 en hoger
# (3) Roker = Smoker

neigh_health = neigh_health[,c( "neighb_code", "Drankgebruik.Voldoet.aan.alcoholrichtlijn" , "Overgewicht.Obesitas", "Roker")]
colnames(neigh_health) = c("neighb_code", "follows_Alcohol_guidelines_perc" , "Obesity_perc", "Smoker_perc")


neigh_health = merge(marginal_2020["neighb_code"],  neigh_health, by = "neighb_code", all.x = T, all.y = F)
neigh_health$Nr_people_19plus = 0
for( i in 1:nrow(neigh_health)){
  neigh_health$Nr_people_19plus[i] = length(which(agents$neighb_code == neigh_health$neighb_code[i] & agents$age > 18))
}

x = c("follows_Alcohol_guidelines_perc" , "Obesity_perc", "Smoker_perc")
for (i in x){
  neigh_health[,c(i)] = as.numeric(neigh_health[,c(i)])/100
  neigh_health[,gsub("_perc", "", i)] = as.integer(neigh_health[,c(i)] * neigh_health$Nr_people_19plus)
  neigh_health[,paste("not_", gsub("_perc", "", i), sep = "")] = neigh_health$Nr_people_19plus - neigh_health[,gsub("_perc", "", i)]
}



obesity_stats = read.csv("table__83021NED.csv")
obesity_stats= obesity_stats[which(obesity_stats$Marges == "Waarde"),]
colnames(obesity_stats)[4:8] = c("Underweight" , "Normal_weight", "Overweight" , "Moderate_Overweight", "Obese")

x = c("Underweight" , "Normal_weight", "Overweight" , "Moderate_Overweight", "Obese")
for (i in x){
  obesity_stats[,c(i)] = gsub(",", ".", obesity_stats[,c(i)])
  obesity_stats[,c(i)] = as.numeric(obesity_stats[,c(i)])/100
}

# 1. Ondergewicht: BMI < 18,5
# 2. Normaal gewicht: BMI >= 18,5 en < 25,0
# 3. Overgewicht: BMI >= 25,0
# a. Matig overgewicht: BMI >= 25,0 en < 30,0
# b. Ernstig overgewicht: BMI >= 30,0

unique(obesity_stats$Kenmerken.personen)
c("Geslacht: Mannen", "Geslacht: Vrouwen", "Leeftijd: 0 tot 4 jaar", "Leeftijd: 4 tot 12 jaar"  , "Leeftijd: 12 tot 16 jaar"  ,
  "Leeftijd: 16 tot 20 jaar" , "Leeftijd: 20 tot 30 jaar" , "Leeftijd: 30 tot 40 jaar" , "Leeftijd: 40 tot 50 jaar" , "Leeftijd: 50 tot 55 jaar",
  "Leeftijd: 55 tot 65 jaar"    , "Leeftijd: 65 tot 75 jaar"  ,  "Leeftijd: 75 jaar of ouder",  "Leeftijd: 0 tot 12 jaar" ,  "Leeftijd: 12 tot 18 jaar",
  "Leeftijd: 18 jaar of ouder"  , "Positie: alleenstaande <40 jaar" ,  "Positie: alleenstaande 40 tot 65 jaar" , "Positie: alleenstaande >=65 jaar" ,
  "Positie: kind <18 jaar, eenoudergezin",  "Positie: kind >= 18 jaar eenoudergezin", "Positie: kind <18 jaar bij paar"  , "Positie: kind >=18 jaar bij paar" ,
 "Positie: ouder in eenoudergezin" , "Positie: partner in paar met kind" , "Positie: partner paar <40, geen kind", "Positie: partner paar 40-65, geen kind",
 "Positie: partner paar >=65, geen kind", "Positie: overig lid" , "Migratieachtergrond: Nederland" , "Migratieachtergrond: westers" ,
 "Migratieachtergrond: 1e gen westers", "Migratieachtergrond: 2e gen westers")


agents$age_group_new[agents$age %in% 0:3] = "Leeftijd: 0 tot 4 jaar"
agents$age_group_new[agents$age %in% 4:11] = "Leeftijd: 4 tot 12 jaar"
agents$age_group_new[agents$age %in% 12:15] = "Leeftijd: 12 tot 16 jaar"
agents$age_group_new[agents$age %in% 16:19] = "Leeftijd: 16 tot 20 jaar" 
agents$age_group_new[agents$age %in% 20:29] = "Leeftijd: 20 tot 30 jaar"
agents$age_group_new[agents$age %in% 30:39] = "Leeftijd: 30 tot 40 jaar"
agents$age_group_new[agents$age %in% 40:49] = "Leeftijd: 40 tot 50 jaar"
agents$age_group_new[agents$age %in% 50:54] = "Leeftijd: 50 tot 55 jaar"
agents$age_group_new[agents$age %in% 55:64] = "Leeftijd: 55 tot 65 jaar" 
agents$age_group_new[agents$age %in% 65:75] = "Leeftijd: 65 tot 75 jaar" 
agents$age_group_new[agents$age >= 75] = "Leeftijd: 75 jaar of ouder" 


BMI_stats = create_stratified_prob_table(nested_cond_attr_list = list(c("Geslacht: Mannen", "Geslacht: Vrouwen"),
                                                                      c("Leeftijd: 0 tot 4 jaar", "Leeftijd: 4 tot 12 jaar"  , "Leeftijd: 12 tot 16 jaar"  ,
                                                                        "Leeftijd: 16 tot 20 jaar" , "Leeftijd: 20 tot 30 jaar" , "Leeftijd: 30 tot 40 jaar" ,
                                                                        "Leeftijd: 40 tot 50 jaar" , "Leeftijd: 50 tot 55 jaar", "Leeftijd: 55 tot 65 jaar",
                                                                        "Leeftijd: 65 tot 75 jaar"  ,  "Leeftijd: 75 jaar of ouder",  "Leeftijd: 0 tot 12 jaar" ,  "Leeftijd: 12 tot 18 jaar",
                                                                        "Leeftijd: 18 jaar of ouder")),
                                         column_names = c("sex", "age_group_new"), var_for_pred = c("Underweight" , "Normal_weight", "Overweight" , "Moderate_Overweight", "Obese"),
                                         orig_df = obesity_stats, strat_var = "Kenmerken.personen")

BMI_stats$sex = gsub("Geslacht: Mannen", "male", BMI_stats$sex)
BMI_stats$sex = gsub("Geslacht: Vrouwen", "female", BMI_stats$sex)

agents = calc_propens_agents(dataframe =  BMI_stats, variable = "Underweight", agent_df =  agents, list_conditional_var = c("sex", "age_group_new"))
agents = calc_propens_agents(dataframe =  BMI_stats, variable = "Normal_weight", agent_df =  agents, list_conditional_var = c("sex", "age_group_new"))
agents = calc_propens_agents(dataframe =  BMI_stats, variable = "Moderate_Overweight", agent_df =  agents, list_conditional_var = c("sex", "age_group_new"))
agents = calc_propens_agents(dataframe =  BMI_stats, variable = "Obese", agent_df =  agents, list_conditional_var = c("sex", "age_group_new"))

agents$weight_exclude = 0
agents$weight_exclude[is.na(agents$prop_Underweight)]= 1


agents = distr_attr_cond_prop(agent_df = agents, variable=  "BMI",   list_agent_propens =  c("prop_Underweight" ,"prop_Normal_weight", "prop_Moderate_Overweight", "prop_Obese"), 
                              list_class_names = c("underweight", "normal_weight", "moderate_overweight", "obese"), agent_exclude = "weight_exclude")

agents = distr_attr_strat_n_neigh_stats_3plus(agent_df = agents, neigh_df = marginal_distributions, neigh_ID = "neighb_code", variable=  "absolved_education", 
                                              list_var_classes_neigh_df = c("LowerEdu" , "MiddleEdu" ,"HigherEdu"), 
                                              list_agent_propens =  c("prop_absolved_low",  "prop_absolved_middle", "prop_absolved_high" ), 
                                              list_class_names = c("low", "middle", "high"),  agent_exclude = c("diplm_exclude"))

setwd("C:/Dokumente/PhD EXPANSE/Data/Amsterdam/Population/CBS statistics")
agents = agents[,c("agent_ID","neighb_code",  "age" , "sex", "age_group" , "age_group_20", "migrationbackground", "hh_single", "ischild", 
                   "havechild", "current_education", "absolved_education", "BMI", "prop_female" ,"prop_Dutch", "prop_Western","prop_Non_Western",
                   "prop_singlehh", "prop_have_kids",  "prop_absolved_high", "prop_absolved_middle","prop_absolved_low" ,"prop_current_high",   
                   "prop_current_middle", "prop_current_low" ,"prop_current_no_edu","prop_Underweight" ,"prop_Normal_weight", "prop_Moderate_Overweight", "prop_Obese" )]
write.csv(agents, "Agent_pop_with_prop.csv", row.names = FALSE)
agents = read.csv("Agent_pop_with_prop.csv")

setwd("C:/Dokumente/PhD EXPANSE/Data/Amsterdam/Population")
agents_clean = agents[,c("agent_ID","neighb_code",  "age" , "sex", "age_group" , "age_group_20", "migrationbackground", "hh_single", "ischild", 
                         "havechild", "current_education", "absolved_education", "BMI" )]
write.csv(agents_clean, "Agent_pop.csv", row.names = FALSE)

setwd("C:/Dokumente/PhD EXPANSE/Data/Amsterdam/Population/CBS statistics/Health")
riskfactor_stats = read.csv("table__83021NED2.csv")
colnames(riskfactor_stats)

x = c("Smokers" , "DailySmokers", "OccasionalSmokers" , "followAlcoholGuidelines")
for (i in x){
  riskfactor_stats[,c(i)] = gsub(",", ".", riskfactor_stats[,c(i)])
  riskfactor_stats[,c(i)] = as.numeric(riskfactor_stats[,c(i)])/100
}
for (i in x){
  riskfactor_stats[which(riskfactor_stats$Kenmerken.personen == "Onderwijsniveau: basisonderwijs"),c(i)] = ((riskfactor_stats[which(riskfactor_stats$Kenmerken.personen == "Onderwijsniveau: basisonderwijs"),c(i)] + 
                                                                                                              riskfactor_stats[which(riskfactor_stats$Kenmerken.personen == "Onderwijsniveau: vmbo,mbo1,avo onderbouw"),c(i)])/2)
  riskfactor_stats[which(riskfactor_stats$Kenmerken.personen == "Onderwijsniveau: hbo, wo bachelor"),c(i)] = ((riskfactor_stats[which(riskfactor_stats$Kenmerken.personen == "Onderwijsniveau: hbo, wo bachelor"),c(i)] + 
                                                                                                              riskfactor_stats[which(riskfactor_stats$Kenmerken.personen == "Onderwijsniveau: wo, master, doctor"),c(i)])/2)
}

riskfactor_stats[,c("Kenmerken.personen")] = gsub("Onderwijsniveau: basisonderwijs", "low_edu", riskfactor_stats[,c("Kenmerken.personen")])
riskfactor_stats[,c("Kenmerken.personen")] = gsub("Onderwijsniveau: havo, vwo, mbo", "middle_edu", riskfactor_stats[,c("Kenmerken.personen")])
riskfactor_stats[,c("Kenmerken.personen")] = gsub("Onderwijsniveau: hbo, wo bachelor", "high_edu", riskfactor_stats[,c("Kenmerken.personen")])



risk_stats = create_stratified_prob_table(nested_cond_attr_list = list(c("Geslacht: Mannen", "Geslacht: Vrouwen"),
                                                                      c("Leeftijd: 12 tot 16 jaar"  ,
                                                                        "Leeftijd: 16 tot 20 jaar" , "Leeftijd: 20 tot 30 jaar" , "Leeftijd: 30 tot 40 jaar" ,
                                                                        "Leeftijd: 40 tot 50 jaar" , "Leeftijd: 50 tot 55 jaar", "Leeftijd: 55 tot 65 jaar",
                                                                        "Leeftijd: 65 tot 75 jaar"  ,  "Leeftijd: 75 jaar of ouder"),
                                                                      c("low_edu", "middle_edu", "high_edu")),
                                         column_names = c("sex", "age_group_new", "absolved_education"), var_for_pred = c("Smokers" , "DailySmokers", "OccasionalSmokers" , "followAlcoholGuidelines"),
                                         orig_df = riskfactor_stats, strat_var = "Kenmerken.personen")



risk_stats$sex = gsub("Geslacht: Mannen", "male", risk_stats$sex)
risk_stats$sex = gsub("Geslacht: Vrouwen", "female", risk_stats$sex)

# Conditional Probabilities
agents = calc_propens_agents(dataframe =  risk_stats, variable = "Smokers", agent_df =  agents, list_conditional_var = c("sex", "age_group_new", "absolved_education"))
agents = calc_propens_agents(dataframe =  risk_stats, variable = "followAlcoholGuidelines", agent_df =  agents, list_conditional_var = c("sex", "age_group_new", "absolved_education"))


## assigning attributes to agents
agents$risk_exclude = 0
agents$risk_exclude[which(is.na(agents$prop_Smokers) | agents$age < 12)] = 1

colnames(neigh_health)

agents = distr_bin_attr_strat_n_neigh_stats(agent_df = agents, neigh_df = neigh_health, neigh_ID = "neighb_code", agent_exclude = c("risk_exclude"),
                                            variable=  "Smoker",  list_var_classes_neigh_df = c("Smoker" , "not_Smoker" ), list_agent_propens =  c("prop_Smokers"), 
                                            list_class_names = c(1, 0))

agents = distr_bin_attr_strat_n_neigh_stats(agent_df = agents, neigh_df = neigh_health, neigh_ID = "neighb_code", agent_exclude = c("risk_exclude"),
                                            variable=  "follows_Alcohol_guidelines",  list_var_classes_neigh_df = c("follows_Alcohol_guidelines" , "not_follows_Alcohol_guidelines" ), list_agent_propens =  c("prop_followAlcoholGuidelines"), 
                                            list_class_names = c(1, 0))

######################## car ownership ##############################
c("PersonenautoSTotaal_99", "PersonenautoSBrandstofBenzine_100" , "PersonenautoSOverigeBrandstof_101", "PersonenautoSPerHuishouden_102" , "PersonenautoSNaarOppervlakte_103" , "Motorfietsen_104" )



agents$agent_ID = paste("Agent_",1:tot_pop, sep="")

write.csv(agents, "Agent_pop.csv")
