################################################################################
#
# Purpose of script: group agents of a synthetic population into households.
#
# Author: Marco Pellegrino
#
# Year: 2022
#
################################################################################

library("this.path")
library(dplyr)
library (readr)
setwd(this.path::this.dir())
source('src/utils-households.R')

start_time <- Sys.time()

# Load marginal distributions
setwd(this.path::this.dir())
setwd('data/processed/individuals')
df_marginal_dist = read.csv("marginal_distributions_84583NED-formatted.csv", sep = ",")

# Load datasets of the municipality for that year
setwd(this.path::this.dir())
setwd('data/processed/households')
df_parents <-
  read_csv("couples_singleparents-71488NED-formatted.csv")
df_individuals_nochildren <-
  read_csv("singles_couples_nochildren-71488NED-formatted.csv")
df_singleparents <-
  read_csv("singleparents_gender-71488NED-formatted.csv")

# Load datasets on national level
setwd(this.path::this.dir())
setwd('data/processed')
df_couples_genders <-
  read.csv("couples_gender_disparity_37772ENG-formatted.csv", sep = ',')
df_couples_ages <- read.csv("couples_age_disparity.csv", sep = ',')

# Load synthetic population
setwd(this.path::this.dir())
setwd("output/synthetic-population")
df_synth_pop = read.csv('synthetic_population_DHZW_2019.csv')
df_synth_pop$hh_ID = NA
df_synth_pop$hh_type = NA

################################################################################
# Group agents into households

# Empty container of households
df_households <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(df_households) <- c("hh_ID", "hh_size", "neighb_code", "hh_type")

df_synth_pop$child_too_old <- FALSE

# For each neighbourhood area
for (neighb_code in unique(df_synth_pop$neighb_code)) {
  
  # Retrieve how many households per children in households we need
  n_households = get_households_children(df_synth_pop, neighb_code)
  
  # list of all unassigned children
  df_children_unassigned = df_synth_pop[df_synth_pop$is_child == 1 &
                                          df_synth_pop$neighb_code == neighb_code, ]
  
  print('Generating families...')
  
  # For quantity of children in household, from largest to smallest (we start with the most difficult constraints)
  for (children_size in (nrow(n_households):1)) {
    # repeat for all the households needed for such quantity of children in household
    counter_houses = 0
    while (counter_houses < (n_households[children_size, ]$num_households)) {
      # Get new household ID
      hh_ID = nrow(df_households) + 1
      
      # empty list of children for this new household
      df_children_house <- df_children_unassigned[0, ]
      
      # Pick a random child c and remove from list
      c = sample_n(df_children_unassigned, 1)
      c$hh_ID = hh_ID
      df_children_unassigned = df_children_unassigned[df_children_unassigned$agent_ID != c$agent_ID, ]
      df_children_house <- rbind(df_children_house, c)
      
      # Assign child to household
      df_synth_pop[df_synth_pop$agent_ID == c$agent_ID, ]$hh_ID = hh_ID
      
      # While household has fewer children than children_in_house
      counter_siblings = 0
      while (counter_siblings < (children_size - 1)) {
        # Get the ages a_c1, . , a_cn of all children c_1, . , c_n thus far assigned
        ages_children = df_children_house$age
        
        # Assign each remaining child x a probability p(x) of being a sibling of c_1, . ,c_n, based on how well their age a_x matches the age difference with argmin (a_c1, a_cn ) or argmax(a_c1, a_c_n)
        # todo, sample next sibling randomly
        x = sample_n(df_children_unassigned, 1)
        df_children_unassigned = df_children_unassigned[df_children_unassigned$agent_ID != x$agent_ID, ]
        df_children_house <- rbind(df_children_house, x)
        df_synth_pop[df_synth_pop$agent_ID == x$agent_ID, ]$hh_ID = hh_ID
        
        counter_siblings = counter_siblings + 1
      }
      
      # Get average age of children in the house
      avg_children_age = mean(df_children_house$age)
      
      # Get age of the oldest child
      oldest_child_age = max(df_children_house$age)
      
      # Sample if household is singleparent or couple
      hh_type = sample(df_parents$hh_type,
                       1,
                       replace = FALSE,
                       prob = df_parents$prob)
      
      if (hh_type == 'singleparents') {
        # case: single-parent
        hh_size = nrow(df_children_house) + 1
        
        # generate gender of the parent from distribution
        singleparent_gender = sample(df_singleparents$gender,
                                     1,
                                     replace = FALSE,
                                     prob = df_singleparents$prob)
        
        if (singleparent_gender == 'female') {
          # case: mother
          parent_ID = get_mother(df_synth_pop,
                                 neighb_code,
                                 avg_children_age,
                                 oldest_child_age)
        } else {
          # case: father
          age_fake_mother = get_age_fake_mother(df_synth_pop,
                                                avg_children_age,
                                                oldest_child_age)
          parent_ID = get_second_partner(df_synth_pop,
                                         neighb_code,
                                         age_fake_mother,
                                         'male_female')
        }
        
        if (parent_ID != -1) {
          # add parent to house
          df_synth_pop[df_synth_pop$agent_ID == parent_ID, ]$hh_ID = hh_ID
          
          df_synth_pop[!is.na(df_synth_pop$hh_ID) &
                         df_synth_pop$hh_ID == hh_ID, ]$hh_type <- 'single_parent'
        } else {
          df_synth_pop[!is.na(df_synth_pop$hh_ID) &
                         df_synth_pop$hh_ID == hh_ID, ]$child_too_old <- TRUE
        }
        
      } else {
        # case: couple
        hh_size = nrow(df_children_house) + 2
        
        couple_gender = sample(df_couples_genders$genders,
                               1,
                               replace = FALSE,
                               prob = df_couples_genders$prob)
        
        # check if there are enough remaining agents with the chosen gender. If not, force to find a couple with the only available gender
        if(nrow(df_synth_pop[df_synth_pop$gender=='female' &
                             is.na(df_synth_pop$hh_ID) &
                             df_synth_pop$is_child==0 &
                             df_synth_pop$neighb_code==neighb_code,])==0){
          couple_gender='male_male'
        } else if (nrow(df_synth_pop[df_synth_pop$gender=='male' &
                                     is.na(df_synth_pop$hh_ID) &
                                     df_synth_pop$is_child==0 &
                                     df_synth_pop$neighb_code==neighb_code,])==0) {
          couple_gender='female_female'
        }
        
        if (couple_gender == 'male_female') {
          # couple with mother and father
          
          # get parents
          mother_ID = get_mother(df_synth_pop,
                                 neighb_code,
                                 avg_children_age,
                                 oldest_child_age)
          if (mother_ID != -1) {
            mother = df_synth_pop[df_synth_pop$agent_ID == mother_ID, ]
            
            father_ID = get_second_partner(df_synth_pop,
                                           neighb_code,
                                           mother$age,
                                           couple_gender)
            
            # add parents to house
            df_synth_pop[df_synth_pop$agent_ID == mother_ID, ]$hh_ID = hh_ID
            df_synth_pop[df_synth_pop$agent_ID == father_ID, ]$hh_ID = hh_ID
            df_synth_pop[!is.na(df_synth_pop$hh_ID) &
                           df_synth_pop$hh_ID == hh_ID, ]$hh_type <- 'couple_children_straight'
          } else {
            df_synth_pop[!is.na(df_synth_pop$hh_ID) &
                           df_synth_pop$hh_ID == hh_ID, ]$child_too_old <- TRUE
          }
          
        } else if (couple_gender == 'male_male') {
          # couple with two fathers
          
          # generate first father based on a fake mother
          age_fake_mother = get_age_fake_mother(df_synth_pop,
                                                avg_children_age,
                                                oldest_child_age)
          first_father_ID = get_second_partner(df_synth_pop,
                                               neighb_code,
                                               age_fake_mother,
                                               'male_female')
          first_father = df_synth_pop[df_synth_pop$agent_ID == first_father_ID, ]
          
          # generate second father based on the first father
          second_father_ID = get_second_partner(df_synth_pop,
                                                neighb_code,
                                                first_father$age,
                                                couple_gender)
          
          # add parents to house
          df_synth_pop[df_synth_pop$agent_ID == first_father_ID, ]$hh_ID = hh_ID
          df_synth_pop[df_synth_pop$agent_ID == second_father_ID, ]$hh_ID = hh_ID
          df_synth_pop[!is.na(df_synth_pop$hh_ID) &
                         df_synth_pop$hh_ID == hh_ID, ]$hh_type <- 'couple_children_gay'
          
        } else if (couple_gender == 'female_female') {
          # couple with two mothers
          
          # get first mother as real
          first_mother_ID = get_mother(df_synth_pop,
                                       neighb_code,
                                       avg_children_age,
                                       oldest_child_age)
          
          if (first_mother_ID != -1) {
            first_mother = df_synth_pop[df_synth_pop$agent_ID == first_mother_ID, ]
            
            # generate second mother based on the first mother
            second_mother_ID = get_second_partner(df_synth_pop,
                                                  neighb_code,
                                                  first_mother$age,
                                                  couple_gender)
            
            # add parents to house
            df_synth_pop[df_synth_pop$agent_ID == first_mother_ID, ]$hh_ID = hh_ID
            df_synth_pop[df_synth_pop$agent_ID == second_mother_ID, ]$hh_ID = hh_ID
            df_synth_pop[!is.na(df_synth_pop$hh_ID) &
                           df_synth_pop$hh_ID == hh_ID, ]$hh_type <- 'couple_children_lesbian'
          } else {
            df_synth_pop[!is.na(df_synth_pop$hh_ID) &
                           df_synth_pop$hh_ID == hh_ID, ]$child_too_old <- TRUE
          }
        }
      }
      
      if (hh_type=='singleparents') {
        hh_type <- 'single-parent'
      } else {
        hh_type <- 'couple_with_children'
      }
      df_households[hh_ID, ] = c(hh_ID,
                                 as.numeric(hh_size),
                                 neighb_code,
                                 hh_type)
      
      counter_houses = counter_houses + 1
    }
  }
  
  # create households without children: couples and singles
  remaining_agents = nrow(df_synth_pop[is.na(df_synth_pop$hh_ID) &
                                         df_synth_pop$neighb_code == neighb_code, ])
  prob_individual_is_couple = (df_marginal_dist[df_marginal_dist$neighb_code ==
                                                  neighb_code, 'hh_no_children'] * 2) / ((df_marginal_dist[df_marginal_dist$neighb_code ==
                                                                                                             neighb_code, 'hh_no_children'] * 2) + df_marginal_dist[df_marginal_dist$neighb_code ==
                                                                                                                                                                      neighb_code, 'hh_single'])
  n_individuals_couples = remaining_agents * prob_individual_is_couple
  n_couples = round(n_individuals_couples / 2) # round to closest even number (couples are made of two individuals)
  
  print(paste0('Probability individual is couple ', prob_individual_is_couple))
  print(paste0('N couples: ', n_couples))
  
  print('Generating couples without children...')
  
  hh_size = 2
  counter_couples = 0
  while (counter_couples < (n_couples - 1)) {
    # Get new household ID
    hh_ID = nrow(df_households) + 1
    
    df_households[hh_ID, ] = c(hh_ID,
                               as.numeric(hh_size),
                               neighb_code,
                               'couple_without_children')
    
    # generate genders of the couple
    couple_gender = sample(df_couples_genders$genders,
                           1,
                           replace = FALSE,
                           prob = df_couples_genders$prob)
    
    # check if there are enough remaining agents with the chosen gender. If not, force to find a couple with the only available gender
    if(nrow(df_synth_pop[df_synth_pop$gender=='female' &
                         is.na(df_synth_pop$hh_ID) &
                         df_synth_pop$is_child==0 &
                         df_synth_pop$neighb_code==neighb_code,])==0){
      couple_gender='male_male'
    } else if (nrow(df_synth_pop[df_synth_pop$gender=='male' &
                                 is.na(df_synth_pop$hh_ID) &
                                 df_synth_pop$is_child==0 &
                                 df_synth_pop$neighb_code==neighb_code,])==0) {
      couple_gender='female_female'
    }
    

    # sample random agent as first partner
    if (couple_gender == 'male_female') {
      first_partner = sample_n(df_synth_pop[df_synth_pop$gender=='female' &
                                              is.na(df_synth_pop$hh_ID) &
                                              df_synth_pop$is_child==0 &
                                              df_synth_pop$neighb_code==neighb_code,], 1)
    } else if (couple_gender == 'male_male') {
      first_partner = sample_n(df_synth_pop[df_synth_pop$gender=='male' &
                                              is.na(df_synth_pop$hh_ID) &
                                              df_synth_pop$is_child==0 &
                                              df_synth_pop$neighb_code==neighb_code,], 1)
    } else if (couple_gender == 'female_female') {
      first_partner = sample_n(df_synth_pop[df_synth_pop$gender=='female' &
                                              is.na(df_synth_pop$hh_ID) &
                                              df_synth_pop$is_child==0 &
                                              df_synth_pop$neighb_code==neighb_code,], 1)
    }
    # find its best partner based on gender and age difference
    second_partner_ID = get_second_partner(df_synth_pop,
                                           neighb_code,
                                           first_partner$age,
                                           couple_gender)
    
    # add partners to house
    df_synth_pop[df_synth_pop$agent_ID == first_partner$agent_ID, ]$hh_ID = hh_ID
    df_synth_pop[df_synth_pop$agent_ID == second_partner_ID, ]$hh_ID = hh_ID
    
    if (couple_gender == 'male_female') {
      
      df_synth_pop[!is.na(df_synth_pop$hh_ID) &
                     df_synth_pop$hh_ID == hh_ID, ]$hh_type <- 'couple_no_children_straight'
    } else if (couple_gender == 'male_male') {
      
      df_synth_pop[!is.na(df_synth_pop$hh_ID) &
                     df_synth_pop$hh_ID == hh_ID, ]$hh_type <- 'couple_no_children_gay'
    } else if (couple_gender == 'female_female') {
      
      df_synth_pop[!is.na(df_synth_pop$hh_ID) &
                     df_synth_pop$hh_ID == hh_ID, ]$hh_type <- 'couple_no_children_lesbian'
    }
    
    counter_couples = counter_couples + 1
  }
  
  
  # assign remaining adults to singles
  remaining_agents = df_synth_pop[is.na(df_synth_pop$hh_ID) &
                                    df_synth_pop$neighb_code == neighb_code, ]
  
  print('Generating singles...')
  print(paste0('N singles: ', nrow(remaining_agents)))
  
  
  hh_size = 1
  counter_singles = 1
  # for each remaining agent
  while (counter_singles <= nrow(remaining_agents)) {
    # get new hh_ID
    hh_ID = nrow(df_households) + 1
    
    # create household
    df_households[hh_ID, ] = c(hh_ID,
                               as.numeric(hh_size),
                               neighb_code,
                               'single')
    
    # get new single
    single_ID = remaining_agents[counter_singles, 'agent_ID']
    
    # add agent to house
    df_synth_pop[df_synth_pop$agent_ID == single_ID, ]$hh_ID = hh_ID
    df_synth_pop[!is.na(df_synth_pop$hh_ID) &
                   df_synth_pop$hh_ID == hh_ID, ]$hh_type <- 'single'
    
    counter_singles = counter_singles + 1
  }
  
}

df_synth_pop <- subset(df_synth_pop, select=-c(child_too_old))

end_time <- Sys.time()
difftime(end_time, start_time, units = "secs")

# Save snapshot
dir_name <- paste0('1_households_', format(Sys.time(), "%F_%H-%M"))
setwd(paste(this.path::this.dir(), 'output/synthetic-population-households', sep = '/'))
dir.create(dir_name)
setwd(dir_name)
write.csv(df_synth_pop, paste0('synthetic_population_DHZW_', year, '.csv'), row.names = FALSE)
write.csv(df_households, paste0('df_households_DHZW_', year, '.csv'), row.names = FALSE)

################################################################################
# Locate households into PC6 codes

# Load CBS dataset PC6 - neighbourhood conversion with proportions of PC6 per each neighbourhood code
setwd(this.path::this.dir())
setwd("data/processed/")
df_PC6_neighb <- read_delim(
  "proportions_PC6_neighbcode_BAG.csv",
  delim = ",",
  escape_double = FALSE,
  trim_ws = TRUE
)

# Prepare PC6 attribute for the household dataset
df_households$PC6 <- NA

# For each neighbourhood assign the PC6 based on its proportion.
# Future work: use partitioning instead of sampling, but the existing function needs to be modified

for(neighb_code in unique(df_households$neighb_code)){
  # Sample n PC6 codes from the PC6 distribution of this neighbourhood, where n is the number of synthetic households

  df_unassigned_hh <- df_households[df_households$neighb_code==neighb_code,]
  
  hh_PC6 = sample(x = df_PC6_neighb[df_PC6_neighb$neighb_code==neighb_code,]$PC6, 
                  size = nrow(df_unassigned_hh),
                  replace = TRUE,
                  prob = df_PC6_neighb[df_PC6_neighb$neighb_code==neighb_code,]$prop)
  
  # Apply the sampled PC6 code to the synthetic households
  df_households[df_households$neighb_code == neighb_code,]$PC6 = hh_PC6
}

# Add the PC6 code also to the individual synthetic agents
tmp <- df_households %>%
  select(hh_ID, PC6, hh_size)
df_synth_pop <- merge(df_synth_pop, tmp, by = 'hh_ID')

################################################################################
# Household income

# Load joint distribution
setwd(this.path::this.dir())
setwd("data/processed/households")
df_joint_income = read.csv("household_income_85064NED-formatted.csv")

# filter household types of the synthetic population
df_joint_income <- df_joint_income[df_joint_income$type %in% unique(df_households$hh_type),]

# Assign the attribute based on the joint distribution proportions
df_households <-
  distribute_attribute_joint_dist(
    df_synth_pop = df_households,
    df_joint = df_joint_income,
    new_attribute = 'income_group',
    attributes_to_match = c('hh_type'),
    values_new_attribute = c("income_1_10", "income_2_10", "income_3_10", "income_4_10", "income_5_10", "income_6_10", "income_7_10", "income_8_10", "income_9_10", "income_10_10"),
    probabilities = c("income_1_10", "income_2_10", "income_3_10", "income_4_10", "income_5_10", "income_6_10", "income_7_10", "income_8_10", "income_9_10", "income_10_10")
  )

# check if it makes sense
table(df_households$income_group)

# Add the attribute also to the individuals that are part of the household
tmp <- df_households %>%
  select(hh_ID, income_group)
df_synth_pop <- merge(df_synth_pop, tmp, by = 'hh_ID')

################################################################################
# Can ownership

# Load joint distribution of percentage car ownership in the NL in 2015 based on household types
setwd(this.path::this.dir())
setwd("data/processed")
df_joint_car_ownership = read.csv("car_ownership_NL_2015-formatted.csv")

# Assign the attribute based on the joint distribution proportions
df_households <-
  distribute_attribute_joint_dist(
    df_synth_pop = df_households,
    df_joint = df_joint_car_ownership,
    new_attribute = 'car_ownership',
    attributes_to_match = c('hh_type'),
    values_new_attribute = c(1, 0),
    probabilities = c('percentage')
  )

# Add the attribute also to the individuals that are part of the household
tmp <- df_households %>%
  select(hh_ID, car_ownership)
df_synth_pop <- merge(df_synth_pop, tmp, by = 'hh_ID')

################################################################################
# Save
setwd(this.path::this.dir())
setwd('output/synthetic-population-households')

# save individuals with households
write.csv(df_synth_pop, paste0('synthetic_population_DHZW_2019.csv'), row.names = FALSE)

# save households only
write.csv(df_households, paste0('df_households_DHZW_2019.csv'), row.names = FALSE)