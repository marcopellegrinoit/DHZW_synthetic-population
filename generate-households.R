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
source('config/config.R')
source('src/utils-households.R')

start_time <- Sys.time()

# Load marginal distribution
setwd(
  paste(
    this.path::this.dir(),
    "data/processed",
    year,
    municipality,
    'individuals_demographics',
    sep = '/'
  )
)
df_marginal_dist = read.csv("marginal_distributions_84583NED-formatted.csv", sep = ",")

# filter DHZW area
if (filter_DHZW) {
  setwd(paste(this.path::this.dir(), 'data', sep = '/'))
  DHZW_neighborhood_codes <-
    read.csv("DHZW_neighbourhoods_codes.csv",
             sep = ";" ,
             header = F)$V1
  df_marginal_dist = df_marginal_dist[df_marginal_dist$neighb_code %in% DHZW_neighborhood_codes, ]
}

# Load datasets of the municipality for that year
setwd(
  paste(
    this.path::this.dir(),
    "data/processed",
    year,
    municipality,
    'households',
    sep = '/'
  )
)
df_parents <-
  read_csv("couples_singleparents-71488NED-formatted.csv")
df_individuals_nochildren <-
  read_csv("singles_couples_nochildren-71488NED-formatted.csv")
df_singleparents <-
  read_csv("singleparents_gender-71488NED-formatted.csv")

# Load datasets on national level
setwd(
  paste(
    this.path::this.dir(),
    "data/processed",
    sep = '/'
  )
)
df_couples_genders <-
  read.csv("couples_gender_disparity_37772ENG-formatted.csv", sep = ',')
df_couples_ages <- read.csv("couples_age_disparity.csv", sep = ',')

# Load synthetic population
setwd(paste0(this.path::this.dir(), "/output/synthetic-population"))
df_synth_pop = read.csv('synthetic_population_DHZW_2019.csv')
df_synth_pop$hh_ID = NA
df_synth_pop$hh_type = NA

# Empty container of households
df_households <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(df_households) <- c("hh_ID", "hh_size", "neighb_code")

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
      
      df_households[hh_ID, ] = c(hh_ID,
                                 as.numeric(hh_size),
                                 neighb_code)
      
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
                               neighb_code)
    
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
                               neighb_code)
    
    # get new single
    single_ID = remaining_agents[counter_singles, 'agent_ID']
    
    # add agent to house
    df_synth_pop[df_synth_pop$agent_ID == single_ID, ]$hh_ID = hh_ID
    df_synth_pop[!is.na(df_synth_pop$hh_ID) &
                   df_synth_pop$hh_ID == hh_ID, ]$hh_type <- 'single'
    
    counter_singles = counter_singles + 1
  }
  
}

end_time <- Sys.time()
difftime(end_time, start_time, units = "secs")
difftime(end_time, start_time, units = "mins")

setwd(paste(this.path::this.dir(), 'output/synthetic-population-households', sep =
              '/'))
if (filter_DHZW) {
  write.csv(df_synth_pop, paste0('synthetic_population_DHZW_', year, '_with_hh.csv'), row.names = FALSE)
  write.csv(df_households, paste0('df_households_DHZW_', year, '.csv'), row.names = FALSE)
} else {
  write.csv(
    df_synth_pop,
    paste0('synthetic_population_', municipality, '_', year, '_with_hh.csv'),
    row.names = FALSE
  )
  write.csv(df_households, paste0('df_households_DHZW_', municipality, '_', year, '.csv'), row.names = FALSE)
}