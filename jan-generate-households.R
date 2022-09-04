library("this.path")
setwd(this.path::this.dir())
library(dplyr)
library (readr)
source('jan-utils-households.R')

municipality = "den_haag_2019"

setwd(this.path::this.dir())
setwd(paste0('data/', municipality, '/households/distributions'))
df_parents <- read_csv("couples_singleparents-71488NED-formatted.csv")
df_singleparents <- read_csv("singleparents_gender-71488NED-formatted.csv")

setwd(paste0(this.path::this.dir(), '/data/'))
df_couples_genders <- read.csv("couples_gender_disparity_37772ENG-formatted.csv", sep=',')
df_couples_ages <- read.csv("couples_age_disparity.csv", sep=',')

setwd(paste0(this.path::this.dir(), "/synthetic-populations"))
df_synth_pop = read.csv('synthetic_population_DHZW_2019.csv')
df_synth_pop$hh_ID = NA
df_synth_pop$hh_type = NA

# Empty container of households
df_households <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(df_households) <- c("hh_ID", "children_in_house", "neighb_code")

# For each neighbourhood area
#for (neighb_code in unique(df_synth_pop$neighb_code)) {
neighb_code = 'BU05183398'
  
  # Retrieve how many households per children in households we need
  n_households = get_households_children(df_synth_pop, neighb_code)
  
  # list of all unassigned children
  df_children_unassigned = df_synth_pop[df_synth_pop$hh_position=='child' & df_synth_pop$neighb_code==neighb_code,]
  
  # For quantity of children in household, from largest to smallest (we start with the most difficult constraints)
  for (children_size in (nrow(n_households):1)) {
    
    # repeat for all the households needed for such quantity of children in household
    counter_houses = 0
    while(counter_houses < (n_households[children_size,]$num_households-1)){
      # Get new household ID
      hh_ID = nrow(df_households) + 1
      
      # empty list of children for this new household
      df_children_house <- df_children_unassigned[0,]
      
      # Pick a random child c and remove from list
      c = sample_n(df_children_unassigned, 1)
      c$hh_ID = hh_ID
      df_children_unassigned = df_children_unassigned[df_children_unassigned$agent_ID != c$agent_ID,]
      df_children_house <- rbind(df_children_house, c)
      
      # Assign child to household
      df_synth_pop[df_synth_pop$agent_ID==c$agent_ID,]$hh_ID = hh_ID
      
      # While household has fewer children than children_in_house
      counter_siblings = 0
      while (counter_siblings < (children_size-1)) {
        # Get the ages a_c1, . , a_cn of all children c_1, . , c_n thus far assigned
        ages_children = df_children_house$age
        
        # Assign each remaining child x a probability p(x) of being a sibling of c_1, . ,c_n, based on how well their age a_x matches the age difference with argmin (a_c1, a_cn ) or argmax(a_c1, a_c_n)
        # todo, sample next sibling randomly
        x = sample_n(df_children_unassigned, 1)
        df_children_unassigned = df_children_unassigned[df_children_unassigned$agent_ID != x$agent_ID,]
        df_children_house <- rbind(df_children_house, x)
        df_synth_pop[df_synth_pop$agent_ID==x$agent_ID,]$hh_ID = hh_ID
        
        counter_siblings = counter_siblings + 1
      }
      
      # Get average age of children in the house
      avg_children_age = mean(df_children_house$age)
      
      # Get age of the oldest child
      oldest_child_age = max(df_children_house$age)
      
      # Sample if household is singleparent or couple
      hh_type = sample(df_parents$hh_type,
             1,
             replace=FALSE,
             prob = df_parents$prob)
      
      if (hh_type=='singleparents'){
        
        # case: single-parent
        hh_size = nrow(df_children_house) + 1
        
        # generate gender of the parent from distribution
        singleparent_gender = sample(df_singleparents$gender,
                                     1,
                                     replace=FALSE,
                                     prob = df_singleparents$prob)
        
        if (singleparent_gender == 'female') {
          # case: mother
          parent_ID = get_mother(df_synth_pop, neighb_code, avg_children_age, oldest_child_age)
        } else {
          # case: father
          age_fake_mother = get_age_fake_mother(df_synth_pop, neighb_code, avg_children_age, oldest_child_age)
          parent_ID = get_second_partner(df_synth_pop, neighb_code, age_fake_mother, 'male_female')
        }

        # add parent to house
        df_synth_pop[df_synth_pop$agent_ID==parent_ID,]$hh_ID = hh_ID
        
        df_synth_pop[!is.na(df_synth_pop$hh_ID) & df_synth_pop$hh_ID==hh_ID,]$hh_type<-'single_parent'
        
      } else {
        
        # case: couple
        hh_size = nrow(df_children_house) + 2
        
        couple_gender = sample(df_couples_genders$genders,
                               1,
                               replace=FALSE,
                               prob = df_couples_genders$prob)
        
        if(couple_gender == 'male_female') {
          # couple with mother and father
          
          # get parents
          mother_ID = get_mother(df_synth_pop, neighb_code, avg_children_age, oldest_child_age)
          mother = df_synth_pop[df_synth_pop$agent_ID==mother_ID,]
          
          father_ID = get_second_partner(df_synth_pop, neighb_code, mother$age, couple_gender)
          
          # add parents to house
          df_synth_pop[df_synth_pop$agent_ID==mother_ID,]$hh_ID = hh_ID
          df_synth_pop[df_synth_pop$agent_ID==father_ID,]$hh_ID = hh_ID
          df_synth_pop[!is.na(df_synth_pop$hh_ID) & df_synth_pop$hh_ID == hh_ID,]$hh_type<-'couple_straight'
          
        } else if (couple_gender == 'male_male') {
          # couple with two fathers
          
          # generate first father based on a fake mother
          age_fake_mother = get_age_fake_mother(df_synth_pop, neighb_code, avg_children_age, oldest_child_age)
          first_father_ID = get_second_partner(df_synth_pop, neighb_code, age_fake_mother, 'male_female')
          first_father = df_synth_pop[df_synth_pop$agent_ID==first_father_ID,]
          
          # generate second father based on the first father
          second_father_ID = get_second_partner(df_synth_pop, neighb_code, first_father$age, couple_gender)
          
          # add parents to house
          df_synth_pop[df_synth_pop$agent_ID==first_father_ID,]$hh_ID = hh_ID
          df_synth_pop[df_synth_pop$agent_ID==second_father_ID,]$hh_ID = hh_ID
          df_synth_pop[!is.na(df_synth_pop$hh_ID) & df_synth_pop$hh_ID == hh_ID,]$hh_type<-'couple_gay'
          
        } else if (couple_gender == 'female_female') {
          # couple with two mothers
          
          # get first mother as real
          first_mother_ID = get_mother(df_synth_pop, neighb_code, avg_children_age, oldest_child_age)
          
          first_mother = df_synth_pop[df_synth_pop$agent_ID==first_mother_ID,]
          
          # generate second mother based on the first mother
          second_mother_ID = get_second_partner(df_synth_pop, neighb_code, first_mother$age, couple_gender)

          # add parents to house
          df_synth_pop[df_synth_pop$agent_ID==first_mother_ID,]$hh_ID = hh_ID
          df_synth_pop[df_synth_pop$agent_ID==second_mother_ID,]$hh_ID = hh_ID
          df_synth_pop[!is.na(df_synth_pop$hh_ID) & df_synth_pop$hh_ID == hh_ID,]$hh_type<-'couple_lesbian'
        }
      }

      df_households[hh_ID,] = c(
        hh_ID,
        as.numeric(hh_size),
        neighb_code)
      
      counter_houses = counter_houses + 1
    }
    
    # todo: create households for couples without children and singles
    
  }
#}

