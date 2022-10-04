library("this.path")
library(dplyr)
library (readr)
setwd(this.path::this.dir())
source('../config/config.R')

# Load datasets of the municipality for that year
setwd(
  paste(
    "../data/processed",
    year,
    municipality,
    'households',
    sep = '/'
  )
)
df_children_aggregated <- read_csv("children_71486NED-formatted.csv")
df_mother_children <- read.csv("mother_children_age_37201-formatted.csv")

setwd(this.path::this.dir())
# Load datasets on national level
setwd(
  paste(
    "../data/processed",
    sep = '/'
  )
)
df_age_couples <- read.csv("couples_age_disparity.csv", sep=',')
df_age_couples_heterosexual <- read_delim("couples_age_disparity_heterosexual-formatted.csv", 
                                                           delim = ";", escape_double = FALSE, trim_ws = TRUE)


## Implementation of Jan's Methods 2
get_households_children <- function(df_synth_pop, neighb_code) {
  n_children = nrow(df_synth_pop[df_synth_pop$is_child==1 & df_synth_pop$neighb_code==neighb_code,])
  
  # calculate probs
  p_child_in_household = df_children_aggregated
  p_child_in_household = subset(p_child_in_household, select=-c(unmarried, married, singleparents, total_children))
  p_child_in_household$prob = p_child_in_household$prob * p_child_in_household$children_in_house
  
  # normalise
  p_child_in_household$prob = p_child_in_household$prob / sum(p_child_in_household$prob)
  
  # Next, we can just use those probabilities as fractions of the number of children in each household
  children_in_households = p_child_in_household
  children_in_households$num_children = children_in_households$prob * n_children
  children_in_households = subset(children_in_households, select=-c(prob))
  
  # Make sure that each bin for a household size contains a number of children that is neatly divisible over the actual size of that household
  for (i in (nrow(children_in_households):2)) {
    remainder = children_in_households[i,]$num_children %% children_in_households[i,]$children_in_house
    if (remainder > 0) {
      children_in_households[i, 'num_children'] = children_in_households[i, 'num_children'] - remainder
      children_in_households[i-1, 'num_children'] = children_in_households[i-1, 'num_children'] - remainder
    }
  }
  # I do the the last step manually
  children_in_households[1,]$num_children = n_children - sum(children_in_households[(2:nrow(children_in_households)),]$num_children)
  
  # Now we calculate the actual number of households again by dividing the number of children by the size
  n_households = children_in_households
  n_households$num_households = n_households$num_children / n_households$children_in_house
  n_households = subset(n_households, select=-c(num_children))
  
  # Print results
  print('')
  print('---------------------------------------------------------------------')
  print(paste0('Neighbourhood code: ', neighb_code))
  print(paste0('Total amount of children: ', n_children))
  for(i in 1:nrow(df_children_aggregated)) {
    print(paste0('Household composed on ', children_in_households[i,]$children_in_house, ' children'))
    print(paste0('Children: ', children_in_households[i,]$num_children))
    print(paste0(n_households[i,]$num_households, ' households of this size'))
    print(paste0('Which is ', n_households[i,]$num_households/sum(n_households$num_households), ' of total'))
    print('----------------')
  }
  
  return(n_households)
}



get_mother <- function(df_synth_pop, neighb_code, avg_children_age, oldest_child_age) {
  # list of all unassigned mothers
  df_mothers = df_synth_pop[is.na(df_synth_pop$hh_ID) & df_synth_pop$gender=='female' & df_synth_pop$is_child==0 & df_synth_pop$neighb_code==neighb_code,]
  
  # Exclude mothers that are too young for this family
  df_mothers = df_mothers[(df_mothers$age - oldest_child_age) > 15,]
  
  if(nrow(df_mothers)>0){
    # Calculate age difference for each mother
    df_mothers$age_diff = df_mothers$age - avg_children_age
    df_mothers$diff_group = ''
    
    # Give to each mother the probability of being mother in this house, from the CBS dataset
    if(nrow(df_mothers[df_mothers$age_diff < 20,])>0)
      df_mothers[df_mothers$age_diff < 20,]$diff_group='less_20'
    if(nrow(df_mothers[df_mothers$age_diff >= 20 & df_mothers$age_diff < 25,])>0)
      df_mothers[df_mothers$age_diff >= 20 & df_mothers$age_diff < 25,]$diff_group='between_20_25'
    if(nrow(df_mothers[df_mothers$age_diff >= 25 & df_mothers$age_diff < 30,])>0)
      df_mothers[df_mothers$age_diff >= 25 & df_mothers$age_diff < 30,]$diff_group='between_25_30'
    if(nrow(df_mothers[df_mothers$age_diff >= 30 & df_mothers$age_diff < 35,])>0)
      df_mothers[df_mothers$age_diff >= 30 & df_mothers$age_diff < 35,]$diff_group='between_30_35'
    if(nrow(df_mothers[df_mothers$age_diff >= 35 & df_mothers$age_diff < 40,])>0)
      df_mothers[df_mothers$age_diff >= 35 & df_mothers$age_diff < 40,]$diff_group='between_35_40'
    if(nrow(df_mothers[df_mothers$age_diff >= 40 & df_mothers$age_diff < 45,])>0)
      df_mothers[df_mothers$age_diff >= 40 & df_mothers$age_diff < 45,]$diff_group='between_40_45'
    if(nrow(df_mothers[df_mothers$age_diff >= 45,])>0)
      df_mothers[df_mothers$age_diff >= 45,]$diff_group='more_45'
    
    df_mothers = merge(df_mothers, df_mother_children, by='diff_group')
    
    # normalise probabilities
    df_mothers$prob = df_mothers$prob / sum(df_mothers$prob)
    
    # sample mother
    mother_ID = sample(df_mothers$agent_ID, 1, replace=FALSE, prob = df_mothers$prob)
    
    return(mother_ID)
  } else {
    return(-1)
  }
  
  
}

get_age_fake_mother <- function(df_synth_pop, avg_children_age, oldest_child_age) {
  # sample age difference of the ideal mother
  age_fake_mother_group = sample(df_mother_children$diff_group,
                           1,
                           replace=FALSE,
                           prob = df_mother_children$prob)
  
  # transform age group difference into integer age difference generating a random integer in such interval
  if(age_fake_mother_group=='less_20')
    age_fake_mother = sample(c(16:19), 1)
  if(age_fake_mother_group=='between_20_25')
    age_fake_mother = sample(c(20:24), 1)
  if(age_fake_mother_group=='between_25_30')
    age_fake_mother = sample(c(25:29), 1)
  if(age_fake_mother_group=='between_30_35')
    age_fake_mother = sample(c(30:34), 1)
  if(age_fake_mother_group=='between_35_40')
    age_fake_mother = sample(c(35:39), 1)
  if(age_fake_mother_group=='between_40_45')
    age_fake_mother = sample(c(40:44), 1)
  if(age_fake_mother_group=='more_45')
    age_fake_mother = sample(c(45:105), 1)
  
  # compute the ideal mother age adding the difference mother-child age to the average child of the house
  age_fake_mother = age_fake_mother + avg_children_age
  
  # if the oldest child is not 15 years older than the mother, add the remaining years to fulfull this contraint
  if (age_fake_mother < oldest_child_age + 15)
    age_fake_mother = oldest_child_age + 15
  
  return(age_fake_mother)
}

get_second_partner <- function(df_synth_pop, neighb_code, age_first_partner, genders) {
  # get all the remaining second_partners
  if(genders=='male_female') {
    # look for the father, given the mother
    df_second_partners = df_synth_pop[is.na(df_synth_pop$hh_ID) & df_synth_pop$gender=='male' & df_synth_pop$is_child==0 & df_synth_pop$neighb_code==neighb_code,]
  } else if (genders=='male_male') {
    # look for the second father
    df_second_partners = df_synth_pop[is.na(df_synth_pop$hh_ID) & df_synth_pop$gender=='male' & df_synth_pop$is_child==0 & df_synth_pop$neighb_code==neighb_code,]
  } else if (genders=='female_female') {
    # look for the second mother
    df_second_partners = df_synth_pop[is.na(df_synth_pop$hh_ID) & df_synth_pop$gender=='female' & df_synth_pop$is_child==0 & df_synth_pop$neighb_code==neighb_code,]
  }
  
  # for each partner, calculate the age difference with the given partner
  df_second_partners$age_diff = df_second_partners$age - age_first_partner
  
  # round the age difference and its absolute value
  df_second_partners$age_diff = abs(round(df_second_partners$age_diff))
  
  # group age differences
  df_second_partners$gap = ''
  # Give to each father the probability of being the partner of such fake mother
  if(nrow(df_second_partners[df_second_partners$age_diff < 1,])>0)
    df_second_partners[df_second_partners$age_diff ==0 ,]$gap='0'
  if(nrow(df_second_partners[df_second_partners$age_diff >= 1 & df_second_partners$age_diff < 5,])>0)
    df_second_partners[df_second_partners$age_diff >= 1 & df_second_partners$age_diff < 5,]$gap='1_4'
  if(nrow(df_second_partners[df_second_partners$age_diff >= 5 & df_second_partners$age_diff < 10,])>0)
    df_second_partners[df_second_partners$age_diff >= 5 & df_second_partners$age_diff < 10,]$gap='5_9'
  if(nrow(df_second_partners[df_second_partners$age_diff >= 10 & df_second_partners$age_diff < 15,])>0)
    df_second_partners[df_second_partners$age_diff >= 10 & df_second_partners$age_diff < 15,]$gap='10_14'
  if(nrow(df_second_partners[df_second_partners$age_diff >= 15 & df_second_partners$age_diff < 20,])>0)
    df_second_partners[df_second_partners$age_diff >= 15 & df_second_partners$age_diff < 20,]$gap='15_19'
  if(nrow(df_second_partners[df_second_partners$age_diff >= 20,])>0)
    df_second_partners[df_second_partners$age_diff >= 20,]$gap='20_or_more'
  
  # assign probabilities from CBS distribution
  if(genders=='male_female') {
    df_second_partners$gender_older='none'
    if (nrow(df_second_partners[df_second_partners$age > age_first_partner,])>0) {
      df_second_partners[df_second_partners$age > age_first_partner,]$gender_older='male'
    }
    if (nrow(df_second_partners[df_second_partners$age < age_first_partner,])>0) {
      df_second_partners[df_second_partners$age < age_first_partner,]$gender_older='female'
    }
    df_second_partners = merge(df_second_partners, df_age_couples_heterosexual, by=c('gap', 'gender_older'))
  } else {
    df_second_partners = merge(df_second_partners, df_age_couples, by='gap')
  }
  
  # normalise probabilities
  if(genders=='male_female')
    df_second_partners$prob = df_second_partners$prob / sum(df_second_partners$prob)
  else if (genders=='male_male') 
    df_second_partners$prob = df_second_partners$male_male / sum(df_second_partners$male_male)
  else if (genders=='female_female') 
    df_second_partners$prob = df_second_partners$female_female / sum(df_second_partners$female_female)
  
  # sample father
  second_partner_ID = sample(df_second_partners$agent_ID, 1, replace=FALSE, prob = df_second_partners$prob)
  
  return(second_partner_ID)
}