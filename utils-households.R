# Generate the gender of the partner based on the agent's gender (@agent_gender) and the distribution
# of genders among couples (@df_gender_disparity).

sample_partner_gender <- function(agent_gender, df_gender_disparity) {
  # subselect the correct genders of the frequency dataframe based on the agent's gender
  if (agent_gender == 'male') {
    df_gender_disparity = df_gender_disparity[df_gender_disparity$type=='male_female' | df_gender_disparity$type=='male_male',]
  } else {
    df_gender_disparity = df_gender_disparity[df_gender_disparity$type=='male_female' | df_gender_disparity$type=='female_female',]
  }
  
  # sample partner's gender based on frequencies
  partner_gender <- sample(
    x = df_gender_disparity$type,
    size = 1,
    replace=TRUE,
    prob = df_gender_disparity$freq
  )
  
  if(agent_gender=='male') {
    if(partner_gender=='male_female') {
      # agent: male, partner = female
      partner_gender='female'
    } else{
      # agent: male, partner = male
      partner_gender='male'
    }
  } else {
    if(partner_gender=='male_female') {
      # agent: female, partner = male
      partner_gender='male'
    } else{
      # agent: female, partner = female
      partner_gender='female'
    }  
  }
  
  return(partner_gender)
}

# Generate all possible partner's ages based on age disparity frequencies and
# each other gender
sample_partner_age <- function (agent_gender, partner_gender, agent_age, df_age_couples_disparity) {
  # subselect the correct age disparities of the frequency dataframe based on agent's and partner's gender
  if (agent_gender != partner_gender) {
    df_age_couples_disparity = df_age_couples_disparity %>%
      select(gap, male_female) %>%
      rename(freq = male_female)
  } else {
    if(agent_gender == 'male') {
      df_age_couples_disparity = df_age_couples_disparity %>%
        select(gap, male_male) %>%
        rename(freq = male_male)
    } else {
      df_age_couples_disparity = df_age_couples_disparity %>%
        select(gap, male_female) %>%
        rename(freq = male_female)     
    }
  }
  
  # sample partner's difference age
  age_diff <- sample(
    x = df_age_couples_disparity$gap,
    size = 1,
    replace=TRUE,
    prob = df_age_couples_disparity$freq
  )
  
  if(age_diff=='0') {
    # same age as partner
    partner_ages = c(agent_age)
  } else if (age_diff == '1_4'){
    age_diff = runif(1, 0, 4)
    partner_ages = c((agent_age-4) : (agent_age-1), (agent_age+1) : (agent_age+4))
  } else if (age_diff == '5_9'){
    age_diff = runif(1, 5, 9)
    partner_ages = c((agent_age-5) : (agent_age-9), (agent_age+5) : (agent_age+9))
  } else if (age_diff == '10_14'){
    age_diff = runif(1, 10, 14)
    partner_ages = c((agent_age-10) : (agent_age-14), (agent_age+10) : (agent_age+14))
  } else if (age_diff == '15_19'){
    age_diff = runif(1, 15, 19)
    partner_ages = c((agent_age-15) : (agent_age-19), (agent_age+15) : (agent_age+19))
  } else {
    age_diff = runif(1, 20, 105)
    partner_ages = c(0: (agent_age-20), (agent_age+20) : 105)
  }
  
  return(partner_ages)  
}

sample_child_age <- function(mother_age, child_position, avg_child_diff_first, avg_child_diff_second, avg_child_diff_all) {
  if (child_position==1) {
    child_age = mother_age - avg_child_diff_first
  } else if (child_position==2) {
    child_age = mother_age - avg_child_diff_second
  } else {
    child_age = mother_age - avg_child_diff_all
  }
  
  if (child_age<0) {
    child_age=0
  }
  return(child_age)
}