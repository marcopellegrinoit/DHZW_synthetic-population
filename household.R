library(GenSynthPop)
library(dplyr)

library("this.path")
setwd(this.path::this.dir())

# Load synthetic population
setwd(paste(this.path::this.dir(), "/data/synthetic-populations", sep = ""))
df_SynthPop = read.csv('synthetic_population_DHZW.csv')

# Load formatted stratified datasets about household position, gender and groupages
setwd(paste(this.path::this.dir(), "/data/households/distributions", sep = ""))
df_StratHousehold = read.csv("household_gender_age-71488NED-formatted.csv", sep = ",", fileEncoding="UTF-8-BOM")

# Load and reformat household size distributions
df_HouseholdSize = read.csv("household_size_71486NED.csv", sep = ";", fileEncoding="UTF-8-BOM")
df_HouseholdSize = df_HouseholdSize %>%
  rename('1' = Eenpersoonshuishouden_22,
         '2' = k_2Personen_24,
         '3' = k_3Personen_25,
         '4' = k_4Personen_26,
         '5_or_more' = k_5OfMeerPersonen_27
         )%>%
  select('1', '2', '3', '4', '5_or_more')
df_HouseholdSize = as.data.frame(t(df_HouseholdSize))
colnames(df_HouseholdSize) = c('freq')
df_HouseholdSize <- cbind(size = rownames(df_HouseholdSize), df_HouseholdSize)
rownames(df_HouseholdSize) <- 1:nrow(df_HouseholdSize)


#################################################
# function to sample an agent from the synthetic population on age, gender and household type, using the household dataset as frequency distribution

sample_agent <- function(df_SynthPop, df_StratHousehold, hh_type, neighb_code) {
  # sample agegroup and gender from the given household type distribution
  sample_age_gender <- df_StratHousehold[sample.int(nrow(df_StratHousehold),
                                                    1,
                                                    replace = TRUE,
                                                    prob = unlist(df_StratHousehold[hh_type])),] 
  
  # sample the agent with the given agegroup, gender, household type and (optionally) neighbourhood code
  if (missing(neighb_code)) {
    agent = df_SynthPop[which.max(df_SynthPop$age_group == sample_age_gender[1, 'age_group'] &
                                  df_SynthPop$gender == sample_age_gender[1, 'gender'] &
                                  df_SynthPop$household_position == hh_type),
                        ]
  } else {
    agent = df_SynthPop[which.max(df_SynthPop$neighb_code == neighb_code,
                                  df_SynthPop$age_group == sample_age_gender[1, 'age_group'] &
                                  df_SynthPop$gender == sample_age_gender[1, 'gender'] &
                                  df_SynthPop$household_position == hh_type),
                        ]
  }
  
  # return the agent, or if not found, code -1
  return (agent)
}

################################################################################
# Main algorithm
################################################################################

# prepare empty dataframe of household IDs and sizes
df_Households <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(df_Households) <- c("hh_ID", "hh_size")
setwd(paste(this.path::this.dir(), "/data/households/output", sep = ""))

# Iterate to generate some households
for (i in 1:50) {
  
  # sample a household size
  hh_size <- sample(
    x = df_HouseholdSize$size,
    size = 1,
    replace=TRUE,
    prob=df_HouseholdSize$freq)
  
  # Act based on the household size
  hh_type = NA
  if(hh_size == 1) { # person living alone
    hh_type = 'singles'
    agent = sample_agent(df_SynthPop, df_StratHousehold, hh_type)
    if (nrow(agent)>0){
      # add new row to the main household dataframe
      hh_ID = nrow(df_Households) + 1
      df_Households[hh_ID,] = c(hh_ID, hh_size)
      
      # create a new file dataframe for this new household
      new_household = agent
      write.csv(new_household, gsub(" ", "", paste('hh_',hh_ID,'.csv')))
      
      # remove agent from agents list
      df_SynthPop = df_SynthPop[df_SynthPop$agent_ID != agent$agent_ID,]
    }
  } else if(hh_size == 2) {
    
  }
}

# save main dataframe of households
write.csv(df_Households, 'df_households.csv')
