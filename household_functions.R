library(dplyr)

sample_agent <- function(df_SynthPop, df_StratHousehold, type) {
  sample_age_gender <- df_StratHousehold[sample.int(nrow(df_StratHousehold),
                                                    1,
                                                    replace = TRUE,
                                                    prob = df_StratHousehold$singles),] 
  
  agent = df_SynthPop[which.max(df_SynthPop$age_group == sample_age_gender[1, 'age_group'] &
                                  df_SynthPop$gender == sample_age_gender[1, 'gender'] &
                                  df_SynthPop$household_position == type),]
  if (nrow(agent)==0){
    return -1
  } else {
    return (agent)
  }
}

