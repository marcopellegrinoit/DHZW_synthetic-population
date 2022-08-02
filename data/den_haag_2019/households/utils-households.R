# function to sample an agent from the synthetic population on age, gender and household type, using the household dataset as frequency distribution

sample_agent <- function(df_SynthPop, df_StratHousehold, hh_position, neighb_code, fixed_groupage, fixed_gender) {
  # sample agegroup and gender from the given household type distribution
  sample_age_gender <- df_StratHousehold[sample.int(nrow(df_StratHousehold),
                                                    1,
                                                    replace = TRUE,
                                                    prob = unlist(df_StratHousehold[hh_position])),]
  
  # sample the agent with the given groupage, gender, household type and (optionally) neighbourhood code
  
  if (missing(fixed_groupage)) {
    # only neighbourhood code fixed
    agent = df_SynthPop[which.max(df_SynthPop$neighb_code == neighb_code &
                                    df_SynthPop$age_group == sample_age_gender[1, 'age_group'] &
                                    df_SynthPop$gender == sample_age_gender[1, 'gender'] &
                                    df_SynthPop$household_position == hh_position),
    ]
  } else {
    if (missing(fixed_gender)) {
      # fix neighbourhood code and groupage
      df_filtered = df_StratHousehold[df_StratHousehold$age_group == fixed_groupage,]
      sample_age_gender <- df_filtered[sample.int(nrow(df_filtered),
                                                  1,
                                                  replace = TRUE,
                                                  prob = unlist(df_filtered[hh_position])),]
      
      agent = df_SynthPop[which.max(df_SynthPop$neighb_code == neighb_code &
                                      df_SynthPop$age_group == fixed_groupage &
                                      df_SynthPop$gender == sample_age_gender[1, 'gender'] &
                                      df_SynthPop$household_position == hh_position),
      ]
    } else {
      # fix neighbourhood code, groupage and gender
      df_filtered = df_StratHousehold[df_StratHousehold$age_group == fixed_groupage &
                                        df_StratHousehold$gender == fixed_gender,]
      
      sample_age_gender <- df_filtered[sample.int(nrow(df_filtered),
                                                  1,
                                                  replace = TRUE,
                                                  prob = unlist(df_filtered[hh_position])),]
      
      agent = df_SynthPop[which.max(df_SynthPop$neighb_code == neighb_code &
                                      df_SynthPop$age_group == fixed_groupage &
                                      df_SynthPop$gender == fixed_gender &
                                      df_SynthPop$household_position == hh_position),
      ]
    }
  }
  
  # return the agent, or if not found, code -1
  return (agent)
}