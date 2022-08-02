library(dplyr)

library("this.path")
setwd(this.path::this.dir())
source('utils-households.R')

municipality = 'den_haag_2019'
flag_save = FALSE

# Load synthetic population
setwd(paste(this.path::this.dir(), "/data/", municipality, "/synthetic-populations", sep = ""))
df_UnassignedAgents = read.csv('synthetic_population_DHZW.csv')

# Load formatted stratified datasets about household position, gender and groupages
setwd(paste(this.path::this.dir(), "/data/", municipality, "/households/distributions", sep = ""))
df_StratHousehold = read.csv("household_gender_age-71488NED-formatted.csv", sep = ",", fileEncoding="UTF-8-BOM")
df_HouseholdSize = read.csv('household_size_71486NED-formatted.csv')

################################################################################
# Main algorithm
################################################################################

# prepare empty dataframe of household IDs and sizes
df_Households <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(df_Households) <- c("hh_ID", "hh_size", "neighb_code")
setwd(paste(this.path::this.dir(), "/data/", municipality, "/households/output", sep = ""))

################################################################################
## First, assign all singles
df_Singles = df_UnassignedAgents[df_UnassignedAgents$hh_position=='single',]

# create households ID and size
df_Households <- tibble::rowid_to_column(df_Singles, "hid")
df_Households$hh_size = 1

# split single individuals into households and save them
if (flag_save) {
  out <- split(df_Households, df_Households$hid) # list of dfs
  lapply(names(out),
         function(x){write.csv(out[[x]], paste0("DHZW_household_",x,".csv"), row.names = FALSE)})
}

# create the main households container
df_Households = df_Households %>%
  select(hid,
         hh_size,
         neighb_code)

# remove single unassigned agents
df_UnassignedAgents <- df_UnassignedAgents[!(df_UnassignedAgents$agent_ID %in% df_Singles$agent_ID),]
remove(df_Singles)

################################################################################
# todo


for (i in 1:10) {
  neighb_code = 'BU05183387'
  
  # create single-person households
  
  
  
  
  # sample a household size
  hh_size <- sample(
    x = df_HouseholdSize$size,
    size = 1,
    replace=TRUE,
    prob=df_HouseholdSize$freq)
  
  
  # Act based on the household size
  hh_type = NA
  if(hh_size == 1) { # person living alone
    hh_type = 'single'
    agent = sample_agent(df_SynthPop = df_SynthPop,
                         df_StratHousehold = df_StratHousehold,
                         hh_position = 'single',
                         neighb_code = neighb_code)
    if (nrow(agent)>0){
      # add new row to the main household dataframe
      hh_ID = nrow(df_Households) + 1
      df_Households[hh_ID,] = c(hh_ID, hh_size, neighb_code)
      
      # create a new file dataframe for this new household
      new_household = agent
      write.csv(new_household, gsub(" ", "", paste('hh_',hh_ID,'.csv')))
      
      # remove agent from agents list
      df_SynthPop = df_SynthPop[df_SynthPop$agent_ID != agent$agent_ID,]
    }
  } else if(hh_size == 2) {
    # decide hh_type between 'single-parent' and 'couple'
    
  }
}

# save main dataframe of households
write.csv(df_Households, 'df_households.csv')
