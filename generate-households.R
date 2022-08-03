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
df_HouseholdSize = df_HouseholdSize[df_HouseholdSize$size != 1,] # since we already knows the single there is no necessity of it in the distribution

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

# calculate total frequencies for couples and single-parents

df_FreqCoupleSingleParents = df_StratHousehold %>%
  select(couple, single_parent)
df_FreqCoupleSingleParents = as.data.frame(t(df_FreqCoupleSingleParents))
df_FreqCoupleSingleParents$freq = rowSums(df_FreqCoupleSingleParents)
df_FreqCoupleSingleParents = df_FreqCoupleSingleParents %>%
  select(freq)
df_FreqCoupleSingleParents <- cbind(type = rownames(df_FreqCoupleSingleParents), df_FreqCoupleSingleParents)
rownames(df_FreqCoupleSingleParents) <- 1:nrow(df_FreqCoupleSingleParents)

# go thhrough each neighbourhood
for (i in 1:100) {
  neighb_code = 'BU05183398'
  
  # sample a household size
  hh_size <- sample(
    x = df_HouseholdSize$size,
    size = 1,
    replace=TRUE,
    prob=df_HouseholdSize$freq)
  
  # decide if it is a couple, or a single-parent
  parent_type <- sample(
    x = df_FreqCoupleSingleParents$type,
    size = 1,
    replace=TRUE,
    prob=df_FreqCoupleSingleParents$freq)
  
  if (parent_type=='single_parent'){
    # single-parent
    agent = sample_agent(df_SynthPop =  df_UnassignedAgents,
                         df_StratHousehold = df_StratHousehold,
                         hh_position = parent_type,
                         neighb_code = neighb_code)
    
    # loop over the children
    for (i in 1:(hh_size-1)){
      if (agent$gender == 'female') {
        # decide child age based on mother-child age disparity
        child_groupage = NA
        
        child = sample_agent(df_SynthPop =  df_UnassignedAgents,
                             df_StratHousehold = df_StratHousehold,
                             hh_position = 'child',
                             neighb_code = neighb_code,
                             fixed_groupage = child_groupage)
      } else {
        # randomly pick a child
        child = sample_agent(df_SynthPop =  df_UnassignedAgents,
                             df_StratHousehold = df_StratHousehold,
                             hh_position = 'child',
                             neighb_code = neighb_code)
      }
      
    }
    
  } else {
    # couple
  }
 
}