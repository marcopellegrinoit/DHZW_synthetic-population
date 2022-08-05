library(dplyr)

library("this.path")
setwd(this.path::this.dir())
source('utils-households.R')

municipality = 'den_haag_2019'
year = 2019
flag_save = FALSE

# Load synthetic population
setwd(paste(this.path::this.dir(), "/synthetic-populations", sep = ""))
df_UnassignedAgents = read.csv('synthetic_population_DHZW.csv')

# Load formatted stratified datasets about household position, gender and groupages
setwd(paste(this.path::this.dir(), "/data/", municipality, "/households/distributions", sep = ""))
df_StratHousehold = read.csv("household_gender_age-71488NED-formatted.csv", sep = ",", fileEncoding="UTF-8-BOM")
df_HouseholdSize = read.csv('household_size_71486NED-formatted.csv')
df_HouseholdSize_original = df_HouseholdSize
df_HouseholdSize = df_HouseholdSize[df_HouseholdSize$size != 1,] # since we already knows the single there is no necessity of it in the distribution

################################################################################
# Main algorithm
################################################################################

# prepare empty dataframe of household IDs and sizes
df_Households <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(df_Households) <- c("hh_ID", "hh_size", "neighb_code")
setwd(paste(this.path::this.dir(), "/data/", municipality, "/households/output", sep = ""))

################################################################################
# First, assign all singles
df_Singles = df_UnassignedAgents[df_UnassignedAgents$hh_position=='single',]

# create households ID and size
df_Households <- tibble::rowid_to_column(df_Singles, "hid")
df_Households$hh_size = 1
df_Households$hh_type = 'single'

# split single individuals into households and save them
if (flag_save) {
  out <- split(df_Households, df_Households$hid) # list of dfs
  lapply(names(out),
         function(x){write.csv(out[[x]], paste0("household_",x,".csv"), row.names = FALSE)})
}

# create the main households container
df_Households = df_Households %>%
  select(hid,
         hh_size,
         neighb_code,
         hh_type)

# remove single unassigned agents
df_UnassignedAgents <- df_UnassignedAgents[!(df_UnassignedAgents$agent_ID %in% df_Singles$agent_ID),]
remove(df_Singles)

################################################################################

# go through each neighbourhood
for (neighb_code in unique(df_UnassignedAgents$neighb_code)) {
  # for each neighbourhood
  #neighb_code = 'BU05183387'
  
  ##############################################################################
  # Assign single-parents and their child(ren)
  
  df_NeighbUnassigned = df_UnassignedAgents[df_UnassignedAgents$neighb_code==neighb_code,]
  
  df_SingleParents = df_NeighbUnassigned[df_NeighbUnassigned$hh_position=='single_parent',]
  df_Couples = df_NeighbUnassigned[df_NeighbUnassigned$hh_position=='couple',]
  
  df_Children_Neighbourhood = df_NeighbUnassigned[df_NeighbUnassigned$hh_position=='child',]
  #n_children_singles = nrow(df_SingleParents)/(nrow(df_SingleParents)+(nrow(df_Couples)/2))
  
  #df_Children_Singles = df_Children_Neighbourhood[sample(nrow(df_Children_Neighbourhood),  (n_children_singles*nrow(df_Children_Neighbourhood)) ), ]
  #df_Children_Couples = df_Children_Neighbourhood[!(df_Children_Neighbourhood$agent_ID %in% df_Children_Singles$agent_ID),]
  df_Children_Singles = df_Children_Neighbourhood
  
  
  print(paste('Assigning now', nrow(df_SingleParents), 'parents', sep = ' '))
  
  while(nrow(df_SingleParents)>0 & (nrow(df_Children_Singles)>0)){
    
    # for each single parent in this neighbourhood
    parent = df_SingleParents[sample(nrow(df_SingleParents),  1), ]
      
      # sample a household size
      hh_size <- sample(
        x = df_HouseholdSize$size,
        size = 1,
        replace=TRUE,
        prob=df_HouseholdSize$freq)
      
      if (nrow(df_Children_Singles)>= hh_size) {
        # if there are enough children for the house
        children_for_parent = df_Children_Singles[sample(nrow(df_Children_Singles), (hh_size-1)), ]
      } else {
        # add the few remaining children
        children_for_parent = df_Children_Singles
        hh_size = nrow(df_Children_Singles)+1 # override with new house size
      }
      
      df_Households[nrow(df_Households) + 1,] = c(
        nrow(df_Households) + 1,
        hh_size,
        neighb_code,
        'single_parent')
      
      # remove parent and children
      df_UnassignedAgents <- df_UnassignedAgents[!(df_UnassignedAgents$agent_ID %in% parent$agent_ID),]
      df_SingleParents = df_SingleParents[!(df_SingleParents$agent_ID %in% parent$agent_ID),]
      df_UnassignedAgents <- df_UnassignedAgents[!(df_UnassignedAgents$agent_ID %in% children_for_parent$agent_ID),]
      df_Children_Singles <- df_Children_Singles[!(df_Children_Singles$agent_ID %in% children_for_parent$agent_ID),]
  }
  
  if(nrow(df_Children_Singles)==0){
    print('Interrupted because there are not enough children  single-parents')
    print(paste(nrow(df_SingleParents), 'unassinged parents left', sep = ' '))
  }
  
  ##############################################################################
  # Assign couples and their child(ren)
  
  df_Children_Couples = df_Children_Singles
  
  df_NeighbUnassigned = df_UnassignedAgents[df_UnassignedAgents$neighb_code==neighb_code,]
  
  while(nrow(df_Couples)>=2) {
    # if there is at least a couple
    
    # get a random parent
    first_parent = df_Couples[sample(nrow(df_Couples), 1), ]
    second_parent = df_Couples[sample(nrow(df_Couples), 1), ]
    
    # sample a household size
    hh_size <- sample(
      x = df_HouseholdSize$size,
      size = 1,
      replace=TRUE,
      prob=df_HouseholdSize$freq)
    
    if (hh_size > 2 & nrow(df_Children_Couples)>0) {
      # if there should be children and if there are still some to be assigned
      
      if (nrow(df_Children_Couples)>= hh_size) {
        # if there are enough children for the house
        children_for_parent = df_Children_Couples[sample(nrow(df_Children_Couples), (hh_size-2)), ]
      } else {
        # add the few remaining children
        children_for_parent = df_Children_Couples
        hh_size = nrow(df_Children_Couples)+2 # override with new house size
        
        print(paste('children are finished are there are still couples people to assign: ', nrow(df_Couples), sep=" "))
      }
      
      # remove children
      df_UnassignedAgents <- df_UnassignedAgents[!(df_UnassignedAgents$agent_ID %in% children_for_parent$agent_ID),]
      df_Children_Couples <- df_Children_Couples[!(df_Children_Couples$agent_ID %in% children_for_parent$agent_ID),]
    } else {
      if(nrow(df_Children_Couples)==0) {
        #print(paste('couples: children are finished and there are still ', nrow(df_Couples), 'couple-parents to be assigned'))
      }
    }
    
    df_Households[nrow(df_Households) + 1,] = c(
      nrow(df_Households) + 1,
      hh_size,
      neighb_code,
      'couple')
    
    # remove parent
    df_UnassignedAgents <- df_UnassignedAgents[!(df_UnassignedAgents$agent_ID %in% first_parent$agent_ID),]
    df_Couples <- df_Couples[!(df_Couples$agent_ID %in% first_parent$agent_ID),]
    df_UnassignedAgents <- df_UnassignedAgents[!(df_UnassignedAgents$agent_ID %in% second_parent$agent_ID),]
    df_Couples <- df_Couples[!(df_Couples$agent_ID %in% second_parent$agent_ID),]
  }
  
}

  
dist_size = df_HouseholdSize_original
dist_size$generated = 0
for (s in 1:nrow(dist_size)) {
  dist_size[s,]$generated = nrow(df_Households[df_Households$neighb_code==neighb_code & df_Households$hh_size==dist_size[s, 'size'],])
}
dist_size$freq = dist_size$freq/sum(dist_size$freq)
dist_size$generated = dist_size$generated/sum(dist_size$generated)