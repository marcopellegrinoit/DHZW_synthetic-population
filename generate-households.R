library(dplyr)

library("this.path")
setwd(this.path::this.dir())
source('utils-households.R')

municipality = 'den_haag_2019'
year = 2019

# Load synthetic population
setwd(paste(this.path::this.dir(), "/synthetic-populations", sep = ""))
df_SynthPop = read.csv('synthetic_population_DHZW.csv')

# Load formatted stratified dataset about household position, gender and groupages (municipality aggregated)
setwd(paste(this.path::this.dir(), "/data/", municipality, "/households/distributions", sep = ""))
df_StratHousehold = read.csv("household_gender_age-71488NED-formatted.csv", sep = ",", fileEncoding="UTF-8-BOM")

# Load households size distribution (municipality aggregated)
df_HouseholdSize = read.csv('household_size_71486NED-formatted.csv')

# Load marginal distributions (neighbourhood aggregated)
setwd(paste(this.path::this.dir(), "/data/", municipality, sep = ""))
df_MarginalDistr = read.csv("marginal_distributions_84583NED-formatted.csv", sep = ",")
# recalculate the correct total of households
df_MarginalDistr$hh_total = df_MarginalDistr$hh_single + df_MarginalDistr$hh_no_children + df_MarginalDistr$hh_no_children

################################################################################
# Generation of appropriate household sizes for each neighbourhood
################################################################################

# prepare empty dataframe of household
df_Households <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(df_Households) <- c("hh_ID", "hh_size", "neighb_code")

for (neighb_code in unique(df_SynthPop$neighb_code)) {
  print(paste('Now working on neighbourhood ', neighb_code, sep=''))
  # for each neighbourhood area
  
  while (sum(as.numeric(df_Households[df_Households$neighb_code==neighb_code,]$hh_size)) < nrow(df_SynthPop[df_SynthPop$neighb_code==neighb_code,])){
    # continue till I do not create households for all the individuals in the neighbourhood
    
    # sample a household size till it fits
    household_fits = TRUE
    while (household_fits) {
      # sample a household size following its municipality aggregated distribution
      hh_size <- sample(
        x = df_HouseholdSize$size,
        size = 1,
        replace=TRUE,
        prob=df_HouseholdSize$freq)
      
      # if the new household fits, add it to the dataframe and continue. Otherwise, sample again
      if (sum(as.numeric(df_Households[df_Households$neighb_code==neighb_code,]$hh_size))+hh_size <= nrow(df_SynthPop[df_SynthPop$neighb_code==neighb_code,])) {
        household_fits = FALSE
        
        df_Households[nrow(df_Households) + 1,] = c(
          nrow(df_Households) + 1,
          as.numeric(hh_size),
          neighb_code)
      }
    }
  }
}

# Save households
setwd(paste(this.path::this.dir(), "/households/", sep = ""))
write.csv(df_Households, 'df_Households.csv', row.names=FALSE)

################################################################################

setwd(paste(this.path::this.dir(), "/households/", sep = ""))
df_Households = read.csv('df_Households.csv')

# check how it related with the households size distribution
df_HouseholdSize$pred=0
for (s in df_HouseholdSize$size) {
  df_HouseholdSize[df_HouseholdSize$size==s,]$pred = nrow(df_Households[df_Households$hh_size==s,])
}

plot(df_HouseholdSize$size, df_HouseholdSize$freq)
plot(df_HouseholdSize$size, df_HouseholdSize$pred)

################################################################################
# Assign household types
################################################################################

df_Households$hh_type=NA

# assign singles
df_Households[df_Households$hh_size==1,]$hh_type='single'

# assign single-parents households based on the stratified proportions to couples
percSingleParents = sum(df_StratHousehold$single_parent)/(sum(df_StratHousehold$single_parent)+(sum(df_StratHousehold$couple)/2))
df_SingleParents = sample_frac(df_Households[df_Households$hh_size!=1,], percSingleParents)
df_Households[df_Households$hh_ID %in% df_SingleParents$hh_ID,]$hh_type= 'single_parent'

# assign the remaining to couples
df_Households[is.na(df_Households$hh_type),]$hh_type='couple'

write.csv(df_Households, 'df_Households.csv', row.names=FALSE)

################################################################################
# Construct marginals distribution
################################################################################

df_GeneratedMarginals = df_MarginalDistr %>%
  select(neighb_code, tot_pop)
df_GeneratedMarginals$pp_single = 0
df_GeneratedMarginals$pp_singleparent = 0
df_GeneratedMarginals$pp_couple = 0
df_GeneratedMarginals$pp_children = 0
df_GeneratedMarginals$pp_households = 0
df_GeneratedMarginals$pp_synthPop = 0


for (neighb_code in unique(df_SynthPop$neighb_code)) {
  
  df_GeneratedMarginals[df_GeneratedMarginals$neighb_code==neighb_code,]$pp_single = nrow(df_Households[df_Households$neighb_code==neighb_code & df_Households$hh_type=='single',])
  df_GeneratedMarginals[df_GeneratedMarginals$neighb_code==neighb_code,]$pp_singleparent = nrow(df_Households[df_Households$neighb_code==neighb_code & df_Households$hh_type=='single_parent',])
  df_GeneratedMarginals[df_GeneratedMarginals$neighb_code==neighb_code,]$pp_couple = nrow(df_Households[df_Households$neighb_code==neighb_code & df_Households$hh_type=='couple',])*2
  
  # The number of children is the number of children of the single_parents and couples households
  df_GeneratedMarginals[df_GeneratedMarginals$neighb_code==neighb_code,]$pp_children = (sum(df_Households[df_Households$neighb_code==neighb_code &
                                                                                                            df_Households$hh_type=='single_parent',]$hh_size) -
                                                                                          nrow(df_Households[df_Households$neighb_code==neighb_code &
                                                                                                               df_Households$hh_type=='single_parent',])
  ) +
    (sum(df_Households[df_Households$neighb_code==neighb_code &
                         df_Households$hh_type=='couple'&
                         df_Households$hh_size>2,]$hh_size) -
       nrow(df_Households[df_Households$neighb_code==neighb_code &
                            df_Households$hh_type=='couple'&
                            df_Households$hh_size>2,])*2
    )
  
  
  df_GeneratedMarginals[df_GeneratedMarginals$neighb_code==neighb_code,]$pp_households = sum(df_Households[df_Households$neighb_code==neighb_code,]$hh_size)
  df_GeneratedMarginals[df_GeneratedMarginals$neighb_code==neighb_code,]$pp_synthPop = nrow(df_SynthPop[df_SynthPop$neighb_code==neighb_code,])
}

# Save synthetic population
setwd(paste(this.path::this.dir(), "/households/", sep = ""))
write.csv(df_SynthPop, 'households_marginals.csv', row.names=FALSE)

################################################################################
# Assign household position based on constructed marginals and stratified dataset
################################################################################

# Reset column if already calculated
df_SynthPop = subset(df_SynthPop, select=-c(hh_position))

# Create group ages in the synthetic population
df_SynthPop$age_group = ""
df_SynthPop$age_group[df_SynthPop$age %in% 0:5] = "age_0_5"
df_SynthPop$age_group[df_SynthPop$age %in% 5:10] = "age_5_10"
df_SynthPop$age_group[df_SynthPop$age %in% 10:15] = "age_10_15"
df_SynthPop$age_group[df_SynthPop$age %in% 15:20] = "age_15_20"
df_SynthPop$age_group[df_SynthPop$age %in% 20:25] = "age_20_25"
df_SynthPop$age_group[df_SynthPop$age %in% 25:30] = "age_25_30"
df_SynthPop$age_group[df_SynthPop$age %in% 30:35] = "age_30_35"
df_SynthPop$age_group[df_SynthPop$age %in% 35:40] = "age_35_40"
df_SynthPop$age_group[df_SynthPop$age %in% 40:45] = "age_40_45"
df_SynthPop$age_group[df_SynthPop$age %in% 45:50] = "age_45_50"
df_SynthPop$age_group[df_SynthPop$age %in% 50:55] = "age_50_55"
df_SynthPop$age_group[df_SynthPop$age %in% 55:60] = "age_55_60"
df_SynthPop$age_group[df_SynthPop$age %in% 60:65] = "age_60_65"
df_SynthPop$age_group[df_SynthPop$age %in% 65:70] = "age_65_70"
df_SynthPop$age_group[df_SynthPop$age %in% 70:75] = "age_70_75"
df_SynthPop$age_group[df_SynthPop$age %in% 75:80] = "age_75_80"
df_SynthPop$age_group[df_SynthPop$age %in% 80:85] = "age_80_85"
df_SynthPop$age_group[df_SynthPop$age %in% 85:90] = "age_85_90"
df_SynthPop$age_group[df_SynthPop$age %in% 90:95] = "age_90_95"
df_SynthPop$age_group[df_SynthPop$age %in% 95:105] = "age_over_95"

library(GenSynthPop)

# Calculate propensities
df_SynthPop = calc_propens_agents(df_StratHousehold, "single", "total", df_SynthPop, c("age_group", "gender") )
df_SynthPop = calc_propens_agents(df_StratHousehold, "child", "total", df_SynthPop, c("age_group", "gender") )
df_SynthPop = calc_propens_agents(df_StratHousehold, "couple", "total", df_SynthPop, c("age_group", "gender") )
df_SynthPop = calc_propens_agents(df_StratHousehold, "single_parent", "total", df_SynthPop, c("age_group", "gender") )

# Distribute attributes
df_SynthPop = distr_attr_strat_neigh_stats_3plus(agent_df =  df_SynthPop,
                                                 neigh_df =  df_GeneratedMarginals,
                                                 neigh_ID =  "neighb_code",
                                                 variable =  "hh_position", 
                                                 list_var_classes_neigh_df =  c("pp_single", "pp_singleparent", "pp_couple", "pp_children"), 
                                                 list_agent_propens =  c("prop_single", "prop_single_parent", "prop_couple", "prop_child"),
                                                 list_class_names =  c("single", "single_parent", "couple", "child")
)

# Remove extra columns
df_SynthPop = subset(df_SynthPop, select=-c(age_group, random_scores, prop_single, prop_single_parent, prop_couple, prop_child))

df_GeneratedMarginals$agents_single = 0
df_GeneratedMarginals$agents_singleparent = 0
df_GeneratedMarginals$agents_couple = 0
df_GeneratedMarginals$agents_children = 0
for (neighb_code in unique(df_SynthPop$neighb_code)) {
  df_GeneratedMarginals[df_GeneratedMarginals$neighb_code==neighb_code,]$agents_single = nrow(df_SynthPop[df_SynthPop$neighb_code==neighb_code & df_SynthPop$hh_position=='single',])
  df_GeneratedMarginals[df_GeneratedMarginals$neighb_code==neighb_code,]$agents_singleparent = nrow(df_SynthPop[df_SynthPop$neighb_code==neighb_code & df_SynthPop$hh_position=='single_parent',])
  df_GeneratedMarginals[df_GeneratedMarginals$neighb_code==neighb_code,]$agents_couple = nrow(df_SynthPop[df_SynthPop$neighb_code==neighb_code & df_SynthPop$hh_position=='couple',])
  df_GeneratedMarginals[df_GeneratedMarginals$neighb_code==neighb_code,]$agents_children = nrow(df_SynthPop[df_SynthPop$neighb_code==neighb_code & df_SynthPop$hh_position=='child',])
}

# Save synthetic population
setwd(paste(this.path::this.dir(), "/synthetic-populations", sep = ""))
write.csv(df_SynthPop, 'synthetic_population_DHZW.csv', row.names=FALSE)

################################################################################
# Assign individuals to households
################################################################################

setwd(paste(this.path::this.dir(), "/households/", sep = ""))
df_Households = read.csv('df_Households.csv')

df_UnAssignedHouseholds = df_Households
df_SynthPop$hh_ID = NA


for (i in 1:nrow(df_SynthPop)) {
  neighb_code = df_SynthPop[i,'neighb_code']
  
  if(TRUE) {
    
    # if agent is single
    if(df_SynthPop[i,'hh_position']=='single') {
      # pick an unassigned house
      household = df_UnAssignedHouseholds[df_UnAssignedHouseholds$neighb_code==neighb_code & df_UnAssignedHouseholds$hh_type=='single',]
      
      if(nrow(household>0)) {
        household = household[1,]
        
        # Assign agent
        df_SynthPop[i,]$hh_ID = household$hh_ID
        
        # Remove household
        df_UnAssignedHouseholds <- df_UnAssignedHouseholds[!(df_UnAssignedHouseholds$hh_ID %in% household$hh_ID),]
      }
    }
  
    if(df_SynthPop[i,'hh_position']=='single_parent') {
      # pick an unassigned house
      household = df_UnAssignedHouseholds[df_UnAssignedHouseholds$neighb_code==neighb_code & df_UnAssignedHouseholds$hh_type=='single_parent',]
      
      # if there is a household and enough children
      if(nrow(household)>0){
        # check if there are enough children left
        df_Children = df_SynthPop[df_SynthPop$neighb_code==neighb_code & df_SynthPop$hh_position=='child' & is.na(df_SynthPop$hh_ID),]
        household = household[1,]
        
        if(nrow(df_Children)>=(household$hh_size-1)){
          # Assign parent
          df_SynthPop[i,]$hh_ID = household$hh_ID
          
          # Assign children
          if((household$hh_size-1)>0){
            for (c in 1:(household$hh_size-1)) {
              df_SynthPop[df_SynthPop$agent_ID==df_Children[c,]$agent_ID,]$hh_ID=household$hh_ID
            }
          }
          # Remove household
          df_UnAssignedHouseholds <- df_UnAssignedHouseholds[!(df_UnAssignedHouseholds$hh_ID %in% household$hh_ID),]   
        }
      }
    }

  }
  
  if(df_SynthPop[i,'hh_position']=='couple' & is.na(df_SynthPop[i,'hh_ID'])) {
    # pick an unassigned house
    household = df_UnAssignedHouseholds[df_UnAssignedHouseholds$neighb_code==neighb_code & df_UnAssignedHouseholds$hh_type=='couple',]
    
    # if there is a household and enough children
    if(nrow(household)>0){
      # check if there are enough parents and children left
      df_Partner = df_SynthPop[df_SynthPop$neighb_code==neighb_code & df_SynthPop$hh_position=='couple' & is.na(df_SynthPop$hh_ID) & df_SynthPop$agent_ID!=df_SynthPop[i,'agent_ID'],]
      df_Children = df_SynthPop[df_SynthPop$neighb_code==neighb_code & df_SynthPop$hh_position=='child' & is.na(df_SynthPop$hh_ID),]
      household = household[1,]
      
      if(nrow(df_Children)>=(household$hh_size-2) & nrow(df_Partner)>0){
        partner = df_Partner[1,]
        # Assign parents
        df_SynthPop[i,]$hh_ID = household$hh_ID
        df_SynthPop[df_SynthPop$agent_ID == partner$agent_ID,]$hh_ID = household$hh_ID
        
        # Assign children
        if((household$hh_size-2)>0){
          for (c in 1:(household$hh_size-2)) {
            df_SynthPop[df_SynthPop$agent_ID==df_Children[c,]$agent_ID,]$hh_ID=household$hh_ID
          }
        }
        
        # Remove household
        df_UnAssignedHouseholds <- df_UnAssignedHouseholds[!(df_UnAssignedHouseholds$hh_ID %in% household$hh_ID),]   
      }
    }
  }
}

# Save synthetic population
setwd(paste(this.path::this.dir(), "/synthetic-populations", sep = ""))
write.csv(df_SynthPop, 'synthetic_population_DHZW.csv', row.names=FALSE)

# save individual household files
library(dplyr)
library(readr)
setwd(paste(this.path::this.dir(), "/households/DHZW_2019", sep = ""))

df_SynthPop %>%
  group_by(hh_ID) %>%
  group_walk(~ write_csv(.x, paste0("hh_", .y$hh_ID, ".csv")))