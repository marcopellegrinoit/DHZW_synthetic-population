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
df_SingleParents = sample_frac(df_Households, percSingleParents)
df_Households[df_Households$hh_ID %in% df_SingleParents$hh_ID,]$hh_type= 'single_parent'

# assign the remaining to couples
df_Households[is.na(df_Households$hh_type),]$hh_type='couple'

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
                                                                                                 df_Households$hh_type=='couples'&
                                                                                                 df_Households$hh_size>2,]$hh_size) -
                                                                               nrow(df_Households[df_Households$neighb_code==neighb_code &
                                                                                                  df_Households$hh_type=='couples'&
                                                                                                  df_Households$hh_size>2,])*2
                                                                               )
  
  
  df_GeneratedMarginals[df_GeneratedMarginals$neighb_code==neighb_code,]$pp_households = sum(df_Households[df_Households$neighb_code==neighb_code,]$hh_size)
  df_GeneratedMarginals[df_GeneratedMarginals$neighb_code==neighb_code,]$pp_synthPop = nrow(df_SynthPop[df_SynthPop$neighb_code==neighb_code,])
}

df_GeneratedMarginals$pp_sum = df_GeneratedMarginals$pp_single + df_GeneratedMarginals$pp_singleparent + df_GeneratedMarginals$pp_couple + df_GeneratedMarginals$pp_children
df_GeneratedMarginals$check = df_GeneratedMarginals$pp_households == df_GeneratedMarginals$pp_synthPop