library("this.path")
setwd(this.path::this.dir())
library(dplyr)
library (readr)

municipality = "den_haag_2019"

setwd(paste(this.path::this.dir(), "/synthetic-populations", sep = ""))
df_SynthPop = read.csv('synthetic_population_DHZW_2019.csv')
df_SynthPop$hh_ID = NA

df_children_unassigned = df_SynthPop[df_SynthPop$hh_position=='child',]

df_children_assigned = df_children_unassigned
df_children_assigned <- df_children_assigned[0,]

# Empty container of households
df_households <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(df_households) <- c("hh_ID", "children_in_house", "neighb_code")


# For each children_in_house, from largest to smallest (we start with the most difficult constraints)
for (children_size in (nrow(n_households):1)) {
  # repeat for all the households needed for such quantity of children in household
  for (n_houses in (1:n_households[children_size,]$num_households)){
    # Pick a random child c and remove from list
    c = sample_n(df_children_unassigned, 1)
    df_children_unassigned = df_children_unassigned[df_children_unassigned$agent_ID != c$agent_ID,]
    df_children_assigned <- rbind(df_children_assigned, c)
    
    # Get household ID and its neighbourhood code
    hh_ID = nrow(df_households) + 1
    neighb_code = c$neighb_code
    
    # Assign child to household
    df_SynthPop[df_SynthPop$agent_ID==c$agent_ID,]$hh_ID = hh_ID
    
    # While household has fewer children than children_in_house
    sibling_counter = 0
    while (sibling_counter < (children_size-1)) {
      # Get the ages a_c1, . , a_cn of all children c_1, . , c_n thus far assigned
      ages_children = df_children_assigned[df_children_assigned$hh_ID == hh_ID,]$age
      
      # Assign each remaining child x a probability p(x) of being a sibling of c_1, . ,c_n, based on how well their age a_x matches the age difference with argmin (a_c1, a_cn ) or argmax(a_c1, a_c_n)
      df_children_unassigned_neighb = df_children_unassigned[df_children_unassigned$neighb_code == neighb_code,]
      
      # todo, sample next sibling randomly
      x = sample_n(df_children_unassigned_neighb, 1)
      df_children_unassigned = df_children_unassigned[df_children_unassigned$agent_ID != x$agent_ID,]
      df_children_assigned <- rbind(df_children_assigned, x)
      df_SynthPop[df_SynthPop$agent_ID==x$agent_ID,]$hh_ID = hh_ID
    
      sibling_counter = sibling_counter + 1
    }
    
    hh_size = 2
    df_households[hh_ID,] = c(
      hh_ID,
      as.numeric(hh_size),
      neighb_code)
  }
  
}