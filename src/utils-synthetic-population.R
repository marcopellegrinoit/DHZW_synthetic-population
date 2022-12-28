library(ggplot2)
library(tidyr)
library(splitstackshape)

refactor_gender = function(df){
  df$gender = gsub(" ", "", df$gender, fixed = TRUE)
  df[df$gender == '3000',]$gender = 'male'
  df[df$gender == '4000',]$gender = 'female'
  df[df$gender == 'T001038',]$gender = 'total'
  
  df = df[df$gender != 'total',]
  
  return(df)
}

refactor_age = function(df, df_codes){
  df = merge(df, df_codes, by='age_code')
  df = subset(df, select=-c(age_code))
  
  df = df[df$age != 'total',]
  
  return(df)
}

refactor_age_group_20 = function(df, df_codes){
  df = merge(df, df_codes, by='age_group_20_code')
  df = subset(df, select=-c(age_group_20_code))
  
  df = df[df$age_group_20 != 'total',]
  
  return(df)
}

refactor_ages_education = function(df, df_codes){
  df = merge(df, df_codes, by='age_code')
  df = subset(df, select=-c(age_code))
  
  df = df[df$age_group_education != 'total',]
  
  return(df)
}

refactor_education = function(df, df_codes){
  df = merge(df, df_codes, by='education_code')
  df = subset(df, select=-c(education_code, education_title))
  
  return(df)
}

refactor_migration = function(df){
  df$migration_background=''
  df[df$migration_background_code=="1012600",]$migration_background = "Dutch"
  df[df$migration_background_code=="2012655",]$migration_background = "Western"
  df[df$migration_background_code=="2012657",]$migration_background = "Non_Western"
  df = subset(df, select=-c(migration_background_code))
  df = df[which(df$migration_background == "Western" | df$migration_background == "Non_Western"  | df$migration_background == "Dutch"),]
  return(df)
}

# Description:
# the function distributes a new attribute to the synthetic population from a stratified dataset.
# The distribution is based on the proportion of each combination of given attributes in common between the synthetic population and the stratified dataset.
# In addition, the distribution is done within neighbourhood areas, for a more uniform distribution, since the stratified dataset is municipality level.
# 
# @df_synt_pop: synthetic population dataframe
# @df_strat: stratified dataset
# @new_attribute: new attribute (column name) to add to the synthetic population
# @attributes_to_match: attributes to use to match the stratified dataset to the synthetic population
# @values_new_attribute: values/classes of the new attribute
# @probabilities: column names of the values of the new attribute in the stratified dataset. The order must correspond to the @values_new_attribute. The probability of the last class can be avoided.
distribute_attribute_stratified <- function(df_synth_pop, df_strat, new_attribute, attributes_to_match, values_new_attribute, probabilities) {
  # Initialise new attribute
  df_synth_pop[new_attribute] = ''
  
  # For each neighbourhood
  for (neighb_code in unique(df_synth_pop$neighb_code)) {
    # For each combination of demographics of the stratified dataset
    for (i in 1:nrow(df_strat)) {
      agents_neighbourhood <- df_synth_pop[df_synth_pop$neighb_code==neighb_code,]
      
      # Get current combination from the stratified dataset
      df_strat_tmp <- df_strat %>%
        slice(i) %>%
        select(attributes_to_match)
      
      # Get agents in that neighbourhood with such demographics combination
      agents_neighbourhood = merge(df_strat_tmp, agents_neighbourhood, by=attributes_to_match)
      n_agents = nrow(agents_neighbourhood)
      
      # For each value class sample agents with its proportion
      n_agents_accumulated = 0
      for (class_index in 1:length(values_new_attribute)) {
        class = values_new_attribute[class_index]
        
        # Calculate the number of synthetic agent for this class based on the proportions of the stratified dataset
        if (class_index != length(values_new_attribute)) {
          n_agents_class = round(n_agents * df_strat[i, probabilities[class_index]])
          n_agents_accumulated = n_agents_accumulated + n_agents_class
        } else {
          # If it is the last class, the quantity is given by the different of the other classes, because of rounding
          n_agents_class = n_agents - n_agents_accumulated
        }
        
        if (n_agents_class > 0) {
          # Select unassigned agents
          agents_unassigned_global <- df_synth_pop %>%
            filter(!!sym(new_attribute) == '')
          
          agents_unassigned_neghbourhood <- agents_neighbourhood %>%
            filter(agent_ID %in% agents_unassigned_global$agent_ID)
          
          # Sample agents for this class
          agent_in_class = sample_n(agents_unassigned_neghbourhood, n_agents_class)
          
          # Write attribute to agents in the synthetic population
          df_synth_pop <- df_synth_pop %>%
            mutate(!!new_attribute := ifelse(agent_ID %in% agent_in_class$agent_ID,
                                             class,
                                             get(!!new_attribute)))
          
        }
        
      }
    }
  }
  
  return(df_synth_pop)
}

# create a dataframe with a number of agents per neighbourhood as indicated in the given marginal distribution
# @df_marginal_dist: marginal distribution
# @col_neighb_code: coloumn name if the neighbourhood codes
# @col_tot: coloumn name which the amount of inhabintas per each neighbouhood
get_synthetic_population_neighborhoods <- function (df_marginal_dist, col_neighb_code, col_tot) {
  # Select coloumns
  df_synth_pop <- df_marginal_dist %>%
    select(!!col_neighb_code, !!col_tot)
  
  # Expand each neighbourhood by the amount of people corresponding
  df_synth_pop <- expandRows(df_synth_pop, col_tot)
  
  # Add coloumn for agent IDs
  df_synth_pop$agent_ID <- paste0('agent_', seq.int(nrow(df_synth_pop)))
  
  return(df_synth_pop)
}

# Distribute some columns of a marginal distribution, such as groupages, on a synthyetic population
# @df_synth_pop: synthetic population
# @df_marginal_dist: marginal distribution
# @new_attribute: new attribute to add
# @columns_new_attribute: columns in the marginal distribution that contains the values/classes of the new attribute
# @col_neighb_code: column in the marginal distribution that contains the neighbourhood codes
# @col_tot_pop: column in the marginal distribution that contains the total amount of individuals of that neighbourhoods
distribute_attribute_marginal <- function (df_synth_pop, df_marginal_dist, new_attribute, columns_new_attribute, col_neighb_code, col_tot_pop) {
  # First part: use quantities of the marginal distributions as proportions of the total amount of inhabintas of that neighbourhood.
  columns <- append(col_neighb_code, columns_new_attribute)
  columns <- append(columns, col_tot_pop)
  df_marginal_dist <- df_marginal_dist[columns]
  
  # Calculate the sum of the classes for each neighbourhood
  df_marginal_dist <- df_marginal_dist %>%
    mutate(tot_class = rowSums(.[columns_new_attribute]))
  
  # Initialise new attribute
  df_synth_pop[new_attribute] <- NA
  
  # For each neighbourhood in the marginal distribution
  for (i in 1:nrow(df_marginal_dist)){
    # For each value of the new attribute
    for (i_class in 1:length(columns_new_attribute)) {
      class = columns_new_attribute[i_class]
      # Calculate the proportion of the class over the sum of the classes
      df_marginal_dist[i, class] <- df_marginal_dist[i, class]/df_marginal_dist[i, 'tot_class']
      
      if(i_class != length(columns_new_attribute)){
        # Multiply the proportion to the real amount of people and round
        df_marginal_dist[i, class] <- round(df_marginal_dist[i, class] * df_marginal_dist[i, col_tot_pop])
      } else {
        # If it is the last class of the neighbourhood, because of rounding I get such value as the difference between the total and the sum of the previously calculated classes
        df_marginal_dist[i, class] <- df_marginal_dist[i, col_tot_pop] - sum(df_marginal_dist[i, head(columns_new_attribute,-1)])
      }
      
      # Sample the amount of agents and apply the class
      unassigned <- df_synth_pop %>%
        filter(neighb_code == df_marginal_dist[i, col_neighb_code] & is.na(!!sym(new_attribute)))
      agent_in_class = sample_n(unassigned, df_marginal_dist[i, class])
      
      df_synth_pop[df_synth_pop$agent_ID %in% agent_in_class$agent_ID, new_attribute] <- class
      
    }
  }
  
  return(df_synth_pop)
}