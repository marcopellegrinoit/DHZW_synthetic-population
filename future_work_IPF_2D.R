library(mipfp)

# Applies IPF to a 2-way joint distribution
update_stratified_IPF_2D <- function(df_joint, df_marginal_V1, df_marginal_V2) {
  options(scipen=999)
  
  cols_stratified <- colnames(df_joint)
  
  colnames(df_joint) <- c('V1', 'V2', 'count')
  
  array_stratified <- xtabs(count ~ V1 + V2, data = df_joint)
  
  target.row <- df_marginal_V2$proportion
  target.col <- df_marginal_V1$proportion
  
  # storing the margins in a list
  tgt.data.2d <- list(target.col, target.row)
  
  # list of dimensions of each marginal constrain
  tgt.list.2d <- list(1, 2)
  
  # calling the Ipfp function
  result <- Ipfp(array_stratified, tgt.list.2d, tgt.data.2d)
  
  # Convert the output into a dataframe
  df_joint_updated <- as.data.frame(result$p.hat)
  
  colnames(df_joint_updated) <-c(cols_stratified[c(1,2)], 'proportion')
  
  return(df_joint_updated)
}

# example on gender

df_synth_pop$gender <- NA

# Format gender dataset
df_joint_gender <- df_joint_gender %>%
  select(age_group, male, female) %>%
  pivot_longer(cols = c(male, female), names_to = 'gender', values_to = 'count') %>%
  group_by(age_group, gender) %>%
  summarise(count = sum(count))

for (neighb_code in unique(df_synth_pop$neighb_code)) {
  df_marginal_neighb <- df_marginal_dist[df_marginal_dist$neighb_code==neighb_code,]
  df_marginal_V1 <- df_marginal_neighb[c("age_0_15" , "age_15_25", "age_25_45", "age_45_65", "age_over65")]
  df_marginal_V1 <- df_marginal_V1 %>%
    pivot_longer(cols=colnames(df_marginal_V1), names_to = 'age_group', values_to = 'count')
  df_marginal_V1$proportion <- df_marginal_V1$count / sum(df_marginal_V1$count)
  
  df_marginal_V2 <- df_marginal_neighb[c("gender_male" , "gender_female")]
  df_marginal_V2 <- df_marginal_V2 %>%
    pivot_longer(cols=colnames(df_marginal_V2), names_to = 'gender', values_to = 'count')
  df_marginal_V2$proportion <- df_marginal_V2$count / sum(df_marginal_V2$count)
  
  # IPF update of the stratified data to match the marginal data
  df_joint_gender_IPF <- update_stratified_IPF_2D(df_joint_gender, df_marginal_V1, df_marginal_V2)
  
  df_joint_gender_IPF <- df_joint_gender_IPF %>%
    pivot_wider(names_from = 'gender', values_from = 'proportion')
  
  # calculate proportions per each age_group
  total <- df_joint_gender_IPF$female + df_joint_gender_IPF$male
  df_joint_gender_IPF$prop_female <- df_joint_gender_IPF$female / total
  df_joint_gender_IPF$prop_male <- df_joint_gender_IPF$male / total
  
  # distribute the new variable (gender) in the neighbourhood of the synthetic population
  df_synth_pop[df_synth_pop$neighb_code==neighb_code,] <- distribute_stratified(
    df_agents = df_synth_pop[df_synth_pop$neighb_code==neighb_code,],
    df_strat = df_joint_gender_IPF,
    new_attribute = "gender",
    attributes_to_match = c('age_group'),
    values_new_attribute = c("female", "male"),
    probabilities = c("prop_female", "prop_male")
  )
}