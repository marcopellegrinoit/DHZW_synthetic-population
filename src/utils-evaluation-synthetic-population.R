
# @df_real_distr: marginal distribution dataset
# @df_synt_pop: synthetic population dataframe
# @join_var: variable to correlate dataframes. Usuall neighbourhood code
# @list_real_df_var: list of column names of distributions in the @df_real_distr. Each column is a value possibility for the variable to analyse
# @var_pred_df: column name of the variable to analyse in the synthetic population
# @list_values: list of values of the variable to analyse (e.g. values contained in the column @var_pred_df)

validation = function(df_real_distr, df_synt_pop, join_var, list_real_df_var, var_pred_df, list_values, age_limits){
  df = expand.grid(unique(df_real_distr[,join_var]), list_values)
  colnames(df) = c(join_var, var_pred_df)
  
  df$pred = NA
  df$real = NA
  
  if (age_limits) {
    # count the ones predicted
    for (i in 1:nrow(df)) {
      df[i,]$pred = nrow(df_synt_pop[df_synt_pop[,join_var]==df[i, join_var] &
                                       df_synt_pop[,var_pred_df]==df[i,var_pred_df] &
                                       df_synt_pop['age']>=15 &
                                       df_synt_pop['age']<=75,])
    }    
  } else {
    # count the ones predicted
    for (i in 1:nrow(df)) {
      df[i,]$pred = nrow(df_synt_pop[df_synt_pop[,join_var]==df[i, join_var] &
                                       df_synt_pop[,var_pred_df]==df[i,var_pred_df],])
    }
  }
  
  
  # extract the true labels from the marginal
  for (a in 1:length(list_real_df_var)) {
    for (i in unique(df_real_distr[,join_var])) {
      df[df[,join_var]==i & df[,var_pred_df]==list_values[a],]$real = df_real_distr[df_real_distr[,join_var]==i, list_real_df_var[a]]
    }
  }
  
  # calculate accuracy
  df$accuracy = ifelse(df$pred < df$real,
                       ifelse(df$pred==0,
                              0,
                              df$pred/df$real),
                       ifelse(df$real==0,
                              0,
                              df$real/df$pred)
  )
  df$diff = df$pred-df$real
  
  return(df)
}

normalise <- function(x){(x-min(x))/(max(x)-min(x))}


R_squared = function(y_actual,y_predict){
  cor(y_actual,y_predict)^2
}

R_squared_manual = function(actual, preds){
  preds = normalise(preds)
  actual = normalise(actual)
  
  rss <- sum((preds - actual) ^ 2)  ## residual sum of squares
  tss <- sum((actual - mean(actual)) ^ 2)  ## total sum of squares
  rsq <- 1 - rss/tss
  return (rsq)
}


plot_heatmap = function(df, join_var, var) {
  ggplot(df,
         aes(df[,join_var],
             df[,var],
             fill= df$accuracy)) +
    geom_tile(colour = "black")+
    geom_text(aes(label = round(df$accuracy, 2))) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    labs(x = join_var,
         y = var)+
    scale_fill_continuous(name = "Accuracy")
}

## Density and histogram plot of the synthetic population
plot_syth_pop_age_density = function(df_synt_pop) {
  ggplot(df_synt_pop, aes(x=age)) + 
    stat_bin(aes(y=..density..), colour="black", fill="white", bins=length(unique(df_synt_pop$age)))+
    geom_density(alpha=.2, fill="#FF6666")+
    labs(x = 'Age',
         y = 'Age frequencies')
}

# Plot densities of both the synthetic population and the stratified dataset
plot_syth_strat_age_density = function(df_synt_pop, df_stratified_age) {
  # Count how many agents per age
  df_synt_pop = df_synt_pop %>%
    group_by(age) %>%
    count()
  
  # Combine datasets and rename attributes
  df = merge(df_synt_pop[c('age', 'n')], df_stratified_age[c('age', 'total')])
  df = df %>%
    rename(
      synthetic_population = n,
      stratified = total
    )
  
  # Normalise data
  df <- transform(df, synthetic_population =
                    (synthetic_population - min(synthetic_population))
                  / (max(synthetic_population) - min(synthetic_population)))
  df <- transform(df, stratified = (stratified - min(stratified)) / (max(stratified) - min(stratified)))
  
  # Format for plot
  df = df %>% 
    pivot_longer(!age, names_to = "dataset", values_to = "normalised_frequencies")
  
  # Plot
  ggplot(df, aes(x=age, y=normalised_frequencies ,col=dataset))+
    geom_line()+
    labs(x = 'Age',
         y = 'Normalised age frequencies')
}