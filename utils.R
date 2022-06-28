library(ggplot2)
library(tidyr)

refactor_gender = function(df){
  df$gender = gsub(" ", "", df$gender, fixed = TRUE)
  df[df$gender == '3000',]$gender = 'male'
  df[df$gender == '4000',]$gender = 'female'
  df[df$gender == 'T001038',]$gender = 'total'
  return(df)
}

refactor_age = function(df, df_codes){
  df = merge(df, df_codes, by='age_code')
  df = subset(df, select=-c(age_code))
  return(df)
}

refactor_migration = function(df, df_codes){
  df = merge(df, df_codes, by='migration_background_code')
  df = subset(df, select=-c(migration_background_code))
  return(df)
}

refactor_education = function(df, df_codes){
  df = merge(df, df_codes, by='education_code')
  df = subset(df, select=-c(education_code))
  return(df)
}


# @df_real_distr: marginal distribution dataset
# @df_synt_pop: synthetic population dataframe
# @join_var: variable to correlate dataframes. Usuall neighbourhood code
# @list_real_df_var: list of column names of distributions in the @df_real_distr. Each column is a value possibility for the variable to analyse
# @var_pred_df: column name of the variable to analyse in the synthetic population
# @list_values: list of values of the variable to analyse (e.g. values contained in the column @var_pred_df)

validation = function(df_real_distr, df_synt_pop, join_var, list_real_df_var, var_pred_df, list_values){
  df = expand.grid(unique(df_real_distr[,join_var]), list_values)
  colnames(df) = c(join_var, var_pred_df)
  
  df$pred = NA
  df$real = NA
  
  # count the ones predicted
  for (i in 1:nrow(df)) {
    df[i,]$pred = nrow(df_synt_pop[df_synt_pop[,join_var]==df[i, join_var] &
                                     df_synt_pop[,var_pred_df]==df[i,var_pred_df],])
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

R_squared = function(y_actual,y_predict){
  cor(y_actual,y_predict)^2
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