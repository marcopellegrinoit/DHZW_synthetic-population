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