# Load marginal distribution
setwd(paste(this.path::this.dir(), "/data/", municipality, sep = ""))
df_marginal_dist = read.csv("marginal_distributions_84583NED-formatted.csv", sep = ",")

setwd(this.path::this.dir())
setwd(paste0(this.path::this.dir(), "/synthetic-populations"))
df_synth_pop = read.csv('synthetic_population_DHZW_2019_with_hh.csv')

setwd(this.path::this.dir())
setwd(paste0('data/', municipality, '/households/output'))
df_households = read.csv('df_households_DHZW_2019.csv')

setwd(this.path::this.dir())
setwd('data')
df_age_couples <- read.csv("couples_age_disparity.csv", sep=',')
df_age_couples_heterosexual <- read_delim("couples_age_disparity_heterosexual-formatted.csv", 
                                          delim = ";", escape_double = FALSE, trim_ws = TRUE)

library("xlsx")

setwd(this.path::this.dir())
setwd('evaluation-households')

################################################################################
# total number of households
households_total = df_marginal_dist %>%
  select(neighb_code, hh_total) %>%
  rename(real_hh_total = hh_total)
households_total$generated_hh_total=0
for (i in 1:nrow(households_total)) {
  households_total[i,]$generated_hh_total = length(unique(df_synth_pop[df_synth_pop$neighb_code == households_total[i,]$neighb_code,]$hh_ID))
}

write.xlsx(households_total,
           'households_total.xlsx',
           sheetName = "Sheet1", 
           col.names = TRUE, row.names = FALSE, append = FALSE)

################################################################################
# number of households for singles
households_singles = df_marginal_dist %>%
  select(neighb_code, hh_single) %>%
  rename(real_hh_single = hh_single)
households_singles$generated_hh_single = 0
for (i in 1:nrow(households_total)) {
  households_singles[i,]$generated_hh_single = nrow(df_synth_pop[df_synth_pop$neighb_code==households_singles[i,]$neighb_code & df_synth_pop$hh_type=='single',])
}

write.xlsx(households_singles,
           'households_singles.xlsx',
           sheetName = "Sheet1", 
           col.names = TRUE, row.names = FALSE, append = FALSE)

################################################################################
# number of households for households with children
households_with_children = df_marginal_dist %>%
  select(neighb_code, hh_with_children) %>%
  rename(real_hh_with_children = hh_with_children)
households_with_children$generated_hh_with_children=0
for (i in 1:nrow(households_total)) {
  households_with_children[i,]$generated_hh_with_children = length(unique(df_synth_pop[df_synth_pop$neighb_code==households_with_children[i,]$neighb_code & (
                                                                                   df_synth_pop$hh_type=='couple_children_straight' |
                                                                                   df_synth_pop$hh_type=='couple_children_gay' |
                                                                                   df_synth_pop$hh_type=='couple_children_lesbian' |
                                                                                   df_synth_pop$hh_type=='single_parent'),]$hh_ID))
}

write.xlsx(households_with_children,
           'households_with_children.xlsx',
           sheetName = "Sheet1", 
           col.names = TRUE, row.names = FALSE, append = FALSE)

################################################################################
# number of households for households with children
households_without_children = df_marginal_dist %>%
  select(neighb_code, hh_with_children) %>%
  rename(real_hh_with_children = hh_with_children)
households_without_children$generated_hh_without_children = 0
for (i in 1:nrow(households_total)) {
  households_without_children[i,]$generated_hh_without_children = length(unique(df_synth_pop[df_synth_pop$neighb_code==households_with_children[i,]$neighb_code & (
    df_synth_pop$hh_type=='couple_no_children_straight' |
      df_synth_pop$hh_type=='couple_no_children_gay'|
      df_synth_pop$hh_type=='couple_no_children_lesbian'),]$hh_ID))
}

write.xlsx(households_without_children,
           'households_without_children.xlsx',
           sheetName = "Sheet1", 
           col.names = TRUE, row.names = FALSE, append = FALSE)

################################################################################
# average households size
households_avg_size = df_marginal_dist %>%
  select(neighb_code, hh_avg_size) %>%
  rename(real_hh_avg_size = hh_avg_size)
households_avg_size$generated_hh_avg_size = 0
for (i in 1:nrow(households_avg_size)) {
  households_avg_size[i,]$generated_hh_avg_size = mean(as.numeric(df_households[df_households$neighb_code==households_avg_size[i,]$neighb_code,]$hh_size))
}

write.xlsx(households_avg_size,
           'households_avg_size.xlsx',
           sheetName = "Sheet1", 
           col.names = TRUE, row.names = FALSE, append = FALSE)


################################################################################
# same-gender couples
households_genders = df_marginal_dist %>%
  select(neighb_code)
households_genders$generated_couples_heterosexual = 0
households_genders$generated_couples_male_male = 0
households_genders$generated_couples_female_female = 0

for (i in 1:nrow(households_genders)) {
  households_genders[i,]$generated_couples_heterosexual = length(unique(df_synth_pop[df_synth_pop$neighb_code==households_genders[i,]$neighb_code & (
    df_synth_pop$hh_type=='couple_children_straight' |
      df_synth_pop$hh_type=='couple_no_children_straight'),]$hh_ID))
  
  households_genders[i,]$generated_couples_male_male = length(unique(df_synth_pop[df_synth_pop$neighb_code==households_genders[i,]$neighb_code & (
    df_synth_pop$hh_type=='couple_children_gay' |
      df_synth_pop$hh_type=='couple_no_children_gay'),]$hh_ID))
  
  households_genders[i,]$generated_couples_female_female = length(unique(df_synth_pop[df_synth_pop$neighb_code==households_genders[i,]$neighb_code & (
    df_synth_pop$hh_type=='couple_children_lesbian' |
      df_synth_pop$hh_type=='couple_no_children_lesbian'),]$hh_ID))
}

households_genders[nrow(households_genders)+1,] = c('total',
                                                    length(unique(df_synth_pop[df_synth_pop$hh_type=='couple_children_straight' |
                                                                               df_synth_pop$hh_type=='couple_no_children_straight',]$hh_ID)),
                                                    length(unique(df_synth_pop[df_synth_pop$hh_type=='couple_children_gay' |
                                                                               df_synth_pop$hh_type=='couple_no_children_gay',]$hh_ID)),
                                                    length(unique(df_synth_pop[df_synth_pop$hh_type=='couple_children_lesbian' |
                                                                               df_synth_pop$hh_type=='couple_no_children_lesbian',]$hh_ID))
                                                    )


households_genders$prop_couple_heterosexual = 0
households_genders$prop_couple_male_male = 0
households_genders$prop_couple_female_female = 0
for (i in 1:nrow(households_genders)) {
  households_genders[i,]$prop_couple_heterosexual = as.numeric(households_genders[i,]$generated_couples_heterosexual)/sum(as.numeric(households_genders[i,]$generated_couples_heterosexual),
                                                                                                                  as.numeric(households_genders[i,]$generated_couples_male_male),
                                                                                                                  as.numeric(households_genders[i,]$generated_couples_female_female))
  
  households_genders[i,]$prop_couple_male_male = as.numeric(households_genders[i,]$generated_couples_male_male)/sum(as.numeric(households_genders[i,]$generated_couples_heterosexual),
                                                                                                            as.numeric(households_genders[i,]$generated_couples_male_male),
                                                                                                            as.numeric(households_genders[i,]$generated_couples_female_female))
  
  households_genders[i,]$prop_couple_female_female = as.numeric(households_genders[i,]$generated_couples_female_female)/sum(as.numeric(households_genders[i,]$generated_couples_heterosexual),
                                                                                                                    as.numeric(households_genders[i,]$generated_couples_male_male),
                                                                                                                    as.numeric(households_genders[i,]$generated_couples_female_female))
}

households_genders[nrow(households_genders)+1,] = c('real',
                                                    NA,
                                                    NA,
                                                    NA,
                                                    0.97,
                                                    0.01,
                                                    0.01)

write.xlsx(households_genders,
           'households_genders.xlsx',
           sheetName = "Sheet1", 
           col.names = TRUE, row.names = FALSE, append = FALSE)

################################################################################
# age disparity in couples
for (i in nrow(df_age_couples)){
  if (df_age_couples[i,]$gap=='0')
    df_parents = df_synth_pop[df_synth_pop$is_child==0 & (df_synth_pop$hh_type=='couple_children_straight' |
                                                          df_synth_pop$hh_type=='couple_no_children_straight'),]
    df_couples 
}
