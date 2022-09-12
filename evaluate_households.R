library("this.path")
library('readr')
library('tidyr')
library('dplyr')
library('ggplot2')


municipality='den_haag_2019'

# Load marginal distribution
setwd(paste(this.path::this.dir(), "/data/", municipality, sep = ""))
df_marginal_dist = read.csv("marginal_distributions_84583NED-formatted.csv", sep = ",")

# Load households size distribution (municipality aggregated)
setwd(paste0(this.path::this.dir(), "/data/", municipality, '/households/distributions'))
df_household_size = read.csv('household_size_71486NED-formatted.csv')

setwd(this.path::this.dir())
setwd(paste0(this.path::this.dir(), "/synthetic-populations"))
df_synth_pop = read.csv('synthetic_population_DHZW_2019_with_hh.csv')

setwd(this.path::this.dir())
setwd(paste0('data/', municipality, '/households/output'))
df_households = read.csv('df_households_DHZW_2019-new.csv')

setwd(this.path::this.dir())
setwd('data')
df_age_couples <- read.csv("couples_age_disparity.csv", sep=',')
df_age_couples_heterosexual <- read_delim("couples_age_disparity_heterosexual-formatted.csv", 
                                          delim = ";", escape_double = FALSE, trim_ws = TRUE)


setwd(this.path::this.dir())
setwd('evaluation-households')

################################################################################
# total number of households
households_total = df_marginal_dist %>%
  select(neighb_code, hh_total) %>%
  rename(total_real = hh_total)
households_total$total_generated=0
for (i in 1:nrow(households_total)) {
  households_total[i,]$total_generated = length(unique(df_synth_pop[df_synth_pop$neighb_code == households_total[i,]$neighb_code,]$hh_ID))
}

households_total_plot = households_total %>%
  rename(real = total_real,
         generated = total_generated) %>%
  pivot_longer(!neighb_code, names_to = "type", values_to = "hh_total")

ggplot(households_total_plot, aes(neighb_code, hh_total)) +
  geom_bar(aes(fill = type),
           position = "dodge",
           stat = "identity",
           width=0.4)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Total number of households per neighbourhood")+
  ylab("Total number of households (units)")+
  xlab("Neighbourhood code")+
  labs(fill = "Population")+
  theme(legend.title=element_text(size=20),
        legend.text=element_text(size=15))

################################################################################
# number of households for singles
households_singles = df_marginal_dist %>%
  select(neighb_code, hh_single) %>%
  rename(real = hh_single)
households_singles$generated = 0
for (i in 1:nrow(households_singles)) {
  households_singles[i,]$generated = nrow(df_synth_pop[df_synth_pop$neighb_code==households_singles[i,]$neighb_code & df_synth_pop$hh_type=='single',])
}
households_singles = merge(households_singles, households_total, by='neighb_code')
households_singles$real = households_singles$real/households_singles$total_real
households_singles$generated = households_singles$generated/households_singles$total_generated

households_singles = households_singles %>%
  select(neighb_code, real, generated) %>%
  pivot_longer(!neighb_code, names_to = "type", values_to = "hh_singles")

ggplot(households_singles, aes(neighb_code, hh_singles)) +
  geom_bar(aes(fill = type),
           position = "dodge",
           stat = "identity",
           width=0.4)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Proportions of households of singles per neighbourhood")+
  ylab("Households of single individuals (%)")+
  xlab("Neighbourhood code")+
  labs(fill = "Population")+
  theme(legend.title=element_text(size=20),
        legend.text=element_text(size=15))


################################################################################
# number of households for households with children
households_with_children = df_marginal_dist %>%
  select(neighb_code, hh_with_children) %>%
  rename(real = hh_with_children)
households_with_children$generated=0
for (i in 1:nrow(households_with_children)) {
  households_with_children[i,]$generated = length(unique(df_synth_pop[df_synth_pop$neighb_code==households_with_children[i,]$neighb_code & (
                                                                                   df_synth_pop$hh_type=='couple_children_straight' |
                                                                                   df_synth_pop$hh_type=='couple_children_gay' |
                                                                                   df_synth_pop$hh_type=='couple_children_lesbian' |
                                                                                   df_synth_pop$hh_type=='single_parent'),]$hh_ID))
}

households_with_children = merge(households_with_children, households_total, by='neighb_code')
households_with_children$real = households_with_children$real/households_with_children$total_real
households_with_children$generated = households_with_children$generated/households_with_children$total_generated

households_with_children = households_with_children %>%
  select(neighb_code, real, generated) %>%
  pivot_longer(!neighb_code, names_to = "type", values_to = "hh_couples_with_children")

ggplot(households_with_children, aes(neighb_code, hh_couples_with_children)) +
  geom_bar(aes(fill = type),
           position = "dodge",
           stat = "identity",
           width=0.4)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Proportions of households with children per neighbourhood")+
  ylab("Households with children (%)")+
  xlab("Neighbourhood code")+
  labs(fill = "Population")+
  theme(legend.title=element_text(size=20),
        legend.text=element_text(size=15))

################################################################################
# number of households for households without children
households_without_children = df_marginal_dist %>%
  select(neighb_code, hh_no_children) %>%
  rename(real = hh_no_children)
households_without_children$generated = 0
for (i in 1:nrow(households_without_children)) {
  households_without_children[i,]$generated = length(unique(df_synth_pop[df_synth_pop$neighb_code==households_without_children[i,]$neighb_code & (
    df_synth_pop$hh_type=='couple_no_children_straight' |
      df_synth_pop$hh_type=='couple_no_children_gay'|
      df_synth_pop$hh_type=='couple_no_children_lesbian'),]$hh_ID))
}

households_without_children = merge(households_without_children, households_total, by='neighb_code')
households_without_children$real = households_without_children$real/households_without_children$total_real
households_without_children$generated = households_without_children$generated/households_without_children$total_generated

households_without_children = households_without_children %>%
  select(neighb_code, real, generated) %>%
  pivot_longer(!neighb_code, names_to = "type", values_to = "hh_couples_without_children")

ggplot(households_without_children, aes(neighb_code, hh_couples_without_children)) +
  geom_bar(aes(fill = type),
           position = "dodge",
           stat = "identity",
           width=0.4)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Proportions of households without children per neighbourhood")+
  ylab("Households without children (%)")+
  xlab("Neighbourhood code")+
  labs(fill = "Population")+
  theme(legend.title=element_text(size=20),
        legend.text=element_text(size=15))

################################################################################
# average households size
households_avg_size = df_marginal_dist %>%
  select(neighb_code, hh_avg_size) %>%
  rename(real = hh_avg_size)
households_avg_size$generated = 0
for (i in 1:nrow(households_avg_size)) {
  households_avg_size[i,]$generated = mean(as.numeric(df_households[df_households$neighb_code==households_avg_size[i,]$neighb_code,]$hh_size))
}

households_avg_size = households_avg_size %>%
  select(neighb_code, real, generated) %>%
  pivot_longer(!neighb_code, names_to = "type", values_to = "hh_avg_size")

ggplot(households_avg_size, aes(neighb_code, hh_avg_size)) +
  geom_bar(aes(fill = type),
           position = "dodge",
           stat = "identity",
           width=0.4)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Average household size per neighbourhood")+
  ylab("Average household size")+
  xlab("Neighbourhood code")+
  labs(fill = "Population")+
  theme(legend.title=element_text(size=20),
        legend.text=element_text(size=15))

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

households_genders[nrow(households_genders)+1,] = c('DHZW generated',
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

households_genders[nrow(households_genders)+1,] = c('Municipality real',
                                                    NA,
                                                    NA,
                                                    NA,
                                                    0.97,
                                                    0.01,
                                                    0.01)


ggplot(households_genders, aes(neighb_code, as.numeric(prop_couple_heterosexual))) +
  geom_bar(position = "dodge",
           stat = "identity",
           width=0.4)+
  scale_y_continuous(limits = c(0, 1))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Proportions of heterosexual couples (with and without children)")+
  ylab("Heterosexual couples (%)")+
  xlab("Area")+
  theme(legend.title=element_text(size=20),
        legend.text=element_text(size=15))

ggplot(households_genders, aes(neighb_code, as.numeric(prop_couple_male_male))) +
  geom_bar(position = "dodge",
           stat = "identity",
           width=0.4)+
  scale_y_continuous(limits = c(0, 0.5))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Proportions of male-male couples (with and without children)")+
  ylab("Male-male couples (%)")+
  xlab("Area")+
  theme(legend.title=element_text(size=20),
        legend.text=element_text(size=15))

ggplot(households_genders, aes(neighb_code, as.numeric(prop_couple_female_female))) +
  geom_bar(position = "dodge",
           stat = "identity",
           width=0.4)+
  scale_y_continuous(limits = c(0, 0.5))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Proportions of female-female couples (with and without children)")+
  ylab("Female-female couples (%)")+
  xlab("Area")+
  theme(legend.title=element_text(size=20),
        legend.text=element_text(size=15))

households_genders = households_genders %>%
  select(neighb_code, prop_couple_heterosexual, prop_couple_male_male, prop_couple_female_female) %>%
  pivot_longer(!neighb_code, names_to = "type", values_to = "prop") %>%
  mutate(type = recode(type, prop_couple_heterosexual = 'Heterosexual', prop_couple_male_male = 'Male-male', prop_couple_female_female =  'Female-female' ))

ggplot(households_genders, aes(neighb_code, as.numeric(prop))) +
  geom_bar(aes(fill = type),
           position = "dodge",
           stat = "identity",
           width=0.4)+
  scale_y_continuous(limits = c(0, 1))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Proportions of all couple types (with and without children)")+
  ylab("Couples (%)")+
  xlab("Area")+
  labs(fill = "Type of couple")+
  theme(legend.title=element_text(size=20),
        legend.text=element_text(size=15))

################################################################################
# Household size distribution
df_household_size = df_household_size %>%
  rename(hh_size = size,
         real = freq)
df_household_size$generated=0
for (i in 1:nrow(df_household_size)){
  df_household_size[i,]$generated = nrow(df_households[df_households$hh_size == df_household_size[i, 'hh_size'],])
}

df_household_size$generated = df_household_size$generated/sum(df_household_size$generated)
df_household_size$real = df_household_size$real/sum(df_household_size$real)

df_household_size = df_household_size %>%
  pivot_longer(!hh_size, names_to = "type", values_to = "prop")

ggplot(df_household_size, aes(hh_size, as.numeric(prop))) +
  geom_bar(aes(fill = type),
           position = "dodge",
           stat = "identity",
           width=0.4)+
  scale_y_continuous(limits = c(0, 1))+
  ggtitle("Distribution of household sizes")+
  ylab("Proportion of households (%)")+
  xlab("Household sizes")+
  labs(fill = "Population")+
  theme(legend.title=element_text(size=20),
        legend.text=element_text(size=15))

################################################################################
# age disparity in couples

setwd(this.path::this.dir())
setwd('data')
df_age_couples <- read.csv("couples_age_disparity.csv", sep=',')
df_age_couples_heterosexual <- read_delim("couples_age_disparity_heterosexual-formatted.csv", 
                                          delim = ";", escape_double = FALSE, trim_ws = TRUE)

df_age_couples$generated_male_female = 0
df_age_couples$generated_female_female = 0
df_age_couples$generated_male_male = 0
df_age_couples_heterosexual$generated = 0

for (hh_ID in unique(df_synth_pop$hh_ID)){
  household = df_synth_pop[df_synth_pop$hh_ID == hh_ID,]

  if(household$hh_type!='single' & household$hh_type!='single_parent') {
    parents = household[household$is_child==0,]
    age_diff = abs(diff(parents$age))
    
    
    if(age_diff==0){
      if (nrow(parents[parents$gender == 'male',])==2) {
        df_age_couples[df_age_couples$gap=='0',]$generated_male_male = df_age_couples[df_age_couples$gap=='0',]$generated_male_male + 1
      } else if (nrow(parents[parents$gender == 'female',])==2) {
        df_age_couples[df_age_couples$gap=='0',]$generated_female_female = df_age_couples[df_age_couples$gap=='0',]$generated_female_female + 1
      } else {
        df_age_couples[df_age_couples$gap=='0',]$generated_male_female = df_age_couples[df_age_couples$gap=='0',]$generated_male_female + 1
        
        df_age_couples_heterosexual[df_age_couples_heterosexual$gap=='0',]$generated = df_age_couples_heterosexual[df_age_couples_heterosexual$gap=='0',]$generated + 1
        
     }
    } else if (age_diff>= 1 &age_diff < 5) {
      if (nrow(parents[parents$gender == 'male',])==2) {
        df_age_couples[df_age_couples$gap=='1_4',]$generated_male_male = df_age_couples[df_age_couples$gap=='1_4',]$generated_male_male + 1
      } else if (nrow(parents[parents$gender == 'female',])==2) {
        df_age_couples[df_age_couples$gap=='1_4',]$generated_female_female = df_age_couples[df_age_couples$gap=='1_4',]$generated_female_female + 1
      } else {
        df_age_couples[df_age_couples$gap=='1_4',]$generated_male_female = df_age_couples[df_age_couples$gap=='1_4',]$generated_male_female + 1
        
        older_parent = parents[which.max(parents$age),]
        gender_older_parent = older_parent$gender
        
        df_age_couples_heterosexual[df_age_couples_heterosexual$gap=='1_4' & df_age_couples_heterosexual$gender_older == gender_older_parent,]$generated = df_age_couples_heterosexual[df_age_couples_heterosexual$gap=='1_4' & df_age_couples_heterosexual$gender_older == gender_older_parent,]$generated + 1
        
      }
    } else if (age_diff >= 5 & age_diff < 10) {
      if (nrow(parents[parents$gender == 'male',])==2) {
        df_age_couples[df_age_couples$gap=='5_9',]$generated_male_male = df_age_couples[df_age_couples$gap=='5_9',]$generated_male_male + 1
      } else if (nrow(parents[parents$gender == 'female',])==2) {
        df_age_couples[df_age_couples$gap=='5_9',]$generated_female_female = df_age_couples[df_age_couples$gap=='5_9',]$generated_female_female + 1
      } else {
        df_age_couples[df_age_couples$gap=='5_9',]$generated_male_female = df_age_couples[df_age_couples$gap=='5_9',]$generated_male_female + 1
        
        older_parent = parents[which.max(parents$age),]
        gender_older_parent = older_parent$gender
        
        df_age_couples_heterosexual[df_age_couples_heterosexual$gap=='5_9' & df_age_couples_heterosexual$gender_older == gender_older_parent,]$generated = df_age_couples_heterosexual[df_age_couples_heterosexual$gap=='5_9' & df_age_couples_heterosexual$gender_older == gender_older_parent,]$generated + 1
        
      }
    } else if (age_diff >= 10 & age_diff < 15) {
      if (nrow(parents[parents$gender == 'male',])==2) {
        df_age_couples[df_age_couples$gap=='10_14',]$generated_male_male = df_age_couples[df_age_couples$gap=='10_14',]$generated_male_male + 1
      } else if (nrow(parents[parents$gender == 'female',])==2) {
        df_age_couples[df_age_couples$gap=='10_14',]$generated_female_female = df_age_couples[df_age_couples$gap=='10_14',]$generated_female_female + 1
      } else {
        df_age_couples[df_age_couples$gap=='10_14',]$generated_male_female = df_age_couples[df_age_couples$gap=='10_14',]$generated_male_female + 1
        
        older_parent = parents[which.max(parents$age),]
        gender_older_parent = older_parent$gender
        
        df_age_couples_heterosexual[df_age_couples_heterosexual$gap=='10_14' & df_age_couples_heterosexual$gender_older == gender_older_parent,]$generated = df_age_couples_heterosexual[df_age_couples_heterosexual$gap=='10_14' & df_age_couples_heterosexual$gender_older == gender_older_parent,]$generated + 1
        
      }
    } else if (age_diff >= 15 & age_diff < 20) {
      if (nrow(parents[parents$gender == 'male',])==2) {
        df_age_couples[df_age_couples$gap=='15_19',]$generated_male_male = df_age_couples[df_age_couples$gap=='15_19',]$generated_male_male + 1
      } else if (nrow(parents[parents$gender == 'female',])==2) {
        df_age_couples[df_age_couples$gap=='15_19',]$generated_female_female = df_age_couples[df_age_couples$gap=='15_19',]$generated_female_female + 1
      } else {
        df_age_couples[df_age_couples$gap=='15_19',]$generated_male_female = df_age_couples[df_age_couples$gap=='15_19',]$generated_male_female + 1
        
        older_parent = parents[which.max(parents$age),]
        gender_older_parent = older_parent$gender
        
        df_age_couples_heterosexual[df_age_couples_heterosexual$gap=='15_19' & df_age_couples_heterosexual$gender_older == gender_older_parent,]$generated = df_age_couples_heterosexual[df_age_couples_heterosexual$gap=='15_19' & df_age_couples_heterosexual$gender_older == gender_older_parent,]$generated + 1
        
      }
    } else if (age_diff >= 20) {
      if (nrow(parents[parents$gender == 'male',])==2) {
        df_age_couples[df_age_couples$gap=='20_or_more',]$generated_male_male = df_age_couples[df_age_couples$gap=='20_or_more',]$generated_male_male + 1
      } else if (nrow(parents[parents$gender == 'female',])==2) {
        df_age_couples[df_age_couples$gap=='20_or_more',]$generated_female_female = df_age_couples[df_age_couples$gap=='20_or_more',]$generated_female_female + 1
      } else {
        df_age_couples[df_age_couples$gap=='20_or_more',]$generated_male_female = df_age_couples[df_age_couples$gap=='20_or_more',]$generated_male_female + 1
       
        older_parent = parents[which.max(parents$age),]
        gender_older_parent = older_parent$gender
        
        df_age_couples_heterosexual[df_age_couples_heterosexual$gap=='20_or_more' & df_age_couples_heterosexual$gender_older == gender_older_parent,]$generated = df_age_couples_heterosexual[df_age_couples_heterosexual$gap=='20_or_more' & df_age_couples_heterosexual$gender_older == gender_older_parent,]$generated + 1
        
      }
    }
  }
}

df_age_couples$generated_male_female = df_age_couples$generated_male_female / sum (df_age_couples$generated_male_female)
df_age_couples$generated_male_male = df_age_couples$generated_male_male / sum (df_age_couples$generated_male_male)
df_age_couples$generated_female_female = df_age_couples$generated_female_female / sum (df_age_couples$generated_female_female)

df_age_couples$male_female = df_age_couples$male_female / sum (df_age_couples$male_female)
df_age_couples$male_male = df_age_couples$male_male / sum (df_age_couples$male_male)
df_age_couples$female_female = df_age_couples$female_female / sum (df_age_couples$female_female)

df_age_couples_heterosexual$generated = df_age_couples_heterosexual$generated / sum (df_age_couples_heterosexual$generated)

# Age disparity of heterosexual couples

df_age_couples_heterosexual_absolute = df_age_couples %>%
  select(gap, male_female, generated_male_female) %>%
  pivot_longer(!gap, names_to = "type", values_to = "prop")%>%
  mutate(gap = recode(gap, '0' = 'no age disparity',
                      '1_4'= '1 to 4 years',
                      '5_9' =  '5 to 9 years',
                      '10_14' =  '10 to 14 years',
                      '15_19' =  '15 to 19 years',
                      '20_or_more' =  '20 or more years')) %>%
  mutate(type = recode(type, 'generated_male_female' = 'generated',
                      'male_female'= 'real'))

df_age_couples_heterosexual_absolute$gap <- as.character(df_age_couples_heterosexual_absolute$gap)
df_age_couples_heterosexual_absolute$gap <- factor(df_age_couples_heterosexual_absolute$gap, levels=unique(df_age_couples_heterosexual_absolute$gap))

ggplot(df_age_couples_heterosexual_absolute, aes(gap, as.numeric(prop))) +
  geom_bar(aes(fill = type),
           position = "dodge",
           stat = "identity",
           width=0.4)+
  scale_y_continuous(limits = c(0, 1))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Age disparity of heterosexual couples")+
  ylab("Proportion of couples (%)")+
  xlab("Age disparity")+
  labs(fill = "Population")+
  theme(legend.title=element_text(size=20),
        legend.text=element_text(size=15))

# Age disparity of gay couples

df_age_couples_gay = df_age_couples %>%
  select(gap, male_male, generated_male_male) %>%
  pivot_longer(!gap, names_to = "type", values_to = "prop")%>%
  mutate(gap = recode(gap, '0' = 'no age disparity',
                      '1_4'= '1 to 4 years',
                      '5_9' =  '5 to 9 years',
                      '10_14' =  '10 to 14 years',
                      '15_19' =  '15 to 19 years',
                      '20_or_more' =  '20 or more years')) %>%
  mutate(type = recode(type, 'generated_male_male' = 'generated',
                         'male_male'= 'real'))

df_age_couples_gay$gap <- as.character(df_age_couples_gay$gap)
df_age_couples_gay$gap <- factor(df_age_couples_gay$gap, levels=unique(df_age_couples_gay$gap))

ggplot(df_age_couples_gay, aes(gap, as.numeric(prop))) +
  geom_bar(aes(fill = type),
           position = "dodge",
           stat = "identity",
           width=0.4)+
  scale_y_continuous(limits = c(0, 1))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Age disparity of male-male couples")+
  ylab("Proportion of couples (%)")+
  xlab("Age disparity")+
  labs(fill = "Population")+
  theme(legend.title=element_text(size=20),
        legend.text=element_text(size=15))

# Age disparity of female-female couples

df_age_couples_lesbian = df_age_couples %>%
  select(gap, female_female, generated_female_female) %>%
  pivot_longer(!gap, names_to = "type", values_to = "prop")%>%
  mutate(gap = recode(gap, '0' = 'no age disparity',
                      '1_4'= '1 to 4 years',
                      '5_9' =  '5 to 9 years',
                      '10_14' =  '10 to 14 years',
                      '15_19' =  '15 to 19 years',
                      '20_or_more' =  '20 or more years')) %>%
  mutate(type = recode(type, 'generated_female_female' = 'generated',
                       'female_female'= 'real'))+
  theme(legend.title=element_text(size=20),
        legend.text=element_text(size=15))

df_age_couples_lesbian$gap <- as.character(df_age_couples_lesbian$gap)
df_age_couples_lesbian$gap <- factor(df_age_couples_lesbian$gap, levels=unique(df_age_couples_lesbian$gap))

ggplot(df_age_couples_lesbian, aes(gap, as.numeric(prop))) +
  geom_bar(aes(fill = type),
           position = "dodge",
           stat = "identity",
           width=0.4)+
  scale_y_continuous(limits = c(0, 1))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Age disparity of female-female couples")+
  ylab("Proportion of couples (%)")+
  xlab("Age disparity")+
  labs(fill = "Population")+
  theme(legend.title=element_text(size=20),
        legend.text=element_text(size=15))

# Age disparity of heterosexual couples, with older gender

df_age_couples_heterosexual_gender = df_age_couples_heterosexual %>%
  rename(real = prob) %>%
  select(gap, gender_older, real, generated) %>%
  pivot_longer(cols = -c(gap, gender_older), names_to = "type", values_to = "prop")%>%
  mutate(gap = recode(gap, '0' = 'no age disparity',
                      '1_4'= '1 to 4 years',
                      '5_9' =  '5 to 9 years',
                      '10_14' =  '10 to 14 years',
                      '15_19' =  '15 to 19 years',
                      '20_or_more' =  '20 or more years'))%>%
  mutate(gender_older = recode(gender_older, 'male' = 'male older',
                      'female' = 'female older'))
df_age_couples_heterosexual_gender$group <- paste(df_age_couples_heterosexual_gender$gap, "-", df_age_couples_heterosexual_gender$gender_older)

df_age_couples_heterosexual_gender = df_age_couples_heterosexual_gender %>%
  mutate(group = recode(group, 'no age disparity - none' = 'no age disparity'))

df_age_couples_heterosexual_gender$group <- as.character(df_age_couples_heterosexual_gender$group)
df_age_couples_heterosexual_gender$group <- factor(df_age_couples_heterosexual_gender$group, levels=unique(df_age_couples_heterosexual_gender$group))


ggplot(df_age_couples_heterosexual_gender, aes(group, as.numeric(prop))) +
  geom_bar(aes(fill = type),
           position = "dodge",
           stat = "identity",
           width=0.4)+
  scale_y_continuous(limits = c(0, 1))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Age disparity of heterosexual couples")+
  ylab("Proportion of couples (%)")+
  xlab("Age disparity")+
  labs(fill = "Population")+
  theme(legend.title=element_text(size=20),
        legend.text=element_text(size=15))


#########################################################################################
# Age difference mother - child

setwd(this.path::this.dir())
setwd(paste0('data/', municipality, '/households/distributions'))
df_mother_children <- read.csv("mother_children_age_37201-formatted.csv")

list_mother_older_child <- c()

df_mother_children$generated=0
for (hh_ID in unique(df_synth_pop$hh_ID)){
  household = df_synth_pop[df_synth_pop$hh_ID == hh_ID,]
  
  if(household$hh_type=='couple_children_straight') {
    
    parents = household[household$is_child==0,]
    
    children = household[household$is_child==1,]
    
    avg_age_children = mean(children$age)
    
    age_diff = parents[parents$gender=='female',]$age - avg_age_children
    
    age_older_child = max(children$age)
    
    diff_with_older_child = (parents[parents$gender=='female',]$age - age_older_child)
    list_mother_older_child = append(list_mother_older_child, diff_with_older_child)
    
    if(age_diff < 20){
      df_mother_children[df_mother_children$diff_group=='less_20',]$generated = df_mother_children[df_mother_children$diff_group=='less_20',]$generated + 1
    } else if (age_diff >= 20 & age_diff < 25) {
      df_mother_children[df_mother_children$diff_group=='between_20_25',]$generated = df_mother_children[df_mother_children$diff_group=='between_20_25',]$generated + 1
    } else if (age_diff >= 25 & age_diff < 30) {
      df_mother_children[df_mother_children$diff_group=='between_25_30',]$generated = df_mother_children[df_mother_children$diff_group=='between_25_30',]$generated + 1
    } else if (age_diff >= 30 & age_diff < 35) {
      df_mother_children[df_mother_children$diff_group=='between_30_35',]$generated = df_mother_children[df_mother_children$diff_group=='between_30_35',]$generated + 1
    } else if (age_diff >= 35 & age_diff < 40) {
      df_mother_children[df_mother_children$diff_group=='between_35_40',]$generated = df_mother_children[df_mother_children$diff_group=='between_35_40',]$generated + 1
    } else if (age_diff >= 40 & age_diff < 45) {
      df_mother_children[df_mother_children$diff_group=='between_40_45',]$generated = df_mother_children[df_mother_children$diff_group=='between_40_45',]$generated + 1
    } else if(age_diff >= 45){
      df_mother_children[df_mother_children$diff_group=='more_45',]$generated = df_mother_children[df_mother_children$diff_group=='more_45',]$generated + 1
    }
  }
}

# Disparity with older child
diff_with_older_child = mean(list_mother_older_child)

df_mother_older_child = data.frame(table(list_mother_older_child))
df_mother_older_child_plot <- df_mother_older_child %>%
  rename(age = list_mother_older_child)
df_mother_older_child_plot = df_mother_older_child_plot %>%
  mutate(age = as.numeric(as.character(age))) %>%
  mutate(age = case_when(age >= 16  & age <= 19 ~ '16 to 19 years',
                         age >= 20  & age < 25 ~ '20 to 24 years',
                         age >= 25  & age < 30 ~ '25 to 29 years',
                         age >= 30  & age < 35 ~ '30 to 34 years',
                         age >= 35  & age < 40 ~ '35 to 39 years',
                         age >= 40  & age < 45 ~ '40 to 44 years',
                         age >= 45 ~ '45 or more years'))%>%
  group_by(age)%>%
  summarise(Freq = sum(Freq))
df_mother_older_child_plot$prop = df_mother_older_child_plot$Freq/sum(df_mother_older_child_plot$Freq)

x_labels = c('16 to 19 years', '20 to 24 years', '25 to 29 years', '30 to 34 years', '35 to 39 years', '40 to 44 years', '45 or more years')
df_mother_older_child_plot$age <- factor(df_mother_older_child_plot$age, levels = x_labels)

ggplot(df_mother_older_child_plot, aes(age, as.numeric(prop))) +
  geom_bar(position = "dodge",
           stat = "identity",
           width=0.4)+
  scale_y_continuous(limits = c(0, 1))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Age disparity between mother and oldest child in her heterosexual household")+
  ylab("Proportion of mothers (%)")+
  xlab("Age disparity")+
  theme(legend.title=element_text(size=20),
        legend.text=element_text(size=15))

# Disparity with average children age

df_mother_children$generated = df_mother_children$generated / sum(df_mother_children$generated)

df_mother_children_plot = df_mother_children %>%
  select(diff_group, prob, generated) %>%
  rename(real = prob) %>%
  pivot_longer(!diff_group, names_to = "type", values_to = "prop")%>%
  mutate(diff_group = recode(diff_group, '0' = 'no age disparity',
                      'less_20'= 'under 20 years',
                      'between_20_25' =  '20 to 24 years',
                      'between_25_30' =  '25 to 29 years',
                      'between_30_35' =  '30 to 34 years',
                      'between_35_40' =  '35 to 39 years',
                      'between_40_45' =  '40 to 44 years',
                      'more_45' =  '45 or more years'))
df_mother_children_plot$diff_group <- as.character(df_mother_children_plot$diff_group)
df_mother_children_plot$diff_group <- factor(df_mother_children_plot$diff_group, levels=unique(df_mother_children_plot$diff_group))

ggplot(df_mother_children_plot, aes(diff_group, as.numeric(prop))) +
  geom_bar(aes(fill = type),
           position = "dodge",
           stat = "identity",
           width=0.4)+
  scale_y_continuous(limits = c(0, 1))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Age disparity between mother and average children age in her heterosexual household")+
  ylab("Proportion of mothers (%)")+
  xlab("Age disparity")+
  labs(fill = "Population")+
  theme(legend.title=element_text(size=20),
        legend.text=element_text(size=15))