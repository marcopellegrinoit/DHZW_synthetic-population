library(ggplot2)
library(dplyr)
library(tidyr)
library("this.path")
setwd(this.path::this.dir())
source('../src/utils-evaluation-synthetic-population.R')

# Load datasets
setwd(this.path::this.dir())
setwd("../data/processed/households")
df_ischild_strat = read.csv("household_gender_age-71488NED-formatted.csv", sep = ",")

# Load synthetic population
setwd(this.path::this.dir())
setwd(paste("../output/synthetic-population-households/", sep = ""))
df_synth_pop = read.csv("synthetic_population_DHZW_2019.csv", sep = ",")

################################################################################
# With stratified dataset

# Create group ages to match the stratified dataset
df_synth_pop$age_group = ""
df_synth_pop$age_group[df_synth_pop$age %in% 0:4] = "age_0_5"
df_synth_pop$age_group[df_synth_pop$age %in% 5:9] = "age_5_10"
df_synth_pop$age_group[df_synth_pop$age %in% 10:14] = "age_10_15"
df_synth_pop$age_group[df_synth_pop$age %in% 15:19] = "age_15_20"
df_synth_pop$age_group[df_synth_pop$age %in% 20:24] = "age_20_25"
df_synth_pop$age_group[df_synth_pop$age %in% 25:29] = "age_25_30"
df_synth_pop$age_group[df_synth_pop$age %in% 30:34] = "age_30_35"
df_synth_pop$age_group[df_synth_pop$age %in% 35:39] = "age_35_40"
df_synth_pop$age_group[df_synth_pop$age %in% 40:44] = "age_40_45"
df_synth_pop$age_group[df_synth_pop$age %in% 45:49] = "age_45_50"
df_synth_pop$age_group[df_synth_pop$age %in% 50:54] = "age_50_55"
df_synth_pop$age_group[df_synth_pop$age %in% 55:59] = "age_55_60"
df_synth_pop$age_group[df_synth_pop$age %in% 60:64] = "age_60_65"
df_synth_pop$age_group[df_synth_pop$age %in% 65:69] = "age_65_70"
df_synth_pop$age_group[df_synth_pop$age %in% 70:74] = "age_70_75"
df_synth_pop$age_group[df_synth_pop$age %in% 75:79] = "age_75_80"
df_synth_pop$age_group[df_synth_pop$age %in% 80:84] = "age_80_85"
df_synth_pop$age_group[df_synth_pop$age %in% 85:89] = "age_85_90"
df_synth_pop$age_group[df_synth_pop$age %in% 90:94] = "age_90_95"
df_synth_pop$age_group[df_synth_pop$age %in% 95:105] = "age_over_95"

df_ischild_strat$non_child <- df_ischild_strat$total - df_ischild_strat$child
df_ischild_strat = df_ischild_strat %>%
  select(gender, age_group, child, non_child)

df_ischild_strat <- df_ischild_strat %>%
  pivot_longer(cols=c('child', 'non_child'), names_to = 'is_child', values_to = 'real')

df_gen = data.frame(table(df_synth_pop$age_group, df_synth_pop$gender, df_synth_pop$is_child))
colnames(df_gen) <- c('age_group', 'gender', 'is_child', 'pred')

df_gen$is_child <- as.character(df_gen$is_child)
df_gen[df_gen$is_child == '0',]$is_child <- 'non_child'
df_gen[df_gen$is_child == '1',]$is_child <- 'child'

df_ischild_strat <- left_join(df_ischild_strat, df_gen, by=c('age_group', 'gender', 'is_child'))

df_ischild_strat <- df_ischild_strat %>%
  pivot_longer(cols = c(real, pred), names_to = 'dataset', values_to = 'proportion')

# rename values
df_ischild_strat$dataset <- recode(df_ischild_strat$dataset,
                                  'real' = 'stratified dataset',
                                  'pred' = 'synthetic population')

setwd(this.dir())
setwd('data_comparison')
write.csv(df_ischild_strat, 'ischild.csv', row.names = FALSE)

df_ischild_strat <- df_ischild_strat %>%
  group_by(age_group, gender, is_child, dataset) %>%
  mutate(proportion = proportion / sum(proportion))

# bar plot
ggplot(df_ischild_strat, aes(age_group, proportion)) +
  facet_grid(vars(gender))+
  geom_bar(aes(fill = dataset),
           position = "dodge",
           stat = "identity",
           width=0.4)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Proportions of ischild per agegroup adn gender")+
  ylab("Individuals (%)")+
  xlab("Gender and age groups")+
  labs(fill = "Population")+
  theme(legend.title=element_text(size=20),
        legend.text=element_text(size=15))