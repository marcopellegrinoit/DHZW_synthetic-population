library(ggplot2)
library(dplyr)
library(tidyr)
library("this.path")
setwd(this.path::this.dir())
source('../src/utils-evaluation-synthetic-population.R')

# Load datasets
setwd(this.path::this.dir())
setwd("../data/processed/individuals")

df_marginal_dist = read.csv("marginal_distributions_84583NED-formatted.csv", sep = ",")
df_gender_strat = read.csv("gender_age-03759NED-formatted.csv", sep = ",")

# Load synthetic population
setwd(this.path::this.dir())
setwd(paste("../output/synthetic-population-households/", sep = ""))
df_synth_pop = read.csv("synthetic_population_DHZW_2019.csv", sep = ",")

################################################################################
# With marginal distribution
df_gender_marginal <- get_proportions_over_marginal(df_marginal_dist = df_marginal_dist,
                                                    df_synth_pop = df_synth_pop,
                                                    aggregation_var = 'neighb_code',
                                                    cols_marginal = c('gender_male', 'gender_female'),
                                                    var_str = 'gender',
                                                    values = c('male', 'female')
)
ggplot(df_gender_marginal, aes(neighb_code, proportion)) +
  facet_grid(vars(gender))+
  geom_bar(aes(fill = dataset),
           position = "dodge",
           stat = "identity",
           width=0.4)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Proportions of gender per neighbourhood")+
  ylab("Individuals (%)")+
  xlab("Neighbourhood code")+
  labs(fill = "Population")+
  theme(legend.title=element_text(size=20),
        legend.text=element_text(size=15))

################################################################################
# With stratified dataset
df_gender_strat = df_gender_strat %>%
  select(age, male, female) %>%
  pivot_longer(cols = -c(age), names_to = 'gender', values_to = "real")
df_gender_strat$pred = 0

for (i in (1:nrow(df_gender_strat))) {
  df_gender_strat[i, 'pred',] = nrow(df_synth_pop[df_synth_pop$age == df_gender_strat[i, 'age']$age &
                                                    df_synth_pop$gender == df_gender_strat[i, 'gender']$gender,])
}
df_gender_strat <- df_gender_strat %>%
  pivot_longer(cols = c(real, pred), names_to = 'dataset', values_to = 'proportion')

# rename values
df_gender_strat$dataset <- recode(df_gender_strat$dataset,
                                  'real' = 'stratified dataset',
                                  'pred' = 'synthetic population')

setwd(this.dir())
setwd('data_comparison')
write.csv(df_gender_strat, 'gender.csv', row.names = FALSE)

df_gender_strat <- df_gender_strat %>%
  group_by(age, dataset) %>%
  mutate(proportion = proportion / sum(proportion))

# bar plot
ggplot(df_gender_strat, aes(age, proportion)) +
  facet_grid(vars(gender))+
  geom_bar(aes(fill = dataset),
           position = "dodge",
           stat = "identity",
           width=0.4)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Proportions of gender per age")+
  ylab("Individuals (%)")+
  xlab("Gender and age groups")+
  labs(fill = "Population")+
  theme(legend.title=element_text(size=20),
        legend.text=element_text(size=15))