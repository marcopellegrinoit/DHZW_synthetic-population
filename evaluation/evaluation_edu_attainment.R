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

# Load synthetic population
setwd(this.path::this.dir())
setwd(paste("../../output/synthetic-population-households/", sep = ""))
df_synth_pop = read.csv("synthetic_population_DHZW_2019.csv", sep = ",")

################################################################################
# only With marginal distribution

df_edu_attainment_marginal <- get_proportions_over_marginal(df_marginal_dist = df_marginal_dist,
                                                            df_synth_pop = df_synth_pop,
                                                            aggregation_var = neighb_code,
                                                            cols_marginal = c(education_absolved_low, education_absolved_middle, education_absolved_high),
                                                            var_str = 'edu_attainment',
                                                            values = c('low', 'middle', 'high'),
                                                            age_limits = c(15, 75)
)

ggplot(df_edu_attainment_marginal, aes(neighb_code, proportion)) +
  facet_grid(vars(edu_attainment))+
  geom_bar(aes(fill = dataset),
           position = "dodge",
           stat = "identity",
           width=0.4)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  ggtitle("Proportions of education attainment levels per neighbourhood")+
  ylab("Individuals (%)")+
  xlab("Neighbourhood code")+
  labs(fill = "Population")+
  theme(legend.title=element_text(size=20),
        legend.text=element_text(size=15))