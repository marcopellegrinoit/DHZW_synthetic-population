library(dplyr)
library(tidyr)
library(ggplot2)
library(AMR)
library("this.path")
library(readr)

setwd(paste0(this.path::this.dir(), "/data/Formatted"))
df_ODiN <- read_csv("df_DHZW.csv")
df_synth_pop <- read_csv("~/DHZW_synthetic-population/synthetic-populations/synthetic_population_DHZW_2019_with_hh.csv")

df_synth_pop$age_group = ""
df_synth_pop$age_group[df_synth_pop$age %in% 0:9] = "age_0_9"
df_synth_pop$age_group[df_synth_pop$age %in% 10:14] =  "age_10_14"
df_synth_pop$age_group[df_synth_pop$age %in% 15:19] = "age_15_19"
df_synth_pop$age_group[df_synth_pop$age %in% 20:24] =  "age_20_24" 
df_synth_pop$age_group[df_synth_pop$age %in% 25:29] = "age_25_29"
df_synth_pop$age_group[df_synth_pop$age %in% 30:39] =  "age_30_39"
df_synth_pop$age_group[df_synth_pop$age %in% 40:49] =  "age_40_49" 
df_synth_pop$age_group[df_synth_pop$age %in% 50:69] = "age_50_69"
df_synth_pop$age_group[df_synth_pop$age >= 70] = "age_over_70"

df_ODiN$age_group = ""
df_ODiN$age_group[df_ODiN$age %in% 0:9] = "age_0_9"
df_ODiN$age_group[df_ODiN$age %in% 10:14] =  "age_10_14"
df_ODiN$age_group[df_ODiN$age %in% 15:19] = "age_15_19"
df_ODiN$age_group[df_ODiN$age %in% 20:24] =  "age_20_24" 
df_ODiN$age_group[df_ODiN$age %in% 25:29] = "age_25_29"
df_ODiN$age_group[df_ODiN$age %in% 30:39] =  "age_30_39"
df_ODiN$age_group[df_ODiN$age %in% 40:49] =  "age_40_49" 
df_ODiN$age_group[df_ODiN$age %in% 50:69] = "age_50_69"
df_ODiN$age_group[df_ODiN$age >= 70] = "age_over_70"

################################################################################
# How many agents are new
print(paste0('There are ',
             nrow(df_ODiN[df_ODiN$agent_new=='yes',]),
             ' agents that are new, which is the ',
             (nrow(df_ODiN[df_ODiN$agent_new=='yes',])/nrow(df_ODiN)),
             '% of the total'))

################################################################################
# Plot combinations of attributes that are not in ODiN

ODiN <- df_ODiN %>%
  select(age_group, gender, migration_background, hh_PC4) %>%
  distinct()

synth <- df_synth_pop %>%
  select(age_group, gender, migration_background, hh_PC4)
  #distinct()

df_difference <- anti_join(synth, ODiN)

df_difference <- df_difference %>%
  group_by(age_group, gender, migration_background, hh_PC4) %>%
  summarise(freq = (n()/nrow(df_synth_pop))*100)

df_difference <- df_difference %>%
  mutate(age_group = recode(age_group,
                            'age_0_9' = 'Between 0 and 9', 
                            'age_10_14' = 'Between 10 and 14',
                            'age_15_19' = 'Between 15 and 19',
                            'age_20_24' = 'Between 20 and 24',
                            'age_25_29' = 'Between 25 and 29',
                            'age_30_39' = 'Between 30 and 39', 
                            'age_40_49' = 'Between 40 and 49',
                            'age_50_69' = 'Between 50 and 69',
                            'age_over_70' = 'Over 70'))

ggplot(df_difference, aes(x=as.character(age_group), y=as.character(hh_PC4), size=freq)) +
  geom_point() + 
  facet_grid(vars(gender), vars(migration_background))+
  theme(title = element_text(size = 20),
        legend.title=element_text(size=15),
        legend.text=element_text(size=15),
        axis.text = element_text(size=15),
        axis.title = element_text(size=20),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("Age group")+
  ylab("Home PC4")+
  labs(size='% missing agents in \n synthetic population')