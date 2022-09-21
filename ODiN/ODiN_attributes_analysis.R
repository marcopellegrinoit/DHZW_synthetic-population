library(dplyr)
library(tidyr)
library(ggplot2)
library(AMR)
library("this.path")
library(readr)

setwd(paste0(this.path::this.dir(), "/data/Formatted"))
df_ODiN <- read_csv("df_DHZW.csv")
df_synth_pop <- read_csv("~/DHZW_synthetic-population/synthetic-populations/synthetic_population_DHZW_2019_with_hh.csv")

df_ODiN_agents <- df_ODiN %>%
  select(agent_new, agent_ID, hh_PC4, age, gender, migration_background) %>%
  distinct() %>%
  rename(ODiN_ID = agent_ID)

###############################################################################################################
# How many agents are new
print(paste0('There are ',
             nrow(df_ODiN_agents[df_ODiN_agents$agent_new=='yes',]),
             ' agents that are new, which is the ',
             (nrow(df_ODiN_agents[df_ODiN_agents$agent_new=='yes',])/nrow(df_ODiN_agents)),
             '% of the total'))

###############################################################################################################
# Plot integer ages

df_ODiN_age <-  df_ODiN_agents %>% 
  group_by(age) %>% 
  summarise(freq = n())
df_ODiN_age$prop = (df_ODiN_age$freq / sum(df_ODiN_age$freq))*100

df_synth_age <- df_synth_pop %>% 
  group_by(age) %>% 
  summarise(freq = n())
df_synth_age$prop = (df_synth_age$freq / sum(df_synth_age$freq))*100

df_age <- left_join(df_synth_age, df_ODiN_age, by='age')
df_age[is.na(df_age)] <- 0
df_age <- df_age %>%
  select(age, prop.x, prop.y) %>%
  rename('Synthetic population' = prop.x,
         'ODiN' = prop.y) %>%
  pivot_longer(!age, names_to = "population", values_to = "prop")

ggplot(df_age, aes(x = age, y = prop, fill = population)) +
  geom_bar(data = subset(df_age, population == "Synthetic population"), aes(y = -prop), stat = "identity") +
  geom_bar(data = subset(df_age, population == "ODiN"), stat = "identity") +
  scale_y_continuous(breaks=seq(-2, 2, 0.5),labels=abs(seq(-2, 2, 0.5))) + 
  scale_x_continuous(breaks=seq(0, max(df_age$age), 10)) +
  coord_flip()+
  theme(panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey'))+
  ggtitle("Age distribution")+
  ylab("Proportion (%)")+
  xlab("Age")+
  theme(title = element_text(size = 20),
        legend.title=element_text(size=20),
        legend.text=element_text(size=15),
        axis.text = element_text(size=15),
        axis.title = element_text(size=20))


###############################################################################################################
# Plot group ages of 5

df_ODiN_agegroups <- df_ODiN_agents %>%
  group_by(age_group = age_groups(age, 1:20 * 5)) %>%
  summarise(freq = n())
df_ODiN_agegroups$prop = (df_ODiN_agegroups$freq / sum(df_ODiN_agegroups$freq))*100

df_synth_agegroups <- df_synth_pop %>%
  group_by(age_group = age_groups(age, 1:20 * 5)) %>%
  summarise(freq = n())
df_synth_agegroups$prop = (df_synth_agegroups$freq / sum(df_synth_agegroups$freq))*100

df_age_groups <- left_join(df_synth_agegroups, df_ODiN_agegroups, by='age_group')
df_age_groups[is.na(df_age_groups)] <- 0
df_age_groups <- df_age_groups %>%
  select(age_group, prop.x, prop.y) %>%
  rename('Synthetic population' = prop.x,
         'ODiN' = prop.y) %>%
  pivot_longer(!age_group, names_to = "population", values_to = "prop")

ggplot(df_age_groups, aes(x = age_group, y = prop, fill = population)) +
  geom_bar(data = subset(df_age_groups, population == "Synthetic population"), aes(y = -prop), stat = "identity") +
  geom_bar(data = subset(df_age_groups, population == "ODiN"), stat = "identity") +
  scale_y_continuous(breaks=seq(-20, 20, 5),labels=abs(seq(-20, 20, 5))) + 
  coord_flip()+
  theme(panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey'))+
  ggtitle("Age groups distribution")+
  ylab("Proportion (%)")+
  xlab("Age groups")+
  theme(title = element_text(size = 20),
        legend.title=element_text(size=20),
        legend.text=element_text(size=15),
        axis.text = element_text(size=15),
        axis.title = element_text(size=20))

###############################################################################################################
# Plot group ages of 10

df_ODiN_agegroups <- df_ODiN_agents %>%
  group_by(age_group = age_groups(age, 1:10 * 10)) %>%
  summarise(freq = n())
df_ODiN_agegroups$prop = (df_ODiN_agegroups$freq / sum(df_ODiN_agegroups$freq))*100

df_synth_agegroups <- df_synth_pop %>%
  group_by(age_group = age_groups(age, 1:10 * 10)) %>%
  summarise(freq = n())
df_synth_agegroups$prop = (df_synth_agegroups$freq / sum(df_synth_agegroups$freq))*100

df_age_groups <- left_join(df_synth_agegroups, df_ODiN_agegroups, by='age_group')
df_age_groups[is.na(df_age_groups)] <- 0
df_age_groups <- df_age_groups %>%
  select(age_group, prop.x, prop.y) %>%
  rename('Synthetic population' = prop.x,
         'ODiN' = prop.y) %>%
  pivot_longer(!age_group, names_to = "population", values_to = "prop")

ggplot(df_age_groups, aes(x = age_group, y = prop, fill = population)) +
  geom_bar(data = subset(df_age_groups, population == "Synthetic population"), aes(y = -prop), stat = "identity") +
  geom_bar(data = subset(df_age_groups, population == "ODiN"), stat = "identity") +
  scale_y_continuous(breaks=seq(-20, 20, 5),labels=abs(seq(-20, 20, 5))) + 
  coord_flip()+
  theme(panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey'))+
  ggtitle("Age groups distribution")+
  ylab("Proportion (%)")+
  xlab("Age groups")+
  theme(title = element_text(size = 20),
        legend.title=element_text(size=20),
        legend.text=element_text(size=15),
        axis.text = element_text(size=15),
        axis.title = element_text(size=20))

###############################################################################################################
# Plot migration background

df_ODiN_migration <- df_ODiN_agents %>%
  group_by(migration_background) %>%
  summarise(freq = n())
df_ODiN_migration$prop = (df_ODiN_migration$freq / sum(df_ODiN_migration$freq))*100

df_synth_migration <- df_synth_pop %>% 
  group_by(migration_background) %>% 
  summarise(freq = n())
df_synth_migration$prop = (df_synth_migration$freq / sum(df_synth_migration$freq))*100


df_migration <- left_join(df_synth_migration, df_ODiN_migration, by='migration_background')
df_migration <- df_migration %>%
  select(migration_background, prop.x, prop.y) %>%
  rename('Synthetic population' = prop.x,
         'ODiN' = prop.y) %>%
  pivot_longer(!migration_background, names_to = "population", values_to = "prop")


ggplot(df_migration, aes(migration_background, as.numeric(prop))) +
  geom_bar(aes(fill = population),
           position = "dodge",
           stat = "identity",
           width=0.4)+
  ggtitle("Migration background distribution")+
  ylab("Proportion of individuals (%)")+
  xlab("Migration backgrounds")+
  labs(fill = "Population")+
  theme(title = element_text(size = 20),
        legend.title=element_text(size=20),
        legend.text=element_text(size=15),
        axis.text = element_text(size=15),
        axis.title = element_text(size=20))


###############################################################################################################
# Plot gender

df_ODiN_gender <- df_ODiN_agents %>%
  group_by(gender) %>%
  summarise(freq = n())
df_ODiN_gender$prop = (df_ODiN_gender$freq / sum(df_ODiN_gender$freq))*100

df_synth_gender <- df_synth_pop %>% 
  group_by(gender) %>% 
  summarise(freq = n())
df_synth_gender$prop = (df_synth_gender$freq / sum(df_synth_gender$freq))*100


df_gender <- left_join(df_synth_gender, df_ODiN_gender, by='gender')
df_gender <- df_gender %>%
  select(gender, prop.x, prop.y) %>%
  rename('Synthetic population' = prop.x,
         'ODiN' = prop.y) %>%
  pivot_longer(!gender, names_to = "population", values_to = "prop")


ggplot(df_gender, aes(gender, as.numeric(prop))) +
  geom_bar(aes(fill = population),
           position = "dodge",
           stat = "identity",
           width=0.4)+
  ggtitle("Gender distribution")+
  ylab("Proportion of individuals (%)")+
  xlab("Genders")+
  theme(title = element_text(size = 20),
        legend.title=element_text(size=20),
        legend.text=element_text(size=15),
        axis.text = element_text(size=15),
        axis.title = element_text(size=20))

###############################################################################################################
# Plot PC4 home addresses

df_ODiN_PC4 <- df_ODiN_agents %>%
  group_by(hh_PC4) %>%
  summarise(freq = n())
df_ODiN_PC4$prop = (df_ODiN_PC4$freq / sum(df_ODiN_PC4$freq))*100

df_synth_PC4 <- df_synth_pop %>% 
  group_by(hh_PC4) %>% 
  summarise(freq = n())
df_synth_PC4$prop = (df_synth_PC4$freq / sum(df_synth_PC4$freq))*100


df_PC4 <- left_join(df_synth_PC4, df_ODiN_PC4, by='hh_PC4')
df_PC4 <- df_PC4 %>%
  select(hh_PC4, prop.x, prop.y) %>%
  rename('Synthetic population' = prop.x,
         'ODiN' = prop.y) %>%
  pivot_longer(!hh_PC4, names_to = "population", values_to = "prop")

ggplot(df_PC4, aes(as.character(hh_PC4), as.numeric(prop))) +
  geom_bar(aes(fill = population),
           position = "dodge",
           stat = "identity",
           width=0.4)+
  ggtitle("Home postalcode PC4 distribution")+
  ylab("Proportion of individuals (%)")+
  xlab("PC4")+
  labs(fill = "Population")+
  theme(title = element_text(size = 20),
        legend.title=element_text(size=20),
        legend.text=element_text(size=15),
        axis.text = element_text(size=15),
        axis.title = element_text(size=20))