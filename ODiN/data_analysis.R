library(readr)
library("dplyr")
library("this.path")
setwd(this.path::this.dir())
source('utils.R')

# Load ODiN dataset
setwd(paste0(this.path::this.dir(), "/data/Formatted"))
df_ODiN <- read_csv("df_DHZW.csv")

monday <- df_ODiN[df_ODiN$weekday==1,]
monday_agents <- get_n_agents(monday)
monday_displacements <- get_n_displacements(monday)
print(paste0('Monday. N agents: ', monday_agents, ' . N displacements: ', monday_displacements))

tuesday <- df_ODiN[df_ODiN$weekday==2,]
tuesday_agents <- get_n_agents(tuesday)
tuesday_displacements <- get_n_displacements(tuesday)
print(paste0('Tuesday. N agents: ', tuesday_agents, ' . N displacements: ', tuesday_displacements))

wednesday <- df_ODiN[df_ODiN$weekday==3,]
wednesday_agents <- get_n_agents(wednesday)
wednesday_displacements <- get_n_displacements(wednesday)
print(paste0('Wednesday. N agents: ', wednesday_agents, ' . N displacements: ', wednesday_displacements))

thursday <- df_ODiN[df_ODiN$weekday==4,]
thursday_agents <- get_n_agents(thursday)
thursday_displacements <- get_n_displacements(thursday)
print(paste0('Thursday. N agents: ', thursday_agents, ' . N displacements: ', thursday_displacements))

friday <- df_ODiN[df_ODiN$weekday==5,]
friday_agents <- get_n_agents(friday)
friday_displacements <- get_n_displacements(friday)
print(paste0('Friday. N agents: ', friday_agents, ' . N displacements: ', friday_displacements))

saturday <- df_ODiN[df_ODiN$weekday==6,]
saturday_agents <- get_n_agents(saturday)
saturday_displacements <- get_n_displacements(saturday)
print(paste0('Saturday. N agents: ', saturday_agents, ' . N displacements: ', saturday_displacements))

sunday <- df_ODiN[df_ODiN$weekday==7,]
sunday_agents <- get_n_agents(sunday)
sunday_displacements <- get_n_displacements(sunday)
print(paste0('Sunday. N agents: ', sunday_agents, ' . N displacements: ', sunday_displacements))

weekdays <- df_ODiN[df_ODiN$weekday!=6 & df_ODiN$weekday!=7,]
weekdays_agents <- get_n_agents(weekdays)
weekdays_displacements <- get_n_displacements(weekdays)
print(paste0('Weekdays. N agents: ', weekdays_agents, ' . N displacements: ', weekdays_displacements))

weekends <- df_ODiN[df_ODiN$weekday==6 | df_ODiN$weekday==7,]
weekends_agents <- get_n_agents(weekends)
weekends_displacements <- get_n_displacements(weekends)
print(paste0('Weekends. N agents: ', weekends_agents, ' . N displacements: ', weekends_displacements))