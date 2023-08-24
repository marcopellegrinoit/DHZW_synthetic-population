library(dplyr)
library (readr)

setwd(this.path::this.dir())
setwd("../../data/raw")

df_couples_genders <- read_delim("couples_gender_disparity_37772ENG.csv", 
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)

df_couples_genders$male_female = df_couples_genders$`Marriages/Between man and woman (number)` + df_couples_genders$`Partnership registrations/Total partnership registrations/Between man and woman (number)`
df_couples_genders$male_male = df_couples_genders$`Marriages/Between men (number)` + df_couples_genders$`Partnership registrations/Total partnership registrations/Between men (number)`
df_couples_genders$female_female = df_couples_genders$`Marriages/Between women (number)` + df_couples_genders$`Partnership registrations/Total partnership registrations/Between women (number)`

df_couples_genders = df_couples_genders[df_couples_genders$Periods==2019,]


df_couples_genders = df_couples_genders %>%
  select(male_female, male_male, female_female)

df_couples_genders = as.data.frame(t(df_couples_genders))

df_couples_genders <- cbind(genders = rownames(df_couples_genders), df_couples_genders)
rownames(df_couples_genders) <- 1:nrow(df_couples_genders)
colnames(df_couples_genders) = c('genders', 'freq')
df_couples_genders$prob = df_couples_genders$freq / sum(df_couples_genders$freq)

# Save dataset
setwd(this.path::this.dir())
setwd("../../data/processed")
write.csv(df_couples_genders, 'couples_gender_disparity_37772ENG-formatted.csv', row.names=FALSE)