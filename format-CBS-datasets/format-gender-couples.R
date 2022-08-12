library(dplyr)
library (readr)

library("this.path")

setwd(this.path::this.dir())
setwd('../data')
            
df_GenderDisparity <- read_delim("~/Documents/DHZW_synthetic-population/data/couples_gender_disparity_37772ENG.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

df_GenderDisparity$male_female = df_GenderDisparity$`Marriages/Between man and woman (number)` + df_GenderDisparity$`Partnership registrations/Total partnership registrations/Between man and woman (number)`
df_GenderDisparity$male_male = df_GenderDisparity$`Marriages/Between men (number)` + df_GenderDisparity$`Partnership registrations/Total partnership registrations/Between men (number)`
df_GenderDisparity$female_female = df_GenderDisparity$`Marriages/Between women (number)` + df_GenderDisparity$`Partnership registrations/Total partnership registrations/Between women (number)`

df_GenderDisparity = df_GenderDisparity %>%
  select(male_female, male_male, female_female, Periods) %>%
  rename(year = Periods)

df_GenderDisparity = as.data.frame(t(df_GenderDisparity))

colnames(df_GenderDisparity) <- df_GenderDisparity['year',]
df_GenderDisparity = df_GenderDisparity[!(row.names(df_GenderDisparity) == 'year'),]

df_GenderDisparity <- cbind(type = rownames(df_GenderDisparity), df_GenderDisparity)
rownames(df_GenderDisparity) <- 1:nrow(df_GenderDisparity)

# Save dataset
write.csv(df_GenderDisparity, 'couples_gender_disparity_37772ENG-formatted.csv', row.names=FALSE)