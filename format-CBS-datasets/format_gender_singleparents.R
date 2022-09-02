library(dplyr)
library (readr)

library("this.path")

municipality = "den_haag_2019"


setwd(this.path::this.dir())
setwd(paste0('../data/', municipality, '/households/distributions'))

df_singleparents <- read_delim("singleparents_gender-71488NED.csv", 
                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

df_singleparents = subset(df_singleparents, select=-c(Leeftijd, Perioden, `Regio's`))

# Select interesting attributes
df_singleparents = df_singleparents %>% 
  rename(gender = Geslacht,
         freq = `Personen in particuliere huishoudens/Ouder in eenouderhuishouden (aantal)`
  )
df_singleparents[df_singleparents$gender == 'Mannen',]$gender = 'male'
df_singleparents[df_singleparents$gender == 'Vrouwen',]$gender = 'female'

df_singleparents$prob = df_singleparents$freq/sum(df_singleparents$freq)

write.csv(df_singleparents, 'singleparents_gender-71488NED-formatted.csv', row.names=FALSE)