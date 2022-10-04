library(dplyr)
library (readr)
library("this.path")
setwd(this.path::this.dir())
source('../../config/config.R')

setwd(this.path::this.dir())
setwd(
  paste(
    "../../data/raw",
    year,
    municipality,
    'households',
    sep = '/'
  )
)

df_parents <- read_delim("couples_singleparents-71488NED.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Select interesting attributes
df_parents = df_parents %>% 
  rename(unmarried = `Personen in particuliere huishoudens/Samenwonend/Partner in niet-gehuwd paar met kinderen (aantal)`,
         married = `Personen in particuliere huishoudens/Samenwonend/Partner in gehuwd paar met kinderen (aantal)`,
         singleparents = `Personen in particuliere huishoudens/Ouder in eenouderhuishouden (aantal)`
  )
df_parents$couples = (df_parents$unmarried + df_parents$married)/2

df_parents = subset(df_parents, select=-c(Geslacht, Leeftijd, Perioden, `Regio's`, unmarried, married))

df_parents = as.data.frame(t(df_parents))
df_parents <- cbind(diff = rownames(df_parents), df_parents)
colnames(df_parents) = c('hh_type', 'freq')
rownames(df_parents) <- 1:nrow(df_parents)
df_parents$prob = df_parents$freq/sum(df_parents$freq)

setwd(this.path::this.dir())
setwd(
  paste(
    "../../data/processed",
    year,
    municipality,
    'households',
    sep = '/'
  )
)
write.csv(df_parents, 'couples_singleparents-71488NED-formatted.csv', row.names=FALSE)