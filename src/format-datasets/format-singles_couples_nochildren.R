library(dplyr)
library (readr)
library("this.path")

setwd(this.path::this.dir())
setwd("../../data/raw/households")

df_nochildren <- read_delim("singles_couples_nochildren-71488NED.csv", 
                               delim = ";", escape_double = FALSE, trim_ws = TRUE)

df_nochildren = subset(df_nochildren, select=-c(Geslacht, Leeftijd, Perioden, `Regio's`))

# Select interesting attributes
df_nochildren = df_nochildren %>% 
  rename(singles = `Personen in particuliere huishoudens/Alleenstaand (aantal)`,
         unmarried = `Personen in particuliere huishoudens/Samenwonend/Partner in niet-gehuwd paar zonder ki... (aantal)`,
         married = `Personen in particuliere huishoudens/Samenwonend/Partner in gehuwd paar zonder kinderen (aantal)`
  )

df_nochildren$couples = df_nochildren$unmarried + df_nochildren$married

df_nochildren = subset(df_nochildren, select=-c(unmarried, married))

df_nochildren = as.data.frame(t(df_nochildren))

df_nochildren <- cbind(individuals_type = rownames(df_nochildren), df_nochildren)
rownames(df_nochildren) <- 1:nrow(df_nochildren)
colnames(df_nochildren) = c('individuals_type', 'freq')

df_nochildren$prob = df_nochildren$freq/sum(df_nochildren$freq)

setwd(this.path::this.dir())
setwd("../../data/processed/households")

write.csv(df_nochildren, 'singles_couples_nochildren-71488NED-formatted.csv', row.names=FALSE)