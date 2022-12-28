library(dplyr)
library (readr)
library("this.path")
setwd(this.path::this.dir())
source('../../config/config.R')

setwd(
  paste(
    "../../data/raw",
    year,
    municipality,
    'households',
    sep = '/'
  )
)
df_mother_children <- read_delim("mother_children_age_37201.csv",
                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)
df_mother_children = subset(df_mother_children, select=-c(Perioden, `Regio's`))

df_mother_children$`Levend geboren kinderen: leeftijd moe.../Jonger dan 20 jaar (aantal)`
df_mother_children$`Levend geboren kinderen: leeftijd moe.../20 tot 25 jaar (aantal)`
df_mother_children$`Levend geboren kinderen: leeftijd moe.../45 jaar of ouder (aantal)`
df_mother_children = df_mother_children %>% 
  rename(less_20 = `Levend geboren kinderen: leeftijd moe.../Jonger dan 20 jaar (aantal)`,
         between_20_25 = `Levend geboren kinderen: leeftijd moe.../20 tot 25 jaar (aantal)`,
         between_25_30 = `Levend geboren kinderen: leeftijd moe.../25 tot 30 jaar (aantal)`,
         between_30_35 = `Levend geboren kinderen: leeftijd moe.../30 tot 35 jaar (aantal)`,
         between_35_40 = `Levend geboren kinderen: leeftijd moe.../35 tot 40 jaar (aantal)`,
         between_40_45 = `Levend geboren kinderen: leeftijd moe.../40 tot 45 jaar (aantal)`,
         more_45 = `Levend geboren kinderen: leeftijd moe.../45 jaar of ouder (aantal)`
  )
df_mother_children = as.data.frame(t(df_mother_children))
df_mother_children <- cbind(diff = rownames(df_mother_children), df_mother_children)
colnames(df_mother_children) = c('diff_group', 'freq')
rownames(df_mother_children) <- 1:nrow(df_mother_children)
df_mother_children$prob = df_mother_children$freq/sum(df_mother_children$freq)

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
write.csv(df_mother_children, 'mother_children_age_37201-formatted.csv', row.names=FALSE)