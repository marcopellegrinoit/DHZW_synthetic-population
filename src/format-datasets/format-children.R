library(dplyr)
library (readr)
library("this.path")
setwd(this.path::this.dir())

setwd(this.path::this.dir())
setwd("../../data/raw/households")

df_children <- read_delim("children_71486NED.csv", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Select interesting attributes
df_children = df_children %>% 
  rename(unmarried_0 = `Particuliere huishoudens: samenstelling/Meerpersoonshuishouden/Niet-gehuwd paar/0 kinderen (aantal)`,
         unmarried_1 = `Particuliere huishoudens: samenstelling/Meerpersoonshuishouden/Niet-gehuwd paar/1 kind (aantal)`,
         unmarried_2 = `Particuliere huishoudens: samenstelling/Meerpersoonshuishouden/Niet-gehuwd paar/2 kinderen (aantal)`,
         unmarried_3 = `Particuliere huishoudens: samenstelling/Meerpersoonshuishouden/Niet-gehuwd paar/3 of meer kinderen (aantal)`,
         married_0 = `Particuliere huishoudens: samenstelling/Meerpersoonshuishouden/Gehuwd paar/0 kinderen (aantal)`,
         married_1 = `Particuliere huishoudens: samenstelling/Meerpersoonshuishouden/Gehuwd paar/1 kind (aantal)`,
         married_2 = `Particuliere huishoudens: samenstelling/Meerpersoonshuishouden/Gehuwd paar/2 kinderen (aantal)`,
         married_3 = `Particuliere huishoudens: samenstelling/Meerpersoonshuishouden/Gehuwd paar/3 of meer kinderen (aantal)`,
         singleparent_1 = `Particuliere huishoudens: samenstelling/Meerpersoonshuishouden/Eenouderhuishouden/1 kind (aantal)`,
         singleparent_2 = `Particuliere huishoudens: samenstelling/Meerpersoonshuishouden/Eenouderhuishouden/2 kinderen (aantal)`,
         singleparent_3 = `Particuliere huishoudens: samenstelling/Meerpersoonshuishouden/Eenouderhuishouden/3 of meer kinderen (aantal)`
  )
df_children = subset(df_children, select=-c( `Leeftijd referentiepersoon`, Perioden, `Regio's`, unmarried_0, married_0))

df_children_aggregated = data.frame(c(df_children$unmarried_1, df_children$unmarried_2,df_children$unmarried_3),
                                    c(df_children$married_1, df_children$married_2,df_children$married_3),
                                    c(df_children$singleparent_1, df_children$singleparent_2,df_children$singleparent_3))
colnames(df_children_aggregated) = c('unmarried', 'married', 'singleparents')
df_children_aggregated$children_in_house = c(1, 2, 3)
df_children_aggregated$total_children = rowSums(df_children_aggregated)
df_children_aggregated$prob = df_children_aggregated$total_children / sum(df_children_aggregated$total_children)

# save
setwd(this.path::this.dir())
setwd("../../data/processed/households")

write.csv(df_children_aggregated, 'children_71486NED-formatted.csv', row.names=FALSE)