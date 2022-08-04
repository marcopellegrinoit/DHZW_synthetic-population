library(dplyr)
library("this.path")

municipality = 'den_haag_2019'

setwd(this.path::this.dir())
setwd(paste("../data/", municipality, "/households/distributions", sep = ""))

# Load and reformat household size distributions
df_HouseholdSize = read.csv("household_size_71486NED.csv", sep = ";", fileEncoding="UTF-8-BOM")
df_HouseholdSize = df_HouseholdSize %>%
  rename('1' = Eenpersoonshuishouden_22,
         '2' = k_2Personen_24,
         '3' = k_3Personen_25,
         '4' = k_4Personen_26,
         '5_or_more' = k_5OfMeerPersonen_27
  )%>%
  select('1', '2', '3', '4', '5_or_more')
df_HouseholdSize = as.data.frame(t(df_HouseholdSize))
colnames(df_HouseholdSize) = c('freq')
df_HouseholdSize <- cbind(size = rownames(df_HouseholdSize), df_HouseholdSize)
rownames(df_HouseholdSize) <- 1:nrow(df_HouseholdSize)

write.csv(df_HouseholdSize, 'household_size_71486NED-formatted.csv', row.names = FALSE)