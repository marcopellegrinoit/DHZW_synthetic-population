library(readr)
library("dplyr")
library("this.path")
setwd(this.path::this.dir())
source('../../config/config.R')

municipality_code = 518 # municipality code of The Hague

setwd(paste("../../data/raw",
            year,
            sep = '/'))
# https://www.cbs.nl/nl-nl/maatwerk/2019/42/buurt-wijk-en-gemeente-2019-voor-postcode-huisnummer
df_PC6_neighb <- read_delim(
  "PC6_neighbourhoods.csv",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE
)

df_PC6_neighb <- df_PC6_neighb %>%
  filter(Gemeente2019 == municipality_code) %>%
  select(PC6, Buurt2019, Huisnummer) %>%  # start to filter the entire The Hague
  rename(neighb_code = Buurt2019,
         house_number = Huisnummer)

df_PC6_neighb$neighb_code <-
  paste0('BU0', df_PC6_neighb$neighb_code)

# filter DHZW area
if (filter_DHZW) {
  setwd(this.path::this.dir())
  setwd("../../data/codes")
  
  DHZW_neighborhood_codes <-
    read.csv("DHZW_neighbourhoods_codes.csv",
             sep = ";" ,
             header = F)$V1
  DHZW_PC4_codes <-
    read.csv("DHZW_PC4_codes.csv", sep = ";" , header = F)$V1
  
  df_PC6_neighb$PC4 = gsub('.{2}$', '', df_PC6_neighb$PC6)
  
  df_PC6_neighb <- df_PC6_neighb %>%
    filter(neighb_code %in% DHZW_neighborhood_codes) %>%
    filter(PC4 %in% DHZW_PC4_codes)
  df_PC6_neighb <- subset(df_PC6_neighb, select = -c(PC4))
}

setwd(this.path::this.dir())
setwd(paste("../../data/processed",
            year,
            municipality,
            sep = '/'))
write.csv(df_PC6_neighb, 'PC6_neighbourhoods.csv', row.names = FALSE)