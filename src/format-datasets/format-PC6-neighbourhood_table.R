library(readr)
library("dplyr")
library("this.path")

# PC6 and neighbourhoods do not exactly overlap. This script takes the PC6 with most households per neighbourhood

municipality_code = 518 # municipality code of The Hague

setwd(this.path::this.dir())
setwd("../../data/raw")
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
setwd(this.path::this.dir())
setwd("../../../DHZW_shapefiles/data/codes")

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

# Remove duplicate and pick the neighbourhood per PC6 with most households
df_PC6_neighb <- df_PC6_neighb %>%
  group_by(neighb_code, PC6) %>%
  mutate(freq = n()) %>%
  select(neighb_code, PC6, freq) %>%
  distinct() %>%
  group_by(PC6) %>%
  slice_max(order_by = freq, n = 1)

# remove frequencies
df_PC6_neighb <- df_PC6_neighb %>%
  group_by(neighb_code, PC6)

setwd(this.path::this.dir())
setwd("../../data/processed")
write.csv(df_PC6_neighb, 'PC6_neighbourhoods.csv', row.names = FALSE)