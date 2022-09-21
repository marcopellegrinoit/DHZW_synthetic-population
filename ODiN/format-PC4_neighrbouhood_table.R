library(readr)
library("dplyr")
library("this.path")
setwd(this.path::this.dir())

municipality_code = 518 # municipality code of The Hague

setwd(paste0(this.path::this.dir(), "/data"))
DHZW_neighborhood_codes <- read.csv("DHZW_neighbourhoods_codes.csv", sep = ";" ,header=F)$V1
DHZW_PC4_codes <- read.csv("DHZW_PC4_codes.csv", sep = ";" ,header=F)$V1

# https://www.cbs.nl/nl-nl/maatwerk/2019/42/buurt-wijk-en-gemeente-2019-voor-postcode-huisnummer
df_PC6_neighb <- read_delim("CBS PC neighbourhoods/PC6_neighbourhoods_2019.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

df_PC6_neighb <- df_PC6_neighb %>%
  filter(Gemeente2019==municipality_code) %>%
  select(PC6, Buurt2019) %>%  # start to filter the entire The Hague
  rename(neighb_code = Buurt2019)

df_PC6_neighb$neighb_code <- paste0('BU0', df_PC6_neighb$neighb_code)

setwd(paste0(this.path::this.dir(), "/data/CBS PC neighbourhoods"))
write.csv(df_PC6_neighb, 'PC6_neighb.csv', row.names = FALSE)

###############################################################################

setwd(paste0(this.path::this.dir(), "/data/CBS PC neighbourhoods"))
df_PC6_neighb <- read_csv("PC6_neighb.csv")

df_PC6_neighb$PC4 = gsub('.{2}$', '', df_PC6_neighb$PC6)
df_PC6_neighb <- subset(df_PC6_neighb, select=-c(PC6))

# Filter DHZW area and calculate proportions PC4 - neighbourhoods. Since the rows are individual houses, the proportion is the amount oh houses of each PC4 in each neighbourhood code.
df_PC6_neighb <- df_PC6_neighb %>%
  filter(neighb_code %in% DHZW_neighborhood_codes) %>%
  filter(PC4 %in% DHZW_PC4_codes) %>%
  group_by(neighb_code, PC4) %>%
  summarise(freq = n()) %>%
  mutate(prop = freq / sum(freq))

setwd(paste0(this.path::this.dir(), "/data/CBS PC neighbourhoods"))
write.csv(df_PC6_neighb, 'PC4_neighb.csv', row.names = FALSE)