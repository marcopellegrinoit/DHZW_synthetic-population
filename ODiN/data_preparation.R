library(haven)
library("dplyr")
library(tibble)
library(tidyr)
library(readr)
library("this.path")
setwd(this.path::this.dir())
source('utils.R')

# load DHZW area
setwd(paste0(this.path::this.dir(), "/data"))
DHZW_neighborhood_codes <- read.csv("DHZW_neighbourhoods_codes.csv", sep = ";" ,header=F)$V1
DHZW_PC4_codes <- read.csv("DHZW_PC4_codes.csv", sep = ";" ,header=F)$V1
municipality_code = 518

# Load ODiNs and OViNs
setwd(paste0(this.path::this.dir(), "/data/CBS"))

OViN2014 <- read_sav("OViN2014.sav")
OViN2015 <- read_sav("OViN2015.sav")
OViN2016 <- read_sav("OViN2016.sav")
OViN2017 <- read_sav("OViN2017.sav")
ODiN2018 <- read_sav("ODiN2018.sav")
ODiN2019 <- read_sav("ODiN2019.sav")

# Filter attributes
OViN2014 <- select_attributes_OViN_2014(OViN2014)
OViN2015 <- select_attributes_OViN(OViN2015)
OViN2016 <- select_attributes_OViN(OViN2016)
OViN2017 <- select_attributes_OViN(OViN2017)
ODiN2018 <- select_attributes_ODiN(ODiN2018)
ODiN2019 <- select_attributes_ODiN(ODiN2019)

# Filter DHZW
OViN2014 <- filter_hh_municipality(OViN2014, municipality_code)
OViN2015 <- filter_hh_municipality(OViN2015, municipality_code)
OViN2016 <- filter_hh_municipality(OViN2016, municipality_code)
OViN2017 <- filter_hh_municipality(OViN2017, municipality_code)

OViN2014 <- home_municipality_to_PC4(OViN2014)
OViN2015 <- home_municipality_to_PC4(OViN2015)
OViN2016 <- home_municipality_to_PC4(OViN2016)
OViN2017 <- home_municipality_to_PC4(OViN2017)

OViN2014 <- filter_hh_PC4(OViN2014, DHZW_PC4_codes)
OViN2015 <- filter_hh_PC4(OViN2015, DHZW_PC4_codes)
OViN2016 <- filter_hh_PC4(OViN2016, DHZW_PC4_codes)
OViN2017 <- filter_hh_PC4(OViN2017, DHZW_PC4_codes)
ODiN2018 <- filter_hh_PC4(ODiN2018, DHZW_PC4_codes)
ODiN2019 <- filter_hh_PC4(ODiN2019, DHZW_PC4_codes)

setwd(paste0(this.path::this.dir(), "/data/Formatted"))
write.csv(OViN2014, 'OViN2014_DHZW.csv', row.names=FALSE)
write.csv(OViN2015, 'OViN2015_DHZW.csv', row.names=FALSE)
write.csv(OViN2016, 'OViN2016_DHZW.csv', row.names=FALSE)
write.csv(OViN2017, 'OViN2017_DHZW.csv', row.names=FALSE)
write.csv(ODiN2018, 'ODiN2018_DHZW.csv', row.names=FALSE)
write.csv(ODiN2019, 'ODiN2019_DHZW.csv', row.names=FALSE)

################################################################################
# Load DHZW datasets

OViN2014 <- read_csv("OViN2014_DHZW.csv")
OViN2015 <- read_csv("OViN2015_DHZW.csv")
OViN2016 <- read_csv("OViN2016_DHZW.csv")
OViN2017 <- read_csv("OViN2017_DHZW.csv")
ODiN2018 <- read_csv("ODiN2018_DHZW.csv")
ODiN2019 <- read_csv("ODiN2019_DHZW.csv")

print(paste0('N agents OViN 2014: ', length(unique(OViN2014$agent_ID))))
print(paste0('N agents OViN 2015: ', length(unique(OViN2015$agent_ID))))
print(paste0('N agents OViN 2016: ', length(unique(OViN2016$agent_ID))))
print(paste0('N agents OViN 2017: ', length(unique(OViN2017$agent_ID))))
print(paste0('N agents ODiN 2018: ', length(unique(ODiN2018$agent_ID))))
print(paste0('N agents ODiN 2019: ', length(unique(ODiN2019$agent_ID))))

print(paste0('N agents in total: ', length(unique(OViN2014$agent_ID))+
               length(unique(OViN2015$agent_ID))+
               length(unique(OViN2016$agent_ID))+
               length(unique(OViN2017$agent_ID))+
               length(unique(ODiN2018$agent_ID))+
               length(unique(ODiN2019$agent_ID))))