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

str(ODiN2018$disp_start_PC4)

################################################################################
# Filter agents that live in DHZW

# Filter The Hague home address in OViN
OViN2014 <- filter_hh_municipality(OViN2014, municipality_code)
OViN2015 <- filter_hh_municipality(OViN2015, municipality_code)
OViN2016 <- filter_hh_municipality(OViN2016, municipality_code)
OViN2017 <- filter_hh_municipality(OViN2017, municipality_code)

# In ODiN, convert the home address from municipality to PC4 level
OViN2014 <- home_municipality_to_PC4(OViN2014)
OViN2015 <- home_municipality_to_PC4(OViN2015)
OViN2016 <- home_municipality_to_PC4(OViN2016)
OViN2017 <- home_municipality_to_PC4(OViN2017)

# Filter DHZW in ODiN and OViN
OViN2014 <- filter_hh_PC4(OViN2014, DHZW_PC4_codes)
OViN2015 <- filter_hh_PC4(OViN2015, DHZW_PC4_codes)
OViN2016 <- filter_hh_PC4(OViN2016, DHZW_PC4_codes)
OViN2017 <- filter_hh_PC4(OViN2017, DHZW_PC4_codes)
ODiN2018 <- filter_hh_PC4(ODiN2018, DHZW_PC4_codes)
ODiN2019 <- filter_hh_PC4(ODiN2019, DHZW_PC4_codes)

OViN <- bind_rows(OViN2014,
                  OViN2015,
                  OViN2016,
                  OViN2017)
ODiN <- bind_rows(ODiN2018,
                  ODiN2019)

# Format modal choices, because they differ between ODiN and OViN
OViN <- format_modal_choice_OViN(OViN)
OViN <- format_ride_role_OViN(OViN)

ODiN <- format_modal_choice_ODiN(ODiN)
ODiN <- format_ride_role_ODiN(ODiN)


# Some statistics
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

# Merge datasets into one
df <- bind_rows(OViN, ODiN)

# Save dataset
setwd(paste0(this.path::this.dir(), "/data/Formatted"))
write.csv(df, 'df_DHZW.csv', row.names=FALSE)

################################################################################
# Formatting of attributes and removal of incomplete

# Load DHZW dataset
setwd(paste0(this.path::this.dir(), "/data/Formatted"))
df <- read_csv("df_DHZW.csv")

print(length(unique(df$agent_ID))) # 834

# Remove displacements that are completely outside of DHZW
df <- df[df$disp_start_PC4 %in% DHZW_PC4_codes | df$disp_arrival_PC4 %in% DHZW_PC4_codes,]

print(length(unique(df$agent_ID))) # 733

# Remove agents that have incomplete information about displacements:
agents_incomplete <- df %>%
  group_by(agent_ID) %>%
  filter(any(
    !is.na(disp_id) & (
      is.na(disp_start_PC4) |
        is.na(disp_arrival_PC4) |
        is.na(disp_modal_choice) |
        is.na(disp_start_hour) |
        is.na(disp_start_min) |
        is.na(disp_arrival_hour) |
        is.na(disp_arrival_min)
    )
  ))
df <- filter(df, !(agent_ID %in% unique(agents_incomplete$agent_ID)))

print(length(unique(df$agent_ID))) # 728

# Format values in attributes
df <- format_values(df)

# Save dataset
setwd(paste0(this.path::this.dir(), "/data/Formatted"))
write.csv(df, 'df_DHZW.csv', row.names=FALSE)