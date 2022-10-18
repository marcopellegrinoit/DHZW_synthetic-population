library("this.path")
library(dplyr)
library (readr)
library(sf)

setwd(this.path::this.dir())
source('../config/config.R')

# Load PC4 DHZW
setwd('../data/codes')
DHZW_PC4_codes <-
  read.csv("DHZW_PC4_codes.csv",
           sep = ";" ,
           header = F)$V1

# Load BAG The Hague
setwd(this.path::this.dir())
df_BAG <- st_read('../data/raw/BAG_The_Hague')
# Translate columns and filter DHZW area
df <- df_BAG %>%
  select(status,
         gebrdoel,
         woonfuncti,
         kantoor,
         sport,
         winkel,
         onderwijs,
         a_huisnum,
         a_huislett,
         a_postcode
         ) %>%
  rename(use = gebrdoel,
         residential = woonfuncti,
         office = kantoor,
         retail = winkel,
         school = onderwijs,
         hh_number = a_huisnum,
         hh_letter = a_huislett,
         PC6 = a_postcode) %>%
  mutate(PC4 = gsub('.{2}$', '', PC6)) %>%
  filter(PC4 %in% DHZW_PC4_codes) %>%
  filter(status == 'Verblijfsobject in gebruik') # Filter buildings that exist already, not future

# Translate building uses into English
df$use <- recode(df$use,
                 'woonfunctie' = 'residential',
                 'winkelfunctie' = 'retail',
                 'overige gebruiksfunctie' = 'other',
                 'kantoorfunctie' = 'office',
                 'bijeenkomstfunctie' = 'meeting',
                 'industriefunctie' = 'factory',
                 'woonfunctie;kantoorfunctie' = 'residential_office',
                 'sportfunctie' = 'sport',
                 'woonfunctie;winkelfunctie' =  'residential_retail',
                 'gezondheidszorgfunctie' = 'health care',
                 'onderwijsfunctie' = 'school',
                 'bijeenkomstfunctie;onderwijsfunctie' = 'meeting_education',
                 'logiesfunctie' = 'lodging',
                 'industriefunctie;overige gebruiksfunctie' = 'factory_other',
                 'bijeenkomstfunctie;sportfunctie' = 'meeting_sport',
                 'industriefunctie;kantoorfunctie;winkelfunctie' = 'factory_office_retail',
                 'bijeenkomstfunctie;winkelfunctie' = 'meeting_retail',
                 'bijeenkomstfunctie;kantoorfunctie' = 'meeting_office',
                 'gezondheidszorgfunctie;winkelfunctie' = 'healthcare_retail',
                 'bijeenkomstfunctie;industriefunctie;kantoorfunctie;overige gebruiksfunctie' = 'meeting_factory_office_other',
                 'woonfunctie;bijeenkomstfunctie' = 'residential_meeting',
                 'industriefunctie;logiesfunctie' = 'factory_lodging',
                 'bijeenkomstfunctie;gezondheidszorgfunctie;kantoorfunctie' = 'meeting_healtcare_office',
                 'gezondheidszorgfunctie;onderwijsfunctie' = 'healthcare_education'
                 )

# Drop Z coordinate because it cannot be saved by st
df <- st_zm(df)

# Filter residential addresses
df_residential <- df %>%
  filter(residential == 1)
st_write(df_residential, '../data/processed/BAG/buildings_residential', driver = "ESRI Shapefile")

# Filter retail shops
df_retails <- df %>%
  filter(use == 'retail')
st_write(df_retails, '../data/processed/BAG/buildings_retail', driver = "ESRI Shapefile")

# Filter schools
df_schools <- df %>%
  filter(use == 'school')
st_write(df_schools, '../data/processed/BAG/buildings_school', driver = "ESRI Shapefile")

# Filter schools
df_offices <- df %>%
  filter(use == 'office')
st_write(df_offices, '../data/processed/BAG/buildings_office', driver = "ESRI Shapefile")

################################################################################
# Assign neighb_code to each PC6 of BAG

# Load CBS matching between PC6 and neighb_code
setwd(this.path::this.dir())
setwd(paste("../data/processed",
            year,
            municipality,
            sep = '/'))
df_PC6_neighb <- read_delim(
  "PC6_neighbourhoods.csv",
  delim = ",",
  escape_double = FALSE,
  trim_ws = TRUE
)
df_PC6_neighb <- df_PC6_neighb %>%
  select(PC6, neighb_code)

# Remove geometries from the BAG residentials
df_residential <- as.data.frame(df_residential)
df_residential <- df_residential%>%
  select(PC6)

# Match PC6 of BAG with its corresponding CBS neighb_code
df_residential <- merge(df_residential, df_PC6_neighb, by='PC6')

# Calculate proportion of PC6 in neighbourhoods based on number of BAG addresses
df_residential <- df_residential %>%
  group_by(neighb_code, PC6) %>%
  summarise(freq = n()) %>%
  mutate(prop = freq / sum(freq))

setwd(this.path::this.dir())
setwd('../data/processed/BAG')
write.csv(df_residential, 'proportions_PC6_neighbcode_BAG.csv', row.names = FALSE)


df <- df_PC6_neighb %>%
  filter(! (PC6 %in% unique(df_PC6_neighb$PC6)))