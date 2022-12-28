library(readr)
library(dplyr)
library("sf")
library("this.path")
setwd(this.path::this.dir())
source('../config/config.R')

# Load synthetic files
setwd("../output/synthetic-population-households")
df_synth_pop = read.csv('synthetic_population_DHZW_2019_with_hh.csv')
df_households = read.csv('df_households_DHZW_2019.csv')

list_PC6 <- unique(df_synth_pop$PC6)
list_genders <- unique(df_synth_pop$gender)
list_migration_backgrounds <-
  unique(df_synth_pop$migration_background)
list_group_ages <-
  c('age_0_15',
    'age_15_25',
    'age_25_45',
    'age_45_65',
    'age_over65')

# Initialise columns
df <- data.frame(PC6 = list_PC6)
df$n_individuals <- 0
df[list_group_ages] <- 0
df[list_genders] <- 0
df[list_migration_backgrounds] <- 0
df$education_low <- 0
df$education_middle <- 0
df$education_high <- 0
df$n_households <- 0
df$household_avg_size <- 0
df$children <- 0
df$couples_with_children <- 0
df$couples_without_children <- 0
df$singleparents <- 0
df$singles <- 0

i = 1
# Fill values from synthetic populations
for (pc6 in df$PC6) {
  #print(paste0('Calculating PC6: ', pc6, '. Work done: ', round((i/length(unique(df$PC6)))*100), 2), '%')
  i = i + 1
  
  # Individuals per PC6
  df[df$PC6 == pc6, ]$n_individuals <-
    nrow(df_synth_pop[df_synth_pop$PC6 == pc6, ])
  
  # Age
  df[df$PC6 == pc6, ]$age_0_15 <-
    nrow(df_synth_pop[df_synth_pop$PC6 == pc6 &
                        df_synth_pop$age %in% 0:14, ])
  
  df[df$PC6 == pc6, ]$age_15_25 <-
    nrow(df_synth_pop[df_synth_pop$PC6 == pc6 &
                        df_synth_pop$age %in% 15:24, ])
  
  df[df$PC6 == pc6, ]$age_25_45 <-
    nrow(df_synth_pop[df_synth_pop$PC6 == pc6 &
                        df_synth_pop$age %in% 25:44, ])
  
  df[df$PC6 == pc6, ]$age_45_65 <-
    nrow(df_synth_pop[df_synth_pop$PC6 == pc6 &
                        df_synth_pop$age %in% 45:64, ])
  
  df[df$PC6 == pc6, ]$age_over65 <-
    nrow(df_synth_pop[df_synth_pop$PC6 == pc6 &
                        df_synth_pop$age >= 65, ])
  # Gender
  df[df$PC6 == pc6, ]$male <-
    nrow(df_synth_pop[df_synth_pop$PC6 == pc6 &
                        df_synth_pop$gender == 'male', ])
  df[df$PC6 == pc6, ]$female <-
    nrow(df_synth_pop[df_synth_pop$PC6 == pc6 &
                        df_synth_pop$gender == 'female', ])
  # Migration background
  df[df$PC6 == pc6, ]$Dutch <-
    nrow(df_synth_pop[df_synth_pop$PC6 == pc6 &
                        df_synth_pop$migration_background == 'Dutch', ])
  df[df$PC6 == pc6, ]$Western <-
    nrow(df_synth_pop[df_synth_pop$PC6 == pc6 &
                        df_synth_pop$migration_background == 'Western', ])
  df[df$PC6 == pc6, ]$Non_Western <-
    nrow(df_synth_pop[df_synth_pop$PC6 == pc6 &
                        df_synth_pop$migration_background == 'Non_Western', ])
  
  # Education attainment
  df[df$PC6 == pc6,]$education_low <-
    nrow(df_synth_pop[df_synth_pop$PC6 == pc6 &
                        df_synth_pop$edu_attainment == 'low',])
  
  df[df$PC6 == pc6,]$education_middle <-
    nrow(df_synth_pop[df_synth_pop$PC6 == pc6 &
                        df_synth_pop$edu_attainment == 'middle',])
  
  df[df$PC6 == pc6,]$education_high <-
    nrow(df_synth_pop[df_synth_pop$PC6 == pc6 &
                        df_synth_pop$edu_attainment == 'high',])
  
  # Children
  df[df$PC6 == pc6,]$children <-
    nrow(df_synth_pop[df_synth_pop$PC6 == pc6 &
                        df_synth_pop$is_child == 1,])
  
  # Households -----------------------------------------------------------------
  
  # N households
  df[df$PC6 == pc6,]$n_households <-
    nrow(df_households[df_households$PC6 == pc6,])
  
  # Average household size
  df[df$PC6 == pc6,]$household_avg_size <-
    round(mean(df_households[df_households$PC6 == pc6,]$hh_size), 2)
  
  # couples with children
  df_couples_with_children <-
    df_synth_pop[df_synth_pop$PC6 == pc6, ]
  df_couples_with_children <- df_couples_with_children %>%
    select(hh_ID, hh_type) %>%
    distinct() %>%
    filter(
      hh_type == 'couple_children_straight' |
        hh_type == 'couple_children_gay' |
        hh_type == 'couple_children_lesbian'
    )
  df[df$PC6 == pc6,]$couples_with_children <-
    nrow(df_couples_with_children)
  
  # couples without children
  df_couples_without_children <-
    df_synth_pop[df_synth_pop$PC6 == pc6, ]
  df_couples_without_children <- df_couples_without_children %>%
    select(hh_ID, hh_type) %>%
    distinct() %>%
    filter(
      hh_type == 'couple_no_children_straight' |
        hh_type == 'couple_no_children_gay' |
        hh_type == 'couple_no_children_lesbian'
    )
  df[df$PC6 == pc6,]$couples_without_children <-
    nrow(df_couples_without_children)
  
  # singleparents
  df_singleparents <- df_synth_pop[df_synth_pop$PC6 == pc6, ]
  df_singleparents <- df_singleparents %>%
    select(hh_ID, hh_type) %>%
    distinct() %>%
    filter(hh_type == 'single_parent')
  df[df$PC6 == pc6,]$singleparents <- nrow(df_singleparents)
  
  # singles
  df[df$PC6 == pc6,]$singles <-
    nrow(df_synth_pop[df_synth_pop$PC6 == pc6 &
                        df_synth_pop$hh_type == 'single', ])
}
df$no_children <- df$n_individuals - df$children


################################################################################
# Add neighbourhood code based on PC6

setwd(this.path::this.dir())
setwd(paste("../data/processed",
            year,
            municipality,
            sep = '/'))
df_PC6_neighborhoods = read.csv('PC6_neighbourhoods.csv', sep = ',')

df_PC6_neighborhoods <- df_PC6_neighborhoods %>%
  select(PC6, neighb_code)

df <- merge(df, df_PC6_neighborhoods, by = 'PC6')

################################################################################
# Add neighbourhood name based on neighbourhood code

setwd(this.path::this.dir())
setwd('../data/codes/')
DHZW_neighbourhoods_codes <-
  read_delim(
    "DHZW_neighbourhoods_codes.csv",
    delim = ";",
    escape_double = FALSE,
    col_names = FALSE,
    trim_ws = TRUE
  )
colnames(DHZW_neighbourhoods_codes) <-
  c('neighb_code', 'neighb_name')

df <- merge(df, DHZW_neighbourhoods_codes, by = 'neighb_code')

################################################################################
# Add PC6 geometry

# Process and filter shapefile of PC6 areas
#setwd(this.path::this.dir())
#setwd('../data/shapefiles/raw')
#shp_nl_PC6 <-
#  st_read('CBS-PC6-2019-v2')
#shp_DHZW_PC6 <- shp_nl_PC6 %>%
#  filter(PC6 %in% list_PC6) %>%
#  select(PC6, geometry)

setwd(this.path::this.dir())
setwd('../data/shapefiles/processed')
#st_write(shp_DHZW_PC6, 'DHZW_PC6_shapefiles', driver = "ESRI Shapefile")

shp_DHZW_PC6 <- st_read('DHZW_PC6_shapefiles')

# Merge PC6 geometry
df <- merge(df, shp_DHZW_PC6, by = 'PC6')

################################################################################
# Write SHP
setwd(this.path::this.dir())
setwd('../output/ArcGIS')
st_write(df, 'PC6_summary_ArcGIS_frequencies', driver = "ESRI Shapefile")

# Write CSV
df <- as.data.frame(df)
df <- subset(df, select = -c(geometry))
write.csv(df, 'PC6_summary_ArcGIS_frequencies.csv', row.names = FALSE)