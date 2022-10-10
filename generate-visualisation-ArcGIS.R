library(readr)
library(dplyr)
library("sf")

setwd(paste0(
  this.path::this.dir(),
  "/output/synthetic-population-households"
))

df_synth_pop = read.csv('synthetic_population_DHZW_2019_with_hh.csv')
df_households = read.csv('df_households_DHZW_2019.csv')

list_PC6 <- unique(df_synth_pop$PC6)
list_genders <- unique(df_synth_pop$gender)
list_migration_backgrounds <-
  unique(df_synth_pop$migration_background)

# Initialise columns
df <- data.frame(PC6 = list_PC6)
df[list_genders] <- 0
df[list_migration_backgrounds] <- 0
df['hh_avg_size'] <- 0

# Fill values from synthetic populations
for (pc6 in df$PC6) {
  df[df$PC6 == pc6, ]$male <-
    nrow(df_synth_pop[df_synth_pop$PC6 == pc6 &
                        df_synth_pop$gender == 'male', ])
  df[df$PC6 == pc6, ]$female <-
    nrow(df_synth_pop[df_synth_pop$PC6 == pc6 &
                        df_synth_pop$gender == 'female', ])
  
  df[df$PC6 == pc6, ]$Dutch <-
    nrow(df_synth_pop[df_synth_pop$PC6 == pc6 &
                        df_synth_pop$migration_background == 'Dutch', ])
  df[df$PC6 == pc6, ]$Western <-
    nrow(df_synth_pop[df_synth_pop$PC6 == pc6 &
                        df_synth_pop$migration_background == 'Western', ])
  df[df$PC6 == pc6, ]$Non_Western <-
    nrow(df_synth_pop[df_synth_pop$PC6 == pc6 &
                        df_synth_pop$migration_background == 'Non_Western', ])
  
  df[df$PC6 == pc6, ]$hh_avg_size <-
    mean(df_households[df_households$PC6 == pc6, ]$hh_size)
  
  df[df$PC6 == pc6, ]$n_households <-
    nrow(df_households[df_households$PC6 == pc6, ])
}

################################################################################
# Add PC6 geometry

# Process and filter shapefile of PC6 areas
setwd(this.path::this.dir())
setwd('data/shapefiles/raw')
shp_nl_PC6 <-
  st_read('CBS-PC6-2019-v2')
shp_DHZW_PC6 <- shp_nl_PC6 %>%
  filter(PC6 %in% list_PC6) %>%
  select(PC6, geometry)

setwd(this.path::this.dir())
setwd('data/shapefiles/processed')
st_write(shp_DHZW_PC6, 'DHZW_PC6_shapefiles', driver = "ESRI Shapefile")

shp_DHZW_PC6 <- st_read('DHZW_PC6_shapefiles')

# Merge PC6 geometry
df <- merge(df, shp_DHZW_PC6, by = 'PC6')

################################################################################
# Add neighbourhood code based on PC6

setwd(this.path::this.dir())
setwd('data/raw/2019')
df_PC6_neighborhoods = read.csv('PC6_neighbourhoods.csv', sep = ';')

df_PC6_neighborhoods <- df_PC6_neighborhoods %>%
  rename(neighb_code = Buurt2019) %>%
  select(PC6, neighb_code) %>%
  distinct()

df <- merge(df, df_PC6_neighborhoods, by='PC6')
df$neighb_code <- paste0('BU0', df$neighb_code)

################################################################################
# Add neighbourhood name based on neighbourhood code

setwd(this.path::this.dir())
setwd('data/codes/')
DHZW_neighbourhoods_codes <- read_delim("~/GitHub projects/DHZW_synthetic-population/data/codes/DHZW_neighbourhoods_codes.csv", 
                                        delim = ";", escape_double = FALSE, col_names = FALSE, 
                                        trim_ws = TRUE)
colnames(DHZW_neighbourhoods_codes) <- c('neighb_code', 'neighb_name')

df <- merge(df, DHZW_neighbourhoods_codes, by='neighb_code')

################################################################################
setwd(this.path::this.dir())
setwd('output/ArcGIS')
st_write(df, 'df_summary_ArcGIS', driver = "ESRI Shapefile")
