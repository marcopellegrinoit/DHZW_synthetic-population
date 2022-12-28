library(readr)
library(dplyr)
library("sf")
library("this.path")
setwd(this.path::this.dir())
source('../config/config.R')

# Load synthetic files
setwd(this.path::this.dir())
setwd("../output/synthetic-population-households")
df_synth_pop = read.csv('synthetic_population_DHZW_2019_with_hh.csv')
df_households = read.csv('df_households_DHZW_2019.csv')

# Load PC6 summary
setwd(this.path::this.dir())
setwd('../output/ArcGIS')
df <- read.csv('PC6_summary_ArcGIS_frequencies.csv')
df <- subset(df, select=-c(PC6))

df <- df %>%
  group_by(neighb_code, neighb_name) %>%
  summarise(n_individuals = sum(n_individuals),
            
            age_0_15 = sum(age_0_15),
            age_15_25 = sum(age_15_25),
            age_25_45 = sum(age_25_45),
            age_45_65 = sum(age_45_65),
            age_over65 = sum(age_over65),
            
            male = sum(male),
            female = sum(female),
            
            Dutch = sum(Dutch),
            Western = sum(Western),
            Non_Western = sum(Non_Western),
            
            education_low = sum(education_low),
            education_middle = sum(education_middle),
            education_high = sum(education_high),
            
            children = sum(children),
            
            n_households = sum(n_households),
            couples_with_children = sum(couples_with_children),
            couples_without_children = sum(couples_without_children),
            singleparents = sum(singleparents),
            singles = sum(singles)
            )

# calculate again the average household size
df$household_avg_size <- 0
for (neighb_code in df$neighb_code) {
  df[df$neighb_code == neighb_code,]$household_avg_size <- round(mean(df_households[df_households$neighb_code == neighb_code,]$hh_size), 2)
}

################################################################################
# Add the neighbourhood geometry to the summary

setwd(this.path::this.dir())
setwd('../data/shapefiles/processed')
shp_DHZW_neighb <- st_read('DHZW_neighbs_shapefiles')
shp_DHZW_neighb <- shp_DHZW_neighb %>%
  rename(neighb_code = BU_CODE) %>%
  select(neighb_code, geometry)

# Merge PC6 geometry
df <- merge(df, shp_DHZW_neighb, by = 'neighb_code')

setwd(this.path::this.dir())
setwd('../output/ArcGIS')
st_write(df, 'Neighb_summary_ArcGIS', driver = "ESRI Shapefile")

################################################################################
# Add the neighbourhood geometry to the marginal distribution

setwd(this.path::this.dir())
# Load marginal distribution
setwd(
  paste(
    "../data/processed",
    year,
    municipality,
    'individuals_demographics',
    sep = '/'
  )
)
df_marginal_dist = read.csv("marginal_distributions_84583NED-formatted.csv", sep = ",")

# Add neighbourhood name based on neighbourhood code
setwd(this.path::this.dir())
setwd('../data/codes/')
DHZW_neighbourhoods_codes <- read_delim("DHZW_neighbourhoods_codes.csv", 
                                        delim = ";", escape_double = FALSE, col_names = FALSE, 
                                        trim_ws = TRUE)
colnames(DHZW_neighbourhoods_codes) <- c('neighb_code', 'neighb_name')

df_marginal_dist <- merge(df_marginal_dist, DHZW_neighbourhoods_codes, by='neighb_code')

df_marginal_dist <- df_marginal_dist %>%
  rename(education_low = education_absolved_low,
         education_middle = education_absolved_middle,
         education_high = education_absolved_high)

df_marginal_dist <- merge(df_marginal_dist, shp_DHZW_neighb, by = 'neighb_code')

setwd(this.path::this.dir())
setwd('../output/ArcGIS')
st_write(df_marginal_dist, 'Neighb_marginals_ArcGIS', driver = "ESRI Shapefile")
