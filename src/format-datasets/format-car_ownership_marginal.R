library(readr)
library(dplyr)
library(this.path)
library(sf)
library(stringr)

setwd(this.dir())
setwd('../../data/raw')

df <- st_read('car_ownership_The_Hague')

df <- df %>%
  select(OBJECTID,
         BRT_KOPPEL,
         BB_NR,
         AUTOBEZ17) %>%
  rename('neighbourhood_name' = 'BRT_KOPPEL',
         'n_cars' = 'AUTOBEZ17')

# list of neighbourhood names in the car dataset
neighbourhoods_names <- c('85 Kerketuinen/Zichtenburg',
                          '84 Leyenburg',
                          '87 Venen/Oorden/Raden',
                          '96 Zijden/Steden/Zichten',
                          '98 Dreven en Gaarden',
                          '99 De Uithof',
                          '80 Morgenstond-Zuid',
                          '88 Morgenstond-West',
                          '89 Morgenstond-Oost',
                          '36 Zuiderpark',
                          '20 Moerwijk-Oost',
                          '37 Moerwijk-West',
                          '38 Moerwijk-Noord',
                          '39 Moerwijk-Zuid')

# filter DHZW area
df_DHZW <- df[df$neighbourhood_name %in% neighbourhoods_names,]

# rename name labels to match the CBS code
df_DHZW$neighbourhood_name <- recode(
  df_DHZW$neighbourhood_name,
  '85 Kerketuinen/Zichtenburg' = 'Kerketuinen en Zichtenburg',
  '84 Leyenburg' = 'Leyenburg',
  '87 Venen/Oorden/Raden' = 'Venen, Oorden en Raden',
  '96 Zijden/Steden/Zichten' = 'Zijden, Steden en Zichten',
  '98 Dreven en Gaarden' = 'Dreven en Gaarden',
  '99 De Uithof' = 'De Uithof',
  '80 Morgenstond-Zuid' = 'Morgenstond-Zuid',
  '88 Morgenstond-West' = 'Morgenstond-West',
  '89 Morgenstond-Oost' = 'Morgenstond-Oost',
  '36 Zuiderpark' = 'Zuiderpark',
  '20 Moerwijk-Oost' = 'Moerwijk-Oost',
  '37 Moerwijk-West' = 'Moerwijk-West',
  '38 Moerwijk-Noord' = 'Moerwijk-Noord',
  '39 Moerwijk-Zuid' = 'Moerwijk-Zuid'
)


# Load neighbourhood codes
setwd(this.dir())
setwd('../../data/codes')

df_DHZW_neighbourhoods <- read.csv('DHZW_neighbourhoods_codes.csv', sep = ';', header = F)
colnames(df_DHZW_neighbourhoods) <- c('neighbourhood_code', 'neighbourhood_name')

df_DHZW <- merge(df_DHZW, df_DHZW_neighbourhoods)

########################################################

df_DHZW <- aggregate(df_DHZW$n_cars, by=list(Category=df_DHZW$neighbourhood_code), FUN=sum)
colnames(df_DHZW) <- c('neighbourhood_code', 'n_cars')

setwd(this.dir())
setwd('../../data/processed')

write.csv(df_DHZW, 'car_ownership_marginals_neighbourhoods_2017-formatted.csv', row.names = FALSE)
