library(dplyr)
library("this.path")

setwd(this.path::this.dir())
source('../../config/config.R')
setwd(
  paste(
    "../../data/raw",
    year,
    municipality,
    'individuals_demographics',
    sep = '/'
  )
)
## Load neighborhood dataset: n people per age group and neighbourhood 
pop_df_marginal_dist = read.csv("marginal_distributions_84583NED.csv", sep = ";")
df_marginal_dist = pop_df_marginal_dist[which(pop_df_marginal_dist$SoortRegio_2 == "Buurt     "),] # select neighborhood data only
remove(pop_df_marginal_dist)

# filter and rename only useful attributes
df_marginal_dist = df_marginal_dist %>%
  select(Codering_3,
         AantalInwoners_5,
         Mannen_6,
         Vrouwen_7,
         k_0Tot15Jaar_8,
         k_15Tot25Jaar_9,
         k_25Tot45Jaar_10,
         k_45Tot65Jaar_11,
         k_65JaarOfOuder_12,
         WestersTotaal_17,
         NietWestersTotaal_18,
         HuishoudensTotaal_28,
         Eenpersoonshuishoudens_29,
         HuishoudensZonderKinderen_30,
         HuishoudensMetKinderen_31,
         GemiddeldeHuishoudensgrootte_32,
         OpleidingsniveauLaag_64,
         OpleidingsniveauMiddelbaar_65,
         OpleidingsniveauHoog_66,
         PersonenautoSTotaal_99,
         PersonenautoSPerHuishouden_102
  )%>%
  rename(neighb_code = Codering_3,
         tot_pop = AantalInwoners_5,
         gender_male = Mannen_6,
         gender_female = Vrouwen_7,
         age_0_15 = k_0Tot15Jaar_8,
         age_15_25 = k_15Tot25Jaar_9,
         age_25_45 = k_25Tot45Jaar_10,
         age_45_65 = k_45Tot65Jaar_11,
         age_over65 = k_65JaarOfOuder_12,
         migration_west = WestersTotaal_17,
         migration_non_west = NietWestersTotaal_18,
         hh_total = HuishoudensTotaal_28,
         hh_single = Eenpersoonshuishoudens_29,
         hh_no_children = HuishoudensZonderKinderen_30,
         hh_with_children = HuishoudensMetKinderen_31,
         hh_avg_size = GemiddeldeHuishoudensgrootte_32,
         education_absolved_low = OpleidingsniveauLaag_64,
         education_absolved_middle = OpleidingsniveauMiddelbaar_65,
         education_absolved_high = OpleidingsniveauHoog_66,
         n_cars = PersonenautoSTotaal_99,
         n_cars_per_hh = PersonenautoSPerHuishouden_102
  )
df_marginal_dist[df_marginal_dist$education_absolved_low=='       .',]$education_absolved_low=0
df_marginal_dist[df_marginal_dist$education_absolved_middle=='       .',]$education_absolved_middle=0
df_marginal_dist[df_marginal_dist$education_absolved_high=='       .',]$education_absolved_high=0
df_marginal_dist$education_absolved_low=as.numeric(df_marginal_dist$education_absolved_low)
df_marginal_dist$education_absolved_middle=as.numeric(df_marginal_dist$education_absolved_middle)
df_marginal_dist$education_absolved_high=as.numeric(df_marginal_dist$education_absolved_high)

# Calculate the missing Dutch migration background in the overall marginal distribution
df_marginal_dist$migration_Dutch = df_marginal_dist$tot_pop - (df_marginal_dist$migration_west + df_marginal_dist$migration_non_west)


################################################################################
# Correct marginal distribution where the area is too small and leads to issues
# for each neighbourhood, calculate the proportion of each attribute. Then, for 
# the selected areas, replace with the average of the the other areas.

# filter DHZW area
if (filter_DHZW) {
  setwd(this.path::this.dir())
  setwd("../../data/codes")
  DHZW_neighborhood_codes <-
    read.csv("DHZW_neighbourhoods_codes.csv",
             sep = ";" ,
             header = F)$V1
  df_marginal_dist = df_marginal_dist[df_marginal_dist$neighb_code %in% DHZW_neighborhood_codes, ]
}

neighb_code_to_correct = c('BU05181785')

# genders
df_marginal_dist$prop_male = df_marginal_dist$gender_male / df_marginal_dist$tot_pop
df_marginal_dist[df_marginal_dist$neighb_code %in% neighb_code_to_correct,]$prop_male = mean(df_marginal_dist[!(df_marginal_dist$neighb_code %in% neighb_code_to_correct),]$prop_male)

# group ages
df_marginal_dist$prop_age_0_15 = df_marginal_dist$age_0_15 / df_marginal_dist$tot_pop
df_marginal_dist[df_marginal_dist$neighb_code %in% neighb_code_to_correct,]$prop_age_0_15 = mean(df_marginal_dist[!(df_marginal_dist$neighb_code %in% neighb_code_to_correct),]$prop_age_0_15)

df_marginal_dist$prop_age_15_25 = df_marginal_dist$age_15_25 / df_marginal_dist$tot_pop
df_marginal_dist[df_marginal_dist$neighb_code %in% neighb_code_to_correct,]$prop_age_15_25 = mean(df_marginal_dist[!(df_marginal_dist$neighb_code %in% neighb_code_to_correct),]$prop_age_15_25)

df_marginal_dist$prop_age_25_45 = df_marginal_dist$age_25_45 / df_marginal_dist$tot_pop
df_marginal_dist[df_marginal_dist$neighb_code %in% neighb_code_to_correct,]$prop_age_25_45 = mean(df_marginal_dist[!(df_marginal_dist$neighb_code %in% neighb_code_to_correct),]$prop_age_25_45)

df_marginal_dist$prop_age_45_65 = df_marginal_dist$age_45_65 / df_marginal_dist$tot_pop
df_marginal_dist[df_marginal_dist$neighb_code %in% neighb_code_to_correct,]$prop_age_45_65 = mean(df_marginal_dist[!(df_marginal_dist$neighb_code %in% neighb_code_to_correct),]$prop_age_45_65)

# migration backgrounds
df_marginal_dist$prop_migration_west  = df_marginal_dist$migration_non_west / df_marginal_dist$tot_pop
df_marginal_dist[df_marginal_dist$neighb_code %in% neighb_code_to_correct,]$prop_migration_west = mean(df_marginal_dist[!(df_marginal_dist$neighb_code %in% neighb_code_to_correct),]$prop_migration_west)

df_marginal_dist$prop_migration_non_west = df_marginal_dist$migration_non_west / df_marginal_dist$tot_pop
df_marginal_dist[df_marginal_dist$neighb_code %in% neighb_code_to_correct,]$prop_migration_non_west = mean(df_marginal_dist[!(df_marginal_dist$neighb_code %in% neighb_code_to_correct),]$prop_migration_non_west)

# absolved education
df_marginal_dist$prop_education_absolved_low  = df_marginal_dist$education_absolved_low / df_marginal_dist$tot_pop
df_marginal_dist[df_marginal_dist$neighb_code %in% neighb_code_to_correct,]$prop_education_absolved_low = mean(df_marginal_dist[!(df_marginal_dist$neighb_code %in% neighb_code_to_correct),]$prop_education_absolved_low)

df_marginal_dist$prop_education_absolved_middle = df_marginal_dist$education_absolved_middle / df_marginal_dist$tot_pop
df_marginal_dist[df_marginal_dist$neighb_code %in% neighb_code_to_correct,]$prop_education_absolved_middle = mean(df_marginal_dist[!(df_marginal_dist$neighb_code %in% neighb_code_to_correct),]$prop_education_absolved_middle)

# households
df_marginal_dist$prop_hh_single = df_marginal_dist$hh_single / df_marginal_dist$hh_total
df_marginal_dist[df_marginal_dist$neighb_code %in% neighb_code_to_correct,]$prop_hh_single = mean(df_marginal_dist[!(df_marginal_dist$neighb_code %in% neighb_code_to_correct),]$prop_hh_single)

df_marginal_dist$prop_hh_no_children = df_marginal_dist$hh_no_children / df_marginal_dist$hh_total
df_marginal_dist[df_marginal_dist$neighb_code %in% neighb_code_to_correct,]$prop_hh_no_children = mean(df_marginal_dist[!(df_marginal_dist$neighb_code %in% neighb_code_to_correct),]$prop_hh_no_children)

df_marginal_dist[df_marginal_dist$neighb_code %in% neighb_code_to_correct,]$hh_avg_size = mean(as.numeric(df_marginal_dist[!(df_marginal_dist$neighb_code %in% neighb_code_to_correct),]$hh_avg_size))

#df_marginal_dist[df_marginal_dist$neighb_code %in% neighb_code_to_correct,]$n_cars = round(mean(as.numeric(df_marginal_dist[!(df_marginal_dist$neighb_code %in% neighb_code_to_correct),]$n_cars)))

#df_marginal_dist[df_marginal_dist$neighb_code %in% neighb_code_to_correct,]$n_cars_per_hh = df_marginal_dist[!(df_marginal_dist$neighb_code %in% neighb_code_to_correct),]$n_cars/

for (neighb_code in neighb_code_to_correct) {
  # genders
  df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$gender_male = round(df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$tot_pop * df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$prop_male)
  df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$gender_female = df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$tot_pop - df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$gender_male
  
  # group ages
  df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$age_0_15 = round(df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$tot_pop * df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$prop_age_0_15)
  df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$age_15_25 = round(df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$tot_pop * df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$prop_age_15_25)
  df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$age_25_45 = round(df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$tot_pop * df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$prop_age_25_45)
  df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$age_45_65 = round(df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$tot_pop * df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$prop_age_45_65)
  df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$age_over65 = df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$tot_pop - sum(df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$age_0_15,
                                                                                                                                                           df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$age_15_25,
                                                                                                                                                           df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$age_25_45,
                                                                                                                                                           df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$age_45_65)
  # migration backgrounds
  df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$migration_non_west = round(df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$tot_pop * df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$prop_migration_non_west)
  df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$migration_west = round(df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$tot_pop * df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$prop_migration_west)
  df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$migration_Dutch = df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$tot_pop - sum(df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$migration_non_west,
                                                                                                                                                                df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$migration_west)
  # absolved education
  df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$education_absolved_low = round(df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$tot_pop * df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$prop_education_absolved_low)
  df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$education_absolved_middle = round(df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$tot_pop * df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$prop_education_absolved_middle)
  df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$education_absolved_high = df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$tot_pop - sum(df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$education_absolved_low,
                                                                                                                                                                        df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$education_absolved_middle)
  # households
  df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$hh_single = round(df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$hh_total * df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$prop_hh_single)
  df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$hh_no_children = round(df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$hh_total * df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$prop_hh_no_children)
  df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$hh_with_children = df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$hh_total - sum(df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$hh_single,
                                                                                                                                                                  df_marginal_dist[df_marginal_dist$neighb_code == neighb_code,]$hh_no_children)
}

df_marginal_dist = subset(df_marginal_dist, select=-c(prop_male,
                                                      prop_age_0_15,
                                                      prop_age_15_25,
                                                      prop_age_25_45,
                                                      prop_age_45_65,
                                                      prop_migration_non_west,
                                                      prop_migration_west,
                                                      prop_education_absolved_low,
                                                      prop_education_absolved_middle,
                                                      prop_hh_single,
                                                      prop_hh_no_children))

df_marginal_dist[df_marginal_dist$n_cars_per_hh=='       .', ] <- 0
df_marginal_dist$n_cars_per_hh <- as.numeric(df_marginal_dist$n_cars_per_hh)

setwd(this.path::this.dir())
setwd(
  paste(
    "../../data/processed",
    year,
    municipality,
    'individuals_demographics',
    sep = '/'
  )
)
write.csv(df_marginal_dist, 'marginal_distributions_84583NED-formatted.csv', row.names=FALSE)