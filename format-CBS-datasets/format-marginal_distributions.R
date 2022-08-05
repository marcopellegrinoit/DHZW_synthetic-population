library(dplyr)

municipality = "den_haag_2019"

## Load neighborhood dataset: n people per age group and neighbourhood 
setwd(this.path::this.dir())
setwd(paste("../data/", municipality, sep = ""))
pop_df_MarginalDistr = read.csv("marginal_distributions_84583NED.csv", sep = ";")
df_MarginalDistr = pop_df_MarginalDistr[which(pop_df_MarginalDistr$SoortRegio_2 == "Buurt     "),] # select neighborhood data only
remove(pop_df_MarginalDistr)

# filter and rename only useful attributes
df_MarginalDistr = df_MarginalDistr %>%
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
         OpleidingsniveauHoog_66
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
         education_absolved_high = OpleidingsniveauHoog_66
  )
df_MarginalDistr[df_MarginalDistr$education_absolved_low=='       .',]$education_absolved_low=0
df_MarginalDistr[df_MarginalDistr$education_absolved_middle=='       .',]$education_absolved_middle=0
df_MarginalDistr[df_MarginalDistr$education_absolved_high=='       .',]$education_absolved_high=0
df_MarginalDistr$education_absolved_low=as.numeric(df_MarginalDistr$education_absolved_low)
df_MarginalDistr$education_absolved_middle=as.numeric(df_MarginalDistr$education_absolved_middle)
df_MarginalDistr$education_absolved_high=as.numeric(df_MarginalDistr$education_absolved_high)

# Calculate the missing Dutch migration background in the overall marginal distribution
df_MarginalDistr$migration_Dutch = df_MarginalDistr$tot_pop - (df_MarginalDistr$migration_west + df_MarginalDistr$migration_non_west)

write.csv(df_MarginalDistr, 'marginal_distributions_84583NED-formatted.csv', row.names=FALSE)