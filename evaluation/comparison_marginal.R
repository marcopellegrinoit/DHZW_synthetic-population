library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(forcats)

library("this.path")
setwd(this.path::this.dir())
source('../src/utils-evaluation-synthetic-population.R')

# Load datasets
setwd(this.path::this.dir())
setwd("../data/processed/individuals")
df_marginal_dist = read.csv("marginal_distributions_84583NED-formatted.csv", sep = ",")

# Load synthetic population
setwd(this.path::this.dir())
setwd(paste("../output/synthetic-population-households", sep = ""))
df_synth_pop = read.csv("synthetic_population_DHZW_2019.csv", sep = ",")

# Calculate proportions
df_marginal_dist$cbs_male <- df_marginal_dist$gender_male / (df_marginal_dist$gender_male + df_marginal_dist$gender_female)
df_marginal_dist$cbs_female <- df_marginal_dist$gender_female / (df_marginal_dist$gender_male + df_marginal_dist$gender_female)

df_marginal_dist$cbs_age_0_15 <- df_marginal_dist$age_0_15 / (df_marginal_dist$age_0_15 + df_marginal_dist$age_15_25 + df_marginal_dist$age_25_45 + df_marginal_dist$age_45_65 + df_marginal_dist$age_over65)
df_marginal_dist$cbs_age_15_25 <- df_marginal_dist$age_15_25 / (df_marginal_dist$age_0_15 + df_marginal_dist$age_15_25 + df_marginal_dist$age_25_45 + df_marginal_dist$age_45_65 + df_marginal_dist$age_over65)
df_marginal_dist$cbs_age_25_45 <- df_marginal_dist$age_25_45 / (df_marginal_dist$age_0_15 + df_marginal_dist$age_15_25 + df_marginal_dist$age_25_45 + df_marginal_dist$age_45_65 + df_marginal_dist$age_over65)
df_marginal_dist$cbs_age_45_65 <- df_marginal_dist$age_45_65 / (df_marginal_dist$age_0_15 + df_marginal_dist$age_15_25 + df_marginal_dist$age_25_45 + df_marginal_dist$age_45_65 + df_marginal_dist$age_over65)
df_marginal_dist$cbs_age_over65 <- df_marginal_dist$age_over65 / (df_marginal_dist$age_0_15 + df_marginal_dist$age_15_25 + df_marginal_dist$age_25_45 + df_marginal_dist$age_45_65 + df_marginal_dist$age_over65)

df_marginal_dist$cbs_Dutch <- df_marginal_dist$migration_Dutch / (df_marginal_dist$migration_Dutch + df_marginal_dist$migration_west + df_marginal_dist$migration_non_west)
df_marginal_dist$cbs_Western <- df_marginal_dist$migration_west / (df_marginal_dist$migration_Dutch + df_marginal_dist$migration_west + df_marginal_dist$migration_non_west)
df_marginal_dist$cbs_NonWestern <- df_marginal_dist$migration_non_west / (df_marginal_dist$migration_Dutch + df_marginal_dist$migration_west + df_marginal_dist$migration_non_west)

df_marginal_dist$cbs_hh_single <- df_marginal_dist$hh_single / (df_marginal_dist$hh_single + df_marginal_dist$hh_with_children + df_marginal_dist$hh_no_children)
df_marginal_dist$cbs_hh_with_children <- df_marginal_dist$hh_with_children / (df_marginal_dist$hh_single + df_marginal_dist$hh_with_children + df_marginal_dist$hh_no_children)
df_marginal_dist$cbs_hh_no_children <- df_marginal_dist$hh_no_children / (df_marginal_dist$hh_single + df_marginal_dist$hh_with_children + df_marginal_dist$hh_no_children)

df_marginal_dist$cbs_edu_attainment_low <- df_marginal_dist$education_absolved_low / (df_marginal_dist$education_absolved_low + df_marginal_dist$education_absolved_middle + df_marginal_dist$education_absolved_high)
df_marginal_dist$cbs_edu_attainment_middle <- df_marginal_dist$education_absolved_middle / (df_marginal_dist$education_absolved_low + df_marginal_dist$education_absolved_middle + df_marginal_dist$education_absolved_high)
df_marginal_dist$cbs_edu_attainment_high <- df_marginal_dist$education_absolved_high / (df_marginal_dist$education_absolved_low + df_marginal_dist$education_absolved_middle + df_marginal_dist$education_absolved_high)

df_marginal_dist <- df_marginal_dist %>%
  dplyr::rename(cbs_hh_avg_size = hh_avg_size) %>%
  select(neighb_code,
         cbs_male, cbs_female,
         cbs_age_0_15, cbs_age_15_25, cbs_age_25_45, cbs_age_45_65, cbs_age_over65,
         cbs_Dutch, cbs_Western, cbs_NonWestern,
         cbs_hh_single, cbs_hh_with_children, cbs_hh_no_children,
         cbs_edu_attainment_low, cbs_edu_attainment_middle, cbs_edu_attainment_high,
         cbs_hh_avg_size)

################################################################################
# Synthetic population

# Gender
df_gen_gender <- as.data.frame(table(df_synth_pop$neighb_code, df_synth_pop$gender))
colnames(df_gen_gender) <- c('neighb_code', 'gender', 'freq')

df_gen_gender <- df_gen_gender %>%
  pivot_wider(names_from = 'gender', values_from = 'freq')

df_gen_gender$gen_male <- df_gen_gender$male / (df_gen_gender$male + df_gen_gender$female)
df_gen_gender$gen_female <- df_gen_gender$female / (df_gen_gender$male + df_gen_gender$female)

# Age groups
df_synth_pop$age_group = "age_over65" # default for non-numeric
df_synth_pop$age_group[df_synth_pop$age %in% 0:14] = "age_0_15"
df_synth_pop$age_group[df_synth_pop$age %in% 15:24] = "age_15_25"
df_synth_pop$age_group[df_synth_pop$age %in% 25:44] = "age_25_45"
df_synth_pop$age_group[df_synth_pop$age %in% 45:64] = "age_45_65"
df_synth_pop$age_group[df_synth_pop$age %in% 65:105] = "age_over65"

df_gen_agegroup <- as.data.frame(table(df_synth_pop$neighb_code, df_synth_pop$age_group))
colnames(df_gen_agegroup) <- c('neighb_code', 'age_group', 'freq')

df_gen_agegroup <- df_gen_agegroup %>%
  pivot_wider(names_from = 'age_group', values_from = 'freq')

df_gen_agegroup$gen_age_0_15 <- df_gen_agegroup$age_0_15 / (df_gen_agegroup$age_0_15 + df_gen_agegroup$age_15_25 + df_gen_agegroup$age_25_45 + df_gen_agegroup$age_45_65 + df_gen_agegroup$age_over65)
df_gen_agegroup$gen_age_15_25 <- df_gen_agegroup$age_15_25 / (df_gen_agegroup$age_0_15 + df_gen_agegroup$age_15_25 + df_gen_agegroup$age_25_45 + df_gen_agegroup$age_45_65 + df_gen_agegroup$age_over65)
df_gen_agegroup$gen_age_25_45 <- df_gen_agegroup$age_25_45 / (df_gen_agegroup$age_0_15 + df_gen_agegroup$age_15_25 + df_gen_agegroup$age_25_45 + df_gen_agegroup$age_45_65 + df_gen_agegroup$age_over65)
df_gen_agegroup$gen_age_45_65 <- df_gen_agegroup$age_45_65 / (df_gen_agegroup$age_0_15 + df_gen_agegroup$age_15_25 + df_gen_agegroup$age_25_45 + df_gen_agegroup$age_45_65 + df_gen_agegroup$age_over65)
df_gen_agegroup$gen_age_over65 <- df_gen_agegroup$age_over65 / (df_gen_agegroup$age_0_15 + df_gen_agegroup$age_15_25 + df_gen_agegroup$age_25_45 + df_gen_agegroup$age_45_65 + df_gen_agegroup$age_over65)

# Migration background
df_gen_migration <- as.data.frame(table(df_synth_pop$neighb_code, df_synth_pop$migration_background))
colnames(df_gen_migration) <- c('neighb_code', 'migration_background', 'freq')

df_gen_migration <- df_gen_migration %>%
  pivot_wider(names_from = 'migration_background', values_from = 'freq')

df_gen_migration$gen_Dutch <- df_gen_migration$Dutch / (df_gen_migration$Dutch + df_gen_migration$Western + df_gen_migration$Non_Western)
df_gen_migration$gen_Western <- df_gen_migration$Western / (df_gen_migration$Dutch + df_gen_migration$Western + df_gen_migration$Non_Western)
df_gen_migration$gen_Non_Western <- df_gen_migration$Non_Western / (df_gen_migration$Dutch + df_gen_migration$Western + df_gen_migration$Non_Western)

# Education attainment
df_gen_edu_attainment <- as.data.frame(table(df_synth_pop$neighb_code, df_synth_pop$edu_attainment))
colnames(df_gen_edu_attainment) <- c('neighb_code', 'edu_attainment', 'freq')

df_gen_edu_attainment <- df_gen_edu_attainment %>%
  pivot_wider(names_from = 'edu_attainment', values_from = 'freq')

df_gen_edu_attainment$gen_edu_attainment_low <- df_gen_edu_attainment$low / (df_gen_edu_attainment$low + df_gen_edu_attainment$middle + df_gen_edu_attainment$high)
df_gen_edu_attainment$gen_edu_attainment_middle <- df_gen_edu_attainment$middle / (df_gen_edu_attainment$low + df_gen_edu_attainment$middle + df_gen_edu_attainment$high)
df_gen_edu_attainment$gen_edu_attainment_high <- df_gen_edu_attainment$high / (df_gen_edu_attainment$low + df_gen_edu_attainment$middle + df_gen_edu_attainment$high)

################################################################################
# Synthetic households

df_households <- df_synth_pop %>%
  select(neighb_code, hh_ID, hh_size, hh_type) %>%
  distinct()

df_households[df_households$hh_type == 'couple_children_straight',]$hh_type <- 'hh_children'
df_households[df_households$hh_type == 'couple_children_gay',]$hh_type <- 'hh_children'
df_households[df_households$hh_type == 'couple_children_lesbian',]$hh_type <- 'hh_children'
df_households[df_households$hh_type == 'single_parent',]$hh_type <- 'hh_children'

df_households[df_households$hh_type == 'couple_no_children_straight',]$hh_type <- 'hh_no_children'
df_households[df_households$hh_type == 'couple_no_children_gay',]$hh_type <- 'hh_no_children'
df_households[df_households$hh_type == 'couple_no_children_lesbian',]$hh_type <- 'hh_no_children'

df_gen_hh_type <- as.data.frame(table(df_households$neighb_code, df_households$hh_type))
colnames(df_gen_hh_type) <- c('neighb_code', 'hh_type', 'freq')

df_gen_hh_type <- df_gen_hh_type %>%
  pivot_wider(names_from = 'hh_type', values_from = 'freq')

df_gen_hh_type$gen_hh_children <- df_gen_hh_type$hh_children / (df_gen_hh_type$hh_children + df_gen_hh_type$hh_no_children + df_gen_hh_type$single)
df_gen_hh_type$gen_hh_no_children <- df_gen_hh_type$hh_no_children / (df_gen_hh_type$hh_children + df_gen_hh_type$hh_no_children + df_gen_hh_type$single)
df_gen_hh_type$gen_single <- df_gen_hh_type$single / (df_gen_hh_type$hh_children + df_gen_hh_type$hh_no_children + df_gen_hh_type$single)

df_gen_avg_hh_size <- df_households %>%
  group_by(neighb_code) %>%
  dplyr::summarise(avg_hh_size=mean(hh_size))

################################################################################
# Comparison
df <- merge(df_marginal_dist, df_gen_gender)
df <- df %>%
  select(neighb_code)

df$diff_male <- abs(df_gen_gender$gen_male - df_marginal_dist$cbs_male)*100
df$diff_female <- abs(df_gen_gender$gen_female - df_marginal_dist$cbs_female)*100

df$diff_age_0_15 <- abs(df_gen_agegroup$gen_age_0_15 - df_marginal_dist$cbs_age_0_15)*100
df$diff_age_15_25 <- abs(df_gen_agegroup$gen_age_15_25 - df_marginal_dist$cbs_age_15_25)*100
df$diff_age_25_45 <- abs(df_gen_agegroup$gen_age_25_45 - df_marginal_dist$cbs_age_25_45)*100
df$diff_age_45_65 <- abs(df_gen_agegroup$gen_age_45_65 - df_marginal_dist$cbs_age_45_65)*100
df$diff_age_over65 <- abs(df_gen_agegroup$gen_age_over65 - df_marginal_dist$cbs_age_over65)*100

df$diff_Dutch <- abs(df_gen_migration$gen_Dutch - df_marginal_dist$cbs_Dutch)*100
df$diff_Western <- abs(df_gen_migration$gen_Western - df_marginal_dist$cbs_Western)*100
df$diff_Non_Western <- abs(df_gen_migration$gen_Non_Western - df_marginal_dist$cbs_NonWestern)*100

df$diff_edu_attainment_low <- abs(df_gen_edu_attainment$gen_edu_attainment_low - df_marginal_dist$cbs_edu_attainment_low)*100
df$diff_edu_attainment_middle <- abs(df_gen_edu_attainment$gen_edu_attainment_middle - df_marginal_dist$cbs_edu_attainment_middle)*100
df$diff_edu_attainment_high <- abs(df_gen_edu_attainment$gen_edu_attainment_high - df_marginal_dist$cbs_edu_attainment_high)*100

df$diff_hh_with_children <- abs(df_gen_hh_type$gen_hh_children - df_marginal_dist$cbs_hh_with_children)*100
df$diff_hh_no_children <- abs(df_gen_hh_type$gen_hh_no_children - df_marginal_dist$cbs_hh_no_children)*100
df$diff_hh_single <- abs(df_gen_hh_type$gen_single - df_marginal_dist$cbs_hh_single)*100

df$diff_hh_avg_size <- abs(df_gen_avg_hh_size$avg_hh_size - df_marginal_dist$cbs_hh_avg_size)

setwd(this.dir())
setwd('data_comparison')
write.csv(df, 'marginal_differences.csv', row.names = FALSE)

################################################################################
# Analysis
df <- df %>%
  select(-c(neighb_code))

median <- df %>%
  dplyr::summarise(across(everything(), stats::median))

mean <- df %>%
  dplyr::summarise(across(everything(), base::mean))

standard_deviation <- df %>%
  dplyr::summarise(across(everything(), stats::sd))

percentiles <- df %>%
  dplyr::summarise(across(everything(), stats::quantile))

df_analysis <- rbind(median, mean, standard_deviation, percentiles)

df_analysis <- data.frame(t(df_analysis))
df_analysis <- cbind(variable = rownames(df_analysis), df_analysis)
rownames(df_analysis) <- 1:nrow(df_analysis)
colnames(df_analysis) <- c('attribute', 'median', 'mean', 'sd', c('perc_0', 'perc_25', 'perc_50', 'perc_75', 'perc_100'))

df_analysis$attribute <- substring(df_analysis$attribute, 6)

df_analysis$variable <- NA

df_analysis[df_analysis$attribute == 'male',]$variable <- 'gender'
df_analysis[df_analysis$attribute == 'female',]$variable <- 'gender'

df_analysis[df_analysis$attribute == 'age_0_15',]$variable <- 'age group'
df_analysis[df_analysis$attribute == 'age_15_25',]$variable <- 'age group'
df_analysis[df_analysis$attribute == 'age_25_45',]$variable <- 'age group'
df_analysis[df_analysis$attribute == 'age_45_65',]$variable <- 'age group'
df_analysis[df_analysis$attribute == 'age_over65',]$variable <- 'age group'

df_analysis[df_analysis$attribute == 'Dutch',]$variable <- 'migration background'
df_analysis[df_analysis$attribute == 'Western',]$variable <- 'migration background'
df_analysis[df_analysis$attribute == 'Non_Western',]$variable <- 'migration background'

df_analysis[df_analysis$attribute == 'edu_attainment_low',]$variable <- 'education attainment'
df_analysis[df_analysis$attribute == 'edu_attainment_middle',]$variable <- 'education attainment'
df_analysis[df_analysis$attribute == 'edu_attainment_high',]$variable <- 'education attainment'

df_analysis[df_analysis$attribute == 'hh_no_children',]$variable <- 'household type'
df_analysis[df_analysis$attribute == 'hh_with_children',]$variable <- 'household type'
df_analysis[df_analysis$attribute == 'hh_single',]$variable <- 'household type'

df_analysis <- df_analysis[df_analysis$attribute != 'hh_avg_size',]

df_scatter <- df %>%
  select(-c(diff_hh_avg_size))

df_scatter <- df_scatter %>%
  pivot_longer(cols=colnames(df_scatter), names_to = 'attribute', values_to = 'perc_diff')
df_scatter$attribute <- substring(df_scatter$attribute, 6)
df_scatter <- merge(df_scatter, df_analysis)

df_scatter$attribute <- recode(
  df_scatter$attribute,
  'age_0_15' = '0 - 14',
  'age_15_25' = '15 - 24',
  'age_25_45' = '25 - 44',
  'age_45_65' = '45 - 64',
  'age_over65' = '>= 65',
  'Non_Western' = 'Non-Western',
  'edu_attainment_low' = 'low',
  'edu_attainment_middle' = 'middle',
  'edu_attainment_high' = 'high',
  'hh_no_children' =	'w/o children',
  'hh_with_children' =	'w/ children',
  'hh_single' =	'single'
)

df_scatter <- df_scatter[df_scatter$variable != 'age group',]
df_scatter <- df_scatter[df_scatter$variable != 'gender',]

# Order age groups
age_order <- c("0 - 14", "15 - 24", "25 - 44", "45 - 64", ">= 65")
df_scatter$attribute <- fct_relevel(df_scatter$attribute, age_order)

# Order current education
edu_order <- c("low", "middle", "high")
df_scatter$attribute <- fct_relevel(df_scatter$attribute, edu_order)

plot <- df_scatter %>%
  ggplot(aes(x = attribute)) +
  geom_point(aes(y = perc_diff, color = 'Point'), size = 2) +
  geom_errorbar(aes(ymin = perc_25, ymax = perc_75), width = .2, color = 'black', size = 1.2) +
  geom_point(aes(y = median, color = 'Median'), size = 3, shape = 4, stroke = 2) +
  facet_grid(~variable, switch = "x", scales = "free_x", space = "free_x") +
  ylab("percentage difference (%)") +
  scale_color_manual(name = 'Legend', 
                     values = c('Point' = '#00BA38', 'Median' = '#F8766D', 'Error bar' = 'black'),
                     labels = c('neighbourhood\npercentage\ndifference', 'median', '25% and 75%\npercentiles')) +
  theme(panel.spacing = unit(0.5, "lines"), 
        strip.placement = "outside",
          axis.text.x = element_text(
            angle = 45,
            hjust = 1,
          )
        )+
        theme(panel.background = element_blank(),
        panel.grid = element_line(colour = alpha('grey', 0.2)),
        panel.border = element_rect(colour = "black", fill = NA),
        text = element_text(size = 15),
        plot.margin = unit(c(0, 0, 0.5, 0), "lines"),  # increase bottom margin
        axis.title.x = element_text(margin = margin(t = 10))) +
  theme(legend.position = c(0.85, 0.65))+
  labs(x = NULL)
plot

setwd(this.dir())
setwd('plots')
png("plot_marginal_percentage_diff_paper.png", width = 1500, height = 1000, units='px', res = 250)
plot
dev.off()
