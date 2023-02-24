library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(this.path)
library(forcats)

setwd(this.dir())
setwd('data_comparison')
df_gender <- read.csv('gender.csv')

setwd(this.dir())
setwd('data_comparison')
df_migration <- read.csv('migration_background.csv')

setwd(this.dir())
setwd('data_comparison')
df_current_edu <- read.csv('current_education.csv')

setwd(this.dir())
setwd('data_comparison')
df_child <- read.csv('ischild.csv')

setwd(this.dir())
setwd('data_comparison')
df_car_license <- read.csv('car_license.csv')

setwd(this.dir())
setwd('data_comparison')
df_income <- read.csv('income_group.csv')

setwd(this.dir())
setwd('data_comparison')
df_car_ownership <- read.csv('car_ownership.csv')

setwd(this.dir())
setwd('data_comparison')
df_hh_type <- read.csv('hh_type.csv')

################################################################################
# Gender

df_gender <- df_gender[df_gender$age < 103,]

# Calculate proportions over the conditional variables
df_gender <- df_gender %>%
  group_by(age, dataset) %>%
  mutate(proportion = proportion / sum(proportion)) %>%
  ungroup

df_gender <- df_gender %>%
  pivot_wider(names_from = 'dataset', values_from = 'proportion')

df_gender$difference <- abs(df_gender$`synthetic population` - df_gender$`stratified dataset`)*100

df_gender <- df_gender %>%
  select(gender, age, difference)

df_gender <- df_gender %>%
  pivot_wider(names_from = 'gender', values_from = 'difference')

df_gender <- df_gender %>%
  select(male, female)

################################################################################
# Migration background

# Calculate proportions over the conditional variables
df_migration <- df_migration %>%
  group_by(gender, age_group, dataset) %>%
  mutate(proportion = proportion / sum(proportion)) %>%
  ungroup

df_migration <- df_migration %>%
  pivot_wider(names_from = 'dataset', values_from = 'proportion')

df_migration$difference <- abs(df_migration$`synthetic population` - df_migration$`stratified dataset`)*100

df_migration <- df_migration %>%
  select(gender, age_group, migration_background, difference)

df_migration <- df_migration %>%
  pivot_wider(names_from = 'migration_background', values_from = 'difference')

df_migration <- df_migration %>%
  select(Dutch, Western, `Non-Western`)

################################################################################
# Current education

# Calculate proportions over the conditional variables
df_current_edu <- df_current_edu %>%
  group_by(gender, age_group, migration_background, dataset) %>%
  mutate(proportion = proportion / sum(proportion)) %>%
  ungroup

df_current_edu <- df_current_edu %>%
  pivot_wider(names_from = 'dataset', values_from = 'proportion')

df_current_edu$difference <- abs(df_current_edu$`synthetic population` - df_current_edu$`stratified dataset`)*100

df_current_edu <- df_current_edu %>%
  select(gender, age_group, migration_background, current_education, difference)

df_current_edu <- df_current_edu %>%
  pivot_wider(names_from = 'current_education', values_from = 'difference')

df_current_edu <- df_current_edu %>%
  select(low, middle, high, `no current education`)

################################################################################
# Child living at home with parents

# Calculate proportions over the conditional variables
df_child <- df_child %>%
  group_by(gender, age_group, dataset) %>%
  mutate(proportion = proportion / sum(proportion)) %>%
  ungroup

df_child <- df_child %>%
  pivot_wider(names_from = 'dataset', values_from = 'proportion')

df_child$difference <- abs(df_child$`synthetic population` - df_child$`stratified dataset`)*100

df_child <- df_child %>%
  select(gender, age_group, is_child, difference)

df_child <- df_child %>%
  pivot_wider(names_from = 'is_child', values_from = 'difference')

df_child <- df_child %>%
  select(child, non_child)

################################################################################
# car license
df_car_license <- df_car_license %>%
  pivot_wider(names_from = 'dataset', values_from = 'proportion')

df_car_license <- df_car_license %>%
  group_by(age_group) %>%
  mutate(`synthetic population` = `synthetic population` / sum(`synthetic population`)) %>%
  ungroup

df_car_license$difference <- abs(df_car_license$`synthetic population` - df_car_license$`stratified dataset`)

df_car_license <- df_car_license %>%
  select(age_group, car, difference)

df_car_license <- df_car_license %>%
  pivot_wider(names_from = 'car', values_from = 'difference')

df_car_license <- df_car_license %>%
  select(with_car_license, no_car_license)

################################################################################
# income groups
df_income <- df_income %>%
  pivot_wider(names_from = 'dataset', values_from = 'proportion')

df_income <- df_income %>%
  group_by(hh_type) %>%
  mutate(`synthetic population` = `synthetic population` / sum(`synthetic population`)) %>%
  ungroup

df_income$difference <- abs(df_income$`synthetic population` - df_income$`stratified dataset`)

df_income <- df_income %>%
  select(hh_type, income_group, difference)

df_income <- df_income %>%
  pivot_wider(names_from = 'income_group', values_from = 'difference')

df_income <- df_income %>%
  select(-c(hh_type))

################################################################################
# car ownership
df_car_ownership <- df_car_ownership %>%
  pivot_wider(names_from = 'dataset', values_from = 'proportion')

df_car_ownership <- df_car_ownership %>%
  group_by(hh_type) %>%
  mutate(`synthetic population` = `synthetic population` / sum(`synthetic population`)) %>%
  ungroup

df_car_ownership$difference <- abs(df_car_ownership$`synthetic population` - df_car_ownership$`stratified dataset`)

df_car_ownership <- df_car_ownership %>%
  select(hh_type, car_ownership, difference)

df_car_ownership <- df_car_ownership %>%
  pivot_wider(names_from = 'car_ownership', values_from = 'difference')

df_car_ownership <- df_car_ownership %>%
  select(-c(hh_type))

################################################################################
# Household type

# Calculate proportions over the conditional variables
df_hh_type <- df_hh_type %>%
  group_by(gender, age_group, dataset) %>%
  mutate(proportion = proportion / sum(proportion)) %>%
  ungroup
df_hh_type$proportion[is.nan(df_hh_type$proportion)]<-0

df_hh_type <- df_hh_type %>%
  pivot_wider(names_from = 'dataset', values_from = 'proportion')

df_hh_type$difference <- abs(df_hh_type$`synthetic population` - df_hh_type$`stratified dataset`)*100

df_hh_type <- df_hh_type %>%
  select(gender, age_group, hh_type, difference)

df_hh_type <- df_hh_type %>%
  pivot_wider(names_from = 'hh_type', values_from = 'difference')

df_hh_type <- df_hh_type %>%
  select(single, couple, single_parent)

################################################################################
# Analysis
do_analysis <- function(df) {
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
  
  df_analysis$variable <- ''
  
  df_analysis$variable <- ifelse(df_analysis$attribute == "low", 'current education', df_analysis$variable)
  df_analysis$variable <- ifelse(df_analysis$attribute == "middle", 'current education', df_analysis$variable)
  df_analysis$variable <- ifelse(df_analysis$attribute == "high", 'current education', df_analysis$variable)
  df_analysis$variable <- ifelse(df_analysis$attribute == "no current education", 'current education', df_analysis$variable)
  
  df_analysis$variable <- ifelse(df_analysis$attribute == "male", 'gender', df_analysis$variable)
  df_analysis$variable <- ifelse(df_analysis$attribute == "female", 'gender', df_analysis$variable)
  
  df_analysis$variable <- ifelse(df_analysis$attribute == "Dutch", 'migration background', df_analysis$variable)
  df_analysis$variable <- ifelse(df_analysis$attribute == "Western", 'migration background', df_analysis$variable)
  df_analysis$variable <- ifelse(df_analysis$attribute == "Non-Western", 'migration background', df_analysis$variable)

  df_analysis$variable <- ifelse(df_analysis$attribute == "child", 'child living with parents', df_analysis$variable)
  df_analysis$variable <- ifelse(df_analysis$attribute == "non_child", 'child living with parents', df_analysis$variable)

  df_analysis$variable <- ifelse(df_analysis$attribute == "with_car_license", 'car license', df_analysis$variable)
  df_analysis$variable <- ifelse(df_analysis$attribute == "no_car_license", 'car license', df_analysis$variable)

  df_analysis$variable <- ifelse(df_analysis$attribute == "with_car", 'household car ownership', df_analysis$variable)
  df_analysis$variable <- ifelse(df_analysis$attribute == "no_car", 'household car ownership', df_analysis$variable)
  
  df_analysis$variable <- ifelse(df_analysis$attribute == "income_1_10", 'household income group', df_analysis$variable)
  df_analysis$variable <- ifelse(df_analysis$attribute == "income_2_10", 'household income group', df_analysis$variable)
  df_analysis$variable <- ifelse(df_analysis$attribute == "income_3_10", 'household income group', df_analysis$variable)
  df_analysis$variable <- ifelse(df_analysis$attribute == "income_4_10", 'household income group', df_analysis$variable)
  df_analysis$variable <- ifelse(df_analysis$attribute == "income_5_10", 'household income group', df_analysis$variable)
  df_analysis$variable <- ifelse(df_analysis$attribute == "income_6_10", 'household income group', df_analysis$variable)
  df_analysis$variable <- ifelse(df_analysis$attribute == "income_7_10", 'household income group', df_analysis$variable)
  df_analysis$variable <- ifelse(df_analysis$attribute == "income_8_10", 'household income group', df_analysis$variable)
  df_analysis$variable <- ifelse(df_analysis$attribute == "income_9_10", 'household income group', df_analysis$variable)
  df_analysis$variable <- ifelse(df_analysis$attribute == "income_10_10", 'household income group', df_analysis$variable)
  
  df_analysis$variable <- ifelse(df_analysis$attribute == "couple", 'positition in household', df_analysis$variable)
  df_analysis$variable <- ifelse(df_analysis$attribute == "single", 'positition in household', df_analysis$variable)
  df_analysis$variable <- ifelse(df_analysis$attribute == "single_parent", 'positition in household', df_analysis$variable)
  
  df_scatter <- df %>%
    pivot_longer(cols=colnames(df), names_to = 'attribute', values_to = 'perc_diff')
  df_scatter <- merge(df_scatter, df_analysis)
  
  return (df_scatter)
}

df_plot_gender <- do_analysis(df_gender)

df_plot_migration <- do_analysis(df_migration)

df_plot_ischild <- do_analysis(df_child)
df_plot_ischild$attribute <- recode(
  df_plot_ischild$attribute,
  'child' = 'is child',
  'non_child' = 'is not child'
)

df_plot_income <- do_analysis(df_income)
df_plot_income$attribute <- recode(
  df_plot_income$attribute,
  'income_1_10' = '0% - 10%',
  'income_2_10' = '10% - 20%',
  'income_3_10' = '20% - 30%',
  'income_4_10' = '30% - 40%',
  'income_5_10' = '40% - 50%',
  'income_6_10' = '50% - 60%',
  'income_7_10' = '60% - 70%',
  'income_8_10' = '70% - 80%',
  'income_9_10' = '80% - 90%',
  'income_10_10' = '90% - 100%'
)

df_plot_car_ownership <- do_analysis(df_car_ownership)
df_plot_car_ownership$attribute <- recode(
  df_plot_car_ownership$attribute,
  'no_car' = 'has not car',
  'with_car' = 'has car'
)

df_plot_car_license <- do_analysis(df_car_license)
df_plot_car_license$attribute <- recode(
  df_plot_car_license$attribute,
  'no_car_license' = 'has not car license',
  'with_car_license' = 'has car license'
)


df_plot_hh_type <- do_analysis(df_hh_type)
df_plot_hh_type$attribute <- recode(
  df_plot_hh_type$attribute,
  'single_parent' = 'single-parent'
)


df_plot_current_edu <- do_analysis(df_current_edu)
edu_order <- c("low", "middle", "high", "no current education")
df_plot_current_edu$attribute <- fct_relevel(df_plot_current_edu$attribute, edu_order)

plot_save_results <- function(df) {
  plot <- df %>%
    ggplot(aes(x = attribute)) +
    geom_point(aes(y = perc_diff, color = 'Point'), size = 2) +
    geom_errorbar(aes(ymin = perc_25, ymax = perc_75), width = .2, color = 'black', size = 1.2) +
    geom_point(aes(y = median, color = 'Median'), size = 3, shape = 4, stroke = 2) +
    facet_grid(~variable, switch = "x", scales = "free_x", space = "free_x") +
    labs(x = NULL, y = NULL) +
    scale_color_manual(name = 'Legend', 
                       values = c('Point' = '#00BA38', 'Median' = '#F8766D', 'Error bar' = 'black'),
                       labels = c('percentage\ndifference', 'median', '25% and 75%\npercentiles')) +
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
          text = element_text(size = 14),
          plot.margin = unit(c(0, 0, 0.5, 0), "lines"),  # increase bottom margin
          axis.title.x = element_text(margin = margin(t = 10))) +
    theme(legend.position = 'right')
  return(plot)
  
}

setwd(this.dir())
setwd('plots/stratified')

plot_gender <- plot_save_results(df_plot_gender)
png(paste0("gender.png"), width = 1000, height = 1000, units='px', res = 250)
plot_gender
dev.off()

plot_migration <- plot_save_results(df_plot_migration)
png(paste0("migration.png"), width = 1000, height = 1000, units='px', res = 250)
plot_migration
dev.off()

plot_ischild <- plot_save_results(df_plot_ischild)
png(paste0("ischild.png"), width = 1000, height = 1000, units='px', res = 250)
plot_ischild
dev.off()

plot_income <- plot_save_results(df_plot_income)
png(paste0("income_group.png"), width = 1000, height = 1000, units='px', res = 250)
plot_income
dev.off()

plot_car_ownership <- plot_save_results(df_plot_car_ownership)
png(paste0("car_ownership.png"), width = 1000, height = 1000, units='px', res = 250)
plot_car_ownership
dev.off()

plot_car_license <- plot_save_results(df_plot_car_license)
png(paste0("license_car_ownership.png"), width = 1000, height = 1000, units='px', res = 250)
plot_car_license
dev.off()

plot_hh_type <- plot_save_results(df_plot_hh_type)
png(paste0("household_position.png"), width = 1000, height = 1000, units='px', res = 250)
plot_hh_type
dev.off()

plot_current_edu <- plot_save_results(df_plot_current_edu)
png(paste0("current_education.png"), width = 1000, height = 1000, units='px', res = 250)
plot_current_edu
dev.off()

