library("this.path")
setwd(this.path::this.dir())
library(dplyr)
library (readr)

municipality = "den_haag_2019"

setwd(paste0('data/', municipality, '/households/distributions'))
df_children_aggregated <- read_csv("children_71486NED-formatted.csv")

## Implementation of Jan's Methods 2
n_children = 1000

# calculate probs
p_child_in_household = df_children_aggregated
p_child_in_household = subset(p_child_in_household, select=-c(unmarried, married, singleparents, total_children))
p_child_in_household$prob = p_child_in_household$prob * p_child_in_household$children_in_house

# normalise
p_child_in_household$prob = p_child_in_household$prob / sum(p_child_in_household$prob)

# Next, we can just use those probabilities as fractions of the number of children in each household
children_in_households = p_child_in_household
children_in_households$num_children = children_in_households$prob * n_children
children_in_households = subset(children_in_households, select=-c(prob))

# Make sure that each bin for a household size contains a number of children that is neatly divisible over the actual size of that household
for (i in (nrow(children_in_households):2)) {
  remainder = children_in_households[i,]$num_children %% children_in_households[i,]$children_in_house
  if (remainder > 0) {
    children_in_households[i, 'num_children'] = children_in_households[i, 'num_children'] - remainder
    children_in_households[i-1, 'num_children'] = children_in_households[i-1, 'num_children'] - remainder
  }
}
# I do the the last step manually
children_in_households[1,]$num_children = n_children - sum(children_in_households[(2:nrow(children_in_households)),]$num_children)

# Now we calculate the actual number of households again by dividing the number of children by the size
n_households = children_in_households
n_households$num_households = n_households$num_children / n_households$children_in_house
n_households = subset(n_households, select=-c(num_children))

# Print results
for(i in 1:nrow(df_children_aggregated)) {
  print(paste0('Household composed on ', children_in_households[i,]$children_in_house, ' children'))
  print(paste0('Children: ', children_in_households[i,]$num_children))
  print(paste0(n_households[i,]$num_households, ' households of this size'))
  print(paste0('Which is ', n_households[i,]$num_households/sum(n_households$num_households), ' of total'))
  print('----------------')
}