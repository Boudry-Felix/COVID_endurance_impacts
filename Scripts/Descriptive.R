# Informations ------------------------------------------------------------

# Title: Descriptive.R
# Author: Félix Boudry
# Contact: <felix.boudry@univ-perp.fr>
# License: Private
# Description: This script makes a descriptive analysis of the data.

# Configuration -----------------------------------------------------------

## Libraries --------------------------------------------------------------
require(janitor)
require(ggplot2)
require(dplyr)

# Environment -------------------------------------------------------------
# Load previously computed environment
rm(list = ls())
load(file = "Env/import.RData")

my_results <- lst()

antrop_measures <- c("age", "sex", "height", "weight")

# Plot antropometrics -----------------------------------------------------
for (my_dataframe in encoded_data) {
  for (my_column in my_dataframe[, antrop_measures]) {
    if (is.numeric(my_column)) {
      hist(my_column)
    } else {
      qplot(my_column)
    }
  }
}

# Compute antropometrics --------------------------------------------------
# Compute antropometric data for the populations
my_counter <- 1
for (my_dataframe in encoded_data) {
  assign(paste0(names(encoded_data[my_counter])),
         summarise(
           my_data$completed,
           across(
             .cols = all_of(antrop_measures),
             .fns = list(
               mean = mean,
               max = max,
               min = min,
               median = median
             )
           ),
           na.rm = TRUE
         ))
}
