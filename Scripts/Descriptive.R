# Informations ------------------------------------------------------------
# Title: Descriptive.R
# Author: Félix Boudry
# Contact: <felix.boudry@univ-perp.fr>
# License: Private
# Description: This script makes a descriptive analysis of the data.

# Configuration -----------------------------------------------------------

## Libraries --------------------------------------------------------------
require(tidyverse)
require(psych)

# Environment -------------------------------------------------------------
# Load previously computed environment
rm(list = setdiff(
  x = ls(),
  y = c(
    lsf.str(),
    "gen_env",
    "my_data",
    "data_transformed",
    "my_results"
  )
))

descriptive_results <- mapply(
  FUN = function(my_columns, my_colnames) {
    answer_profile <-
      # Check number of possible answer to determine plots
      my_columns %>% unique() %>% na.omit() %>% length()
    if (answer_profile < 2) {
      # Only count answer if no groups
      my_count <-
        my_var_counting(input = my_data, my_vars = my_colnames) %>%
        getElement(name = my_colnames)
      return(lst(my_count))
    } else if (answer_profile == 2) {
      # Optimized plots for double bar plots
      my_count <-
        my_var_counting(input = my_data, my_vars = my_colnames) %>%
        getElement(name = my_colnames)
      my_plot <-
        bar_plot_multi(input = my_data, my_columns = my_colnames) %>%
        getElement(name = my_colnames)
      return(lst(my_count, my_plot))
    } else if (answer_profile > 2) {
      # Optimizes plots for multiple bar plots
      my_count <-
        my_var_counting(input = my_data, my_vars = my_colnames) %>%
        getElement(name = my_colnames)
      my_plot <-
        bar_plot_multi(input = my_data, my_columns = my_colnames) %>%
        getElement(name = my_colnames)
      return(lst(my_count, my_plot))
    } else {
      return(NA)
    }
  },
  my_columns = my_data,
  my_colnames = names(my_data)
)

descriptive_plots <- # Put all plots in a new list
  lapply(descriptive_results[!is.na(descriptive_results)], "[[", "my_plot") %>%
  discard(is.null)
descriptive_values <- # Put all counts in a new list
  lapply(descriptive_results[!is.na(descriptive_results)], "[[", "my_count") %>%
  discard(is.null)

my_results <-
  append(x = my_results,
         values = lst(num = descriptive_values, plots = descriptive_plots))

rm(list = setdiff(
  x = ls(),
  y = c(
    lsf.str(),
    "gen_env",
    "my_data",
    "data_transformed",
    "my_results"
  )
))

# Export data -------------------------------------------------------------
# Save environment data
save.image(file = "Env/descriptive.RData")
