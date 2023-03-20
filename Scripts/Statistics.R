# Informations ------------------------------------------------------------
# Title: Statistics.R
# Author: Félix Boudry
# Contact: <felix.boudry@univ-perp.fr>
# License: Private
# Description: Basic statistics

# Configuration -----------------------------------------------------------

## Libraries --------------------------------------------------------------
require(tidyverse)
require(effectsize)
require(pwr)
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

# Statistics --------------------------------------------------------------
stat_results <- mapply(
  FUN = function(my_columns, my_colnames) {
    answer_profile <-
      # Check number of possible answer to determine test
      my_columns %>% unique() %>% na.omit() %>% length()
    if (answer_profile < 2) {
      return(NA)
    } else if (answer_profile == 2) {
      # Compute chi2 statistics
      prop_stats(input = my_data,
                 var1 = my_colnames,
                 var2 = gen_env$my_var)
    } else if (answer_profile > 2) {
      # Compute ANOVA statistics
      anova_stats(input = my_data,
                  var1 = my_colnames,
                  var2 = gen_env$my_var)
    } else {
      return(NA)
    }
  },
  my_columns = my_data,
  my_colnames = names(my_data)
)

# Structure ---------------------------------------------------------------
# Structure data
my_results <- lst(stat_results)

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
save.image(file = "Env/statistics.RData")
