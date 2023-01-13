# Informations ------------------------------------------------------------

# Title: Profiling.R
# Author: Félix Boudry
# Contact: <felix.boudry@univ-perp.fr>
# License: Private
# Description: Create profile for each reaction types.

# Configuration -----------------------------------------------------------

## Libraries --------------------------------------------------------------
require(DTWBI)
require(tidyr)

# Environment -------------------------------------------------------------
# Load previously computed environment
rm(list = setdiff(
  x = ls(),
  y = c(lsf.str(),
        "imported_data",
        "my_var",
        "gen_env",
        "my_population",
        "my_data")
))
my_profiles <- lst()

my_data <- imported_data$encoded_data$completed
my_data <- my_data %>% select_if(~ !any(is.na(.)))
my_data_derived <- local.derivative.ddtw(my_data)

matplot(t(my_data[-c(6, 7, 8, 9)]), type = 'l')
matplot(t(my_data_derived[-c(6, 7, 8, 9)]), type = 'l')

chains <- unite(my_data[-c(1:12)], col = "chains", sep = "-")
