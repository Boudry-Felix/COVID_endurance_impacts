# Informations ------------------------------------------------------------
# Title: Profiling.R
# Author: Félix Boudry
# Contact: <felix.boudry@univ-perp.fr>
# License: Private
# Description: Create profile for each reaction types.

# Configuration -----------------------------------------------------------

## Libraries --------------------------------------------------------------
require(tidyverse)
require(DTWBI)

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

my_profiles <- lst()
data_profiles <- data_transformed$encoded_data
data_profiles <- data_profiles %>% select_if( ~ !any(is.na(.)))
data_profiles_derived <- local.derivative.ddtw(data_profiles)

matplot(t(data_profiles[-c(6, 7, 8, 9)]), type = 'l')
matplot(t(data_profiles_derived[-c(6, 7, 8, 9)]), type = 'l')

chains <- unite(data = data_profiles[-c(1:12)], col = "chains", sep = "-")

plot_data <-
  data_profiles[, -c(3, 7, 8, 9, 11, 12, 13, 14, 15, 65, 85)]
plot_data <- plot_data %>%
  rownames_to_column() %>%
  gather(colname, value, -rowname)

ggplot(plot_data) +
  geom_tile(aes(x = colname, y = rowname, fill = factor(value)))

ggplot(plot_data) +
  geom_tile(aes(x = colname, y = rowname, fill = value))

# Structure ---------------------------------------------------------------
# Structure data
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
save.image(file = "Env/profiling.RData")
