# Informations ------------------------------------------------------------

# Title: Subset.R
# Author: Félix Boudry
# Contact: <felix.boudry@univ-perp.fr>
# License: Private
# Description: Create different subset off data to study


# Configuration -----------------------------------------------------------
require(dplyr)

gen_env$studied_populations <-
  lst(complete = my_data$raw_data$completed,
      all = my_data$raw_data$all)
