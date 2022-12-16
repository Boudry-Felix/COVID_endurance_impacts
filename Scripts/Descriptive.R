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
require(psych)

# Environment -------------------------------------------------------------
# Load previously computed environment
rm(list = ls())
load(file = "Env/import.RData")

my_results <- lst()
antrop_measures <- c("age", "height", "weight")

# Antropometrics ----------------------------------------------------------

## Plot -------------------------------------------------------------------
antrop_plots <-
  lapply(
    X = my_data$encoded_data,
    FUN = function(my_dataframe) {
      mapply(
        FUN = function(my_column, my_colname) {
          ggplot(
            data = as.data.frame(my_column),
            mapping = aes(x = my_column),
            stat = "count"
          ) +
            geom_histogram() +
            xlab(my_colname) +
            ggtitle(paste0("Distribution of subjects " , my_colname))
        },
        my_column = my_dataframe[, antrop_measures],
        my_colname = antrop_measures,
        SIMPLIFY = FALSE
      )
    }
  )

## Compute ----------------------------------------------------------------
## Compute antropometric data for the populations
antrop_data <- lapply(
  X = my_data$raw_data,
  FUN = function(my_dataframe) {
    describe(x = my_dataframe) %>%
      t() %>%
      as.data.frame() %>%
      select(all_of(antrop_measures)) %>%
      round(digits = 2)
  }
)

my_results <- lst(antrop_data, antrop_plots)

rm(list = setdiff(
  ls(),
  c("my_data", "my_results")
))

# Export data -------------------------------------------------------------
# Save environment data
save.image(file = "Env/descriptive.RData")
