# Informations ------------------------------------------------------------

# Title: Generate.R
# Author: Félix Boudry
# Contact: <felix.boudry@univ-perp.fr>
# License: Private
# Description: Generate reports using different datasets.

# Configuration -----------------------------------------------------------
library(rmarkdown)
library(tidyverse)

# Sourcing ----------------------------------------------------------------
rm(list = ls())
gen_env <- new.env() # Create environment to store variables used in this script
source(file = "Scripts/Import.R") # Import data
source(file = "Scripts/Subset.R") # Create subset
source(file = "Scripts/Functions.R")

# Generate ----------------------------------------------------------------
if (!dir.exists(paths = "Reports")) {
  # Create "Reports" directory if not existing
  dir.create(path = "Reports")
}

gen_env$categorical_vars <-
  # Select categorical variable to compare (COVID impact differences on
  # endurance athletes following those factors)
  c("sex",
    "age",
    "pathology",
    "endurance_sport",
    "train_volume",
    "train_method")

gen_env$my_count <- 1
for (my_population in gen_env$studied_populations) {
  lapply(
    X = gen_env$categorical_vars,
    FUN = function(my_var) {
      gen_env$my_var <- my_var
      source(file = "Scripts/Compute.R")
      source(file = "Scripts/Statistics.R")
      source(file = "Scripts/Descriptive.R")
      render(
        input = "COVID_and_endurance.Rmd",
        output_dir = "Reports",
        output_file = paste(
          "COVID impact on endurance training for",
          names(x = gen_env$studied_populations[gen_env$my_count]),
          "results by",
          my_var
        ),
        params = list(new_title = paste(
          "COVID impact on endurance training for",
          names(x = gen_env$studied_populations[gen_env$my_count]),
          "results by",
          my_var
        ))
      )
    }
  )
  gen_env$my_count <- gen_env$my_count + 1
}

source(file = "Scripts/Profiling.R")
