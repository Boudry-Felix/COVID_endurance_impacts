# Informations ------------------------------------------------------------
# Title: Generate.R
# Author: Félix Boudry
# Contact: <felix.boudry@univ-perp.fr>
# License: Private
# Description: Generate reports using different datasets.

# Configuration -----------------------------------------------------------
library(rmarkdown)
library(tidyverse)
library(plyr)
library(janitor)
library(gridExtra)
library(CatEncoders)
library(psych)
library(effectsize)
library(pwr)
library(kableExtra)
library(DTWBI)
library(stringi)
library(TraMineR)
library(reshape2)
library(cluster)
source(file = "Scripts/Functions.R") # Import own functions

# Sourcing ----------------------------------------------------------------
rm(list = setdiff(x = ls(), y = lsf.str()))
gen_env <-
  new.env() # Create environment to store variables used in this script
source(file = "Scripts/Import.R") # Import data

# Generate ----------------------------------------------------------------
if (!dir.exists(paths = "Reports")) {
  # Create "Reports" directory if not existing
  dir.create(path = "Reports")
}

gen_env$studied_populations <-
  gen_env$imported_data[c("complete")] # Select data
gen_env$categorical_vars <- # Select categorical variable to compare
  c("sex",
    "age",
    "pathology",
    "endurance_sport",
    "train_volume",
    "train_method")

gen_env$my_count <- 1
for (my_data in gen_env$studied_populations) {
  lapply(
    X = gen_env$categorical_vars,
    FUN = function(my_var) {
      gen_env$my_var <-
        my_var # Store var in other environment for use in other scripts
      source(file = "Scripts/Compute.R") # Compute new values and subset data
      source(file = "Scripts/Statistics.R") # Does statistical analysis
      source(file = "Scripts/Descriptive.R") # Does descriptive analysis
      source(file = "Scripts/Profiling.R") # Does a chain/profile analysis
      render(
        # Create a report using the .Rmd template
        input = "Report_template.Rmd",
        output_dir = "Reports",
        output_file = paste(
          "COVID impact on endurance training for",
          names(x = gen_env$studied_populations[gen_env$my_count]),
          "results by",
          my_var
        ),
        params = list(
          new_title = paste(
            "COVID impact on endurance training for",
            names(x = gen_env$studied_populations[gen_env$my_count]),
            "results by",
            my_var
          )
        )
      )
    }
  )
  gen_env$my_count <- gen_env$my_count + 1
}
