# Informations ------------------------------------------------------------

# Title: Generate.R
# Author: Félix Boudry
# Contact: <felix.boudry@univ-perp.fr>
# License: Private
# Description: Generate reports using different datasets.

# Configuration -----------------------------------------------------------
library(rmarkdown)

# Sourcing ----------------------------------------------------------------
source(file = "Scripts/Import.R") # Import data
gen_env <- new.env()
source(file = "Scripts/Subset.R") # Create subset

# Generate ----------------------------------------------------------------
if (!dir.exists("Reports")) {
  # Create "Reports" directory if not existing
  dir.create("Reports")
}

gen_env$categorical_vars <-
  # Select categorical variable to compare (COVID impact differences on
  # endurance athletes following those factors)
  c("sex",
    # "age",
    # "pathology",
    # "endurance_training",
    # "train_volume",
    "train_method")

gen_env$my_count <- 1
for (my_population in gen_env$studied_populations) {
  mapply(
    FUN = function(my_var, var_name) {
      my_data <- my_population
      source("Scripts/Descriptive.R")
      render(
        input = "COVID_and_endurance.Rmd",
        # input = "test.Rmd",
        output_dir = "Reports",
        output_file = paste(
          "COVID impact on endurance training for",
          names(gen_env$studied_populations[gen_env$my_count]),
          "results by",
          var_name
        ),
        params = list(new_title = paste(
          "COVID impact on endurance training for",
          names(gen_env$studied_populations[gen_env$my_count]),
          "results by",
          var_name
        ))
      )
    },
    my_var = gen_env$categorical_vars,
    var_name = gen_env$categorical_vars
  )
  gen_env$my_count <- gen_env$my_count + 1
}
