# Informations ------------------------------------------------------------
# Title: Import.R
# Author: Félix Boudry
# Contact: <felix.boudry@univ-perp.fr>
# License: Private
# Description: Import questionnaire data, clean and labelise them.

# Configuration -----------------------------------------------------------

## Libraries --------------------------------------------------------------
## List of used libraries.
require(janitor)
require(dplyr)

## Global vectors ---------------------------------------------------------
## Define vectors used in entire script.
rm(list = setdiff(x = ls(), y = c(lsf.str(), "gen_env"))) # Clean environment
imported_data <- lst()

# Import ------------------------------------------------------------------
# Import data sets
imported_data_all <- read.csv(file = "Data/Answers_all.csv")
imported_data_complete <-
  read.csv(file = "Data/Answers_completed.csv")
gen_env$col_questions <- read.csv(file = "Data/col_question.csv")

# Format ------------------------------------------------------------------
# Formating data for analysis
imported_data <- # Put data in list and clean it
  list("complete" = imported_data_complete, "all" = imported_data_all) %>%
  lapply(FUN = clean_names) %>%
  lapply(FUN =  remove_empty, which = c("rows", "cols")) %>%
  lapply(FUN = `colnames<-`, gen_env$col_questions$col_name) # Change colnames from question to shorter name

gen_env$imported_data <-
  imported_data # Store data in another environment

rm(list = setdiff(x = ls(), y = c(lsf.str(), "gen_env")))

# Export data -------------------------------------------------------------
# Save environment data
if (!dir.exists(paths = "Env")) {
  dir.create(path = "Env")
}
save.image(file = "Env/import.RData")
