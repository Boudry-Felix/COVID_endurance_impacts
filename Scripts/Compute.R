# Informations ------------------------------------------------------------
# Title: Compute.R
# Author: Félix Boudry
# Contact: <felix.boudry@univ-perp.fr>
# License: Private
# Description: Compute new values to analyze

# Configuration -----------------------------------------------------------
require(tibble)
require(CatEncoders)
require(stringi)

# New values --------------------------------------------------------------
# Compute new useful variables
my_data$duration_to_training_binary <-
  my_data$duration_to_training %>%
  stri_replace_all_regex(
    pattern = c(
      "Reprise immédiate|Quelques jours après",
      ".*mois.*|.*semaine.*"
    ),
    replacement = c("court", "long"),
    vectorize_all = FALSE
  )

my_data$difficulties_duration_binary <-
  my_data$difficulties_duration %>%
  stri_replace_all_regex(
    pattern = c("Aucune|1 semaine",
                ".*mois.*|.*semaine.*"),
    replacement = c("court", "long"),
    vectorize_all = FALSE
  )

my_data$time_to_normal_training_volume_binary <-
  my_data$time_to_normal_training_volume %>%
  stri_replace_all_regex(
    pattern = c("1 semaine|1 moins",
                ".*mois.*|.*semaine.*"),
    replacement = c("court", "long"),
    vectorize_all = FALSE
  )

my_data$modified_training_volume_binary <-
  my_data$modified_training_volume %>%
  stri_replace_all_regex(
    pattern = c(".*diminished.*",
                ".*augmented.*|.*no*"),
    replacement = c("diminished", "augmented/unchanged"),
    vectorize_all = FALSE
  )

# Encoding ----------------------------------------------------------------
# Encode answers as labels
data_transformed <-
  df_encode(input = my_data) # Create a data set with labelled data

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
save.image(file = "Env/compute.RData")
