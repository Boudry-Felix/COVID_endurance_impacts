# Informations ------------------------------------------------------------

# Title: Statistics.R
# Author: Félix Boudry
# Contact: <felix.boudry@univ-perp.fr>
# License: Private
# Description: Basic statistics

# Environment -------------------------------------------------------------
# Load previously computed environment
rm(list = setdiff(
  x = ls(),
  y = c(lsf.str(),
        "my_var",
        "gen_env",
        "my_population",
        "my_data")
))

binary_answers <- c(
  "cough",
  "fever",
  "runny_nose",
  "headache",
  "sore_throat",
  "tired",
  "muscular_pain",
  "vomiting",
  "diarrhea",
  "breathlessness",
  "respiratory_difficulties",
  "other_symptoms",
  "after_cough",
  "after_fever",
  "after_runny_nose",
  "after_headache",
  "after_sore_throat",
  "after_tired",
  "after_muscular_pain",
  "after_vomiting",
  "after_diarrhea",
  "after_breathlessness",
  "after_respiratory_difficulties",
  "training_tired",
  "training_respiratory_difficulties",
  "training_muscular_pain",
  "training_concentration_difficulties",
  "difficulties_duration",
  "restart_training_medication",
  "respiratory_medication",
  "cortisone_medication",
  "vitamin_medication",
  "other_medication",
  "medication_duration",
  "high_intensity_training",
  "force_training",
  "endurance_training",
  "intermittent_training",
  "other_training",
  "hypoxia_training",
  "after_hypoxia_training",
  "500-2000_hypoxia",
  "2000-3000_hypoxia",
  "3000-5500_hypoxia",
  "5500-more_hypoxia",
  "hypoxia_duration",
  "hypoxia_difficulties",
  "hypoxia_difficulties_detail",
  "hypoxia_respiratory_difficulties"
)
continuous_answers <- c()

lapply(X = binary_answers, FUN = function(my_param) {
  print(my_param)
  prop.test(table(my_data[[my_param]], my_data[[gen_env$my_var]]), correct = FALSE)
})
# prop.test(table(my_data$cough, my_data$sex), correct = FALSE)
