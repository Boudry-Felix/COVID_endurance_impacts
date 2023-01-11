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
  y = c(
    lsf.str(),
    "my_var",
    "gen_env",
    "my_population",
    "my_data",
    "my_results"
  )
))

quantitative_answers <- c("age", "height", "weight")
qualitative_answers <- c(
  "adult",
  "sex",
  "region",
  "profession",
  "what_sport",
  "train_volume",
  "train_sessions_week",
  "train_method",
  "pathology_description",
  "covid_test",
  "covid_date",
  "covid_type",
  "hypoxia_difficulties_detail"
)
binary_answers <- c(
  "endurance_sport",
  "federal_license",
  "pathology",
  "pathology_medication",
  "covid_positive",
  "covid_multi",
  "long_covid_diagnostic",
  "long_covid_supposed",
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
  "after_symptoms",
  "restart_training",
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
  "high_intensity_training",
  "force_training",
  "endurance_training",
  "intermittent_training",
  "other_training",
  "longer_recuperation",
  "training_volume_back_normal",
  "hypoxia_training",
  "after_hypoxia_training",
  "500-2000_hypoxia",
  "2000-3000_hypoxia",
  "3000-5500_hypoxia",
  "5500-more_hypoxia",
  "hypoxia_difficulties",
  "hypoxia_respiratory_difficulties"
)
continuous_answers <-
  c(
    "covid_duration",
    "covid_how_much",
    "duration_to_training",
    "training_tired_note",
    "training_respiratory_difficulties_note",
    "training_muscular_pain_note",
    "training_concentration_difficulties_note",
    "difficulties_duration",
    "medication_duration",
    "modified_training_volume",
    "time_to_normal_training_volume",
    "endurance_training_more_difficult",
    "hypoxia_duration",
    "hypoxia_respiratory_difficulties_note"
  )

binary_proportions <- lapply(
  X = binary_answers,
  FUN = function(my_param) {
    prop.test(table(my_data[[my_param]], my_data[[gen_env$my_var]]), n = NULL, correct = FALSE)
  }
) %>% `names<-`(value = binary_answers)

continuous_results <- lapply(
  X = continuous_answers,
  FUN = function(my_param) {
    tmp_data <-
      lm(as.numeric(as.factor(my_data[[my_param]])) ~ my_data[[gen_env[["my_var"]]]])
    anova(tmp_data)
  }
) %>% `names<-`(value = continuous_answers)

# Structure ---------------------------------------------------------------
# Structure data
my_results <-
  append(x = my_results,
         values = lst(binary_proportions, continuous_results))

rm(list = setdiff(
  ls(),
  c(
    lsf.str(),
    "my_var",
    "gen_env",
    "my_data",
    "my_results",
    "my_population"
  )
))

# Export data -------------------------------------------------------------
# Save environment data
save.image(file = "Env/statistics.RData")
