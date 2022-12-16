# Informations ------------------------------------------------------------

# Title: Transform.R
# Author: Félix Boudry
# Contact: <felix.boudry@univ-perp.fr>
# License: Private
# Description: Script to make short data transformations to be used in graphs.

# Configuration -----------------------------------------------------------

## Libraries --------------------------------------------------------------
require(plyr)
require(thinkr)

# Environment -------------------------------------------------------------
# Load previously computed environment
rm(list = ls())
load(file = "Env/descriptive.RData")

data_trans <- lst()

# Sex count ---------------------------------------------------------------
sex_count <-
  plyr::count(my_data$raw_data$completed$sex) %>%
  t() %>%
  row_to_names(row_number = 1) %>%
  as.data.frame() %>%
  clean_names() %>%
  append(x = my_results)

# Endurance trained -------------------------------------------------------
endurance_count <-
  plyr::count(my_data$raw_data$completed$endurance_sport) %>%
  t() %>%
  row_to_names(row_number = 1) %>%
  as.data.frame() %>%
  clean_names()

# Sports ------------------------------------------------------------------
sports <-
  gsub(pattern = ",.*$", "", my_data$raw_data$completed$what_sport) %>%
  gsub(pattern = " \n.*$", replacement = "") %>%
  gsub(pattern = "\\(.*$", replacement =  "") %>%
  gsub(pattern = "/.*$", replacement = "") %>%
  gsub(pattern = "-.*$", replacement = "") %>%
  clean_vec() %>%
  gsub(pattern = "_.*$", replacement = "") %>%
  gsub(pattern = "athle.*$", replacement = "athletisme") %>%
  gsub(pattern = "basket.*$", replacement = "basketball") %>%
  gsub(pattern = "cycliste.*$", replacement = "cyclisme") %>%
  gsub(pattern = "^le.*$", replacement = "na") %>%
  gsub(pattern = "^la.*$", replacement = "na") %>%
  gsub(pattern = "fff.*$", replacement = "football") %>%
  gsub(pattern = "fftri.*$", replacement = "triathlon") %>%
  gsub(pattern = "ffa.*$", replacement = "athletisme") %>%
  gsub(pattern = "ffs.*$", replacement = "ski") %>%
  gsub(pattern = "saut.*$", replacement = "athletisme") %>%
  gsub(pattern = "lancer.*$", replacement = "athletisme") %>%
  gsub(pattern = "sprint.*$", replacement = "athletisme") %>%
  gsub(pattern = "x400.*$", replacement = "athletisme") %>%
  gsub(pattern = "volley.*$", replacement = "volleyball") %>%
  gsub(pattern = "foot.*$", replacement = "football") %>%
  gsub(pattern = "demi.*$", replacement = "athletisme") %>%
  gsub(pattern = "^na.*$", replacement = NA) %>%
  na.omit()

covid_symp <-
  my_data$raw_data$completed[, c(
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
    "other_symptoms"
  )]

post_covid_symp <-
  my_data$raw_data$completed[, c(
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
    "after_respiratory_difficulties"
  )]

training_symptoms <-
  my_data$raw_data$completed[, c(
    "training_tired",
    "training_respiratory_difficulties",
    "training_muscular_pain",
    "training_concentration_difficulties"
  )]

training_symptoms_notes <-
  my_data$raw_data$completed[, c(
    "training_tired_note",
    "training_respiratory_difficulties_note",
    "training_muscular_pain_note",
    "training_concentration_difficulties_note"
  )]

data_trans <-
  lst(sex_count,
      endurance_count,
      sports,
      covid_symp,
      post_covid_symp,
      training_symptoms,
      training_symptoms_notes)

rm(list = setdiff(ls(),
                  c("my_data", "my_results", "data_trans")))

# Export data -------------------------------------------------------------
# Save environment data
save.image(file = "Env/descriptive.RData")
