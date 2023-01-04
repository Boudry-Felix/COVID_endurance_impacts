# Informations ------------------------------------------------------------

# Title: Descriptive.R
# Author: Félix Boudry
# Contact: <felix.boudry@univ-perp.fr>
# License: Private
# Description: This script makes a descriptive analysis of the data.

# Configuration -----------------------------------------------------------

## Libraries --------------------------------------------------------------
require(tidyverse)
require(janitor)
require(psych)
require(plyr)
require(thinkr)
require(gridExtra)
require(kableExtra)

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
my_results <- lst()
my_data <- my_population

# Population description --------------------------------------------------

## Antropometrics ---------------------------------------------------------
antrop_measures <- c("age", "height", "weight")
population_var <-
  c("adult",
    "sex",
    "region",
    "profession")

### Values ----------------------------------------------------------------
### Compute anthropometric and population values
antrop_values <- describe(x = my_data) %>%
  t() %>%
  as.data.frame() %>%
  select(all_of(antrop_measures)) %>%
  round(digits = 2)
population_values <-
  my_var_counting(my_vars = population_var)

results_antrop <- lst(antrop_values, population_values)

### Plot ------------------------------------------------------------------
antrop_plots <-
  hist_plot_multi(input = my_data,
                  my_columns = antrop_measures)
population_plots <-
  bar_plot_multi(
    input = my_data,
    my_columns = population_var,
    graph_fill = FALSE
  )

plots_antrop <- lst(antrop_plots, population_plots)

## Training ---------------------------------------------------------------
training <-
  c(
    "endurance_sport",
    "federal_license",
    "what_sport",
    "train_volume",
    "train_sessions_week",
    "train_method"
  )

### Values ----------------------------------------------------------------
training_values <- my_var_counting(my_vars = training)

results_training <- lst(training_values)

### Plots -----------------------------------------------------------------
training_plots <-
  bar_plot_multi(input = my_data,
                 my_columns = training,
                 graph_fill = FALSE)

plots_training <-
  lst(training_plots)

## Pathologies ------------------------------------------------------------
## Population pathologies statistics
pathologies <-
  c("pathology", "pathology_description", "pathology_medication")

### Values ----------------------------------------------------------------
pathologies_values <- my_var_counting(my_vars = pathologies)

results_pathologies <- lst(pathologies_values)

### Plots -----------------------------------------------------------------
pathologies_plots <-
  bar_plot_multi(input = my_data,
                 my_columns = pathologies,
                 graph_fill = FALSE)

plots_pathologies <- lst(pathologies_plots)

# COVID -------------------------------------------------------------------

## Infos ------------------------------------------------------------------
covid_infos <-
  c(
    "covid_positive",
    "covid_test",
    "covid_date",
    "covid_duration",
    "covid_type",
    "covid_multi",
    "covid_how_much",
    "long_covid_diagnostic",
    "long_covid_supposed"
  )

### Values ----------------------------------------------------------------
covid_infos_values <- my_var_counting(my_vars = covid_infos)

results_covid_infos <- lst(covid_infos_values)

### Plots -----------------------------------------------------------------
covid_infos_plots <-
  bar_plot_multi(input = my_data,
                 my_columns = covid_infos)

plots_covid_infos <- lst(covid_infos_plots)

## Symptoms ---------------------------------------------------------------
covid_symptoms <-
  c(
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
  )

### Values ----------------------------------------------------------------
covid_symptoms_values <- my_var_counting(my_vars = covid_symptoms)

results_covid <- lst(covid_symptoms_values)

### Plots -----------------------------------------------------------------
covid_symptoms_plots <-
  bar_plot_multi(input = my_data,
                 my_columns = covid_symptoms)

plots_covid <- lst(covid_symptoms_plots)

# Post COVID --------------------------------------------------------------

## Symptoms ---------------------------------------------------------------
post_covid_infos <-
  c("after_symptoms", "restart_training", "duration_to_training")
post_covid_symptoms <-
  c(
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
  )
training_symptoms <-
  c(
    "training_tired",
    "training_respiratory_difficulties",
    "training_muscular_pain",
    "training_concentration_difficulties"
  )
training_symptoms_notes <-
  c(
    "training_tired_note",
    "training_respiratory_difficulties_note",
    "training_muscular_pain_note",
    "training_concentration_difficulties_note"
  )
post_covid_difficulties <-
  c(
    "difficulties_duration",
    "restart_training_medication",
    "respiratory_medication",
    "cortisone_medication",
    "vitamin_medication",
    "other_medication",
    "medication_duration"
  )

### Values ----------------------------------------------------------------
post_covid_infos_values <-
  my_var_counting(my_vars = post_covid_infos)
post_covid_symptoms_values <-
  my_var_counting(my_vars = post_covid_symptoms)
training_symptoms_values <-
  my_var_counting(my_vars = training_symptoms)
training_symptoms_notes_values <-
  my_var_counting(my_vars = training_symptoms_notes)
post_covid_difficulties_values <-
  my_var_counting(my_vars = post_covid_difficulties)

results_post_covid <-
  lst(
    post_covid_infos_values,
    post_covid_infos_values,
    training_symptoms_values,
    training_symptoms_notes_values,
    post_covid_difficulties_values
  )

### Plots -----------------------------------------------------------------
post_covid_infos_plots <-
  bar_plot_multi(input = my_data,
                 my_columns = post_covid_infos)
post_covid_symptoms_plots <-
  bar_plot_multi(input = my_data,
                 my_columns = post_covid_symptoms)
training_symptoms_plots <-
  bar_plot_multi(input = my_data,
                 my_columns = training_symptoms)
training_symptoms_notes_plots <-
  bar_plot_multi(input = my_data,
                 my_columns = training_symptoms_notes)
post_covid_difficulties_plots <-
  bar_plot_multi(input = my_data,
                 my_columns = post_covid_difficulties)

plots_post_covid <-
  lst(
    post_covid_infos_plots,
    post_covid_symptoms_plots,
    training_symptoms_plots,
    training_symptoms_notes_plots,
    post_covid_difficulties_plots
  )

## Training difficulties --------------------------------------------------
training_types <-
  c(
    "high_intensity_training",
    "force_training",
    "endurance_training",
    "intermittent_training",
    "other_training"
  )
training_difficulties <-
  c(
    "longer_recuperation",
    "modified_training_volume",
    "training_volume_back_normal",
    "time_to_normal_training_volume",
    "endurance_training_more_difficult"
  )

### Values ----------------------------------------------------------------
training_types_values <-
  my_var_counting(my_vars = training_types)
training_difficulties_values <-
  my_var_counting(my_vars = training_difficulties)

results_train_diff <-
  lst(training_types_values, training_difficulties_values)

### Plots -----------------------------------------------------------------
training_types_plots <-
  bar_plot_multi(input = my_data,
                 my_columns = training_types)
training_difficulties_plots <-
  bar_plot_multi(input = my_data,
                 my_columns = training_difficulties)

plots_train_diff <- lst(training_types_plots, training_difficulties_plots)

## Hypoxia ----------------------------------------------------------------
hypoxia <-
  c(
    "hypoxia_training",
    "after_hypoxia_training",
    "500-2000_hypoxia",
    "2000-3000_hypoxia",
    "3000-5500_hypoxia",
    "5500-more_hypoxia",
    "hypoxia_duration",
    "hypoxia_difficulties",
    "hypoxia_difficulties_detail",
    "hypoxia_respiratory_difficulties",
    "hypoxia_respiratory_difficulties_note"
  )

### Values ----------------------------------------------------------------
hypoxia_values <- my_var_counting(my_vars = hypoxia)

results_hypoxia <- lst(hypoxia_values)

### Plots -----------------------------------------------------------------
hypoxia_plots <-
  bar_plot_multi(input = my_data, my_columns = hypoxia)

plots_hypoxia <- lst(hypoxia_plots)

# Structure ---------------------------------------------------------------
# Structure data
my_results_num <- sapply(
  X = ls(pattern = "results_.*"),
  FUN = get,
  simplify = FALSE,
  USE.NAMES = TRUE
)

my_plots <- sapply(
  X = ls(pattern = "plots_.*"),
  FUN = get,
  simplify = FALSE,
  USE.NAMES = TRUE
)

my_results <- lst(num = my_results_num, plots = my_plots)

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
save.image(file = "Env/descriptive.RData")
