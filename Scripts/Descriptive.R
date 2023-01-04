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
require(plyr)
require(thinkr)
require(tidyverse)

# Environment -------------------------------------------------------------
# Load previously computed environment
rm(list = setdiff(
  x = ls(),
  y = c(
    "my_var",
    "gen_env",
    "studied_populations",
    "my_population",
    "my_data"
  )
))
my_results <- lst()
my_data <- my_population

# Functions ---------------------------------------------------------------
my_table <- function(input) {
  kable(count(input)) %>%
    kable_styling(bootstrap_options = c("striped"))
}
my_var_counting <- function(input = my_data, my_vars) {
  lapply(
    X = my_vars,
    FUN = function(my_dataframe) {
      count(input[[my_dataframe]]) %>%
        t() %>%
        row_to_names(row_number = 1) %>%
        as.data.frame() %>%
        clean_names()
    }
  )
}
bar_graph <- function(input,
                      graph_xlab,
                      graph_ylab,
                      graph_title) {
  ggplot(mapping = aes(x = input)) +
    geom_bar() +
    xlab(graph_xlab) +
    ylab(graph_ylab) +
    ggtitle(graph_title)
}
hist_plot <- function(input,
                      graph_xlab,
                      graph_ylab,
                      graph_title) {
  gplot(mapping = aes(x = input)) +
    geom_histogram() +
    xlab(graph_xlab) +
    ylab(graph_ylab) +
    ggtitle(graph_title)
}
hist_plot_multi <-
  function(input,
           my_colnames = antrop_measures,
           graph_title,
           my_stats = "count") {
    lapply(
      X = input,
      FUN = function(my_dataframe) {
        mapply(
          FUN = function(my_column, my_colname) {
            ggplot(
              data = as.data.frame(my_column),
              mapping = aes(x = my_column),
              stat = my_stats
            ) +
              geom_histogram() +
              xlab(my_colname) +
              ggtitle(paste(graph_title , my_colname))
          },
          my_column = my_dataframe[, antrop_measures],
          my_colname = antrop_measures,
          SIMPLIFY = FALSE
        )
      }
    )
  }
bar_plot_multi <-
  function(input,
           my_colnames = antrop_measures,
           graph_title,
           my_stats = "count") {
    lapply(
      X = input,
      FUN = function(my_dataframe) {
        mapply(
          FUN = function(my_column, my_colname) {
            ggplot(
              data = as.data.frame(my_column),
              mapping = aes(x = my_column),
              stat = my_stats
            ) +
              geom_bar() +
              xlab(my_colname) +
              ggtitle(paste(graph_title , my_colname))
          },
          my_column = my_dataframe[, antrop_measures],
          my_colname = antrop_measures,
          SIMPLIFY = FALSE
        )
      }
    )
  }

# Antropometrics ----------------------------------------------------------

## Values -----------------------------------------------------------------
## Compute anthropometric and population values
antrop_measures <- c("age", "height", "weight")
population_var <-
  c("adult",
    "sex",
    "region",
    "profession")

population_caracteristics <-
  my_var_counting(my_vars = population_var)
names(population_caracteristics) <- population_var
antrop_values <- describe(x = my_data) %>%
  t() %>%
  as.data.frame() %>%
  select(all_of(antrop_measures)) %>%
  round(digits = 2)

results_antrop <- lst(population_caracteristics, antrop_values)

## Plot -------------------------------------------------------------------
antrop_plots <-
  hist_plot_multi(input = my_data$encoded_data, graph_title = "Distribution of subjects")
professions <- ggplot(mapping = aes(y = my_data$profession)) +
  geom_bar() +
  ylab(label = "Profession") +
  ggtitle(label = "Subject's profession")

plots_antrop <- lst(antrop_plots, professions)

# Training ----------------------------------------------------------------

## Values -----------------------------------------------------------------
my_training <-
  c(
    "endurance_sport",
    "federal_license",
    "what_sport",
    "train_volume",
    "train_sessions_week",
    "train_method"
  )

my_data$what_sport <-
  gsub(pattern = ",.*$", replacement = "", my_data$what_sport) %>%
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
  gsub(pattern = "^na.*$", replacement = NA)
  # na.omit()

my_training_counts <- my_var_counting(my_vars = my_training)
names(my_training_counts) <- my_training

results_training <- lst(my_training_counts)

## Plots ------------------------------------------------------------------
endu <-
  bar_graph(
    input = my_data$endurance_sport,
    graph_xlab = "Endurance trained",
    graph_ylab = "Count",
    graph_title = "Number of endurance trained and not trained subjects"
  )
fede <-
  bar_graph(
    input = my_data$federal_license,
    graph_xlab = "Have a federal license",
    graph_ylab = "Count",
    graph_title = "Number of endurance trained and not trained subjects"
  )
sport <- ggplot(mapping = aes(y = my_data$what_sport)) +
  geom_bar() +
  ylab(label = "Sport") +
  ggtitle(label = "Subject's sport")
train_volume <- ggplot(mapping = aes(
  x = reorder(my_data$train_volume, my_data$train_volume, function(x)
    -length(x))
)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
train_sessions <- ggplot(mapping = aes(
  x = reorder(my_data$train_sessions_week, my_data$train_sessions_week, function(x)
    -length(x))
)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plots_training <- lst(endu, fede, sport, train_volume, train_sessions)

# COVID -------------------------------------------------------------------

## Values -----------------------------------------------------------------
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

covid_infos_counts <- my_var_counting(my_vars = covid_infos)
my_covid_symptoms <- my_var_counting(my_vars = covid_symptoms)

results_covid <- lst(covid_infos_counts, my_covid_symptoms)

## Plots ------------------------------------------------------------------
# covid_symtoms <- mapply(
#   FUN = function(my_column, my_colname) {
#     ggplot(mapping = aes(x = my_column)) +
#       geom_bar(aes(fill = my_data[[my_var]])) +
#       geom_text(aes(label = after_stat(count)), stat = "count", vjust = 1) +
#       ggtitle(my_colname) +
#       labs(fill = my_var)
#   },
#   my_column = covid_symptoms,
#   my_colname = colnames(covid_symptoms),
#   SIMPLIFY = FALSE
# ) %>%
#   marrangeGrob(nrow = 2, ncol = 3)

plots_covid <- lst(covid_symptoms)

# Post COVID --------------------------------------------------------------

## Values -----------------------------------------------------------------
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

post_covid_infos_counts <-
  my_var_counting(my_vars = post_covid_infos)

post_covid_symptoms_counts <-
  my_var_counting(my_vars = post_covid_symptoms)
my_training_symptoms_counts <-
  my_var_counting(my_vars = training_symptoms)
my_training_symptoms_notes_counts <-
  my_var_counting(my_vars = training_symptoms_notes)
post_covid_difficulties_counts <-
  my_var_counting(my_vars = post_covid_difficulties)

results_post_covid <-
  lst(
    post_covid_infos,
    post_covid_infos_counts,
    my_training_symptoms_counts,
    my_training_symptoms_notes_counts,
    post_covid_difficulties_counts
  )

## Plots ------------------------------------------------------------------

plots_post_covid <- lst()

# Training difficulties ---------------------------------------------------

## Values -----------------------------------------------------------------
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

my_training_types_counts <-
  my_var_counting(my_vars = training_types)
my_training_difficulties_counts <-
  my_var_counting(my_vars = training_difficulties)

results_train_diff <-
  lst(my_training_types_counts, my_training_difficulties_counts)

## Plots ------------------------------------------------------------------

plots_train_diff <- lst()

# Hypoxia -----------------------------------------------------------------

## Values -----------------------------------------------------------------
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

my_hypoxia_counts <- my_var_counting(my_vars = hypoxia)

results_hypoxia <- lst(my_hypoxia_counts)

## Plots ------------------------------------------------------------------

plots_hypoxia <- lst()

# Pathologies -------------------------------------------------------------
# Population pathologies statistics

## Values -----------------------------------------------------------------
my_pathologies <-
  c("pathology", "pathology_description", "pathology_medication")

my_pathologies_counts <- my_var_counting(my_vars = my_pathologies)

results_pathologies <- lst(my_pathologies_counts)

## Plots ------------------------------------------------------------------
my_data$pathology_description <- clean_vec(my_data$pathology_description) %>%
  gsub(pattern = "na_.*", replacement = NA) %>%
  gsub(pattern = "^na", replacement = NA) %>%
  gsub(pattern = "_([0-9]).*$", replacement = "")
patho_description <- ggplot(mapping = aes(x = my_data$pathology_description)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
patho_medication <- ggplot(mapping = aes(na.omit(my_data$pathology_medication))) +
  geom_bar()
plots_pathologies <- lst(patho_description, patho_medication)

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
    "my_var",
    "gen_env",
    "my_data",
    "my_results",
    "my_count",
    "var_count",
    "my_table",
    "my_population"
  )
))

# Export data -------------------------------------------------------------
# Save environment data
save.image(file = "Env/descriptive.RData")
