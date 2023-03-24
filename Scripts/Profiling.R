# Informations ------------------------------------------------------------
# Title: Profiling.R
# Author: Félix Boudry
# Contact: <felix.boudry@univ-perp.fr>
# License: Private
# Description: Create profile for each reaction types.

# Configuration -----------------------------------------------------------

## Libraries --------------------------------------------------------------
require(tidyverse)
require(DTWBI)
require(TraMineR)
require(reshape2)
require(cluster)
require(dendextend)
require(gtsummary)
require(GGally)

# Environment -------------------------------------------------------------
# Load previously computed environment
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

# Data structure ----------------------------------------------------------
# Structure and select data to analyse chains answers

my_profiles <- lst()
data_profiles <-
  data_transformed$encoded_data %>%
  select_if(~ !any(is.na(.))) %>%
  select(
    -c(
      adult,
      profession,
      age,
      height,
      weight,
      pathology,
      pathology_description,
      pathology_medication,
      region,
      what_sport,
      train_volume,
      train_method,
      train_sessions_week,
      covid_positive,
      covid_test,
      covid_date,
      covid_duration,
      covid_type,
      covid_how_much,
      after_fever,
      medication_duration,
      restart_training,
      # hypoxia_difficulties_detail,
      hypoxia_duration,
      difficulties_duration,
      duration_to_training,
      # other_medication,
      time_to_normal_training_volume
      # training_concentration_difficulties_note,
      # training_muscular_pain_note,
      # training_respiratory_difficulties_note,
      # training_tired_notes
    )
  )
data_profiles_derived <- local.derivative.ddtw(data_profiles)

chains <-
  unite(data = data_profiles, col = "chains", sep = "-") %>%
  seqdef()

# Plots -------------------------------------------------------------------
# Create chain plots
matplot(t(data_profiles), type = 'l')
simple_chain_plot <- recordPlot()
matplot(t(data_profiles_derived), type = 'l')
derived_chain_plot <- recordPlot()

plot_data <-
  data_profiles %>%
  rownames_to_column() %>%
  gather(colname, value, -rowname)

factor_tile_plot <- ggplot(plot_data) +
  geom_tile(aes(x = colname, y = rowname, fill = factor(value))) +
  scale_fill_brewer(palette = 10)
factor_tile_plot

tile_plot <- ggplot(plot_data) +
  geom_tile(aes(x = colname, y = rowname, fill = value))

seqIplot(chains)
chain_graph <- recordPlot()

my_transitions <-
  seqtrate(seqdata = chains) %>% round(digits = 2) %>% melt()

transition_plot <- ggplot(my_transitions, aes(Var2, Var1)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = round(value, 2))) +
  scale_fill_continuous(high = "#132B43",
                        low = "#56B1F7",
                        name = "Transitions")

sub_cost <-
  seqsubm(chains, method = "TRATE") %>% round(digits = 2) %>% melt()

sub_cost_plot <- ggplot(sub_cost, aes(Var2, Var1)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = round(value, 2))) +
  scale_fill_continuous(high = "#132B43",
                        low = "#56B1F7",
                        name = "Substitution rate")

dissim <-
  seqdist(
    chains,
    method = "OM",
    sm = seqsubm(chains, method = "TRATE"),
    indel = 1
  ) %>% round(digits = 2) %>%  melt()

dissim_plot <- ggplot(dissim, aes(Var2, Var1)) +
  geom_tile(aes(fill = value)) +
  # geom_text(aes(label = round(value, 2))) +
  scale_fill_continuous(high = "#132B43",
                        low = "#56B1F7",
                        name = "Dissimilarities")

dissim_cluster <-
  seqdist(
    chains,
    method = "OM",
    sm = seqsubm(chains, method = "TRATE"),
    indel = 1
  ) %>% agnes(diss = TRUE, method = "ward")

as.dendrogram(dissim_cluster) %>% plot(type = "rectangle")
clust_plot <- recordPlot()

my_data$clust <- cutree(dissim_cluster, k = 2)
cluster_caract <- tbl_summary(my_data, by = clust)

# Structure ---------------------------------------------------------------
# Structure data
chains_plots <-
  lst(
    simple_chain_plot,
    derived_chain_plot,
    factor_tile_plot,
    tile_plot,
    chain_graph,
    transition_plot,
    sub_cost_plot,
    dissim_plot,
    clust_plot,
    cluster_caract
  )
my_results <- append(x = my_results, values = lst(chains_plots))

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
save.image(file = "Env/profiling.RData")
