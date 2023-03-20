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
# test <- lapply(my_data, unique) %>% lapply(length) %>% as.data.frame() %>% rbind(1:ncol(.))
ignored_vars <- c(1, 3:9, 11, 15, 19, 53, 55, 57, 59)

my_profiles <- lst()
data_profiles <-
  data_transformed$encoded_data %>%
  select_if( ~ !any(is.na(.))) %>%
  select(-all_of(ignored_vars))
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
    clust_plot
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
