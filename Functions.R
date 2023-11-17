# Informations ------------------------------------------------------------
# Title: Functions.R
# Author: Félix Boudry
# Contact: <felix.boudry@univ-perp.fr>
# License: Private
# Description: Functions used in COVID and endurance analysis.

# Tables ------------------------------------------------------------------
# Functions used to print tables
my_table <- function(input, ...) {
  # Custom kable table
  kableExtra::kable(x = input, ... = ...) %>%
    kableExtra::kable_styling(bootstrap_options = c("striped"),
                              full_width = FALSE)
}

# Data transformation -----------------------------------------------------
# Functions that transform, recompute or re-encode data
df_encode <- function(input = my_data) {
  # Encode (labeling entire data frames)
  convert_dic <-
    dplyr::lst() # Initialize a dictionary with label correspondences
  encoded_data <- lapply(# Encode each column
    X = input,
    function(my_col) {
      if (is.numeric(x = my_col)) {
        # If the column is numeric, no change is made
        my_col
      } else {
        # In any other case, the values are encoded as numbers
        label <- CatEncoders::LabelEncoder.fit(y = my_col)
        convert_dic <<- append(x = convert_dic, values = label)
        CatEncoders::transform(enc = label, my_col)
      }
    }) %>% as.data.frame() # Return all encoded columns in a single data frame
  return(dplyr::lst(convert_dic, encoded_data))
}

# Statistic computing -----------------------------------------------------
# Functions to process data statistically
occurrences <- function(input = my_data, feature) {
  # Generate a data frame with answer occurrences
  plyr::count(df = input[[feature]]) %>% # Counting occurrences
    t() %>% # Transposing data frame to have answers as features
    janitor::row_to_names(row_number = 1) %>%
    as.data.frame() %>%
    janitor::clean_names()
}

chi2_stats <- function(input = my_data, var1, var2) {
  # Pearson's chi-squared statistics (independence test)
  data_table <- # Create the contingency table
    table(input[[var1]], input[[var2]])
  if (all(dim(data_table) == c(2, 2))) {
    prop_result <- # Chi-squared test
      stats::chisq.test(x = data_table, simulate.p.value = T)
    p_value <- prop_result$p.value # Store the p-value
    effect_size <- # Compute the effect size
      effectsize::phi(x = input[[var1]], y = input[[var2]])[1, 1]
    my_power <- # Compute the statistical power
      pwr::pwr.chisq.test(
        w = effect_size,
        N = sum(data_table),
        df = 1,
        sig.level = 0.05
      )[[5]]
  } else {
    prop_result <- # Chi-squared test
      stats::chisq.test(x = data_table, simulate.p.value = T)
    p_value <- prop_result$p.value # Store the p-value
    effect_size <- # Compute the effect size
      effectsize::cramers_v(x = input[[var1]], y = input[[var2]])[1, 1]
    my_power <- # Compute the statistical power
      pwr::pwr.chisq.test(
        w = effect_size,
        N = sum(data_table),
        df = ncol(data_table) - 1,
        sig.level = 0.05
      )[[5]]
  }
  return(dplyr::lst(prop_result, p_value, effect_size, my_power))
}

anova_stats <- function(input = my_data, var1, var2) {
  # Analysis of variance statistics
  anova_results <- # ANOVA test
    stats::lm(as.numeric(as.factor(input[[var1]])) ~ input[[var2]]) %>%
    stats::anova()
  p_value <- anova_results[1, 5] # Store the p-value
  effect_size <- # Compute the effect size
    effectsize::eta_squared(anova_results, partial = FALSE)[1, 2]
  my_power <- # Compute the statistical power
    pwr::pwr.anova.test(
      k = input[[var1]] %>% unique %>% na.omit %>% length,
      n = length(var2) + 1,
      f = effect_size,
      sig.level = 0.05
    )[[5]]
  return(dplyr::lst(anova_results, p_value, effect_size, my_power))
}

# Plotting ----------------------------------------------------------------
# Functions to plot data
histograms <-
  function(input,
           features,
           graph_title = "",
           my_stats = "count") {
    # Plots multiple histograms based on data from a list
    mapply(
      function(my_dataset, my_feature) {
        # Single histogram plot function
        ggplot2::ggplot(
          # Create ggplot object
          data = as.data.frame(my_dataset),
          mapping = ggplot2::aes(x = my_dataset),
          stat = my_stats # Statistics to apply on the graph's data
        ) +
          ggplot2::geom_histogram(bins = 30) + # Add histogram graph
          ggplot2::xlab(label = my_feature) + # Add x labels
          ggplot2::ggtitle(label = paste(graph_title , my_feature)) # Add title
      },
      my_dataset = input[features],
      my_feature = features,
      SIMPLIFY = FALSE
    )
  }

barplots <-
  function(input = my_data,
           features,
           fill_feature = NULL,
           statistics_results,
           graph_title = "",
           my_stats = "count",
           graph_fill = TRUE) {
    # Plots multiple bar plots based on data from a list
    mapply(
      function(my_dataset, my_feature) {
        # Single bar plot function
        # Creating statistical values to print on plot
        my_effect_size <-
          statistics_results[[my_feature]]["effect_size"]
        my_p_value <- statistics_results[[my_feature]]["p_value"]
        my_power <- statistics_results[[my_feature]]["my_power"]
        if (graph_fill == FALSE) {
          # Bar plot without fill color
          mapping_data <- as.factor(my_dataset)
          my_plot <- ggplot2::ggplot(
            data = as.data.frame(my_dataset),
            mapping = ggplot2::aes(x = mapping_data),
            stat = my_stats
          )
        } else {
          mapping_data <- as.factor(my_dataset)
          my_plot <- ggplot2::ggplot(
            data = as.data.frame(my_dataset),
            mapping = ggplot2::aes(x = mapping_data, fill = input[[my_var]]),
            stat = my_stats
          )
        }
        # Adding elements to ggplot object
        my_plot +
          ggplot2::geom_bar() + # Adding bar plot
          {
            # Adding a fill legend if required
            if (graph_fill)
              ggplot2::labs(fill = col_questions[col_questions$col_name == fill_feature, "plot_name"])
          } +
          ggplot2::xlab(label = my_feature) +
          ggplot2::ggtitle(label = paste(col_questions[col_questions$col_name == my_feature, "plot_name"])) +
          # Adding descriptive values on bar plots
          ggplot2::geom_text(
            # Group count
            aes(label = paste0(
              ggplot2::after_stat(count),
              " / ",
              round(
                # Percentage values
                ggplot2::after_stat(count) * 100 / tapply(after_stat(count), ..x.., sum)[as.character(..x..)],
                digits = 1
              ),
              "%"
            )),
            position = ggplot2::position_stack(vjust = 0.5),
            stat = "count"
          ) +
          ggplot2::geom_text(
            # Total count
            ggplot2::aes(
              group = my_feature,
              y = after_stat(count),
              label = after_stat(count)
            ),
            position = ggplot2::position_dodge(width = 1),
            vjust = -0.5,
            stat = "count"
          ) +
          ggplot2::scale_x_discrete(na.translate = FALSE) +
          ggplot2::scale_y_continuous(expand = ggplot2::expansion(add = c(10, 35))) +
          # Adding statistical test results
          ggplot2::labs(caption = paste(
            "p-value: ",
            signif(x = as.numeric(my_p_value), digits = 5),
            "; eff size: ",
            signif(x = as.numeric(my_effect_size), digits = 5),
            "; pwr: ",
            signif(x = as.numeric(my_power), digits = 5)
          )) +
          scale_fill_manual(values = c("darkgrey", "lightgrey"))
      },
      my_dataset = input[features],
      my_feature = features,
      SIMPLIFY = FALSE
    )
  }
