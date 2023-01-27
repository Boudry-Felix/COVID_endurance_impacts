# Informations ------------------------------------------------------------
# Title: Functions.R
# Author: Félix Boudry
# Contact: <felix.boudry@univ-perp.fr>
# License: Private
# Description: Functions used in COVID and endurance analysis.

## Libraries --------------------------------------------------------------
## List of used libraries.
require(tidyverse)
require(kableExtra)
require(plyr)
require(gridExtra)
require(janitor)
require(effectsize)

# Tables ------------------------------------------------------------------
# Functions used to print tables
my_table <- function(input, ...) {
  # Custom kable table
  kable(x = input, ... = ...) %>%
    kable_styling(bootstrap_options = c("striped"),
                  full_width = FALSE)
}

# Data transformation -----------------------------------------------------
# Functions that transform, recompute or re-encode data
df_encode <- function(input = my_data, list_names) {
  # Encode (labeling entire dataframes)
  convert_dic <- lst()
  encoded_data <- lapply(
    X = input,
    FUN = function(my_col) {
      if (is.numeric(x = my_col)) {
        my_col
      } else {
        label <- LabelEncoder.fit(y = my_col)
        convert_dic <<- append(x = convert_dic, values = label)
        transform(enc = label, my_col)
      }
    }
  ) %>% as.data.frame()
  output <- lst(convert_dic, encoded_data)
  if (!missing(x = list_names)) {
    names(output) <- list_names
  }
  return(output)
}

# Statistic computing -----------------------------------------------------
# Functions to process data statistically
my_var_counting <- function(input = my_data, my_vars) {
  lapply(
    X = my_vars,
    FUN = function(my_dataframe) {
      count(df = input[[my_dataframe]]) %>%
        t() %>%
        row_to_names(row_number = 1) %>%
        as.data.frame() %>%
        clean_names()
    }
  ) %>%
    `names<-`(value = my_vars)
}

prop_stats <- function(input = my_data, var1, var2) {
  data_table <- table(my_data[[var1]], my_data[[var2]])
  prop_result <- data_table %>%
    prop.test(x = ., n = NULL, correct = FALSE)
  p_value <- prop_result$p.value
  effect_size <- phi(x = my_data[[var1]], y = my_data[[var2]])[1, 1]
  my_power <- pwr.chisq.test(
    w = effect_size,
    N = sum(data_table),
    df = as.numeric(prop_result[[2]]),
    sig.level = 0.05
  )[[5]]
  return(lst(prop_result, p_value, effect_size, my_power))
}

anova_stats <- function(input = my_data, var1, var2) {
  anova_results <-
    lm(as.numeric(as.factor(input[[var1]])) ~ input[[var2]]) %>%
    anova()
  p_value <- anova_results[1, 5]
  effect_size <- eta_squared(anova_results, partial = FALSE)[1, 2]
  my_power <- pwr.anova.test(
    k = input[[var1]] %>% unique %>% na.omit %>% length,
    n = length(var2) + 1,
    f = effect_size,
    sig.level = 0.05
  )[[5]]
  return(lst(anova_results, p_value, effect_size, my_power))
}

# Plotting ----------------------------------------------------------------
# Functions to plot data
hist_plot_multi <-
  function(input,
           my_columns,
           graph_title = "",
           my_stats = "count") {
    mapply(
      FUN = function(my_dataset, my_colname) {
        ggplot(
          data = as.data.frame(my_dataset),
          mapping = aes(x = my_dataset),
          stat = my_stats
        ) +
          geom_histogram() +
          xlab(label = my_colname) +
          ggtitle(label = paste(graph_title , my_colname))
      },
      my_dataset = input[my_columns],
      my_colname = my_columns,
      SIMPLIFY = FALSE
    )
  }

bar_plot_multi <-
  function(input = my_data,
           my_columns,
           graph_title = "",
           my_stats = "count",
           graph_fill = TRUE,
           factorize_val = FALSE) {
    mapply(
      FUN = function(my_dataset, my_colname) {
        if (factorize_val & graph_fill == FALSE) {
          my_plot <- ggplot(
            data = as.data.frame(my_dataset),
            mapping = aes(x = as.factor(my_dataset)),
            stat = my_stats
          )
        } else if (factorize_val & graph_fill) {
          my_plot <- ggplot(
            data = as.data.frame(my_dataset),
            mapping = aes(x = as.factor(my_dataset), fill = input[[gen_env$my_var]]),
            stat = my_stats
          )
        } else if (graph_fill & factorize_val == FALSE) {
          my_plot <- ggplot(
            data = as.data.frame(my_dataset),
            mapping = aes(x = as.factor(my_dataset), fill = input[[gen_env$my_var]]),
            stat = my_stats
          )
        } else {
          my_plot <- ggplot(
            data = as.data.frame(my_dataset),
            mapping = aes(x = my_dataset),
            stat = my_stats
          )
        }
        my_plot +
          geom_bar() +
          {
            if (graph_fill)
              labs(fill = gen_env$my_var)
          } +
          xlab(label = my_colname) +
          ggtitle(label = paste(graph_title, my_colname)) +
          geom_text(aes(label = paste0(
            after_stat(count),
            " / ",
            round(
              after_stat(count) * 100 / tapply(..count.., ..x.., sum)[as.character(..x..)],
              digits = 1
            ),
            "%"
          )),
          position = position_stack(vjust = 0.5),
          stat = "count") +
          geom_text(
            aes(
              group = my_colname,
              y = ..count..,
              label = ..count..
            ),
            position = position_dodge(),
            vjust = -0.5,
            stat = "count"
          ) +
          scale_x_discrete(na.translate = FALSE) +
          scale_y_continuous(expand = expansion(add = c(10, 35))) +
          labs(caption = paste(
            "p-value: ",
            signif(
              x = as.numeric(my_results$stat_results[[my_colname]]["p_value"]),
              digits = 5
            ),
            "; eff size: ",
            signif(x = as.numeric(my_results$stat_results[[my_colname]]["effect_size"], digits = 5)),
            "; pwr: ",
            signif(x = as.numeric(my_results$stat_results[[my_colname]]["my_power"], digits = 5))
          ))
      },
      my_dataset = input[my_columns],
      my_colname = my_columns,
      SIMPLIFY = FALSE
    )
  }
