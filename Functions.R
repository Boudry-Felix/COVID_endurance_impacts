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
df_encode <- function(input = my_data, list_names) {
  # Encode (labeling entire dataframes)
  convert_dic <- dplyr::lst()
  encoded_data <- lapply(
    X = input,
    FUN = function(my_col) {
      if (is.numeric(x = my_col)) {
        my_col
      } else {
        label <- CatEncoders::LabelEncoder.fit(y = my_col)
        convert_dic <<- append(x = convert_dic, values = label)
        CatEncoders::transform(enc = label, my_col)
      }
    }
  ) %>% as.data.frame()
  output <- dplyr::lst(convert_dic, encoded_data)
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
      plyr::count(df = input[[my_dataframe]]) %>%
        t() %>%
        janitor::row_to_names(row_number = 1) %>%
        as.data.frame() %>%
        janitor::clean_names()
    }
  ) %>%
    `names<-`(value = my_vars)
}

prop_stats <- function(input = my_data, var1, var2) {
  data_table <- table(input[[var1]], input[[var2]])
  prop_result <-
    stats::chisq.test(x = data_table, simulate.p.value = T)
  p_value <- prop_result$p.value
  effect_size <-
    effectsize::phi(x = input[[var1]], y = input[[var2]])[1, 1]
  my_power <- pwr::pwr.chisq.test(
    w = effect_size,
    N = sum(data_table),
    df = 2,
    sig.level = 0.05
  )[[5]]
  return(dplyr::lst(prop_result, p_value, effect_size, my_power))
}

anova_stats <- function(input = my_data, var1, var2) {
  anova_results <-
    stats::lm(as.numeric(as.factor(input[[var1]])) ~ input[[var2]]) %>%
    stats::anova()
  p_value <- anova_results[1, 5]
  effect_size <-
    effectsize::eta_squared(anova_results, partial = FALSE)[1, 2]
  my_power <- pwr::pwr.anova.test(
    k = input[[var1]] %>% unique %>% na.omit %>% length,
    n = length(var2) + 1,
    f = effect_size,
    sig.level = 0.05
  )[[5]]
  return(dplyr::lst(anova_results, p_value, effect_size, my_power))
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
        ggplot2::ggplot(
          data = as.data.frame(my_dataset),
          mapping = ggplot2::aes(x = my_dataset),
          stat = my_stats
        ) +
          ggplot2::geom_histogram(bins = 30) +
          ggplot2::xlab(label = my_colname) +
          ggplot2::ggtitle(label = paste(graph_title , my_colname))
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
          my_plot <- ggplot2::ggplot(
            data = as.data.frame(my_dataset),
            mapping = ggplot2::aes(x = as.factor(my_dataset)),
            stat = my_stats
          )
        } else if (factorize_val & graph_fill) {
          my_plot <- ggplot2::ggplot(
            data = as.data.frame(my_dataset),
            mapping = ggplot2::aes(x = as.factor(my_dataset), fill = input[[my_var]]),
            stat = my_stats
          )
        } else if (graph_fill & factorize_val == FALSE) {
          my_plot <- ggplot2::ggplot(
            data = as.data.frame(my_dataset),
            mapping = ggplot2::aes(x = as.factor(my_dataset), fill = input[[my_var]]),
            stat = my_stats
          )
        } else {
          my_plot <- ggplot2::ggplot(
            data = as.data.frame(my_dataset),
            mapping = ggplot2::aes(x = my_dataset),
            stat = my_stats
          )
        }
        my_plot +
          ggplot2::geom_bar() +
          {
            if (graph_fill)
              ggplot2::labs(fill = my_var)
          } +
          ggplot2::xlab(label = my_colname) +
          ggplot2::ggtitle(label = paste(graph_title, my_colname)) +
          ggplot2::geom_text(
            aes(label = paste0(
              ggplot2::after_stat(count),
              " / ",
              round(
                ggplot2::after_stat(count) * 100 / tapply(after_stat(count), ..x.., sum)[as.character(..x..)],
                digits = 1
              ),
              "%"
            )),
            position = ggplot2::position_stack(vjust = 0.5),
            stat = "count"
          ) +
          ggplot2::geom_text(
            ggplot2::aes(
              group = my_colname,
              y = after_stat(count),
              label = after_stat(count)
            ),
            position = ggplot2::position_dodge(width = 1),
            vjust = -0.5,
            stat = "count"
          ) +
          ggplot2::scale_x_discrete(na.translate = FALSE) +
          ggplot2::scale_y_continuous(expand = ggplot2::expansion(add = c(10, 35))) +
          ggplot2::labs(caption = paste(
            "p-value: ",
            signif(x = as.numeric(stat_results[[my_colname]]["p_value"]),
                   digits = 5),
            "; eff size: ",
            signif(x = as.numeric(stat_results[[my_colname]]["effect_size"], digits = 5)),
            "; pwr: ",
            signif(x = as.numeric(stat_results[[my_colname]]["my_power"], digits = 5))
          ))
      },
      my_dataset = input[my_columns],
      my_colname = my_columns,
      SIMPLIFY = FALSE
    )
  }
