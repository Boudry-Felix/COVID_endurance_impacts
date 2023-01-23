# Informations ------------------------------------------------------------

# Title: Functions.R
# Author: Félix Boudry
# Contact: <felix.boudry@univ-perp.fr>
# License: Private
# Description: Functions used in COVID and endurance analysis.

# Functions ---------------------------------------------------------------
my_table_count <- function(input, my_colnames) {
  kable(x = count(df = input), col.names = my_colnames) %>%
    kable_styling(bootstrap_options = c("striped"),
                  full_width = FALSE)
}

my_table <- function(input, ...) {
  kable(x = input, ... = ...) %>%
    kable_styling(bootstrap_options = c("striped"),
                  full_width = FALSE)
}

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
  function(input,
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
          labs(caption = paste("p-value = ", signif(
            x = as.numeric(my_results$p_values[[my_colname]]), digits = 5
          ),
          "; eff size : ",
          signif(x = as.numeric(my_results$eff_values[[my_colname]], digits = 5))))
      },
      my_dataset = input[my_columns],
      my_colname = my_columns,
      SIMPLIFY = FALSE
    )
  }
