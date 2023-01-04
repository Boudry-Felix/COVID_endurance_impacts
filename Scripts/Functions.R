# Functions ---------------------------------------------------------------
my_table_count <- function(input, my_colnames) {
  kable(x = count(df = input), col.names = my_colnames) %>%
    kable_styling(bootstrap_options = c("striped"))
}
my_table <- function(input) {
  kable(x = input) %>%
    kable_styling(bootstrap_options = c("striped"))
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
           graph_fill = TRUE) {
    mapply(
      FUN = function(my_dataset, my_colname) {
        ggplot(
          data = as.data.frame(my_dataset),
          mapping = aes(x = my_dataset),
          stat = my_stats
        ) +
          geom_bar() +
          {if (graph_fill) geom_bar(mapping = aes(fill = input[[gen_env$my_var]]))} +
          {if (graph_fill) labs(fill = gen_env$my_var)} +
          xlab(label = my_colname) +
          ggtitle(label = paste(graph_title , my_colname))
      },
      my_dataset = input[my_columns],
      my_colname = my_columns,
      SIMPLIFY = FALSE
    )

  }
