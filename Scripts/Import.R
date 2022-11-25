# Informations ------------------------------------------------------------

# Title: Import.R
# Author: Félix Boudry
# Contact: <felix.boudry@univ-perp.fr>
# License: Private
# Description: Import questionnaire data, clean and labelise them.

# Configuration -----------------------------------------------------------

## Libraries --------------------------------------------------------------
## List of used libraries.
require(tibble)
require(dplyr)
require(CatEncoders)

## Global vectors ---------------------------------------------------------
## Define vectors used in entire script.
rm(list = ls()) # Clean environment
my_data <- lst()

# Import ------------------------------------------------------------------
# Import data sets
my_data_all <- read.csv(file = "Data/Answers_all.csv")
my_data_completed <- read.csv(file = "Data/Answers_completed.csv")

my_data <- # Put data in list and clean it
  list("all" = my_data_all, "completed" = my_data_completed) %>%
  lapply(clean_names) %>%
  na.omit()

my_counter <- 0
for (my_dataframe in my_data) {
  my_counter <- my_counter + 1
  convert_dic <- lst()
  assign(
    x = paste0("encoded_", names(x = my_data)[my_counter]),
    value = lapply(
      X = my_dataframe,
      FUN = function(my_col) {
        label <- LabelEncoder.fit(y = my_col)
        convert_dic <<- append(x = convert_dic, values = label)
        transform(enc = label, my_col)
      }
    ) %>% as.data.frame()
  )
  assign(x = paste0("convert_dic_", names(x = my_data)[my_counter]), value = convert_dic)
}

encoded_data <-
  list("all" = encoded_all, "completed" = encoded_completed)
convert_dic <-
  list("all" = convert_dic_all, "completed" = convert_dic_completed)

rm(list = setdiff(ls(), c("convert_dic", "encoded_data", "my_data")))
