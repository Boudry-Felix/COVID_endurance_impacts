# Informations ------------------------------------------------------------

# Title: Import.R
# Author: Félix Boudry
# Contact: <felix.boudry@univ-perp.fr>
# License: Private
# Description: Import questionnaire data, clean and labelise them.

# Configuration -----------------------------------------------------------

## Libraries --------------------------------------------------------------
## List of used libraries.
require(janitor)
require(dplyr)
require(CatEncoders)

## Global vectors ---------------------------------------------------------
## Define vectors used in entire script.
rm(list = setdiff(x = ls(), y = c("gen_env"))) # Clean environment
my_data <- lst()

# Import ------------------------------------------------------------------
# Import data sets
my_data_all <- read.csv(file = "Data/Answers_all.csv")
my_data_completed <- read.csv(file = "Data/Answers_completed.csv")
my_colnames <- read.csv(file = "Data/col_question.csv")

# Format ------------------------------------------------------------------
# Formating data for analysis
my_data <- # Put data in list and clean it
  list("all" = my_data_all, "completed" = my_data_completed) %>%
  lapply(FUN = clean_names) %>%
  lapply(FUN =  remove_empty, which = c("rows", "cols"))

my_data <- # Change colnames from question to shorter name
  lapply(X = my_data,
         FUN = `colnames<-`, my_colnames$col_name)

# Encoding ----------------------------------------------------------------
# Encode answers as labels
my_counter <- 0
for (my_dataframe in my_data) {
  # Encode data to numerical values if not already numerical
  my_counter <- my_counter + 1
  convert_dic <- lst()
  assign(
    x = paste0("encoded_", names(x = my_data)[my_counter]),
    value = lapply(
      X = my_dataframe,
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
  )
  assign(x = paste0("convert_dic_", names(x = my_data)[my_counter]), value = convert_dic)
}

encoded_data <-
  list("all" = encoded_all, "completed" = encoded_completed)
convert_dic <-
  list("all" = convert_dic_all, "completed" = convert_dic_completed)

my_data <- lst(raw_data = my_data, encoded_data, convert_dic, my_colnames)

rm(list = setdiff(x = ls(), y = c("my_data", "gen_env")))

# Export data -------------------------------------------------------------
# Save environment data
if (!dir.exists(paths = "Env")) {
  dir.create(path = "Env")
}
save.image(file = "Env/import.RData")
