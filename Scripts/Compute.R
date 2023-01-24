# Informations ------------------------------------------------------------

# Title: Compute.R
# Author: Félix Boudry
# Contact: <felix.boudry@univ-perp.fr>
# License: Private
# Description: Compute new values to analyze

my_data <- my_population

duration_to_training2 <-
  my_data[, c("endurance_sport", "duration_to_training")]
duration_to_training2 <-
  as.data.frame(sapply(duration_to_training2, function(x)
    gsub("Reprise immédiate", "court", x)))
duration_to_training2 <-
  as.data.frame(sapply(duration_to_training2, function(x)
    gsub("Quelques jours après", "court", x)))
duration_to_training2 <-
  as.data.frame(sapply(duration_to_training2, function(x)
    gsub(".*mois.*", "long", x)))
duration_to_training2 <-
  as.data.frame(sapply(duration_to_training2, function(x)
    gsub(".*semaine.*", "long", x)))

my_data$duration_to_training2 <-
  duration_to_training2$duration_to_training

difficulties_duration2 <-
  my_data[, c("endurance_sport", "difficulties_duration")]
difficulties_duration2 <-
  as.data.frame(sapply(difficulties_duration2, function(x)
    gsub("Aucune", "court", x)))
difficulties_duration2 <-
  as.data.frame(sapply(difficulties_duration2, function(x)
    gsub("1 semaine", "court", x)))
difficulties_duration2 <-
  as.data.frame(sapply(difficulties_duration2, function(x)
    gsub(".*mois.*", "long", x)))
difficulties_duration2 <-
  as.data.frame(sapply(difficulties_duration2, function(x)
    gsub(".*semaine.*", "long", x)))

my_data$difficulties_duration2 <-
  difficulties_duration2$difficulties_duration

time_to_normal_training_volume2 <-
  my_data[, c("endurance_sport", "time_to_normal_training_volume")]
time_to_normal_training_volume2 <-
  as.data.frame(sapply(time_to_normal_training_volume2, function(x)
    gsub("1 semaine", "court", x)))
time_to_normal_training_volume2 <-
  as.data.frame(sapply(time_to_normal_training_volume2, function(x)
    gsub("1 mois", "court", x)))
time_to_normal_training_volume2 <-
  as.data.frame(sapply(time_to_normal_training_volume2, function(x)
    gsub(".*mois.*", "long", x)))
time_to_normal_training_volume2 <-
  as.data.frame(sapply(time_to_normal_training_volume2, function(x)
    gsub(".*semaine.*", "long", x)))

my_data$time_to_normal_training_volume2 <-
  time_to_normal_training_volume2$time_to_normal_training_volume

train_vol_modif <-
  my_data[, c("endurance_sport", "modified_training_volume")]
train_vol_modif <-
  as.data.frame(sapply(train_vol_modif, function(x)
    gsub(".*diminué.*", "diminished", x)))
train_vol_modif <-
  as.data.frame(sapply(train_vol_modif, function(x)
    gsub(".*augmenté.*", "augmented/unchanged", x)))
train_vol_modif <-
  as.data.frame(sapply(train_vol_modif, function(x)
    gsub(".*inchangé.*", "augmented/unchanged", x)))

my_data$train_vol_modif <-
  train_vol_modif$modified_training_volume
