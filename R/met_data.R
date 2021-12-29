mean_met_data <- read.csv("data/Means_met_data.csv")

set.seed(10) # For reproducibility

met_quantity_data <- purrr::map2(
  mean_met_data$MEAN, mean_met_data$SD, rnorm, n = 3
) 
met_quantity_data <- signif(unlist(met_quantity_data), 3)

met_data <- data.frame(
  MET  = rep(mean_met_data$MET, each = 3),
  TIME = rep(mean_met_data$TIME, each = 3),
  SAMPLE = rep(1:3), # Replication number
  QT   = met_quantity_data
) 

write.csv(met_data, "data/Met_data.csv")

met_qt_data <- tidyr::pivot_wider(met_data, names_from = MET, values_from = QT)
met_qt_data <- dplyr::select(met_qt_data, -TIME, -SAMPLE)

write.csv(met_qt_data, "data/Met_quantities_data.csv", row.names = FALSE)

met_time <- dplyr::select(met_data, TIME)

write.csv(met_time[1:24, ], "data/Met_time.csv", row.names = FALSE)
