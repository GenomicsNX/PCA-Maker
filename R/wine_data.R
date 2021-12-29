library(dplyr)

# Respones values
wine_values    <- read.csv("data/Pred_values.csv", header = FALSE)

# Sample labels
wine_samples   <- read.csv("data/Label_Wine_samples.csv", header = FALSE)

# Variable names
wine_variables <- read.csv("data/Label_Pred_values_IR.csv")

# Wine data
wine_data <- wine_values
colnames(wine_data) <- names(wine_variables)

write.csv(wine_data, "data/Wine_data.csv", row.names = FALSE)

# Wine region
region <- unname(substr(unlist(wine_samples), 0, 3))

region <- data.frame(Region = region)

write.csv(region, "data/Wine_region.csv", row.names = FALSE)
