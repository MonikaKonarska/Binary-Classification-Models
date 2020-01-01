
source(file.path(functionPath,"functions.R"))

removeOutliers <- function(data_set) {
  library(purrr)
  numeric_columns <- names(data_set)[map_lgl(data_set, is.numeric)]
  removedOtliersFromTrainingData <- integer(length = length(numeric_columns))
  names(removedOtliersFromTrainingData) <- numeric_columns
  
  for (variable in numeric_columns) {
    outliers <- !detect_not_outliers(data_set, column_name = variable)
    number_otliers <- sum(outliers)
    removedOtliersFromTrainingData[[variable]] <- number_otliers
    data_set <- data_set[!outliers, ]
  }
  print(removedOtliersFromTrainingData)
  return(data_set)
}




