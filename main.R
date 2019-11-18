source("0a.importData.R")
source("0b.choose_time_series_to_modeling.R")
source("1.data_cleaning.R")
source("2.sampling.R")
source("3.feature_engineering.R")
source("outliers_detection_and_correlations.R")
#source("4.feature_selection.R")

data <- importData()
chooseTimeSeriesToModeling()
dataCleaning()
samplingTrainTestValid()
dataTrainWithNewFeatures <<- featurEngineering()
dataTrainWithNewFeatures <<- removeOutliers(data_set = dataTrainWithNewFeatures)





