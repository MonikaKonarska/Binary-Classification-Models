dataPath                          <<- file.path(getwd(), "data")
folderToSavecalculations          <<- file.path(getwd(), "calculations")
folderToSavePlots                 <<- file.path(getwd(), "plots")
codePath                          <<- file.path(getwd(), "code")
functionPath                      <<- file.path(codePath, "functions")
folderToSavePlotsSelectedFeatures <<- file.path(folderToSavePlots, "selectedFeatures")

source(file.path(codePath, "0a_importData.R"))
source(file.path(codePath,"0b_choose_time_series_to_modeling.R"))
source(file.path(codePath,"1_data_cleaning.R"))
source(file.path(codePath,"2_sampling.R"))
source(file.path(codePath,"3_feature_engineering.R"))
source(file.path(functionPath, "remove_outliers.R"))
source(file.path(codePath,"4_feature_selection.R"))

data <- importData()
data <- data %>% filter(term == "36 months") 
chooseTimeSeriesToModeling()
dataCleaning()
samplingTrainTestValid()
dataTrainWithNewFeatures <<- featurEngineering(data = dataTrain)
dataTrainWithNewFeatures <<- removeOutliers(data_set = dataTrainWithNewFeatures)
save(dataTrainWithNewFeatures, file = file.path(dataPath, "dataTrainWithNewFeatures.Rdata"))
featureSelection()








