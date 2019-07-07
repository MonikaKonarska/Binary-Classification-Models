library(tidyverse)
library(grid)
library(gridExtra)

source("functions.R")


dataPath <- file.path(getwd(), "data")
load(file.path(dataPath, "dataToModeling.Rdata"))

columnNames <- names(dataTrain)
numericColumns <- columnNames[purrr::map_lgl(dataTrain, is.numeric)] 
numericColumns <- numericColumns[!numericColumns %in% c('id', 'member_id')]


visualizeDensityVariablesGroupedByCategories(data = dataTrain,
                                             names_numeric_variables = numericColumns,
                                             names_variable_to_group = "target")








