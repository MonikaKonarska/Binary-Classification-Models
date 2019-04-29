
library(data.table)
library(tidyverse)
library(mlr)
library(reshape2)
library(reshape)
library(scales)
library(openxlsx)
library(tree)

source("prepare_data_func.R")
source("graphics_data_func.R")


saveResultsPath <- file.path(getwd(), "dataResults")
dataPath <- file.path(getwd(), "data")
dictionary <- read.xlsx(xlsxFile = file.path(dataPath, "LCDataDictionary.xlsx"), sheet = "LoanStats")
data <- fread(file.path(dataPath, "loan.csv"))
data <- setDF(data)



