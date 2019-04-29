
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


data$target <- factor(case_when(data$loan_status == "Fully Paid"  ~ 0,
                                data$loan_status == "Charged Off" ~ 1,
                                data$loan_status == "Late (31-120 days)" ~ 1,
                                data$loan_status == "Default Does not meet the credit policy. Status:Charged Off" ~ 1), levels = c(0, 1))
data <- data[which(!is.na(data$target)), ]

