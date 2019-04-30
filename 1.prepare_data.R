
library(data.table)
library(tidyverse)
library(mlr)
library(reshape2)
library(reshape)
library(scales)
library(openxlsx)
library(tree)

source("functions.R")



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



structureData <- DescribeVariables(data)

levelOfNA <- 0.95
pOfUniqueValues <- 0.30

removeNas <- structureData$variable[which(structureData$p_numberOfNa >= levelOfNA )]
removeCharacters <- structureData$variable[ which(structureData$type == 'character' & structureData$p_uniqueValues > pOfUniqueValues)]
removeAnother <- c("loan_status","id", "member_id", "desc", "loan_amnt", "funded_amnt_inv","zip_code","addr_state", "out_prncp", "out_prncp_inv","total_pymnt","total_pymnt_inv","total_rec_prncp","total_rec_int","total_rec_late_fee" )
removeAnotherFromFuture <- c( "next_pymnt_d","installment","int_rate" ,"grade", "sub_grade","recoveries","collection_recovery_fee","last_pymnt_d","last_pymnt_amnt" )

variablesToGet <- names(data)
variablesToGet <- variablesToGet[!variablesToGet %in% c(removeNas, removeCharacters,  removeAnother, removeAnotherFromFuture)]  

dataWork <- data[, variablesToGet]






