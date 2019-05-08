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

structureData <- describe_variables(data)
levelOfNA <- 0.95
maxUniqueValues <- 30

variablesWithalotNA         <- structureData$variable[which(structureData$p_numberOfNa >= levelOfNA )]
variablesWithalotUniqueText <- structureData$variable[ which(structureData$type == 'character' & structureData$uniqueValues > maxUniqueValues)]
variablesWithalotUniqueText <- variablesWithalotUniqueText[!variablesWithalotUniqueText %in% c("last_credit_pull_d", "earliest_cr_line", "issue_d")]
variablesWithOneValue       <- structureData$variable[which(structureData$uniqueValues == 1 )]
variablesFromFeature        <- c("next_pymnt_d", "installment", "int_rate", "term", "recoveries", "collection_recovery_fee",  "last_pymnt_amnt", "grade", "term")
variablesAnotherToDelete    <- c("loan_status","id", "member_id", "loan_amnt", "funded_amnt_inv", "out_prncp", "out_prncp_inv", "total_pymnt", "total_pymnt_inv", "total_rec_prncp", "total_rec_int", "total_rec_late_fee" )


variables <- names(data)
variablesToGet <- variables[!variables %in% c(variablesWithalotNA, variablesWithalotUniqueText,  variablesFromFeature, variablesAnotherToDelete, variablesWithOneValue)]  
dataWork <- data[, variablesToGet]

dataWork <- dataWork %>%
  mutate(funded_loan_date = convert_date_from_month_year(issue_d),
         earliest_cr_line_date = convert_date_from_month_year(earliest_cr_line),
         last_credit_pull_date = convert_date_from_month_year(last_credit_pull_d)) %>%
  select(-c("issue_d","earliest_cr_line", "last_credit_pull_d"))

dataWork <- changeCharacter2FactorVariableWithLackGroup(dataWork)

variablesWithLargeCategories <- findLargeCategories(data = dataWork[,which(names(dataWork) != "target")], minLevelOfLargeCategoryInAll = 0.9)
dataWork <- dataWork %>% select(-c(variablesWithLargeCategories) )

