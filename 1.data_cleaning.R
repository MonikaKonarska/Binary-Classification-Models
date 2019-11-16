library(tidyverse)
library(reshape2)
library(reshape)
library(scales)
source("functions.R")

dataCleaning <- function() {
  
  load(file.path(dataPath, "dataToTranTestValid.Rdata"))

  structureData <<- describe_variables(dataToTranTestValid)
  levelOfNA <- 0.80
  maxUniqueValues <- 30
  
  variablesWithalotNA         <- structureData$variable[which(structureData$p_numberOfNa >= levelOfNA )]
  variablesWithalotUniqueText <- structureData$variable[ which(structureData$type == 'character' & structureData$uniqueValues > maxUniqueValues)]
  variablesWithOneValue       <- structureData$variable[which(structureData$uniqueValues == 1 )]
  variablesToRemove           <- c(variablesWithalotNA, variablesWithalotUniqueText, variablesWithOneValue)
  variablesFromFeature        <<- c("sub_grade", "grade", "int_rate", "installment", "total_pymnt", "total_pymnt_inv", "total_rec_prncp", "total_rec_int", "total_rec_late_fee", "recoveries", "collection_recovery_fee", "last_pymnt_amnt", "initial_list_status")
  variablesAnotherToDelete    <<- c("id", "member_id", "issue_d", "loan_status", "funded_amnt_inv", "term", "verification_status", "funded_amnt", "group", "last_credit_pull_d", "collections_12_mths_ex_med", "acc_now_delinq")
  
  dataToTranTestValid_cleaned <- dataToTranTestValid %>%
    select(-variablesToRemove)
  
  dataToTranTestValid_cleaned <- dataToTranTestValid_cleaned[which(!dataToTranTestValid_cleaned$home_ownership %in% c("ANY", "NONE", "OTHER")), ]
  save(dataToTranTestValid_cleaned, file = file.path(dataPath, "dataToTranTestValid_cleaned.RData"))
}