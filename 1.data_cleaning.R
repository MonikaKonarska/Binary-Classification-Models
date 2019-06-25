library(tidyverse)
library(reshape2)
library(reshape)
library(scales)
source("functions.R")

dataPath <- file.path(getwd(), "data")
load(file.path(dataPath, "dataToTranTestValid.Rdata"))


# Data cleaning
structureData <- describe_variables(dataToTranTestValid)
levelOfNA <- 0.80
maxUniqueValues <- 30

variablesWithalotNA         <- structureData$variable[which(structureData$p_numberOfNa >= levelOfNA )]
variablesWithalotUniqueText <- structureData$variable[ which(structureData$type == 'character' & structureData$uniqueValues > maxUniqueValues)]
variablesWithOneValue       <- structureData$variable[which(structureData$uniqueValues == 1 )]
variablesToRemove           <- c(variablesWithalotNA, variablesWithalotUniqueText, variablesWithOneValue)
variablesFromFeature        <- c("sub_grade", "grade", "int_rate", "installment", "total_pymnt", "total_pymnt_inv", "total_rec_prncp", "total_rec_int", "total_rec_late_fee", "recoveries", "collection_recovery_fee")
variablesAnotherToDelete    <- c("id", "member_id", "issue_id", "loan_status")

dataToTranTestValid_cleaned <- dataToTranTestValid %>%
  select(-variablesToRemove)

dataToTranTestValid_cleaned <- dataToTranTestValid_cleaned[which(!dataToTranTestValid_cleaned$home_ownership == "ANY"), ]


dataTrain  <- dataToTranTestValid_cleaned %>% filter(group == "train")
dataTest   <- dataToTranTestValid_cleaned %>% filter(group == "test")
dataValid  <- dataToTranTestValid_cleaned %>% filter(group == "valid")

save(dataTrain, dataTest, dataValid, file = file.path(dataPath, "dataToModeling.RData"))
