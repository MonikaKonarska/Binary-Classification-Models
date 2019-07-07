library(tidyverse)
library(reshape2)
library(lubridate)
library(scales)

source("functions.R")

dataPath <- file.path(getwd(), "data")
load(file.path(dataPath, "dataToModeling.Rdata"))



# Feature engineering / feature selection only on training data, EDA analyst only from train data
 xxxTmp <- changeCharacter2FactorVariableWithLackGroup(data = dataReducedByVariables,  typeOfLack = "n/a" )

 xxxTmp$time_credit_history_year <- round((dataReducedByVariables$funded_loan_date- dataReducedByVariables$earliest_cr_line_date)/365, 2)



# feature selection: https://machinelearningmastery.com/feature-selection-with-the-caret-r-package/


dataTrain$month <- substr(dataTrain$funded_loan_date, start = 1, stop = 7) # moze nie month ale busket ?
dataTrain$month <- factor(dataTrain$month, levels = sort(unique(dataTrain$month)))
#dataTrain <- dataTrain %>% select(-c("mths_since_last_record", "mths_since_last_major_derog", "mths_since_last_delinq"))


# in all samples (train, test, vaid, out of time) the attributes: NONE, OTHER, ANY have got a very small percentage so I decide to remove it from all data
dataTrain <- dataTrain %>% filter(!home_ownership %in% c('ANY', 'NONE', 'OTHER'))
dataTrain$home_ownership <- factor(dataTrain$home_ownership, levels = sort(unique(dataTrain$home_ownership)))

dataTrain <- dataTrain %>% mutate(purpose = ifelse(purpose %in% c('educational', 'renewable_energy'),'other', as.character(purpose)))
dataTrain$purpose <- factor(dataTrain$purpose, levels = sort(unique(dataTrain$purpose)))
  
dataTrain$emp_length <- ifelse(as.character(dataTrain$emp_length) == 'n/a', 'LACK', as.character(dataTrain$emp_length))
dataTrain$emp_length <- factor(dataTrain$emp_length, levels = sort(unique(dataTrain$emp_length)))


dataWork <- changeCharacter2FactorVariableWithLackGroup(data = dataWork,  typeOfLack = "n/a" )

# find to large percent of attribute in factor variables
factorColumns <- names(select_if(.tbl =  dataWork, is.factor))
attributesOfVariables <- sapply(factorColumns, function(x) prop.table(table(dataWork[[x]])))

variablesWithLargeAttribute <- c("verification_status_joint", "application_type", "pymnt_plan")
dataWork <- dataWork %>% select(-one_of(variablesWithLargeAttribute))








