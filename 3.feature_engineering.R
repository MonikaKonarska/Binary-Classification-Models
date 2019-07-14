library(tidyverse)
library(reshape2)
library(lubridate)
library(scales)
library(corrplot)
library(Information)

source("functions.R")
source("1.data_cleaning.R")

dataPath <- file.path(getwd(), "data")
load(file.path(dataPath, "dataToModeling.Rdata"))

dataTrain <- dataTrain[, which(!colnames(dataTrain) %in% c(variablesFromFeature, variablesAnotherToDelete))]

dataTrainWithNewFeatures <- dataTrain %>%
  mutate(delinq_2yrs_binary = factor(case_when(delinq_2yrs == 0 ~ 1,
                                        TRUE ~0), levels = c(0,1)),
         inq_last_6mths_cat = factor(case_when(inq_last_6mths == 0 ~ "0",
                                        inq_last_6mths == 1 ~ "1",
                                        inq_last_6mths >= 2 ~ "2+"), levels = c("0", "1", "2+")),
         mths_since_last_delinq_binary = factor(case_when(mths_since_last_delinq >= 1 ~ 1,
                                                          TRUE ~ 0), levels = c(0,1)),
         pub_rec_value_binary = factor(case_when(pub_rec == 0 ~ 0,
                                            TRUE ~1), levels = c(0,1)),
         month = substr(x = funded_loan_date, start = 1, stop = 7),
         month = factor(month, levels = sort(unique(month))),
         quarter = as.yearqtr(dataTrain$funded_loan_date)) %>%
  select(-c(funded_loan_date, earliest_cr_line_date, last_credit_pull_date))
         #quarter = factor(as.character(quarter), levels = sort(unique(quarter)))
        

dataTrainWithNewFeatures <- changeCharacter2FactorVariableWithLackGroup(data = dataTrainWithNewFeatures,
                                                                        typeOfLack = "n/a" )


