library(tidyverse)
library(reshape2)
source("functions.R")

featurEngineering <- function() {

  load(file.path(dataPath, "dataToModeling.Rdata"))
  
  dataTrain <- dataTrain[, which(!colnames(dataTrain) %in% c(variablesFromFeature, variablesAnotherToDelete))]
  
  dataTrainWithNewFeatures <- dataTrain %>%
    mutate(delinq_2yrs_binary = factor(case_when(delinq_2yrs == 0 ~ 1,
                                                 TRUE ~0), levels = c(0,1)),
           inq_last_6mths_cat = factor(case_when(inq_last_6mths == 0 ~ "0",
                                                 inq_last_6mths == 1 ~ "1",
                                                 inq_last_6mths >= 2 ~ "2+"), levels = c("0", "1", "2+")),
           if_inq_last_6mths_cat = factor(case_when(inq_last_6mths == 0 ~ "0",
                                                   inq_last_6mths >= 1 ~ "2+"), levels = c("0", "2+")),
           mths_since_last_delinq_binary = factor(case_when(mths_since_last_delinq >= 1 ~ 1,
                                                            TRUE ~ 0), levels = c(0,1)),
           pub_rec_value_binary = factor(case_when(pub_rec == 0 ~ 0,
                                                   TRUE ~1), levels = c(0,1)),
           month = substr(x = funded_loan_date, start = 1, stop = 7),
           month = factor(month, levels = sort(unique(month))),
           number_of_quarter = lubridate::quarter(funded_loan_date),
           year_of_funded_loan = lubridate::year(funded_loan_date),
           quarter = paste(year_of_funded_loan, "Q", number_of_quarter, sep = "")) %>%
    select(-c(funded_loan_date, earliest_cr_line_date, last_credit_pull_date, number_of_quarter, year_of_funded_loan))
  
  dataTrainWithNewFeatures <- changeCharacter2FactorVariableWithLackGroup(data = dataTrainWithNewFeatures,
                                                                          typeOfLack = "n/a")
  return(dataTrainWithNewFeatures)
}


# kategoryzacja zmiennej przez random forest, svm, 



