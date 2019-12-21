library(tidyverse)
library(reshape2)
source("functions.R")
load(file.path(dataPath, "dataToModeling.Rdata"))

featurEngineering <- function(data) {
  
  data <- data[, which(!colnames(data) %in% c(variablesFromFeature, variablesAnotherToDelete))]
  
  dataTrainWithNewFeatures <- data %>%
    mutate(if_delinq_in2yrs = factor(case_when(delinq_2yrs == 0 ~ 0, TRUE ~ 1), levels = c(0, 1)),
           
           if_delinq_in_last_year = factor(case_when(is.na(mths_since_last_delinq) ~ "LACK",
                                               mths_since_last_delinq <= 12 ~ "1",
                                               TRUE ~ "0")),
           if_delinq_ever = factor(case_when(is.na(mths_since_last_delinq) ~ 0, TRUE ~ 1)),
           if_inq_in_last_6moths = factor(case_when(inq_last_6mths == "0" ~ 0, TRUE ~ 1)),
           inq_last_6mths_grouped = factor(case_when(inq_last_6mths >= 4 ~ "4+",
                                                      TRUE ~ as.character(inq_last_6mths)), levels = c("0","1","2","3","4+")),
           if_ever_pub_rec = factor(case_when(pub_rec == 0 ~ 0, TRUE ~1), levels = c(0,1)),
           if_purpose_debt_consolidation = factor(case_when(purpose == 'debt_consolidation' ~ 1, TRUE ~ 0), levels = c(0,1)),
           if_employment_more_10years = factor(case_when(emp_length == '10+ years' ~ 1, TRUE ~ 0)),
           month = substr(x = funded_loan_date, start = 1, stop = 7),
           month = factor(month, levels = sort(unique(month))),
           number_of_quarter = lubridate::quarter(funded_loan_date),
           year_of_funded_loan = lubridate::year(funded_loan_date),
           quarter = paste(year_of_funded_loan, "Q", number_of_quarter, sep = "")) %>%
    dplyr::select(-c(funded_loan_date, earliest_cr_line_date, last_credit_pull_date, number_of_quarter, year_of_funded_loan, delinq_2yrs, mths_since_last_major_derog, mths_since_last_delinq, inq_last_6mths, pub_rec))
  
  dataTrainWithNewFeatures <- changeCharacter2FactorVariableWithLackGroup(data = dataTrainWithNewFeatures,
                                                                          typeOfLack = "LACK")
  return(dataTrainWithNewFeatures)
}









