library(data.table)
library(tidyverse)
library(mlr)
library(reshape2)
library(reshape)
library(scales)
library(openxlsx)
source("functions.R")


saveResultsPath <- file.path(getwd(), "dataResults")
dataPath <- file.path(getwd(), "data")
dictionary <- read.xlsx(xlsxFile = file.path(dataPath, "LCDataDictionary.xlsx"), sheet = "LoanStats")
data <- fread(file.path(dataPath, "loan.csv"))
data <- setDF(data)

data <- data %>% mutate(funded_loan_date = convert_date_from_month_year(issue_d),
                        earliest_cr_line_date = convert_date_from_month_year(earliest_cr_line),
                        last_credit_pull_date = convert_date_from_month_year(last_credit_pull_d)) 

data$target <- factor(case_when(data$loan_status == "Fully Paid"  ~ 0,
                                data$loan_status == "Charged Off" ~ 1,
                                data$loan_status == "Default Does not meet the credit policy. Status:Charged Off" ~ 1), levels = c(0, 1))

data <- data[which(!is.na(data$target)), ]


target_plot_all_data <- data %>%
  mutate(target = as.factor(as.numeric(as.character(target)))) %>%
  select(c("target", "funded_loan_date")) %>%
  group_by(funded_loan_date, target) %>%
  summarise( N = n()) %>%
  ggplot(aes(x = funded_loan_date, y = N, fill=target)) +
  geom_bar(stat = 'identity', position= position_dodge())+
  ylab("Number of observations")+
  labs(title = "Target in all data")+
  theme(plot.title = element_text(hjust = 0.5))

dataReduced <- data[which(data$funded_loan_date >= '2013-01-01' & data$funded_loan_date <= '2015-06-01'), ]

target_plot_reduced_data <- dataReduced %>%
  mutate(target = as.factor(as.numeric(as.character(target)))) %>%
  select(c("target", "funded_loan_date")) %>%
  group_by(funded_loan_date, target) %>%
  summarise( N = n()) %>%
  ggplot(aes(x = funded_loan_date, y = N, fill=target)) +
  geom_bar(stat = 'identity', position= position_dodge())+
  ylab("Number of observations")+
  labs(title = "Target in all data")+
  theme(plot.title = element_text(hjust = 0.5))

structureData <- describe_variables(dataReduced)
levelOfNA <- 0.80
maxUniqueValues <- 30

variablesWithalotNA         <- structureData$variable[which(structureData$p_numberOfNa >= levelOfNA )]
variablesWithalotUniqueText <- structureData$variable[ which(structureData$type == 'character' & structureData$uniqueValues > maxUniqueValues)]
variablesWithOneValue       <- structureData$variable[which(structureData$uniqueValues == 1 )]
variablesToRemove           <- c(variablesWithalotNA, variablesWithalotUniqueText, variablesWithOneValue)
variablesFromFeature        <- c("sub_grade", "grade", "int_rate", "installment", "total_pymnt", "total_pymnt_inv", "total_rec_prncp", "total_rec_int", "total_rec_late_fee", "recoveries", "collection_recovery_fee")
variablesAnotherToDelete    <- c("id", "member_id", "issue_id", "loan_status")

dataReducedByVariables <- dataReduced %>% select(-variablesToRemove)




dataWork <- changeCharacter2FactorVariableWithLackGroup(data = dataWork,  typeOfLack = "n/a" )

# find to large percent of attribute in factor variables
factorColumns <- names(select_if(.tbl =  dataWork, is.factor))
attributesOfVariables <- sapply(factorColumns, function(x) prop.table(table(dataWork[[x]])))

variablesWithLargeAttribute <- c("verification_status_joint", "application_type", "pymnt_plan")
dataWork <- dataWork %>% select(-one_of(variablesWithLargeAttribute))

save(dataWork, file = file.path(dataPath, "dataWork.RData"))
