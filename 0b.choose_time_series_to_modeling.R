library(tidyverse)
library(reshape)
library(lubridate)
library(openxlsx)
source("functions.R")


chooseTimeSeriesToModeling <- function() {
  
  folderToSavePlots               <<- file.path(getwd(), "plots")
  folderToSavecalculations        <<- file.path(getwd(), "calculations")
  listOfPlotsTimeSeries           <- list()
  dir.create(folderToSavePlots, showWarnings = FALSE)
  dir.create(folderToSavecalculations, showWarnings = FALSE)

  
  data <- data %>% mutate(funded_loan_date = convert_date_from_month_year(issue_d),
                          earliest_cr_line_date = convert_date_from_month_year(earliest_cr_line),
                          last_credit_pull_date = convert_date_from_month_year(last_credit_pull_d)) 
  
  data$target <- factor(case_when(data$loan_status == "Fully Paid"  ~ 0,
                                  data$loan_status == "Charged Off" ~ 1,
                                  data$loan_status == "Default Does not meet the credit policy. Status:Charged Off" ~ 1), levels = c(0, 1))
  
  data <- data[which(!is.na(data$target)), ]
  
  plotTargetInAllData <- data %>%
    select(c("target", "funded_loan_date")) %>%
    group_by(funded_loan_date, target) %>%
   dplyr::summarise( N = n()) %>%
    ggplot(aes(x = funded_loan_date, y = N, fill=target)) +
    geom_bar(stat = 'identity', position= position_dodge())+
    ylab("Number of observations") +
    labs(title = "Target in all data") +
    theme(plot.title = element_text(hjust = 0.5))
  listOfPlotsTimeSeries[["plotTargetInAllData"]] <- plotTargetInAllData
  
  defaultRates <- data %>%
    select(c("target", "funded_loan_date")) %>%
    group_by(funded_loan_date, target) %>%
    dplyr::summarise(N = n()) %>%
    melt(id = c("funded_loan_date", "target")) %>%
    cast(funded_loan_date ~ target)
  
  names(defaultRates)[c(2,3)] <- c("goods", "bads")
  
  defaultRates <- defaultRates %>%
    mutate(goods = ifelse(is.na(goods), 0, goods),
           bads = ifelse(is.na(bads), 0, bads),
           badRate = bads/goods,
           N = goods + bads)
  
  numberOfMinObsInMonth      <- 1500
  numberOfMinDefaultsInMonth <- 500
  divTrain <- 0.6
  divTest  <- 0.2
  divValid <- 0.2
  group_by_time <- 'month'
  
  modelingTimeInterval <- defaultRates %>%
    filter(N > numberOfMinObsInMonth & bads >= numberOfMinDefaultsInMonth) %>%
    select(funded_loan_date) %>%
   dplyr::summarise(minDate = min(funded_loan_date),
              maxDate = max(funded_loan_date))
  
  plotBadRateInAllData <- ggplot(defaultRates, aes(x = funded_loan_date, y = badRate))+
    geom_line() +
    labs(title = "Bad rate in all data")+
    theme(plot.title = element_text(hjust = 0.5))
  listOfPlotsTimeSeries[["plotBadRateInAllData"]] <- plotBadRateInAllData
    
  plotBadRateSelectedData <- defaultRates %>% 
    filter(N > numberOfMinObsInMonth & bads >= numberOfMinDefaultsInMonth) %>%
    ggplot( aes(x = funded_loan_date, y = badRate))+
    geom_line() +
    labs(title = "Bad rate - selected data",
         subtitle = paste("Number of observations in each month is greater than", numberOfMinObsInMonth, "and the number defaults greater than",numberOfMinDefaultsInMonth))+
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(size = 9))
  
  listOfPlotsTimeSeries[["plotBadRateSelectedData"]] <- plotBadRateSelectedData
  
  plotTargetInSelectedData <- data %>%
    select(c("target", "funded_loan_date")) %>%
    filter(funded_loan_date >= modelingTimeInterval$minDate & funded_loan_date <= modelingTimeInterval$maxDate) %>%
    group_by(funded_loan_date, target) %>%
    dplyr::summarise( N = n()) %>%
    ggplot(aes(x = funded_loan_date, y = N, fill = target)) +
    geom_bar(stat = 'identity', position = position_dodge())+
    ylab("Number of observations")+
    labs(title = "Target in selected data")+
    theme(plot.title = element_text(hjust = 0.5))
  listOfPlotsTimeSeries[["plotTargetInSelectedData"]] <- plotTargetInSelectedData
  
  modelingTimeInterval$minDateUpdate <- as.Date('2013-01-01')
  modelingTimeInterval$maxDateUpdate <- as.Date('2015-06-01')
    
  dataToTranTestValid <- data %>%
    filter(funded_loan_date >= modelingTimeInterval$minDateUpdate & funded_loan_date <= modelingTimeInterval$minDateUpdate %m+% years(2))
  
  dataToOutOfTime <- data %>%
    filter(funded_loan_date > modelingTimeInterval$minDateUpdate %m+% years(2) & funded_loan_date <= modelingTimeInterval$maxDateUpdate) 
  
  save(dataToTranTestValid, file = file.path(dataPath, "dataToTranTestValid.Rdata"))
  save(dataToOutOfTime, file = file.path(dataPath, "dataToOutOfTime.Rdata"))
  save(listOfPlotsTimeSeries, file = file.path(folderToSavePlots, "listOfPlotsTimeSeries.Rdata"))
  return(listOfPlotsTimeSeries)
}
