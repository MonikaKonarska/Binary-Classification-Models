library(tidyverse)
library(reshape)
library(lubridate)
library(openxlsx)
library(cowplot)
source(file.path(functionPath,"functions.R"))
source(file.path(functionPath,"functions_to_feature_selection.R"))

chooseTimeSeriesToModeling <- function() {
  
  listOfPlotsTimeSeries           <- list()
  dir.create(folderToSavePlots, showWarnings = FALSE)
  dir.create(folderToSavecalculations, showWarnings = FALSE)

  data <- data %>%
    mutate(funded_loan_date = convert_date_from_month_year(issue_d),
           earliest_cr_line_date = convert_date_from_month_year(earliest_cr_line),
           last_credit_pull_date = convert_date_from_month_year(last_credit_pull_d)) 
  
  data$target <- case_when(data$loan_status == "Fully Paid"  ~ '0',
                                  data$loan_status == "Charged Off" ~ '1',
                                  data$loan_status == "Default Does not meet the credit policy. Status:Charged Off" ~ '1',
                           TRUE ~ as.character(data$loan_status))
  
  data <- data[which(data$target %in% c("0", "1")), ]
  data$target <- as.factor(data$target)
  
  plotTargetInAllData <- data %>%
    dplyr::select(target, funded_loan_date) %>%
    group_by(funded_loan_date, target) %>%
    dplyr::summarise( N = n()) %>%
    ggplot(aes(x = funded_loan_date, y = N, fill=target)) +
    geom_bar(stat = 'identity', position= position_dodge())+
    ylab("Number of observations") +
    labs(title = "Distribution of target") +
    theme(plot.title = element_text(hjust = 0.5))

  defaultRates <- data %>%
    dplyr::select(c("target", "funded_loan_date")) %>%
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
  
  numberOfMinObsInMonth      <- 1000
  numberOfMinDefaultsInMonth <- 100
    
  divTrain <- 0.6
  divTest  <- 0.2
  divValid <- 0.2
  group_by_time <- 'month'
  
  modelingTimeInterval <- defaultRates %>%
    dplyr::filter(N > numberOfMinObsInMonth & bads >= numberOfMinDefaultsInMonth) %>%
    dplyr::select(funded_loan_date) %>%
   dplyr::summarise(minDate = min(funded_loan_date),
                    maxDate = max(funded_loan_date))
  
  plotBadRateInAllData <- ggplot(defaultRates, aes(x = funded_loan_date, y = badRate))+
    geom_line() +
    labs(title = "Bad rate")+
    scale_y_continuous(limits = c(0.15, 0.35), labels = scales::percent)+
    theme(plot.title = element_text(hjust = 0.5))
  
  minDateInDataSet <- as.Date('2013-01-01')
  maxDateInDataSet <- as.Date('2014-12-31')
  
  plotBadRateSelectedData <- defaultRates %>% 
    dplyr::filter(funded_loan_date >= minDateInDataSet & funded_loan_date <= maxDateInDataSet) %>%
    ggplot(aes(x = funded_loan_date, y = badRate))+
    geom_line() +
    labs(title = "Bad rate - selected data") +
    scale_y_continuous(limits = c(0.15, 0.35), labels = scales::percent)+
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(size = 9))
  
  plotTargetInSelectedData <- data %>%
    dplyr::select(c("target", "funded_loan_date")) %>%
    dplyr::filter(funded_loan_date >= minDateInDataSet & funded_loan_date <= maxDateInDataSet) %>%
    dplyr::group_by(funded_loan_date, target) %>%
    dplyr::summarise( N = n()) %>%
    ggplot(aes(x = funded_loan_date, y = N, fill = target)) +
    geom_bar(stat = 'identity', position = position_dodge())+
    ylab("Number of observations")+
    labs(title = "Distribution of target - selected data")+
    theme(plot.title = element_text(hjust = 0.5))
  
  plotTargetsInformation <<- plot_grid(plotTargetInAllData,plotTargetInSelectedData, plotBadRateInAllData, plotBadRateSelectedData, nrow = 2)  
  
  listOfPlotsTimeSeries[["plotBadRateSelectedData"]] <- plotBadRateSelectedData
  save_plot_jpg(path = folderToSavePlots, plot = plotBadRateSelectedData, nameOfPlot = "plotBadRateSelectedData")
  listOfPlotsTimeSeries[["plotBadRateInAllData"]] <- plotBadRateInAllData
  save_plot_jpg(path = folderToSavePlots, plot = plotBadRateInAllData, nameOfPlot = "plotBadRateInAllData")
  listOfPlotsTimeSeries[["plotTargetInAllData"]] <- plotTargetInAllData
  save_plot_jpg(path = folderToSavePlots, plot = plotTargetInAllData, nameOfPlot = "plotTargetInAllData")
  listOfPlotsTimeSeries[["plotTargetInSelectedData"]] <- plotTargetInSelectedData
  save_plot_jpg(path = folderToSavePlots, plot = plotTargetInSelectedData, nameOfPlot = "plotTargetInSelectedData")
    
  listOfPlotsTimeSeries[["plotTargetInSelectedData"]] <- plotTargetInSelectedData
  save_plot_jpg(path = folderToSavePlots, plot = plotTargetsInformation, nameOfPlot = "plotTargetsInformation")
  
  maxDateInDataSetForTrainTestValid <- max(seq((maxDateInDataSet %m-% years(1)), by = "1 month", length = 7))
  
  dataToTranTestValid <- data %>%
    dplyr::filter(funded_loan_date >= minDateInDataSet & funded_loan_date <= maxDateInDataSetForTrainTestValid)
    
  dataNew <- data %>%
    dplyr::filter(funded_loan_date >= maxDateInDataSetForTrainTestValid & funded_loan_date <= maxDateInDataSet) 
  
  save(dataToTranTestValid, file = file.path(dataPath, "dataToTranTestValid.Rdata"))
  save(dataNew, file = file.path(dataPath, "dataNew.Rdata"))
  save(listOfPlotsTimeSeries, file = file.path(folderToSavePlots, "listOfPlotsTimeSeries.Rdata"))
  
  return(listOfPlotsTimeSeries)
}
