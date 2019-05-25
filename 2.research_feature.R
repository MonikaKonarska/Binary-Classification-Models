library(tidyverse)
library(reshape2)
library(lubridate)
library(scales)

source("functions.R")

dataPath <- file.path(getwd(), "data")
load(file.path(dataPath, "dataToModeling.Rdata"))

# for test decision to reduce observations
dataTrain <- dataTrain %>% filter(funded_loan_date >= '2013-01-01')
dataTrain$month <- substr(dataTrain$funded_loan_date, start = 1, stop = 7)
dataTrain$month <- factor(dataTrain$month, levels = sort(unique(dataTrain$month)))

dataTrain <- dataTrain %>% select(-c("mths_since_last_record", "mths_since_last_major_derog", "mths_since_last_delinq"))


attributesOfVariables <- list()

for (var in names(dataTrain)) {
  if(is.factor(dataTrain[[var]])) {
    attributesOfVariables[[var]] <- table(dataTrain[[var]])
  } else {
    attributesOfVariables[[var]] <- summary(dataTrain[[var]])
    }
}

# in all samples (train, test, vaid, out of time) the attributes: NONE, OTHER, ANY have got a very small percentage so I decide to remove it from all data
dataTrain <- dataTrain %>% filter(!home_ownership %in% c('ANY', 'NONE', 'OTHER'))
dataTrain$home_ownership <- factor(dataTrain$home_ownership, levels = sort(unique(dataTrain$home_ownership)))

dataTrain <- dataTrain %>% mutate(purpose = ifelse(purpose %in% c('educational', 'renewable_energy'),'other', as.character(purpose)))
dataTrain$purpose <- factor(dataTrain$purpose, levels = sort(unique(dataTrain$purpose)))
  
dataTrain$emp_length <- ifelse(as.character(dataTrain$emp_length) == 'n/a', 'LACK', as.character(dataTrain$emp_length))
dataTrain$emp_length <- factor(dataTrain$emp_length, levels = sort(unique(dataTrain$emp_length)))


variablesWithNA <- c()

for (i in names(dataTrain)) {
  if(sum(is.na(dataTrain[i])) > 0) {
    print(i)
    dataTrain       <- dataTrain[!is.na(dataTrain[[i]]), ]
    variablesWithNA <- c(i, variablesWithNA)
  }
}  


##  Exploratory Data Analysis
numericVariables <- select_if(dataTrain, is.numeric) %>% names()
categoricalVariables <- select_if(dataTrain, is.factor) %>% names()


# plots of numerical variables 
dataTrain %>%
  select_(.dots = numericVariables) %>%
  gather_("variable", "value", gather_cols = numericVariables) %>%
  ggplot(aes(x = value)) +
  facet_wrap(~ variable, scales = "free_x", ncol = 3) +
  geom_histogram()


densityPlots <- lapply(numericVariables, function(x) createPlotsForContinousVariables(variable_name = x, data = dataTrain, type_of_plots = "density", groupBy = "month"))
boxplotPlots <- lapply(numericVariables, function(x) createPlotsForContinousVariables(variable_name = x, data = dataTrain, type_of_plots = "boxplot", groupBy = "month"))
histogramPlots <- lapply(numericVariables, function(x) createPlotsForContinousVariables(variable_name = x, data = dataTrain, type_of_plots = "histogram", groupBy = "month"))

