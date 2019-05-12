library(tidyverse)
library(reshape2)
library(ggplot2)
library(lubridate)

dataPath <- file.path(getwd(), "data")
load(file.path(dataPath, "dataToModeling.Rdata"))

# for test decision to reduce observations
dataTrain <- dataTrain %>% filter(funded_loan_date >= '2013-01-01')
dataTrain$month <- substr(dataTrain$funded_loan_date, start = 1, stop = 7)
dataTrain$month <- factor(dataTrain$month, levels = sort(unique(dataTrain$month)))



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

for (i in seq_along(attributesOfVariables)) {
  if(sum(grepl(pattern = "NA", x = names(attributesOfVariables[[i]]))) > 0) {
    nameVariable    <- names(attributesOfVariables[i])
    variablesWithNA <- c(nameVariable, variablesWithNA)
  }
}

