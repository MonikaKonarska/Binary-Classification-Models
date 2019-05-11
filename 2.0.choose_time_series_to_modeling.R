library(tidyverse)
library(reshape)
library(ggplot2)
library(lubridate)

set.seed(1234)
dataPath <- file.path(getwd(), "data")
load(file.path(dataPath, "dataWork.RData"))

numberOfMinObsInMonth      <- 1500
numberOfMinDefaultsInMonth <- 500
divTrain <- 0.6
divTest  <- 0.2
divValid <- 0.2
group_by_time <- 'month'

dataWork %>%
  mutate(target = as.factor(as.numeric(as.character(target)))) %>%
  select(c("target", "funded_loan_date")) %>%
  group_by(funded_loan_date, target) %>%
  summarise( N = n()) %>%
  ggplot(aes(x = funded_loan_date, y = N, fill=target)) +
  geom_bar(stat = 'identity', position= position_dodge())+
  ylab("Number of observations")+
  labs(title = "Target in all data")+
  theme(plot.title = element_text(hjust = 0.5))

defaultRates <- dataWork %>%
  mutate(target = as.numeric(as.character(target))) %>%
  select(c("target", "funded_loan_date")) %>%
  group_by(funded_loan_date, target) %>%
  summarise(N = n()) %>%
  melt(id = c("funded_loan_date", "target")) %>%
  cast(funded_loan_date ~ target)

names(defaultRates)[c(2,3)] <- c("goods", "bads")
defaultRates<- defaultRates %>%
  mutate(goods = ifelse(is.na(goods), 0, goods),
         bads = ifelse(is.na(bads), 0, bads),
         badRate = bads/goods,
         N = goods+bads)


modelingTimeInterval <- defaultRates %>%
  filter(N > numberOfMinObsInMonth & bads >= numberOfMinDefaultsInMonth) %>%
  select(funded_loan_date) %>%
  summarise(minDate = min(funded_loan_date),
            maxDate = max(funded_loan_date))

ggplot(defaultRates, aes(x = funded_loan_date, y = badRate))+
  geom_line() +
  labs(title = "Bad rate in all data")+
  theme(plot.title = element_text(hjust = 0.5))

defaultRates %>% filter(N > numberOfMinObsInMonth & bads >= numberOfMinDefaultsInMonth) %>%
  ggplot( aes(x = funded_loan_date, y = badRate))+
  geom_line() +
  labs(title = "Bad rate - selected data",
       subtitle = paste("Number of observations in each month is greater than", numberOfMinObsInMonth, "and the number defaults greater than",numberOfMinDefaultsInMonth))+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(size = 9))


dataWork %>%
  mutate(target = as.factor(as.numeric(as.character(target)))) %>%
  select(c("target", "funded_loan_date")) %>%
  filter(funded_loan_date >= modelingTimeInterval$minDate & funded_loan_date <= modelingTimeInterval$maxDate) %>%
  group_by(funded_loan_date, target) %>%
  summarise( N = n()) %>%
  ggplot(aes(x = funded_loan_date, y = N, fill=target)) +
  geom_bar(stat = 'identity', position= position_dodge())+
  ylab("Number of observations")+
  labs(title = "Target in selected data")+
  theme(plot.title = element_text(hjust = 0.5))


dataToTranTestValid <- dataWork %>%
  filter(funded_loan_date >= modelingTimeInterval$minDate & funded_loan_date <= modelingTimeInterval$minDate %m+% years(2))

dataToOutOfTime <- dataWork %>%
  filter(funded_loan_date > modelingTimeInterval$minDate %m+% years(2) & funded_loan_date <= modelingTimeInterval$maxDate) %>%
  summarise(n())


dataToTranTestValid$group <- sample(c('train', 'test', 'valid'), size = nrow(dataToTranTestValid), prob = c(divTrain, divTest,  divValid), replace = TRUE)

dataTrain  <- dataToTranTestValid %>% filter(group == "train")
dataTest   <- dataToTranTestValid %>% filter(group == "test")
dataValid  <- dataToTranTestValid %>% filter(group == "valid")

save(dataTrain, dataTest, dataValid, file = file.path(dataPath, "dataToModeling.Rdata") )
save(dataToOutOfTime, file = file.path(dataPath, "dataToOutOfTime.Rdata"))
