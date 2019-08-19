library(tidyverse)
library(grid)
library(gridExtra)
library(lubridate)
library(zoo)
library(reshape)
library("InformationValue")
source("functions.R")

dataPath <- file.path(getwd(), "data")
load(file.path(dataPath, "dataToModeling.Rdata"))

columnNames <- names(dataTrain)
numericColumns <- columnNames[purrr::map_lgl(dataTrain, is.numeric)] 
numericColumns <- numericColumns[!numericColumns %in% c('id', 'member_id')]

visualizeDensityVariablesGroupedByCategories(data = dataTrain,
                                             names_numeric_variables = numericColumns,
                                             names_variable_to_group = "target")


list_of_boxplots <- list()
for (variable in numericColumns) {
  print(variable)
  boxplot <- createPlotsForContinousVariables(variable_name = variable,
                                            data = dataTrain,
                                            type_of_plots = "boxplot") 
  list_of_boxplots[[variable]] <- boxplot
}

boxplotsPage1 <- multiplot(plotlist = list_of_boxplots[1:10], cols = 4)
boxplotsPage2 <- multiplot(plotlist = list_of_boxplots[11:21], cols = 4)
boxplotsPage3 <- multiplot(plotlist = list_of_boxplots[22:29], cols = 4)



dataTrain$month   <- substr(x = dataTrain$funded_loan_date, start = 1, stop = 7)
dataTrain$quarter <- as.Date(as.yearqtr(dataTrain$funded_loan_date),  frac = 1)


# about grade
ggplot(data = dataTrain, aes(y = funded_amnt, x = quarter, group = quarter))+
  geom_boxplot(outlier.colour = "red", fill = "white", outlier.shape = 1) +
  facet_wrap(~ loan_status)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  ggtitle(label = "Boxplots of funded amounts for bad and good loans", subtitle = "at the each quarter")+
  xlab("")

ggplot(data = dataTrain, aes(y = installment, x = grade, color = term))+
  geom_boxplot(outlier.colour = "red")+
  facet_grid(~ term) +
  ggtitle(label = "Installment by grade")

ggplot(data = dataTrain, aes(x = grade, y = int_rate))+
  geom_boxplot(outlier.colour = "red", fill = "white", outlier.shape = 1) +
  facet_wrap(~ loan_status)

defaultRatesInQuarter <- dataTrain %>%
  select(target, quarter) %>%
  group_by(quarter, target) %>%
  summarise(N = n()) %>%
  melt(id = c("quarter", "target")) %>%
  cast(quarter ~ target)%>%
  plyr::rename(c("0" = "not_default", "1" = "default")) %>%
  mutate(default_rate = default/(not_default + default),
         number_of_loans = not_default+default) 

xyplot(default_rate ~quarter,
       data = defaultRatesInQuarter,
       type = 'b',
       main = "Default rate by quarter in train dataset")

dataTrain %>% 
  select(target, month, grade)%>%
  group_by(month, grade) %>%
  summarise(sum_of_default = sum(as.numeric(target)-1))%>%
  ggplot(aes(x = month, y = sum_of_default))+
  geom_bar(stat= 'identity', aes(fill = grade))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

defaultRatesByQuarterGrade <- dataTrain %>%
  select(target, quarter, grade) %>%
  group_by(quarter, target, grade) %>%
  summarise(N = n()) %>%
  filter(target == 1) %>%
  left_join(defaultRatesInQuarter %>% select(quarter, number_of_loans), by = c("quarter" = "quarter")) %>%
  mutate(default_rate = N/number_of_loans)

ggplot(defaultRatesByQuarterGrade, aes(x = quarter, y = default_rate, group = grade, color = grade))+
  geom_line(size = 1)+
  facet_wrap(~grade)

dataTrain %>%
  select(loan_status, quarter) %>%
  group_by(loan_status, quarter) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = quarter, y = count))+
  geom_bar(stat = "identity", aes(fill = loan_status), position = "dodge")+
  ggtitle(label = "Number of loans by loan status")

dataTrain %>% 
  select(int_rate, quarter, grade)%>%
  group_by(quarter, grade) %>%
  summarise(mean_int_rate = mean(int_rate))%>%
  ggplot(aes(x= quarter, y = mean_int_rate))+
  geom_bar(stat= 'identity', aes(fill = grade))

ggplot(data = dataTrain, aes(x = home_ownership, y=(..count..), fill = home_ownership))+
  geom_bar() +
  facet_wrap(~ term) +
  ggtitle(label = "Number of loans by term and home ownership")+
  ylab("Number of loans")

dataTrain %>%
  select(term, month) %>%
  group_by(term, month) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = month, y = count))+
  geom_bar(stat = 'identity',aes(fill = term), position = "dodge2")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Number of loans")+
  ggtitle(label = "Number of loans grouped by number of payments (term) on the loans")
  
dataTrain %>%
  select(c(grade, funded_loan_date, installment)) %>%
  group_by(grade, funded_loan_date) %>%
  summarise(mean_of_installment = mean(installment)) %>%
  ggplot(aes(x = funded_loan_date, y = mean_of_installment)) +
  geom_line(size = 1.5, color = "darkblue") +
  facet_wrap(~grade) +
  ggtitle(label = "Mean installment by each grade")



# Cleveland dotplots

dataTrain %>%
  select(c(emp_length, home_ownership, target)) %>% 
  ggplot()+
  geom_bar(aes(x = emp_length, fill = home_ownership))+
  coord_flip()

dataTrain %>%
  select(c(purpose, loan_status, target)) %>% 
  ggplot()+
  geom_bar(aes(x = purpose, fill = loan_status))+
  coord_flip()

dataTrain%>%
  select(annual_inc, grade, term) %>%
  group_by(grade, term) %>%
  summarise(mean_annual_inc = mean(annual_inc)) %>%
  ggplot(aes(x = mean_annual_inc, y = grade, color = term))+
  geom_point(size = 3) +
  ggtitle(label = "Mean of annual income by grade")

dataTrain%>%
  select(installment, grade, loan_status) %>%
  group_by(grade, loan_status) %>%
  summarise(mean_installment = mean(installment)) %>%
  ggplot(aes(x = mean_installment, y = grade, color = loan_status))+
  geom_point(size = 3) +
  ggtitle(label = "Mean of installment by grade", subtitle = "in 36 months or 60 months")

dataTrain%>%
  select(funded_amnt, grade, term) %>%
  group_by(grade, term) %>%
  summarise(mean_loan = mean(funded_amnt)) %>%
  ggplot(aes(x = mean_loan, y = grade, color = term))+
  geom_point(size = 3) +
  ggtitle(label = "Mean of funded loans by grade", subtitle = "in 36 months or 60 months")

dataTrain%>%
  select(int_rate, grade, term) %>%
  group_by(grade, term) %>%
  summarise(mean_int_rate = mean(int_rate)) %>%
  ggplot(aes(x = mean_int_rate, y = grade, color = term))+
  geom_point(size = 3) +
  ggtitle(label = "Mean of interest rate by grade", subtitle = "in 36 months or 60 months")


