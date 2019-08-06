library(tidyverse)
library(Information)
source("3.feature_engineering.R")
source("functions_to_feature_selection.R")

dir.create(file.path(getwd(), "plots"), showWarnings = FALSE)
dir.create(file.path(getwd(), "calculations"), showWarnings = FALSE)

folderToSavePlots               <- file.path(getwd(), "plots")
folderToSavecalculations        <- file.path(getwd(), "results")
dataTrainWithNewFeatures$target <- as.numeric(as.character(dataTrainWithNewFeatures$target))

results <- calculate_information_value_for_variables(data = dataTrainWithNewFeatures,
                                                     name_of_dependent_variable = "target",
                                                     create_plot_IV = TRUE,
                                                     create_plots_woe = TRUE,
                                                     name_of_time_variable_to_iv = "quarter")

variable_selection_step_iv       <- results$information_table$Summary[which(results$information_table$Summary$IV > 0.02), ]
names_variable_selection_step_iv <- variable_selection_step_iv$Variable

dataTrainWithSelectionIvFeatures <- dataTrainWithNewFeatures %>% select(c(names_variable_selection_step_iv, "target", "quarter"))
# Stability information value in quarters for each variable
results_for_variable_selection <- calculate_information_value_for_variables(data = dataTrainWithSelectionIvFeatures,
                                                                            name_of_dependent_variable = "target",
                                                                            create_plot_IV = TRUE,
                                                                            create_plots_iv_in_time = TRUE,
                                                                            name_of_time_variable_to_iv = "quarter")

save(results_for_variable_selection, file = file.path(folderToSavePlots, "results_for_variable_selection.RData"))
continuous_variable_with_not_monotonic <- c("pub_rec", "loan_amnt", "emp_length", "open_acc", "revol_bal")
variables_joining_woe_category         <- c("inq_last_6_mths_cat", "mths_since_last_delinq", "mths_since_last_major_derog")

# optimal number of bins for variables
numeric_variables <- dataTrainWithNewFeatures %>% select_if(is.numeric) %>% names()
numeric_variables <- numeric_variables[which(!numeric_variables %in% c('target'))]
