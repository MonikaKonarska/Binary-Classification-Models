library(tidyverse)
library(Information)
library(corrplot)
source("functions_to_feature_selection.R")

featureSelection <- function() {
  
  folderToSavePlotsSelectedFeatures <- file.path(folderToSavePlots, "selectedFeatures")
  dir.create(folderToSavePlotsSelectedFeatures, showWarnings = FALSE)
  dataTrainWithNewFeatures$target <- as.numeric(as.character(dataTrainWithNewFeatures$target))
  
  results <- calculate_information_value_for_variables(data = dataTrainWithNewFeatures,
                                                       name_of_dependent_variable = "target",
                                                       create_plot_IV = TRUE,
                                                       create_plots_woe = TRUE,
                                                       create_plots_iv_in_time = FALSE,
                                                       name_of_time_variable_to_iv = "quarter",
                                                       path_to_save_plot = folderToSavePlots)

  variable_selection_step_iv       <- results$information_table$Summary[which(round(results$information_table$Summary$IV,2) > 0.02), ]
  names_variable_selection_step_iv <- variable_selection_step_iv$Variable
  dataTrainWithSelectionIvFeatures <- dataTrainWithNewFeatures %>% select(c(names_variable_selection_step_iv, "target", "quarter"))
  
  continuous_variable_with_not_monotonic <- c("loan_amnt",  "open_acc", "revol_bal", "inq_last_6_mths_cat", "mths_since_last_delinq", "mths_since_last_major_derog")
  discrete_variable_with_not_monotonic   <- c('month', 'purpose', 'emp_length')

  variablesToOptimumBins <- dataTrainWithNewFeatures %>% select_if(is.numeric) %>% names()
  variablesToOptimumBins <- variablesToOptimumBins[which(!variablesToOptimumBins %in% c('target', continuous_variable_with_not_monotonic))]
  
  iv_in_variable_bins    <- calculate_iv_for_variable_with_different_number_bins(data = dataTrainWithNewFeatures,
                                                                                 numeric_variables = variablesToOptimumBins,
                                                                                 max_number_bins = 6,
                                                                                 save_results_path = folderToSavecalculations)
  
  plots_of_iv_and_bins <- create_plots_iv_depends_of_number_bins(iv_in_variable_bins = iv_in_variable_bins)

  selectedVariables <- iv_in_variable_bins %>%
    filter(!is.na(number_of_bins)) %>%
    group_by(variable_name) %>% slice(which.max(iv)) 
    
  iVForSelectedcontinuousVariablesWithChoosenBins        <- integer(length(selectedVariables$variable_name))
  names(iVForSelectedcontinuousVariablesWithChoosenBins) <- selectedVariables[['variable_name']]
  
  for(variable in selectedVariables[['variable_name']]) {
    computedIv <- create_infotables(data = dataTrainWithNewFeatures[, c("target", variable)],
                                    y = "target",
                                    bins = selectedVariables[[which(selectedVariables$variable_name == variable),'number_of_bins']])
    iVForSelectedcontinuousVariablesWithChoosenBins[[variable]] <- computedIv$Summary$IV
  }
  
  continuousVariableSelected <<- names(iVForSelectedcontinuousVariablesWithChoosenBins[iVForSelectedcontinuousVariablesWithChoosenBins > 0.02])
  discriteVariableSelected   <<-  c("home_ownership")
  selectecVariables          <<- c(continuousVariableSelected, discriteVariableSelected)  
  listOfSeletedVariables     <- list('continuous' = continuousVariableSelected,
                                     'discrete' = discriteVariableSelected)
  
  informationTableForSelectedContinuousVariable <- c()
  
  for (variable in continuousVariableSelected) {
    print(variable)
    result <- calculate_information_value_for_variables(data = dataTrainWithSelectionIvFeatures
                                                        ,names_of_independent_variable = variable
                                                        ,name_of_dependent_variable = "target"
                                                        ,number_of_bins_for_continuous_variable = selectedVariables[[which(selectedVariables$variable_name == variable),'number_of_bins']]
                                                        ,create_plots_iv_in_time = TRUE
                                                        ,create_plots_woe = TRUE
                                                        ,create_plot_IV = FALSE
                                                        ,name_of_time_variable_to_iv = "quarter"
                                                        ,path_to_save_plot = folderToSavePlotsSelectedFeatures)
    
    informationTableForSelectedContinuousVariable[[variable]] <- result$information_table$Tables[[variable]]
  }

  informationTableForSelectedDiscriteVariable <- calculate_information_value_for_variables(data = dataTrainWithNewFeatures
                                                                          ,name_of_dependent_variable = "target"
                                                                          ,names_of_independent_variable = discriteVariableSelected
                                                                          ,create_plot_IV = FALSE
                                                                          ,create_plots_woe = TRUE
                                                                          ,create_plots_iv_in_time = TRUE
                                                                          ,name_of_time_variable_to_iv = "quarter"
                                                                          ,path_to_save_plot = folderToSavePlotsSelectedFeatures)
  
  informationTableSelectedVariables                     <- informationTableForSelectedContinuousVariable
  informationTableSelectedVariables[["home_ownership"]] <- informationTableForSelectedDiscriteVariable$information_table$Tables$home_ownership
  indxInformationTableVariablesWithoutTimeVariable      <- unlist(lapply(informationTableSelectedVariables, function(x) names(x)[1] != "quarter"))  
  informationTableSelectedVariables                     <- informationTableSelectedVariables[indxInformationTableVariablesWithoutTimeVariable]
  categorisationVariableParametrs                       <- list()
  categorisationVariableParametrs[["bins"]]             <- lapply(informationTableSelectedVariables, FUN = function(x) x[, 1])
  categorisationVariableParametrs[["woe"]]              <- lapply(informationTableSelectedVariables, FUN = function(x) x[, 4])
  names(categorisationVariableParametrs[["bins"]])      <- lapply(informationTableSelectedVariables, FUN = function(x) names(x[1]))
  names(categorisationVariableParametrs[["woe"]])       <- lapply(informationTableSelectedVariables, FUN = function(x) names(x[1]))

  categorisationVariableParametrs[["bins"]]$revol_util  <- sort(categorisationVariableParametrs[["bins"]]$revol_util)
  categorisationVariableParametrs[["woe"]]$revol_util   <- c(categorisationVariableParametrs[["woe"]]$revol_util[2:8], categorisationVariableParametrs[["woe"]]$revol_util[1])
  
  save(categorisationVariableParametrs,informationTableSelectedVariables, file = file.path(folderToSavecalculations, "categorisationVariableParametrs.Rdata"))
  save(dataTrainWithSelectionIvFeatures, file = file.path(folderToSavecalculations,"dataTrainWithSelectionIvFeatures.Rdata"))
  save(iVForSelectedcontinuousVariablesWithChoosenBins, file = file.path(folderToSavecalculations, "iVForSelectedcontinuousVariablesWithChoosenBins.Rdata"))
  save(listOfSeletedVariables, file = file.path(folderToSavecalculations, "listOfSeletedVariables.Rdata"))
  save(plots_of_iv_and_bins, iv_in_variable_bins, file = file.path(folderToSavecalculations, "iv_in_variable_bins_plots.Rdata"))
  save(results,  file = file.path(folderToSavecalculations, "resultsOfIV.Rdata"))


}  
     







