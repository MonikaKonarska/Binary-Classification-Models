

calculate_information_value_for_variables <- function(data = NA,
                                                      name_of_dependent_variable = NA,
                                                      names_of_independent_variable = NA,
                                                      number_of_bins_for_continuous_variable = 6,
                                                      create_plot_IV = FALSE,
                                                      create_plots_woe = FALSE,
                                                      create_plots_iv_in_time = FALSE,
                                                      name_of_time_variable_to_iv = NA,
                                                      path_to_save_plot = NA ) {
  # Function computes information value for each variables in dataset, create woe plots and iv plot in time
  # Args:
  # data: data frame
  # name_of_dependent_variable: name of dependent variable - y
  # names_of_independent_variable: name of independent variable/variables
  # number_of_bins_for_continuous_variable: umber of bins for continuous variable to compute information value
  # create_plot_IV: TRUE/FALSE
  # create_plots_woe: TRUE/FALSE
  # create_plots_iv_in_time: TRUE/FALSE
  # name_of_time_variable_to_iv: grouped time variable for example month, quarter, year, half year
  library(Information)
  library(ggplot2)
  library(dplyr)
  library(purrr)
  
  if(sum(!is.na(names_of_independent_variable)) > 0) {
    data <- data[, c(names_of_independent_variable, name_of_dependent_variable, name_of_time_variable_to_iv)]
  }
  
  all_results               <- list()
  information_table         <- create_infotables(data = data, y = name_of_dependent_variable, bins = number_of_bins_for_continuous_variable)
  all_results[["information_table"]] <- information_table
  dataToPlotingIV           <- information_table$Summary[order(-information_table$Summary$IV), ]
  dataToPlotingIV$Variable  <- factor(dataToPlotingIV$Variable, levels = dataToPlotingIV$Variable[order(-dataToPlotingIV$IV)])
  iv_for_all_variables_plot <- create_plot_iv_for_all_variables(dataToPlotingIV, Variable, IV)
  all_results[['iv_for_all_variables_plot']] <- iv_for_all_variables_plot
  all_results[['information_table']]         <- information_table
  
  if(!is.na(path_to_save_plot)) {
    save_plot_jpg(path = path_to_save_plot,
                  plot = iv_for_all_variables_plot,
                  prefix_text_plot_name_file = "",
                  nameOfPlot = "iv_for_all_variables")
  }
  prospectiveVariables <- names(information_table$Tables)
  prospectiveVariables <- prospectiveVariables[which(!prospectiveVariables %in% name_of_time_variable_to_iv)]
  if(create_plots_woe) {
    plotsOfWoe <- create_plot_woe_for_each_variable(prospectiveVariables, information_table)
  }
  if(!is.na(name_of_time_variable_to_iv) & create_plots_iv_in_time == TRUE) { 
    list_of_values_time_variable <- as.character(levels(data[[name_of_time_variable_to_iv]]))
    plotsOfIvInTime <- create_plot_iv_in_time_for_each_variable(data,
                                                                prospectiveVariables,
                                                                name_of_dependent_variable,
                                                                name_of_time_variable_to_iv,
                                                                list_of_values_time_variable,
                                                                number_of_bins_for_continuous_variable)
  }
  if(exists("plotsOfWoe")) {
    all_results[["plotsOfWoe"]]<- plotsOfWoe
    if(!is.na(path_to_save_plot)) {
      for(variable in names(plotsOfWoe)) {
        save_plot_jpg(path = path_to_save_plot, plot = plotsOfWoe[[variable]], prefix_text_plot_name_file = "woe_", nameOfPlot = variable)
      }
    }
  }
  if(exists("plotsOfIvInTime")) {
    all_results[["plotsOfIvInTime"]] <- plotsOfIvInTime
    if(!is.na(path_to_save_plot)) {
      for(variable in names(plotsOfIvInTime)) {
        save_plot_jpg(path = path_to_save_plot, prefix_text_plot_name_file = "Iv_in_time_", plot = plotsOfIvInTime[[variable]], nameOfPlot = variable)
      }
    }
  }
  return(all_results)
}    


create_plot_iv_for_all_variables <- function(dataToPlotingIV, Variable, IV) {
  plot <- ggplot(dataToPlotingIV, aes(x = Variable, y = IV)) +
    geom_bar(width = 0.35, stat ='identity', color = 'darkblue', fill = 'white') +
    ggtitle('Information Value') +
    theme_bw() +
    theme(plot.title = element_text(size = 10)) +
    theme(axis.text.x = element_text(angle = 90))
  return(plot)
}


save_plot_jpg <- function(path, plot, prefix_text_plot_name_file = "", nameOfPlot = "plot") {
  # Function saves plot
  # Args:
  # path: the path to save plot
  # prefix_text_plot_name_file:
  # plot: 
  if(!is.null(prefix_text_plot_name_file)){
    path_to_save_plot <- file.path(path, paste0(prefix_text_plot_name_file, nameOfPlot, ".jpg"))
    #path_to_save_plot <- file.path(path, paste0(prefix_text_plot_name_file, plot))
  } else {
    path_to_save_plot <- file.path(path, paste0(nameOfPlot, ".jpg"))
    #path_to_save_plot <- file.path(path, paste0(plot))
  }
  jpeg(filename = path_to_save_plot)
  print(plot)
  dev.off()
}

create_plot_woe_for_each_variable <- function(prospectiveVariables, information_table) {
  # Function creates plots using woe values for each independent variable
  # Args:
  # prospectiveVariables: vector of names of independent variables
  # information_table: object from result calculate information value/weight of evidence for variables 
  plotsOfWoe <- list()
  for (variable in prospectiveVariables) {
    tableWithWoes <- information_table$Tables[[variable]]
    tableWithWoes[[variable]] <- factor(tableWithWoes[[variable]], levels = tableWithWoes[[variable]])
    IValue <- information_table$Summary[which(information_table$Summary$Variable == variable), "IV"]
    woe_plot <-  ggplot(tableWithWoes,  aes_string(x = variable, y = 'WOE'))+
      geom_bar(stat = 'identity', fill = "darkblue")+
      theme(axis.text.x = element_text(angle = 90))+
      ggtitle(label = "Weight of Evidence (WOE)", subtitle = paste0("Variable: ", variable, "\n", "Information value:", round(IValue,4)))
    
    plotsOfWoe[[variable]] <- woe_plot
  }
  return(plotsOfWoe)
}


create_plot_iv_in_time_for_each_variable <- function(data,
                                                     prospectiveVariables,
                                                     name_of_dependent_variable,
                                                     name_of_time_variable_to_iv,
                                                     list_of_values_time_variable,
                                                     number_of_bins_for_continuous_variable) {
  # Function computes information value in time for independent variable and create plots
  # Args:
  # data: data frame object with dependent and independent variables
  # prospectiveVariables: vector of names independent variables
  # name_of_time_variable_to_iv: name of time variable to group information value
  # number_of_bins_for_continuous_variable: number of bins for continuous variable to compute information value
  plotsOfIvInTime                           <- list()
  number_of_columns                         <- length(prospectiveVariables) + 1
  number_of_rows                            <- length(list_of_values_time_variable)
  tablesOfWoeInTimeByEachVariable           <- data.frame(matrix(ncol = number_of_columns, nrow = number_of_rows), stringsAsFactors = FALSE)
  colnames(tablesOfWoeInTimeByEachVariable) <- c("time", prospectiveVariables)
  tablesOfWoeInTimeByEachVariable$time      <- list_of_values_time_variable
  
  for (variable in prospectiveVariables) {
    for (idx_time in list_of_values_time_variable) {
      dataToPlot <- data %>%
        select_(name_of_time_variable_to_iv, variable, name_of_dependent_variable) %>%
        filter(!!sym(name_of_time_variable_to_iv) == idx_time) %>%
        select(-c(name_of_time_variable_to_iv))
    
      informationTableForVariable <- create_infotables(dataToPlot, y = name_of_dependent_variable, bins = number_of_bins_for_continuous_variable)
      tablesOfWoeInTimeByEachVariable[which(tablesOfWoeInTimeByEachVariable[['time']] == idx_time), variable] <- informationTableForVariable$Summary$IV
    }
    
    plot_of_ivs <- ggplot(data = tablesOfWoeInTimeByEachVariable, aes_string(x = 'time', y = variable))+
      geom_bar(stat = 'identity', fill = "green")+
      theme(axis.text.x = element_text(angle = 90))+
      ylab("Information Value")+
      ggtitle(label = "Information value in time",
              subtitle = paste0("Variable: ", variable))
    
    plotsOfIvInTime[[variable]] <- plot_of_ivs
  }
  #results <- list("plotsOfIvInTime" = plotsOfIvInTime, "tablesOfWoeInTimeByEachVariable" = tablesOfWoeInTimeByEachVariable)
  return(plotsOfIvInTime)
}


calculate_iv_for_variable_with_different_number_bins <- function(data = dataTrainWithNewFeatures,
                                                                 numeric_variables,
                                                                 max_number_bins = 7,
                                                                 save_results_path = NA) {
  # Function computes information value for continuous variable with different number of bins
  # Args:
  # data: data frame object with target variable 
  # numeric_variables: one or more names of variables
  # max_number_bins: max number of bins in variable
  # save_results_path: 
  if(max_number_bins <= 2){stop("Max number of bins is required greater than 2")}
  
  iv_in_variable_bins <- data.frame(variable_name = NA, number_of_bins = NA, iv = NA, stringsAsFactors = FALSE)
  for(variable in numeric_variables) {
    print(variable)
    for(i in 2:max_number_bins) {
      data_with_variable <- data[, c(variable, "target")]
      iv <- create_infotables(data_with_variable, y = 'target', bins = i)
      table <- data.frame('variable_name' = variable, 'number_of_bins' = i, 'iv' = iv$Summary$IV)
      iv_in_variable_bins <- bind_rows(iv_in_variable_bins, table)
    }
  }
  if(!is.na(save_results_path)){
    save(iv_in_variable_bins, file = file.path(save_results_path, "iv_in_variable_bins.RData"))
  }
  return(iv_in_variable_bins)
}


create_plots_iv_depends_of_number_bins <- function(iv_in_variable_bins) {
  
  max_and_min_iv_variables <- iv_in_variable_bins %>%
    group_by(variable_name) %>% 
   dplyr:: summarise(maxIv = max(iv, na.rm = TRUE),
              minIv = min(iv, na.rm = TRUE)) %>%
    mutate(diffrence_iv = round(maxIv - minIv, 4)) %>% 
    filter(diffrence_iv > 0) %>%
    arrange(desc(diffrence_iv)) 
  
  plots_of_iv_and_bins <- list()
  for(variable in max_and_min_iv_variables[['variable_name']]) {
    plot <- iv_in_variable_bins %>%
      filter(variable_name == variable) %>%
      ggplot(aes(x = number_of_bins, y= iv))+
      geom_point(size = 4, color = "red") +
      xlab("Number of bins")+
      ggtitle(label = "Information value in different number of bins", subtitle = paste("Variable: ", variable, sep = ""))+
      theme_light()
    plots_of_iv_and_bins[[variable]] <- plot
  }
  return(plots_of_iv_and_bins)
}



categoriseIndependentVariableInModel <- function(data = NA) {
  
  data <- data %>%
    mutate(dtiCateg = cut(x = dti, breaks = c(0, 10.18, 14.62, 18.95, 24.2, Inf), include.lowest = TRUE),
           annual_incCateg = cut(annual_inc, breaks = c(0, 42982, 56992, 72010, 96999, Inf), include.lowest = TRUE),
           revol_utilCateg = case_when(is.na(revol_util) ~ "NA",
                                       TRUE ~ as.character(cut(x = revol_util, breaks = c(0, 34, 49.5, 63.1, 77.4, 892.3, Inf)))),
           revol_utilCateg = factor(revol_utilCateg, levels = c("NA", sort(x = unique(data$revol_utilCateg))[1:5])),
           total_rev_hi_limCateg = cut(x = total_rev_hi_lim, breaks = c(0, 11595, 18250, 26800, 41486, Inf)))
  
  levels(data$dtiCateg)              <- c("[0,10.18]", "[10.19,14.62]", "[14.63,18.95]", "[18.96,24.2]","[24.21,39.99]")
  levels(data$annual_incCateg)       <- c("[3000,42982]", "[43000,56992]", "[57000,72010]", "[72012,96999]", "[97000,8706582]")
  levels(data$revol_utilCateg)       <- c("NA", "[0,34]", "[34.1,49.5]", "[49.6,63.1]", "[63.2,77.4]", "[77.5,892.3]")
  levels(data$total_rev_hi_limCateg) <- c("[0,11595]", "[11600,18250]", "[18300,26800]", "[26809,41486]", "[41500,1998700]")
  
  data <- data %>%
    mutate(dtiWoe = plyr::mapvalues(x = dtiCateg,
                                    from = categorisationVariableParametrs[["bins"]]$dti,
                                    to = categorisationVariableParametrs[["woe"]]$dti)
           ,annual_incWoe = plyr::mapvalues(x = annual_incCateg,
                                            from = categorisationVariableParametrs[["bins"]]$annual_inc,
                                            to = categorisationVariableParametrs[["woe"]]$annual_inc)
           ,revol_utilWoe = plyr::mapvalues(x = revol_utilCateg,
                                            from = categorisationVariableParametrs[["bins"]]$revol_util,
                                            to = categorisationVariableParametrs[["woe"]]$revol_util)
           ,total_rev_hi_limWoe = plyr::mapvalues(x = total_rev_hi_limCateg,
                                                  from = categorisationVariableParametrs[["bins"]]$total_rev_hi_lim,
                                                  to = categorisationVariableParametrs[["woe"]]$total_rev_hi_lim),
           home_ownershipWoe = plyr::mapvalues(x = home_ownership,
                                               from = categorisationVariableParametrs[["bins"]]$home_ownership,
                                               to = categorisationVariableParametrs[["woe"]]$home_ownership))
  return(data)
}



