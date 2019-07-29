
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
  results <- list(plotsOfIvInTime, tablesOfWoeInTimeByEachVariable)
  return(results)
}



calculate_information_value_for_variables <- function(data = NA,
                                                      name_of_dependent_variable = NA,
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
  # number_of_bins_for_continuous_variable: umber of bins for continuous variable to compute information value
  # create_plot_IV: TRUE/FALSE
  # create_plots_woe: TRUE/FALSE
  # create_plots_iv_in_time: TRUE/FALSE
  # name_of_time_variable_to_iv: grouped time variable for example month, quarter, year, half year
  
  library(Information)
  library(ggplot2)
  library(dplyr)
  library(purrr)
  all_results              <- list()
  information_table        <- create_infotables(data = data, y = name_of_dependent_variable, bins = number_of_bins_for_continuous_variable)
  dataToPlotingIV          <- information_table$Summary[order(-information_table$Summary$IV), ]
  dataToPlotingIV$Variable <- factor(dataToPlotingIV$Variable, levels = dataToPlotingIV$Variable[order(-dataToPlotingIV$IV)])
  
  iv_for_all_variables_plot <- ggplot(dataToPlotingIV, aes(x = Variable, y = IV)) +
    geom_bar(width = 0.35, stat ='identity', color = 'darkblue', fill = 'white') +
    ggtitle('Information Value') +
    theme_bw() +
    theme(plot.title = element_text(size = 10)) +
    theme(axis.text.x = element_text(angle = 90))
  
  all_results[['iv_for_all_variables_plot']] <- iv_for_all_variables_plot
  all_results[['information_table']]         <- information_table
  
  if(!is.na(path_to_save_plot)) {
    jpeg(file = file.path(path_to_save_plot, paste0("iv_for_all_variables.jpg")))
    print(iv_for_all_variables_plot)
    dev.off()
  }
  
  prospectiveVariables <- names(information_table$Tables)
  
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
  
  if(!is.na(path_to_save_plot)) {
    if(exists("plotsOfWoe")) {
      all_results[["plotsOfWoe"]]<- plotsOfWoe
      for(variable in names(plotsOfWoe)) {
        path <- file.path(path_to_save_plot, paste0("woe_", variable, ".jpg"))
        jpeg(file = path)
        print(plotsOfWoe[[variable]])
        dev.off()
      }
    }
    
    if(exists("plotsOfIvInTime")) {
      all_results[["plotsOfIvInTime"]] <- plotsOfIvInTime
      for(variable in names(plotsOfIvInTime)){
        path <- file.path(path_to_save_plot, paste0("Iv_in_time_", variable, ".jpg"))
        jpeg(file = path)
        print(plotsOfIvInTime[[variable]])
        dev.off()
      }
    }
  }
  return(all_results)
}    



