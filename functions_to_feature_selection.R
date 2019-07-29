
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





