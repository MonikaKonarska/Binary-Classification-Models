
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





