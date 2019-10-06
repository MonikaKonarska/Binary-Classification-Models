library(ggplot2)
library(dplyr)
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  # Multiple plot function
  # 
  # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
  # then plot 1 will go in the upper left, 2 will go in the upper right, and
  # 3 will go all the way across the bottom.
  # source: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
  
  library(grid)
  # Make a list from the ... arguments and plotlist
  plots    <- c(list(...), plotlist)
  numPlots <- length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



plotBoxPlotWithVariableAndTarget <- function(data,
                                             variable,
                                             targetVariable) {
  # data: data frame with columns defined in parameters: variable, targetVariable, groupedVariable
  # variable: name of continuous variable, name the same as in data frame
  # targetVariable:
  titleToplotBoxplot <- paste0("Boxplot of ", variable)
  subtitleToplotBox  <- paste0("")
  
  plotBoxplot <- ggplot(data = data, aes_string(x = targetVariable, y = variable, fill = targetVariable))+
    geom_boxplot(outlier.colour = "red", outlier.shape = 16) +
    theme(axis.text.x=element_text(angle = 90, hjust = 0),
          legend.position = "none")+
    labs(title = titleToplotBoxplot, subtitle = subtitleToplotBox)
  return(plotBoxplot)
} 


plotLinePlotWithMeanVariableAndTarget <- function(data,
                                                  variable,
                                                  targetVariable,
                                                  timeVariable) {
  # data: data frame with columns defined in parameters: variable, targetVariable, timeVariable
  # variable: name of continuous variable, name the same as in column in data
  # targetVariable:
  # timeVariable:
  
  data           <- data[!is.na(data[, variable]), ]
  titleOfPlot    <- paste0("Mean of ", variable, " in ", timeVariable)
  varString      <- enquos(variable)
  numberOfNARows <-  sum(is.na(data[[variable]])) 
  numberRows     <-  nrow(data)                       
  percentOfNA    <- percent(round(numberOfNARows/numberRows, 2))
  
  plotLineMeanValueOfVariableInTime <- data %>% 
    group_by_(.dots = c(targetVariable, timeVariable)) %>%
    dplyr::summarise_at(dplyr::vars(!!!varString), mean) %>% 
    ggplot(aes_string(x = timeVariable, y = variable, colour = targetVariable))+
    geom_line(aes_string(group = targetVariable))+
    scale_size_manual(values=c(2, 2))+
    theme(panel.grid.minor = element_blank(),
          panel.background = element_rect(colour = "black", size=0.5),
          axis.text.x=element_text(angle = 90, hjust = 0))+
    labs(title = titleOfPlot, caption = paste0("Percent of NA: ", percentOfNA))
  return(plotLineMeanValueOfVariableInTime)
}  


createVariableWithQuantiles <- function(variable, breakToDivideVectorOfProb = 0.25) {
  #Args:
  #variable: numeric vector of variable
  #breakToDivideVectorOfProb
  breaksQuantileInVariable <- seq(0, 1- breakToDivideVectorOfProb, by = breakToDivideVectorOfProb)
  variableWithQuantiles <- cut(variable, 
                               breaks = c(quantile(variable, probs = seq(0, 1, by = breakToDivideVectorOfProb))),
                               na.rm = TRUE,
                               names = TRUE,
                               include.lowest = TRUE,
                               right = TRUE,
                               labels = purrr::map_chr(breaksQuantileInVariable,
                                                       function(x) sprintf("%s-%s", x, x + breakToDivideVectorOfProb)))
  return(variableWithQuantiles)
}


plotTargetRateInQuantileVariable <- function(data,
                                             variable,
                                             targetVariable) {
  # data: data frame with columns defined in parameters: variable, targetVariable, timeVariable
  # variable: name of continuous variable, name the same as in column in data
  # targetVariable: 'target' columns
  variableNameCol      <- paste(variable, "quantile" , sep = "_")
  variableNameColquo   <- quo_name(variableNameCol) 
  variableNameColenquo <- enquo(variableNameCol)
  newValuesOfVariable  <- createVariableWithQuantiles(data[[variable]])
  
  data <- data %>% mutate(!!variableNameColquo := newValuesOfVariable)
  
  plotBarWithTargetRate <- data %>%
    dplyr::group_by(!!sym(variableNameCol)) %>%
    dplyr::summarise(sumOfTarget = sum(as.numeric(as.character(target))),
                     sumOfCustomers = n()) %>%
    mutate(rateTarget = sumOfTarget/sumOfCustomers) %>%
    ggplot(aes_string(x = variableNameCol, y = "rateTarget"))+
    geom_bar(stat= 'identity', fill ="brown2", color = "black")+
    scale_y_continuous(labels = scales::percent)+
    theme(axis.text.x=element_text(angle = 90, hjust = 0),
          panel.border = element_rect(linetype = "dashed", fill = NA))+
    geom_text(aes(label = scales::percent(round(rateTarget, 2)), y = rateTarget), position = position_stack(vjust = 0.5))+
    labs(title = "Target rate", subtitle = paste("variable: ", variableNameCol))
  return(plotBarWithTargetRate)
}


plotBarsOfCountCategoriesInTime <- function(data,
                                            variable,
                                            timeVariable) {
  # data: data frame with columns defined in parameters: variable, targetVariable, timeVariable
  # variable: name of continuous variable, name the same as in column in data
  plotBar <- ggplot(data, aes_string(x = timeVariable, fill = variable)) +
    geom_bar(stat = "count")+
    theme(panel.border = element_rect(linetype = "dashed", fill = NA),
          axis.text.x=element_text(angle = 90, hjust = 0)) +
    scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE), expand = c(0,0))
  return(plotBar)
}


plotBarsOfTargetInEachCategories <- function(data,
                                             variable,
                                             targetVariable) {
  # data: data frame with columns defined in parameters: variable, targetVariable, timeVariable
  # variable: name of continuous variable, name the same as in column in data
  # targetVariable: factor variable with values 0 or 1
  plotBar <- ggplot(data, aes_string(x = variable, fill = targetVariable)) +
    geom_bar(stat = "count", position = "dodge2")+
    theme(panel.border = element_rect(linetype = "dashed", fill = NA),
          axis.text.x=element_text(angle = 90, hjust = 0)) +
    scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE), expand = c(0,0))
  return(plotBar)
}