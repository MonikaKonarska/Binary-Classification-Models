# library(ggplot2)
# library(dplyr)
library(tidyverse)
library(cowplot)
library(scales)

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


plotTargetRateInEachCategories <- function(data,
                                           variable,
                                           targetVariable) {
  # data: data frame with columns defined in parameters: variable, targetVariable, timeVariable
  # variable: name of continuous variable, name the same as in column in data
  # targetVariable: factor variable with values 0 or 1
  targetVariableEnquo <- enquo(targetVariable)
  plotBarWithTargetRate <- data %>%
    dplyr::group_by(!!sym(variable)) %>% 
    dplyr::summarise(sumOfTarget = sum(as.numeric(as.character(!!sym(targetVariable)))),
                     amountConsumer = n()) %>%
    mutate(targetRate = sumOfTarget/amountConsumer) %>% 
    ggplot(aes_string(x = variable, y = "targetRate")) +
    geom_bar(stat = 'identity', fill ="brown2", color = "black")+
    scale_y_continuous(expand = c(0,0), labels = scales::percent)+
    theme(axis.text.x=element_text(angle = 90, hjust = 0),
          panel.border = element_rect(linetype = "dashed", fill = NA))+
    geom_text(aes(label = scales::percent(round(targetRate, 2)), y = targetRate),
              position = position_stack(vjust = 0.5))+
    labs(title = "Target rate", subtitle = paste("variable:", variable))
  
  return(plotBarWithTargetRate)
}

plotTargetRateInTimeForCategories <- function(data,
                                              variable,
                                              targetVariable,
                                              timeVariable) {
  # data: data frame with columns defined in parameters: variable, targetVariable, timeVariable
  # variable: name of continuous variable, name the same as in column in data
  # targetVariable: factor variable with values 0 or 1
  
  plotLineWithTargetRate <- data %>%
    dplyr::group_by(!!sym(variable), !!sym(timeVariable)) %>% 
    dplyr::summarise(sumOfTarget = sum(as.numeric(as.character(!!sym(targetVariable)))),
                     amountConsumer = n()) %>%
    mutate(targetRate = sumOfTarget/amountConsumer) %>%
    ggplot(aes_string(x = timeVariable, y = "targetRate", group = variable, colour = variable)) +
    geom_line(size = 1, aes(linetype =  variable))+
    theme(axis.text.x=element_text(angle = 90, hjust = 0),
          panel.border = element_rect(linetype = "dashed", fill = NA))+
    scale_y_continuous(labels = scales::percent)
  return(plotLineWithTargetRate)
}




plotTargetInTimeForEachCategory <- function(data,
                                            variable,
                                            timeVariable) {
  # data: data frame with columns defined in parameters: variable, targetVariable, timeVariable
  # variable: name of continuous variable, name the same as in column in data
  plotBar <-   data %>%
    group_by(!!sym(timeVariable), !!sym(targetVariable), !!sym(variable)) %>%
    dplyr::summarise(n =n()) %>%
    mutate(target01 = ifelse(target == 1, "target: 1", "target: 0"))%>%
    ggplot(aes_string(x = timeVariable, y = "n", fill = variable)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ target01, nrow = 2)+
    theme(axis.text.x=element_text(angle = 90, hjust = 0),
          panel.border = element_rect(linetype = "dashed", fill = NA))+
    labs(subtitle = paste("variable:", variable))
  
  return(plotBar)
}



plotContinuousVariableWithTarget <- function(data, variable, targetVariable, timeVariable, groupedVariable) {
  boxplot <- plotBoxPlotWithVariableAndTarget(data, variable, targetVariable)
  plotLine <- plotLinePlotWithMeanVariableAndTarget(data, variable, targetVariable, timeVariable)
  plotRate <- plotTargetRateInQuantileVariable(data, variable, targetVariable)
  
  title_theme <- ggdraw() + draw_label(variable)
  plotWithTheme <- plot_grid(title_theme, boxplot,  nrow = 1)+theme(plot.background = element_rect(fill = "cornsilk"))
  plots2 <- plot_grid(plotLine, plotRate, nrow=1)
  plot_grid(plotWithTheme, plots2, nrow = 2)
  #multiplot(boxplot, plotLine, plotRate, cols = 2)
}


plotDiscreteVariableWithTarget <- function(data, variable, timeVariable, targetVariable) {
  plotBarCounts            <- plotBarsOfCountCategoriesInTime(data, variable, timeVariable)
  plotTargetRate           <- plotTargetRateInEachCategories(data, variable, targetVariable)
  plotLineTargetRateInTime <- plotTargetRateInTimeForCategories(data, variable, targetVariable, timeVariable)
  #multiplot(plotBarCounts, plotTargetRate, plotLineTargetRateInTime, cols = 2)
  title_theme <- ggdraw() + draw_label(variable)
  plotWithTheme <- plot_grid(title_theme, plotBarCounts,  nrow = 1)+theme(plot.background = element_rect(fill = "cornsilk"))
  plots2 <- plot_grid(plotTargetRate, plotLineTargetRateInTime, nrow=1)
 
  plot_grid(plotWithTheme, plots2, nrow = 2)
}


createPlotsForContinousVariables <- function(variable_name = "",
                                             data = "",
                                             type_of_plots = c("density", "histogram", "boxplot"),
                                             groupBy = NULL) {
  # Function creates plots (density or histogram, boxplot)
  # Args:
  # variable_name: name of continous variable 
  # data: data frame 
  # type_of_plots: types of plots to create
  # groupBy: variable to grouping plots
  
  type_of_plots <- match.arg(type_of_plots)
  
  if(is.null(groupBy)) {
    if(type_of_plots == "density") {
      plot <- ggplot(data = dataTrain, aes_string(variable_name))+
        geom_density()+
        labs(title = paste("Density of", variable_name))
    } else if (type_of_plots == "histogram") {
      plot <- ggplot(data, aes_string(variable_name))+
        geom_histogram() +
        labs(title = paste("Histogram of", variable_name))
    } else if (type_of_plots == "boxplot") {
      plot <- ggplot(data, aes_string(y = variable_name))+
        geom_boxplot()+
        labs(title = paste("Boxplot of", variable_name))+
        theme(axis.title = element_text(size = 6))
    }
  } else {
    if(type_of_plots == "density"){
      plot <- ggplot(data, aes_string(x = variable_name, y = "..density.."))+
        geom_density(aes_string(fill = groupBy), position = "stack")+
        labs(title = paste("Density of", variable_name, "grouping by", groupBy))
    } else if (type_of_plots == "histogram") {
      plot <- ggplot(data, aes_string(variable_name))+
        geom_histogram()+
        facet_wrap(groupBy)
    } else if (type_of_plots == "boxplot") {
      plot <- ggplot(data, aes_string(y = variable_name, x = groupBy))+
        geom_boxplot()+
        theme(axis.text.x = element_text(angle = 45))
    }
  }
  return(plot)
}  


grid_arrange_shared_legend <- function(..., 
                                       plotlist=NULL,
                                       ncol = length(list(...)),
                                       nrow = NULL,
                                       position = c("bottom", "right")) {
  # Function shares a legend between multiple plots that do not also share axes
  # Code of function from website: https://github.com/tidyverse/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
  
  plots <- c(list(...), plotlist)
  
  if (is.null(nrow)) nrow = ceiling(length(plots)/ncol)
  
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  
  grid.newpage()
  grid.draw(combined)
  
  # return gtable invisibly
  invisible(combined)
}





visualizeDensityVariablesGroupedByCategories <- function(data = NA,
                                                         names_numeric_variables = NA,
                                                         names_variable_to_group = NA,
                                                         position = c("bottom", "right")) {
  # Visualization density of variables on one page
  #
  # Args:
  # data: data as data frame,
  # names_numeric_variables: vector of names numeric variables,
  # names_variable_to_group: name of grouped variable on plot
  # position: position of main legend on page
  
  list_of_plots <- list()
  
  for(variable in names_numeric_variables) {
    plot <- ggplot(data = data, aes_string(x = variable, fill = names_variable_to_group, color = names_variable_to_group)) +
      geom_density(alpha = 0.3, size = 0.5) +
      scale_fill_brewer(palette = "Set1") +
      scale_color_brewer(palette = "Set1")
    list_of_plots[[variable]] <- plot
  }
  all_plots <- grid_arrange_shared_legend(plotlist = list_of_plots, ncol = 3)
  return(all_plots)
}
