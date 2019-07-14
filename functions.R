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


describe_variables <- function(data) {
  #Args:
  #data: data frame object
  
  if (! is.data.frame(data)) {
    stop("Object data is not a data frame.")
  }
  
  cols <- dim(data)[2]
  rows <- dim(data)[1]
  describeTable <- data.frame(variable = matrix( nrow = cols))
  
  for (var in seq_along(names(data))) {
    
    var_name <- names(data[var])
    describeTable[var, "variable"]       <- var_name
    describeTable[var, "type"]           <- class(data[[var]])
    describeTable[var, "numberOfNa"]     <- sum(is.na(data[[var]]))
    describeTable[var, "p_numberOfNa"]   <- describeTable[var, "numberOfNa"]/ rows
    describeTable[var, "uniqueValues"]   <- length(unique(data[[var]]))
    describeTable[var, "p_uniqueValues"] <- describeTable[var,"uniqueValues"]/rows
    describeTable[var, "zeros"]          <- sum(data[[var]] == 0 )
    describeTable[var, "p_zeros"]        <- ifelse(!is.na(describeTable[var, "zeros"]),
                                                   describeTable[var, "zeros"]/ rows,
                                                   describeTable[var, "zeros"])
  }
  return(describeTable)
}


convert_date_from_month_year <- function(date_month_year) {
  # Args:
  # date_month_year: this parametr is type character with format for example: Apr-2019
  
  month <- substr(date_month_year, 1, 3)
  year  <- substr(date_month_year, regexpr("-", date_month_year)[1]+1, nchar(date_month_year))
  
  tmp <- data.frame(data_string=date_month_year, month, year) %>%
    mutate(monthInNumber = case_when(month == "Jan" ~ 1,
                                     month == "Feb" ~ 2,
                                     month == "Mar" ~ 3,
                                     month == "Apr" ~ 4,
                                     month == "May" ~ 5,
                                     month == "Jun" ~ 6,
                                     month == "Jul" ~ 7,
                                     month == "Aug" ~ 8,
                                     month == "Sep" ~ 9,
                                     month == "Oct" ~ 10,
                                     month == "Nov" ~ 11,
                                     month == "Dec" ~ 12 ),
           date = as.Date(paste(year, monthInNumber, "01", sep = "-")))
  
  return(tmp$date)
}


changeCharacter2FactorVariableWithLackGroup <- function(data, variable_character = NULL, typeOfLack = NULL) {
  # Changes the character variable to factor type. In case when the variable has NA values then function return value 'LACK' for observation
  #
  # Args:
  #  x: data as data frame
  #  variable_character: vector of names of variables 
  if( is.character(data[variable_character]) ) {
    stop("The variable: ",variable_character, " is not character type")
  }
  if(is.null(variable_character)) {
    varTypeAll <- sapply(data, class)
    varChar <- names(varTypeAll[grep(x = varTypeAll, pattern = "character")])
    for(zm in varChar) {
      data[,is.na(zm)] <- 'LACK'
      
      if(!is.null(typeOfLack)) {
        data[which(data[[zm]] %in% typeOfLack), zm] <- 'LACK'
      }
      
      numberOfLevels   <- sort(unique(data[[zm]]))
      data[[zm]]       <- factor(data[[zm]], levels = numberOfLevels)
    }
  } else {
    data[[variable_character]]        <- as.character(data[[variable_character]])
    data[ ,is.na(variable_character)] <- 'LACK'
    if(!is.null(typeOfLack)) {
      data[which(data[[variable_character]] %in% typeOfLack), variable_character] <- 'LACK'
    }
    numberOfLevels                    <- sort(unique(data[[variable_character]]))
    data[[variable_character]]        <- factor(data[[variable_character]], levels = numberOfLevels)
  }
  return(data)
}


findLargeCategories <- function(data, minLevelOfLargeCategoryInAll = 0.85, searchFactors = TRUE) {
  # Function finds variables with a huge percentage of one category in variable
  # Args:
  #  data: data as data frame
  #  minLevelOfLargeCategoryInAll: the minimum level of percentage of one category in variable
  #  searchFactors: TRUE if looking for only in factor variables
  
  typesVariables <- sapply(data, class)
  if(searchFactors) {
    variablesToCount <- names(typesVariables[which(typesVariables == 'factor')])
  } else {
    variablesToCount <- names(typesVariables)
  }
  variablesWithCategories        <- vector(length = length(variablesToCount), mode = 'numeric') 
  names(variablesWithCategories) <- variablesToCount
  
  for(i in variablesToCount) {
    categoriesAmount                  <- table(data[[i]])
    maxAmount                         <- categoriesAmount[which.max(categoriesAmount)]
    maxAmountCategory                 <- names(maxAmount)
    percentOfMaxAmountCategory        <- maxAmount/sum(categoriesAmount)
    variablesWithCategories[[i]]      <- percentOfMaxAmountCategory
  }
  variablesWithLargeCategories        <- names(variablesWithCategories[which(variablesWithCategories >= minLevelOfLargeCategoryInAll)])
  return(variablesWithLargeCategories)
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
  all_plots <- grid_arrange_shared_legend(plotlist = list_of_plots, ncol = 5)
  return(all_plots)
}


