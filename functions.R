
# Multiple plot function
# 
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
# source: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
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




DescribeVariables <- function(data) {
  #Args:
  # data: data frame objects with data to get basic infromations about variables
  
  if (! is.data.frame(data)) {
    stop("Object data is not a data frame.")
  }
  
  cols <- dim(data)[2]
  rows <- dim(data)[1]
  describeTable <- data.frame(variable = matrix( nrow = cols))
  
  for (var in seq_along(names(data))) {
    
    var_name <- names(data[var])
    
    describeTable[var, "variable"]  <- var_name
    describeTable[var, "type"]  <- class(data[[var]])
    describeTable[var, "numberOfNa"]  <- sum(is.na(data[[var]]))
    describeTable[var, "p_numberOfNa"]  <- describeTable[var, "numberOfNa"]/ rows
    describeTable[var, "uniqueValues"]  <- length(unique(data[[var]]))
    describeTable[var, "p_uniqueValues"]  <- describeTable[var,"uniqueValues"]/rows
    
    
    describeTable[var, "zeros"]  <- sum(data[[var]]== 0 )
    describeTable[var, "p_zeros"]  <- ifelse(!is.na(describeTable[var, "zeros"]),
                                             describeTable[var, "zeros"]/ rows,
                                             describeTable[var, "zeros"])
    
  }
  return(describeTable)
}



GiveDate <- function(date_month_year) {
  # Args:
  # date_month_year: this parametr is character with format for example: Apr-2019
  
  month <- substr(date_month_year, 1, 3)
  year = substr(date_month_year, regexpr("-", date_month_year)[1]+1, nchar(date_month_year))
  
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



