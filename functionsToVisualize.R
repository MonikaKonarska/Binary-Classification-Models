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