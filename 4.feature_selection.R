library(Information)
source("3.feature_engineering.R")


dataTrainWithNewFeatures$target <- as.numeric(as.character(dataTrainWithNewFeatures$target))

information_table <- create_infotables(data = dataTrainWithNewFeatures, y = "target", bins = 6)

dataToPlotingIV          <- information_table$Summary[order(-information_table$Summary$IV), ]
dataToPlotingIV$Variable <- factor(dataToPlotingIV$Variable,
                                   levels = dataToPlotingIV$Variable[order(-dataToPlotingIV$IV)])

iv_for_all_variables_plot <- ggplot(dataToPlotingIV, aes(x = Variable, y = IV)) +
  geom_bar(width = .35, stat ='identity', color = 'darkblue', fill = 'white') +
  ggtitle('Information Value') +
  theme_bw() +
  theme(plot.title = element_text(size = 10)) +
  theme(axis.text.x = element_text(angle = 90))





prospectiveVariables <- names(information_table$Tables)
plotsOfWoe           <- list()
plotsOfIvInTime      <- list()

variable<-'loan_amnt'
for (variable in prospectiveVariables) {
  
  tableWithWoes <- information_table$Tables
  IValue <- information_table$Summary[which(information_table$Summary$Variable == variable), "IV"]
  
  woe_plot <-  ggplot(tableWithWoes[[variable]],  aes_string(x = variable, y = 'WOE'))+
    geom_bar(stat = 'identity', fill = "darkblue")+
    theme(axis.text.x = element_text(angle = 90))+
    ggtitle(label = "Weight of Evidence (WOE)", subtitle = paste0("Variable: ", variable, "\n", "Information value:", round(IValue,4)))
  
  plotsOfWoe[[variable]] <- woe_plot
  
  
  quarters <- as.character(unique(dataTrainWithNewFeatures$quarter))
  woeInTime <- data.frame(quarter = quarters, iv = NA, stringsAsFactors = FALSE)
  
  for (q in quarters) {
    
    data <- dataTrainWithNewFeatures %>%
      select_('quarter', variable, 'target') %>%
      filter(quarter == q) %>%
      select(-c(quarter))
    
    informationTableForVariable <- create_infotables(data, y = "target", bins = 6)
    woeInTime[which(woeInTime[['quarter']] == q), 'iv'] <- informationTableForVariable$Summary$IV
  }
  
  plotIvInTime <- ggplot(data = woeInTime, aes(x = quarter, y = iv))+
    geom_bar(stat = 'identity', fill = "green")+
    theme(axis.text.x = element_text(angle = 90))+
    ggtitle(label = "Information value for each quarter in train dataset",
            subtitle = paste0("Variable: ", variable))
  
  plotsOfIvInTime[[variable]] <- plotIvInTime
}



dir.create(path = file.path(getwd(), "plots"))
folderToSavePlots <- file.path(getwd(), "plots")

for(variable in names(plotsOfIvInTime)){
  
  path <- file.path(folderToSavePlots, paste0("Iv_in_time_", variable, ".jpg"))
  jpeg(file = path)
  print(plotsOfIvInTime[[variable]])
  dev.off()
}

for(variable in names(plotsOfWoe)){
  
  path <- file.path(folderToSavePlots, paste0("woe_", variable, ".jpg"))
  jpeg(file = path)
  print(plotsOfWoe[[variable]])
  dev.off()
}

jpeg(file = file.path(folderToSavePlots, paste0("iv_for_all_variables.jpg")))
print(iv_for_all_variables_plot)
dev.off()

