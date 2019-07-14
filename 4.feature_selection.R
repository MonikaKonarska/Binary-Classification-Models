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
