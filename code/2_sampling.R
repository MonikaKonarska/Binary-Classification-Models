
samplingTrainTestValid <- function() {
  set.seed(1234)
  load(file.path(dataPath, "dataToTranTestValid_cleaned.RData"))
  
  dataToTranTestValid_cleaned$group <- sample(c("train", "test", "valid"),
                                              size = nrow(dataToTranTestValid_cleaned),
                                              replace = TRUE,
                                              prob = c(0.6, 0.2, 0.2))
  
  dataTrain  <- dataToTranTestValid_cleaned %>% filter(group == "train")
  dataTest   <- dataToTranTestValid_cleaned %>% filter(group == "test") %>% filter(!is.na(tot_cur_bal) & !is.na(total_rev_hi_lim))
  dataValid  <- dataToTranTestValid_cleaned %>% filter(group == "valid")
  
  save(dataTrain, dataTest, dataValid, file = file.path(dataPath, "dataToModeling.RData"))
}