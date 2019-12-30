library(data.table)

importData <- function() {
  data            <- fread(file.path(dataPath, "loan.csv"))
  data            <- setDF(data)
}