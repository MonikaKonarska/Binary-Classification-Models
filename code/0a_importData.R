library(data.table)

importData <- function() {
  dir.create(path = dataPath, showWarnings = FALSE)
  data            <- fread(file.path(dataPath, "loan.csv"))
  data            <- setDF(data)
}