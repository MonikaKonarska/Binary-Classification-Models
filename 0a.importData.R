library(data.table)

importData <- function() {
  saveResultsPath <<- file.path(getwd(), "dataResults")
  dataPath        <<- file.path(getwd(), "data")
  data            <- fread(file.path(dataPath, "loan.csv"))
  data            <- setDF(data)
}