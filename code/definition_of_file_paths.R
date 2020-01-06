# The script defines main file paths and loads needed Rdata

mainPath                          <<- file.path(getwd(), "..", "..")
dataPath                          <<- file.path(mainPath, "data")
folderToSavePlots                 <<- file.path(mainPath, "plots")
folderToSavecalculations          <<- file.path(mainPath, "calculations")
folderToSavePlotsSelectedFeatures <<- file.path(folderToSavePlots, "selectedFeatures")
folderToSavecalculations          <<- file.path(folderToSavePlots, "..", "calculations")
functionPath                      <<- file.path("..", "functions")


variablesFromFeature        <<- c("sub_grade", "grade", "int_rate", "installment", "total_pymnt", "total_pymnt_inv", "total_rec_prncp", "total_rec_int", "total_rec_late_fee", "recoveries", "collection_recovery_fee", "last_pymnt_amnt", "initial_list_status")
variablesAnotherToDelete    <<- c("id", "member_id", "issue_d", "loan_status", "funded_amnt_inv", "term", "verification_status", "funded_amnt", "group", "last_credit_pull_d", "collections_12_mths_ex_med", "acc_now_delinq")
groupedVariables            <- c("quarter", "month")

