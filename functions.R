

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


detect_not_outliers <- function(data_set, column_name, thres = 3, na.rm = TRUE) {
  mean <- mean(data_set[[column_name]], na.rm = na.rm)
  sd <- sd(data_set[[column_name]], na.rm = na.rm)
  
  dataset <- data_set %>%
    mutate(isvalueNA = case_when(is.na(!!sym(column_name)) ~1, TRUE ~ 0)) %>%
    mutate(is_outlier = case_when(isvalueNA == 1 ~ FALSE,
                                  isvalueNA == 0 ~ (abs(!!sym(column_name)- mean(!!sym(column_name), na.rm = na.rm)) <= thres * sd(!!sym(column_name), na.rm = na.rm)))) %>%
    select(is_outlier)
  
  return(dataset[["is_outlier"]])
}


assignWoeValueInVariables <- function(variables_name, listOfWoe, data) {
  # Function creates new variables (_woe) with woe values 
  # Args:
  #  variables_name: vector of names variables
  #  listOfWoe: list of tables (woe values in each inteval variable) for each variables
  #  data: data.frame
  
  for(i in variables_name) {
    variableNameenquo <- enquo(i)
    variableNameenquoname <- quo_name(variableNameenquo)
    variableNameenquocut <- paste0(quo_name(variableNameenquo), "_cut")
    variableNameenquowoe <- paste0(quo_name(variableNameenquo), "_woe")
    
    woeData <- data.frame(listOfWoe[[i]])
    woeData <- woeData %>%
      separate(!! quo_name(variableNameenquo), into = c("bin1", "bin2"), sep = ",") %>%
      mutate(bin1 = as.numeric(substr(bin1, 2, nchar(bin1))),
             bin2 = as.numeric(substr(bin2, 1, nchar(bin2)-1)))
    
    woeData[["bin1"]][1] <- -Inf
    woeData$binToCut1 <- woeData[["bin1"]]
    woeData[["binToCut1"]][nrow(woeData)] <- Inf 
    
    data[variableNameenquocut] <- cut(x = data[[i]], breaks = woeData[["binToCut1"]], include.lowest = TRUE, dig.lab=10)
    woeData <- woeData[1:nrow(woeData)-1, ]
    
    levels <- levels(data[[variableNameenquocut]])
    woeData["bins"] <- levels
    data[variableNameenquowoe] <- plyr::mapvalues(x = data[[variableNameenquocut]], from = woeData[["bins"]], to = woeData[["WOE"]])
  }
  return(data)
}





