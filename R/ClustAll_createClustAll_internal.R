# createClustAll_internal functions definition ---------------------------------
# These functions are not meant to be invoked directly by the user.
# See the createClusteAll function instead.


# This functions checks if the data frame introduced contains character vectors.
# In that case, it transforms them into numerical values assuming they are categorical values.
checkDataIntroduced <- function(data) {
  if (any(sapply(data, is.character))) {
    message("\nWARNING!")
    message("The dataset contains character values. Assuming they are categorical variables, transforming the categorical variables into numerical variables.")
    message("Before continuing, check that the transformation has been carried out correctly.\n")
    must_convert_variables <- which(sapply(data, is.character))

    data_new <- data
    data_new[, must_convert_variables] <- sapply(data_new[, must_convert_variables], function(x) as.numeric(as.factor(x)))


  } else {
    data_new <- data
  }

  return(data_new)
}


# This function checks the numerical vector of the number of imputations is valid
validnImputation <- function(nImputation, imputedNull) {
  if (!is.numeric(nImputation)) {
    if (imputedNull == FALSE) {
      message("Argument nImputation is not valid. Please include a positive number of imputations.")
      stop()
    }
  } else if (nImputation < 0) {
    message("Argument nImputation is not valid. Please include a positive number of imputations.")
    stop()
  } else if (nImputation %% 1 != 0) { # check is not float value
    message("Decimal number included, instead the decimal value will be rounded.")
    nImputation <- round(nImputation, digits=0)
  } else if (is.null(nImputation) & imputedNull == TRUE) {
    nImputation = 0
  }

  return(nImputation)
}


# This function checks if the introduced dataset has missing values and the arguments introduced to createClustAll
validData <- function(data, nImputation, dataImputed) {
  if (anyNA(data) == TRUE & nImputation == 0 & is.null(dataImputed)) {
    message("The dataset contains NA values. Please specify number of imputations (nImputation) for imputation.")
    stop()

  } else if (anyNA(data) == FALSE & nImputation > 0 & is.null(dataImputed)) {
    message("The dataset does NOT contain NA values. The imputation process will not be applied.")
    return(TRUE)

  } else if (anyNA(data) == FALSE  & is.null(dataImputed) == FALSE) {
    message("The dataset does NOT contain NA values. The imputed data introduced will not be used.")
    return(TRUE)

  } else {
    return(FALSE)
  }
}


# This function checks that the dataImputed manually is mids class and that the same input data have been used for imputation
validDataImputed <- function(data, dataImputed) {
  if (is(dataImputed, "mids")) {
    if (identical(data, dataImputed$data)){
      return(TRUE)

    } else {
      message("The introduced data and the data used for imputation are different. Please make sure you are using the same.")
      stop()
    }

  } else {
    message("You must introduce mice::mice function output. An object of class mice. For more information visit <INCLUDE HELP>") #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    stop()
  }
}


checkColumn <- function(data, colValidation) {
  if (class(colValidation) != "character") {
    message("Please make sure to introduce the name of the valdiation column.")
    stop()
  }
  if (!colValidation %in% colnames(data)) {
    message("The introduced column names is not in the dataset. Please make sure to introduce it correclty.")
    stop()
  }
}
# END OF ClustAll_createClustAll_internal.R
