# createClustAll ---------------------------------------------------------------
#' @import mice
#' @title Creates ClustAllObject and perform imputations to deal with missing values and c
#' @aliases createClustAll,ClustAllObject-method,missOrNumeric,midsOrNA
#' @description
#' This method creates the ClustAllObject  and perform the imputations if the dataset contains missing values. The next step would be \code{\link{runClustAll}}
#' @usage createClustAll(data,
#'        nImputation=NULL,
#'        dataImputed=NULL)
#'
#' @param data Data Frame of the using data. It may contain missing (NA) values.
#' @param nImputation Numeric vector giving the number of imptations that want to be performed if data contains NAs.
#' @param dataImputed mids object created with \link[mice]{mice}. The data used for the imputation to create mids object and the data using must be the same.
#'
#' @return An object of class \code{\link{ClustAllObject-class}}
#'
#' @seealso \code{\link{runClustAll}}, \code{\link{ClustAllObject-class}}
#'
#' @examples
#' # scenario 1: data does not contain missing values
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc <- wdbc[,-c(1,2)]
#' obj_noNA <- createClustAll(data = wdbc)
#'
#' # scenario 2: data contains NA and imputations automatically
#' data("data_use", package = "ClustAll") # load example data
#' obj_NA <- createClustAll(data_NA, nImputation = 5)
#'
#' # scenario 3: data contains NA and imputations manually
#' data("data_use", package = "ClustAll") # load example data
#' ini <- mice(data_use, maxit = 0, print = FALSE)
#' pred <- ini$pred # predictor matrix
#' pred["bmi", c("tobacco", "alcohol", "PEnum")] <- 0 # example of removing predictors for each variable
#' imp <- mice(data_use, m=5, pred=pred, maxit=5, seed=1234, print=FALSE)
#' obj_imp <- createClustAll(data=data_use, dataImputed = imp)
#'
#' @export
setGeneric(
  name="createClustAll",
  def=function(data, nImputation=NULL, dataImputed=NULL, colValidation=NULL){standardGeneric("createClustAll")}
)

setMethod(
  f="createClustAll",
  signature=signature(
    data="data.frame",
    nImputation="numericOrNA",
    dataImputed="midsOrNA",
    colValidation="characterOrNA"),
  definition=function(data, nImputation=NULL, dataImputed=NULL, colValidation=NULL) {
    if (is.null(dataImputed)) {imputedNull <- TRUE} else {imputedNull <- FALSE}
    if (is.null(nImputation)) {nImputation <- 0}
    data <- checkDataIntroduced(data)
    nImputation <- validnImputation(nImputation, imputedNull)
    if (!is.null(colValidation)) {
      checkColumn(data, colValidation)
      dataValidation <- data[, colValidation]
      data <- subset(data, select = -which(colnames(data)==colValidation))
    } else {
      dataValidation <- NULL
      }

    if (validData(data, nImputation, dataImputed)) {nImputation <- 0}
    if (nImputation == 0 & is.null(dataImputed)) {
      dataImputed <- NULL
    } else if (is.null(dataImputed)) {
      message("Running default multiple imputation method. For manual imputation or more information visit <INCLUIR HELP O ALGO>") #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      dataImputed <- mice(data, m=nImputation, maxit=5, seed=1234)
    } else if (!is.null(dataImputed)) {
      if (validDataImputed(data, dataImputed)) { # check the imputed data is valid
        nImputation <- dataImputed$m
      }
    }
    clustAllObj <- new("ClustAllObject", data=data, dataImputed=dataImputed, nImputation=nImputation, dataValidation)
    message("\nClustALL object created successfully. You may use runClustAll.\n")
    return(clustAllObj)
  }
)
# END OF ClustAll_createClustAll.R
