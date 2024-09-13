# createClustAll ---------------------------------------------------------------
#' @title createClustAll: Creates ClustAllObject
#' @aliases createClustAll,data.frame,numericOrNA,ANY,characterOrNA-method
#' @description
#' Creates the ClustAllObject object. Applies one-hot encoding to columns with
#' categorical values from the input data and extracts the validation column if
#' available. It performs data imputation if the dataset contains missing values
#' and no imputed dataset is provided.
#' In total there are 3 scenarios when we create the object:
#' - Scenario 1: data does not contain missing values.
#' -Scenario 2: data contains NAs and there is no imputed data. Then, it
#' performs the imputations automatically.
#' - Scenario 3: data contains NAs and imputed data is provided manually.
#' Once \code{\link{ClustAllObject-class}} has been created, the ClustALL
#' pipeline can be run executing \code{\link{runClustAll}}.
#' @usage createClustAll(data=data,
#'                       nImputation=NULL,
#'                       dataImputed=NULL,
#'                       colValidation=NULL)
#' @param data Data Frame of the using data. It may contain missing values (NA).
#' @param nImputation Number of imputations to be computed in case the data
#' contains NAs and impued data is not available.
#' @param dataImputed mids object created with mice package. The input data
#' for the imputation and the data must be the same.
#' @param colValidation vector with the referece labeling of the original
#' dataset provided in “data” (if available). Default is NULL.
#' @return \code{\link{ClustAllObject-class}} object.
#' @seealso \code{\link{runClustAll}}, \code{\link{ClustAllObject-class}}
#' @examples
#' # Scenario 1: data does not contain missing values
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc <- wdbc[,-c(1,2)]
#' obj_noNA <- createClustAll(data = wdbc)
#'
#' # Scenario 2: data contains NAs and there is no imputed data.
#' # Then it performs the imputations automatically
#' data("BreastCancerWisconsinMISSING", package = "ClustAll")
#' \donttest{
#' obj_NA <- createClustAll(wdbcNA, nImputation = 2)
#' }
#' # Scenario 3: data contains NAs and imputed data is provided manually
#' data("BreastCancerWisconsinMISSING", package = "ClustAll")
#' ini <- mice::mice(wdbcNA, maxit = 0, print = FALSE)
#' pred <- ini$pred # predictor matrix
#' pred["radius1", c("perimeter1", "area1", "smoothness1")] <- 0 # example of
#' # how to remove predictors
#' \donttest{
#' imp <- mice::mice(wdbcNA, m=5, pred=pred, maxit=5, seed=1234, print=FALSE)
#' obj_imp <- createClustAll(data=wdbcNA, dataImputed = imp)
#' }
#'
#' @export
setGeneric(
  name="createClustAll",
  def=function(data=data, nImputation=NULL, dataImputed=NULL,
               colValidation=NULL)
  {standardGeneric("createClustAll")}
)

setMethod(
  f="createClustAll",
  signature=signature(
    data="data.frame",
    nImputation="numericOrNA",
    dataImputed="ANY",
    colValidation="characterOrNA"),
  definition=function(data=data, nImputation=NULL,
                      dataImputed=NULL, colValidation=NULL) {
    dataOriginal <- data
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
      message("Running default multiple imputation method.")
      message("For more information check mice package.")
      dataImputed <- mice(data, m=nImputation, maxit=5, seed=1234)
    } else if (!is.null(dataImputed)) {
      # check the imputed data
      if (validDataImputed(data=data, dataImputed=dataImputed,
                           dataOriginal=dataOriginal)) {
        nImputation <- dataImputed$m
      }
    }
    clustAllObj <- new("ClustAllObject",data=data,dataOriginal=dataOriginal,
                       dataImputed=dataImputed, nImputation=nImputation,
                       dataValidation=dataValidation)
    message("\nClustALL object was created successfully. You can run runClustAll.")

    return(clustAllObj)
  }
)
# END OF ClustAll_createClustAll.R
