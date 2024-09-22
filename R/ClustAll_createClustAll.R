# createClustAll ---------------------------------------------------------------
#' @title Create ClustAllObject for Patient Stratification Analysis
#' @aliases createClustAll,data.frame,numericOrNA,ANY,characterOrNA-method
#' @description
#' This function initializes a ClustAllObject, which serves as the core data
#' structure for the ClustAll package. It preprocesses the input data, handles
#' missing values through imputation if necessary, and prepares the data for
#' subsequent clustering analysis.
#'
#' @usage createClustAll(data, nImputation = NULL, dataImputed = NULL,
#'                       colValidation = NULL)
#'
#' @param data A data frame containing the input data. It may include missing
#' values (NAs) and can contain both numeric and categorical variables.
#' @param nImputation An integer specifying the number of imputations to perform
#' if the data contains missing values. Default is NULL, which means no imputation
#' unless missing values are detected.
#' @param dataImputed A 'mids' object created by the mice package, containing
#' pre-computed imputations. If provided, this will be used instead of performing
#' new imputations. Default is NULL.
#' @param colValidation A character string specifying the name of the column in
#' 'data' that contains validation labels (true classifications). This column
#' will be extracted and stored separately. Default is NULL.
#'
#' @return An unprocessed ClustAllObject without stratification results. Use
#' runClustAll on this object to execute the ClustAll pipeline.
#'
#' @details
#' The createClustAll function performs several key steps in preparing data for
#' the ClustAll analysis pipeline:
#'
#' \enumerate{
#'   \item Data Preprocessing:
#'   \itemize{
#'     \item Applies one-hot encoding to categorical variables, converting them to
#'           a format suitable for clustering algorithms.
#'     \item Extracts the validation column (if specified) and stores it separately.
#'   }
#'
#'   \item Missing Data Handling:
#'   \itemize{
#'     \item If the data contains missing values and no imputed data is provided,
#'           it performs multiple imputation using the mice package.
#'     \item The number of imputations is determined by the 'nImputation' parameter
#'           or set to a default if not specified.
#'   }
#'
#'   \item Object Initialization:
#'   \itemize{
#'     \item Creates a new ClustAllObject with slots for original data, processed data,
#'           imputed data (if applicable), and validation data (if provided).
#'   }
#' }
#'
#' The function accommodates three main scenarios:
#' \itemize{
#'   \item Data without missing values: Preprocessing is applied, no imputation needed.
#'   \item Data with missing values, no pre-computed imputations: Automatic imputation is performed.
#'   \item Data with missing values and pre-computed imputations: The provided imputations are used.
#' }
#'
#' @note
#' \itemize{
#'   \item Categorical variables in the input data should be coded as factors.
#'   \item If 'dataImputed' is provided, it must correspond exactly to the input data
#'         in terms of dimensions and variable names.
#'   \item The 'colValidation' parameter allows for the incorporation of known
#'         classifications, which can be used later for validating clustering results.
#'   \item After creating the ClustAllObject, use \code{\link{runClustAll}} to perform
#'         the actual clustering analysis.
#' }
#'
#' @seealso \code{\link{runClustAll}}, \code{\link{ClustAllObject-class}},
#' \code{\link{addValidationData}}
#'
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
