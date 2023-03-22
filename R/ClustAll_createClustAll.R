# createClustAll ---------------------------------------------------------------
#' @import mice
#' @title Perform imputations to deal with missing values and creates ClustAllObject
#' @aliases createClustAll,ClustAllObject-method,missOrNumeric,midsOrNA
#' @description 
#' This function perform the imputation if the dataset contains missing values and creates the ClustAllObject. The next step would be \code{\link{runClustAll}} 
#' @usage createClustAll(data, 
#'                       nImputation=NA, 
#'                       dataImputed=NA)
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
#' MISSING EXAMPLES
#' data("data_use")
#' 
#' # scenario 1: data contains na and nimp is introduces > 0
#' # scenario 2: data no contains na and neither nimp nor dataImputed are included. Not impputations performed
#' # scenario 3: contain character values and transfor to numerical assuming they are categoticals
#' # scenario 4: data contains na and dataImptued is introduces and no nimputations
#' # scenario 3:
#' 
#' 
#' 
#' 
#' 
#' @export
setGeneric(
  name="createClustAll",
  def=function(data, nImputation=NA, dataImputed=NA){standardGeneric("createClustAll")}
)

setMethod(
  f="createClustAll",
  signature=signature(
    data="data.frame",
    nImputation="numericOrNA",
    dataImputed="midsOrNA"),
  definition=function(data, nImputation=0, dataImputed=NA) {
    data <- checkDataIntroduced(data)
    nImputation <- validnImputation(nImputation)
    
    if (validData(data, nImputation, dataImputed)) { # check the data is valid
      nImputation <- 0
    }
    
    if (nImputation == 0 & anyNA(dataImputed)) {
      dataImputed <- NULL
      
    } else if (anyNA(dataImputed)) {
      message("Running default multiple imputation method. For manual imputation or more information visit <INCLUIR HELP O ALGO>") #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      dataImputed <- mice(data, m=nImputation, maxit=5, seed=1234)
      
    } else {
      if (validDataImputed(data, dataImputed)) { # check the imputed data is valid
        nImputation <- object@dataImputed$m
      }
    }
    
    return(new("ClustAllObject", data=data, dataImputed=dataImputed, nImputation=nImputation))
  }
)
# END OF ClustAll_createClustAll.R