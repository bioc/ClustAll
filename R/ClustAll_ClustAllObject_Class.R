# setClassUnion includes defined new classes -----------------------------------
#' @import mice
setClassUnion("midsOrNULL", c("mids", "NULL"))
setClassUnion("midsOrNA", c("mids", "missing", "NULL"))
setClassUnion("listOrNULL", c("list", "NULL", "missing"))
setClassUnion("numericOrNA", c("numeric", "missing", "NULL"))
setClassUnion("characterOrNA", c("character", "missing", "NULL"))
setClassUnion("logicalOrNA", c("logical", "missing", "NULL"))
setClassUnion("matrixOrNULL", c("matrix", "NULL"))
setClassUnion("numericOrCharacter", c("numeric", "character"))


# ClustAllObject Class ---------------------------------------------------------
#' @title ClustAllObject
#' @aliases ClustAllObject-class
#' @description Stores the using data, imputations and results of the ClustAll pipeline.
#' @slot data Data Frame of the using data
#' @slot dataImputed midsOrNULL. Mids object from mice package that stores imputed data in case imputations have been made. Otherwise NULL
#' @slot dataValidation numericOrNA. Numeric vector of the TRUE labels
#' @slot nImputation Numeric giving the number of imputations
#' @slot processed Logical if the ClustAll pipeline have been runned
#' @slot summary_clusters listOrNULL. List of matrices of the clustering methods in case ClustAll pipeline have been runned. Othrwise NULL
#' @slot JACCARD_DISTANCE_F matrixOrNULL. Matrix containing the robust values of Jaccard distances if the ClustAll pipeline have been runned. Otherwise NULL
#' @export
setClass(
  Class="ClustAllObject",
  slots=list(
    data="data.frame",
    dataImputed="midsOrNULL",
    dataValidation="numericOrNA",
    nImputation ="numeric",
    processed="logical",
    summary_clusters="listOrNULL",
    JACCARD_DISTANCE_F="matrixOrNULL"
  )
)


# ClustAllObject constructor
#' constuctor for \code{\link{ClustAllObject-class}}
#' @title initializeClustAllObject
setMethod(
  f="initialize",
  signature="ClustAllObject",
  function(.Object,
           data,
           dataImputed,
           dataValidation,
           nImputation,
           processed,
           summary_clusters,
           JACCARD_DISTANCE_F) {
    .Object@data <- data
    .Object@dataImputed <- dataImputed
    .Object@nImputation <- nImputation
    .Object@dataValidation <- dataValidation
    .Object@processed <- FALSE
    .Object@summary_clusters <- NULL
    .Object@JACCARD_DISTANCE_F <- NULL
    validObject(.Object)

    return(.Object)
  }
)



# ClustAllObject MEMBER ACCESS METHODS -----------------------------------------

#' @title Retrieve the initial data from ClustAllObject
#' @aliases showData,ClustAllObject-method
#' @description
#' Generic function to retrieve the initial data used for \code{\link{createClustAll}} from a \code{\link{ClustAllObject-class}} object
#' @usage showData(Object)
#' @param Object \code{\link{ClustAllObject-class}} object
#'
#' @return The Data Frame with the initial data
#'
#' @seealso \code{\link{createClustAll}}, \code{\link{ClustAllObject-class}}, \code{\link{runClustAll}}
#'
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc$Diagnosis <- as.numeric(as.factor(wdbc$Diagnosis))
#' wdbc <- wdbc[,-c(1)] # delete patients IDs
#' obj_noNA <- createClustAll(data = wdbc, colValidation = "Diagnosis")
#' showData(obj_noNA)
#' @export
setGeneric(
  name="showData",
  def=function(Object){standardGeneric("showData")}
)

setMethod(
  f="showData",
  signature=signature(
    Object="ClustAllObject"),
  definition=function(Object) {

    return(Object@data)
  }
)


#' @title Retrieve the imputed data from ClustAllObject
#' @aliases showDataImputed,ClustAllObject-method
#' @description
#' Generic function to retrieve the imputed data obtained in \code{\link{createClustAll}} from a \code{\link{ClustAllObject-class}} object
#' @usage showDataImputed(Object)
#' @param Object \code{\link{ClustAllObject-class}} object
#'
#' @return Mids class object with the imputed data or NULL if no imputations have been made
#'
#' @seealso \code{\link{createClustAll}}, \code{\link{ClustAllObject-class}}, \code{\link{runClustAll}}
#'
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc$Diagnosis <- as.numeric(as.factor(wdbc$Diagnosis))
#' wdbc <- wdbc[,-c(1)] # delete patients IDs
#' obj_noNA <- createClustAll(data = wdbc, colValidation = "Diagnosis")
#' showDataImputed(obj_noNA)
#' @export
setGeneric(
  name="showDataImputed",
  def=function(Object){standardGeneric("showDataImputed")}
)

setMethod(
  f="showDataImputed",
  signature=signature(
    Object="ClustAllObject"),
  definition=function(Object) {

    return(Object@dataImputed)
  }
)


#' @title Retrieve the number of imputations made for the imputation step from ClustAllObject
#' @aliases showNumberImputations,ClustAllObject-method
#' @description
#' Generic function to retrieve the number of imputations in \code{\link{createClustAll}} from a \code{\link{ClustAllObject-class}} object
#' @usage showNumberImputations(Object)
#' @param Object \code{\link{ClustAllObject-class}} object
#'
#' @return Numeric vector giving the number of imputations. 0 in the case of no imputations
#'
#' @seealso \code{\link{createClustAll}}, \code{\link{ClustAllObject-class}}, \code{\link{runClustAll}}
#'
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc$Diagnosis <- as.numeric(as.factor(wdbc$Diagnosis))
#' wdbc <- wdbc[,-c(1)] # delete patients IDs
#' obj_noNA <- createClustAll(data = wdbc, colValidation = "Diagnosis")
#' showNumberImputations(obj_noNA)
#' @export
setGeneric(
  name="showNumberImputations",
  def=function(Object){standardGeneric("showNumberImputations")}
)

setMethod(
  f="showNumberImputations",
  signature=signature(
    Object="ClustAllObject"),
  definition=function(Object) {

    return(Object@nImputation)
  }
)


#' @title Retrieve the list with matrieces for different cluster methods from ClustAllObject
#' @aliases showSummaryClusters,ClustAllObject-method
#' @description
#' Generic function to retrieve the list with matrieces for different cluster methods results of \code{\link{runClustAll}} from a \code{\link{ClustAllObject-class}} object
#' @usage showSummaryClusters(Object)
#' @param Object \code{\link{ClustAllObject-class}} object
#'
#' @return List of matrices with all the clustering methods or NULL if runClustAll method have not been runned
#'
#' @seealso \code{\link{runClustAll}}, \code{\link{ClustAllObject-class}}
#'
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc$Diagnosis <- as.numeric(as.factor(wdbc$Diagnosis))
#' wdbc <- wdbc[,-c(1)] # delete patients IDs
#' obj_noNA <- createClustAll(data = wdbc, colValidation = "Diagnosis")
#' showSummaryClusters(obj_noNA)
#' @export
setGeneric(
  name="showSummaryClusters",
  def=function(Object){standardGeneric("showSummaryClusters")}
)

setMethod(
  f="showSummaryClusters",
  signature=signature(
    Object="ClustAllObject"),
  definition=function(Object) {

    return(Object@summary_clusters)
  }
)


#' @title Retrieve the matrix with Jaccard distances from ClustAllObject
#' @aliases showJaccardDistances,ClustAllObject-method
#' @description
#' Generic function to retrieve the matrix with Jaccard distances results of \code{\link{runClustAll}} from a \code{\link{ClustAllObject-class}} object
#' @usage showJaccardDistances(Object)
#' @param Object \code{\link{ClustAllObject-class}} object
#'
#' @return Matrix with Jaccard distances or NULL if runClustAll method have not been runned
#'
#' @seealso \code{\link{runClustAll}}, \code{\link{ClustAllObject-class}}
#'
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc$Diagnosis <- as.numeric(as.factor(wdbc$Diagnosis))
#' wdbc <- wdbc[,-c(1)] # delete patients IDs
#' obj_noNA <- createClustAll(data = wdbc, colValidation = "Diagnosis")
#' obj_noNA1 <- runClustAll(Object = obj_noNA, threads = 8)
#' showJaccardDistances(obj_noNA1)
#' @export
setGeneric(
  name="showJaccardDistances",
  def=function(Object){standardGeneric("showJaccardDistances")}
)

setMethod(
  f="showJaccardDistances",
  signature=signature(
    Object="ClustAllObject"),
  definition=function(Object) {

    return(Object@JACCARD_DISTANCE_F)
  }
)


#' @title Retrieve logical if runClustAll have been runned from ClustAllObject
#' @aliases isProcessed,ClustAllObject-method
#' @description
#' Generic function to retrieve the logical if \code{\link{runClustAll}} have been runned from a \code{\link{ClustAllObject-class}} object
#' @usage isProcessed(Object)
#' @param Object \code{\link{ClustAllObject-class}} object
#'
#' @return TRUE if runClustAll have been runned. Otherwise FALSE
#'
#' @seealso \code{\link{runClustAll}}, \code{\link{ClustAllObject-class}}
#'
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc$Diagnosis <- as.numeric(as.factor(wdbc$Diagnosis))
#' wdbc <- wdbc[,-c(1)] # delete patients IDs
#' obj_noNA <- createClustAll(data = wdbc, colValidation = "Diagnosis")
#' obj_noNA1 <- runClustAll(Object = obj_noNA, threads = 8)
#' isProcessed(obj_noNA1)
#' @export
setGeneric(
  name="isProcessed",
  def=function(Object){standardGeneric("isProcessed")}
)

setMethod(
  f="isProcessed",
  signature=signature(
    Object="ClustAllObject"),
  definition=function(Object) {

    return(Object@processed)
  }
)


#' @title Retrieve the numeric vector of validation data from ClustAllObject
#' @aliases showValidationData,ClustAllObject-method
#' @description
#' Generic function to retrieve numeric vector if it has been added with the true labels from a \code{\link{ClustAllObject-class}} object
#' @usage showValidationData(Object)
#' @param Object \code{\link{ClustAllObject-class}} object
#'
#' @return numeric vector if true labels have been added. Otherwise NULL
#'
#' @seealso \code{\link{ClustAllObject-class}}
#'
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc$Diagnosis <- as.numeric(as.factor(wdbc$Diagnosis))
#' wdbc <- wdbc[,-c(1)] # delete patients IDs
#' obj_noNA <- createClustAll(data = wdbc, colValidation = "Diagnosis")
#' obj_noNA1 <- runClustAll(Object = obj_noNA, threads = 8)
#' showValidationData(obj_noNA1)
#' @export
setGeneric(
  name="showValidationData",
  def=function(Object){standardGeneric("showValidationData")}
)

setMethod(
  f="showValidationData",
  signature=signature(
    Object="ClustAllObject"),
  definition=function(Object) {

    return(Object@dataValidation)
  }
)


#' @title Add the validation data into the ClustAllObject
#' @aliases addValidationData,ClustAllObject-method
#' @description
#' Generic function to add validation data to the \code{\link{ClustAllObject-class}} object
#' @usage addValidationData(Object, dataValidation)
#' @param Object \code{\link{ClustAllObject-class}} object
#' @param dataValidation numeric vector with the validation data
#'
#' @return \code{\link{ClustAllObject-class}} object
#'
#' @seealso \code{\link{ClustAllObject-class}}
#'
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc$Diagnosis <- as.numeric(as.factor(wdbc$Diagnosis))
#' wdbc <- wdbc[,-c(1)] # delete patients IDs
#' obj_noNA <- createClustAll(data = wdbc, colValidation = "Diagnosis")
#' obj_noNA1 <- addValidationData(Object = obj_noNA, dataValidation = wdbc$Diagnosis)
#' @export
setGeneric(
  name="addValidationData",
  def=function(Object, dataValidation){standardGeneric("addValidationData")}
)

setMethod(
  f="addValidationData",
  signature=signature(
    Object="ClustAllObject",
    dataValidation="numericOrCharacter"),
  definition=function(Object, dataValidation) {

    if (!is.null(Object@dataValidation)) {
      message("WARNING! The object already have validation data. Rewriting the validation data...")
    }

    dataValidation <- checkVectorIntroduced(dataValidation)

    if (length(dataValidation) != nrow(Object@data)) {
      message("The length of the data and introduced validation data are different. Make sure you are introducing it correcly.")
      stop()
    }

    Object@dataValidation <- dataValidation

    return(Object)
  }
)

#' @name mids-class
#' @rdname mids-class
#' @aliases mids-class mids
#' @author Stef van Buuren, Karin Groothuis-Oudshoorn, 2000
#' @references van Buuren S and Groothuis-Oudshoorn K (2011). \code{mice}:
#' Multivariate Imputation by Chained Equations in \code{R}. \emph{Journal of
#' Statistical Software}, \bold{45}(3), 1-67.
NULL
# END OF ClustAll_ClustAllObject_Class.R
