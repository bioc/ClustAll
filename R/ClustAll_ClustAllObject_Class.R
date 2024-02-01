# setClassUnion includes the new classes defined -------------------------------
setClassUnion("listOrNULL", c("list", "NULL", "missing"))
setClassUnion("numericOrNA", c("numeric", "missing", "NULL"))
setClassUnion("characterOrNA", c("character", "missing", "NULL"))
setClassUnion("logicalOrNA", c("logical", "missing", "NULL"))
setClassUnion("matrixOrNULL", c("matrix", "NULL"))
setClassUnion("numericOrCharacter", c("numeric", "character"))


# ClustAllObject Class ---------------------------------------------------------
#' @title ClustAllObject
#' @aliases ClustAllObject-class
#' @description Stores the original data used, the imputed datasets and the
#' results of the ClustAll pipeline.
#' @slot data Data Frame of the data used. Maybe modified from the input
#' data.
#' @slot dataOriginal Data Frame of the original data introduced.
#' @slot dataImputed  Mids object derived from the
#' mice package that stores the imputed data, in case
#' imputation was applied. Otherwise NULL.
#' @slot dataValidation labelling numericOrNA. Original data labelling.
#' @slot nImputation Number of multiple imputations to be applied.
#' @slot processed Logical if the ClustAll pipeline has been executed previously
#' @slot summary_clusters listOrNULL. List with the resulting stratifications
#' for each combination of clustering methods (distance + clustering algorithm)
#' and depth, in case ClustAll pipeline has been executed previously.
#' Otherwise NULL.
#' @slot JACCARD_DISTANCE_F matrixOrNULL. Matrix containing the Jaccard
#' distances derived from the robust populations stratifications if ClustAll
#' pipeline has been executed previously. Otherwise NULL.
#' @export
setClass(
  Class="ClustAllObject",
  slots=list(
    data="data.frame",
    dataOriginal="data.frame",
    dataImputed="ANY",
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
#' @return An object of class \code{\link{ClustAllObject-class}}

setMethod(
  f="initialize",
  signature="ClustAllObject",
  function(.Object,
           data,
           dataOriginal,
           dataImputed,
           dataValidation,
           nImputation,
           processed,
           summary_clusters,
           JACCARD_DISTANCE_F) {
    .Object@data <- data
    .Object@dataOriginal <- data
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
#' Generic function to retrieve the initial data used for
#' \code{\link{createClustAll}} from a \code{\link{ClustAllObject-class}} object
#' @usage showData(Object)
#' @param Object \code{\link{ClustAllObject-class}} object
#' @return The Data Frame with the initial data
#' @seealso \code{\link{createClustAll}}, \code{\link{ClustAllObject-class}},
#' \code{\link{runClustAll}}
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
#' Generic function to retrieve the imputed data obtained in
#' \code{\link{createClustAll}} from a \code{\link{ClustAllObject-class}} object
#' @usage showDataImputed(Object)
#' @param Object \code{\link{ClustAllObject-class}} object
#' @return Mids class object with the imputed data or NULL if imputation was
#' not required
#' @seealso \code{\link{createClustAll}}, \code{\link{ClustAllObject-class}},
#' \code{\link{runClustAll}}
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


#' @title Retrieve the number of imputations applied at the imputation step from
#' ClustAllObject
#' @aliases showNumberImputations,ClustAllObject-method
#' @description
#' Generic function to retrieve the number of imputations in
#' \code{\link{createClustAll}} from a \code{\link{ClustAllObject-class}} object
#' @usage showNumberImputations(Object)
#' @param Object \code{\link{ClustAllObject-class}} object
#' @return Numeric vector that contains the number of imputations. 0 in the
#' case of no imputations were required
#' @seealso \code{\link{createClustAll}}, \code{\link{ClustAllObject-class}},
#' \code{\link{runClustAll}}
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


#' @title Retrieve the resulting stratifications for each combination of
#' clusterings method (distance + clustering algorithm) and depth from
#' ClustAllObject
#' @aliases showSummaryClusters,ClustAllObject-method
#' @description
#' Generic function to retrieve the resulting stratifications for each
#' combination of clusterings method (distance + clustering algorithm) and
#' depth of \code{\link{runClustAll}} from a \code{\link{ClustAllObject-class}}
#' object
#' @usage showSummaryClusters(Object)
#' @param Object \code{\link{ClustAllObject-class}} object
#' @return List with the resulting stratifications for each combination of
#' clusterings method (distance + clustering algorithm) and depth methods or
#' NULL if runClustAll method has not been executed yet.
#' @seealso \code{\link{runClustAll}}, \code{\link{ClustAllObject-class}}
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


#' @title Retrieve the matrix with the Jaccard distances derived from the robust
#' populations stratifications in ClustAllObject
#' @aliases showJaccardDistances,ClustAllObject-method
#' @description
#' Generic function to retrieve the matrix with the Jaccard distances derived
#' from the robust populations stratifications in\code{\link{runClustAll}} from
#' a \code{\link{ClustAllObject-class}} object
#' @usage showJaccardDistances(Object)
#' @param Object \code{\link{ClustAllObject-class}} object
#' @return Matrix containing the Jaccard distances derived from the robust
#' populations stratifications or NULL if runClustAll method has not been
#' executed yet
#' @seealso \code{\link{runClustAll}}, \code{\link{ClustAllObject-class}}
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


#' @title Retrieve logical if runClustAll has been executed considering
#' ClustAllObject as input
#' @aliases isProcessed, ClustAllObject-method
#' @description
#' Generic function to retrieve the logical if \code{\link{runClustAll}} have
#' been runned from a \code{\link{ClustAllObject-class}} object
#' @usage isProcessed(Object)
#' @param Object \code{\link{ClustAllObject-class}} object
#' @return TRUE if runClustAll has been already executed. Otherwise FALSE
#' @seealso \code{\link{runClustAll}}, \code{\link{ClustAllObject-class}}
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


#' @title Retrieve the original data labelling from ClustAllObject
#' @aliases showValidationData,ClustAllObject-method
#' @description
#' Generic function to retrieve numeric vector if it has been added with the
#' true labels from a \code{\link{ClustAllObject-class}} object
#' @usage showValidationData(Object)
#' @param Object \code{\link{ClustAllObject-class}} object
#' @return numeric vector if true labels have been added. Otherwise NULL
#' @seealso \code{\link{ClustAllObject-class}}
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
#' Generic function to add validation data to the
#' \code{\link{ClustAllObject-class}} object
#' @usage addValidationData(Object, dataValidation)
#' @param Object \code{\link{ClustAllObject-class}} object
#' @param dataValidation numeric vector with the validation data
#' @return \code{\link{ClustAllObject-class}} object
#' @seealso \code{\link{ClustAllObject-class}}
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
      message("The object already has a labelling.")
      message("Rewriting the labelling data...")
    }

    dataValidation <- checkVectorIntroduced(dataValidation)

    if (length(dataValidation) != nrow(Object@data)) {
      message("The introduced data  and the original data labelling have different lenghts.")
      message("Make sure the introduced data is correct.")
      stop()
    }

    Object@dataValidation <- dataValidation
    return(Object)
  }
)

# END OF ClustAll_ClustAllObject_Class.R
