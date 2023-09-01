# setClassUnion includes defined new classes -----------------------------------
#' @import mice
setClassUnion("midsOrNULL", c("mids", "NULL"))
setClassUnion("midsOrNA", c("mids", "missing", "NULL"))
setClassUnion("listOrNULL", c("list", "NULL", "missing"))
setClassUnion("numericOrNA", c("numeric", "missing", "NULL"))
setClassUnion("logicalOrNA", c("logical", "missing", "NULL"))
setClassUnion("matrixOrNULL", c("matrix", "NULL"))

# ClustAllObject Class ---------------------------------------------------------
#' @title ClustAllObject
#' @aliases ClustAllObject-class
#' @description Stores the using data, imputations and results of the ClustAll pipeline.
#' @slot data Data Frame of the using data
#' @slot dataImputed midsOrNULL. Mids object from mice package that stores imputed data in case imputations have been made. Otherwise NULL value
#' @slot nImputation Numeric giving the number of imputations
#' @slot processed Logical value if the ClustAll pipeline have been runned
#' @slot summary_clusters listOrNULL. List of matrices of the clustering methods in case ClustAll pipeline have been runned. Othrwise NULL value
#' @slot JACCARD_DISTANCE_F matrixOrNULL. Matrix containing the robust values of Jaccard distances if the ClustAll pipeline have been runned. Otherwise NULL value
#' @export
setClass(
  Class="ClustAllObject",
  slots=list(
    data="data.frame",
    dataImputed="midsOrNULL",
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
           nImputation,
           processed,
           summary_clusters,
           JACCARD_DISTANCE_F) {
    .Object@data <- data
    # if (nImputation == 0) {
    #   dataImputed <- NULL
    # }
    .Object@dataImputed <- dataImputed
    .Object@nImputation <- nImputation
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
#' # MISSING EXAMPLES
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
#' @return Mids class object with the imputed data or NULL value if no imputations have been made
#'
#' @seealso \code{\link{createClustAll}}, \code{\link{ClustAllObject-class}}, \code{\link{runClustAll}}
#'
#' @examples
#' # MISSING EXAMPLES
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
#' # MISSING EXAMPLES
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
#' @return List of matrices with all the clustering methods or NULL value if runClustAll method have not been runned
#'
#' @seealso \code{\link{runClustAll}}, \code{\link{ClustAllObject-class}}
#'
#' @examples
#' # MISSING EXAMPLES
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
#' @return Matrix with Jaccard distances or NULL value if runClustAll method have not been runned
#'
#' @seealso \code{\link{runClustAll}}, \code{\link{ClustAllObject-class}}
#'
#' @examples
#' # MISSING EXAMPLES
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


#' @title Retrieve logical value if runClustAll have been runned from ClustAllObject
#' @aliases isProcessed,ClustAllObject-method
#' @description
#' Generic function to retrieve the logical value if \code{\link{runClustAll}} have been runned from a \code{\link{ClustAllObject-class}} object
#' @usage isProcessed(Object)
#' @param Object \code{\link{ClustAllObject-class}} object
#'
#' @return TRUE if runClustAll have been runned. Otherwise FALSE
#'
#' @seealso \code{\link{runClustAll}}, \code{\link{ClustAllObject-class}}
#'
#' @examples
#' # MISSING EXAMPLES
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
# END OF ClustAll_ClustAllObject_Class.R
