# setClassUnion includes the new classes defined -------------------------------

#' Class union of list, null and missing
#' @title Class Union listOrNULL
#' @aliases listOrNULL-class
#' @name listOrNULL
#' @description Contains either list, NULL or missing object
#' @return listOrNULL class object
#' @exportClass listOrNULL
#' @export
setClassUnion("listOrNULL", c("list", "NULL", "missing"))


#' Class union of numeric, null and missing
#' @title Class Union numericOrNA
#' @aliases numericOrNA-class
#' @name numericOrNA
#' @description Contains either numeric, NULL or missing object
#' @return numericOrNA class object
#' @exportClass numericOrNA
#' @export
setClassUnion("numericOrNA", c("numeric", "missing", "NULL"))


#' @title characterOrNA
#' Class union of character, null and missing
#' @aliases characterOrNA-class
#' @name characterOrNA
#' @description Contains either character, NULL or missing object
#' @return characterOrNA class object
#' @exportClass characterOrNA
#' @export
setClassUnion("characterOrNA", c("character", "missing", "NULL"))


#' Class union of logical, null and missing
#' @title logicalOrNA
#' @aliases logicalOrNA-class
#' @name logicalOrNA
#' @description Contains either logical, NULL or missing object
#' @return logicalOrNA class object
#' @exportClass logicalOrNA
#' @export
setClassUnion("logicalOrNA", c("logical", "missing", "NULL"))


#' Class union of matrix, null and missing
#' @title matrixOrNULL
#' @aliases matrixOrNULL-class
#' @name matrixOrNULL
#' @description Contains either matrix or NULL object
#' @return matrixOrNULL class object
#' @exportClass matrixOrNULL
#' @export
setClassUnion("matrixOrNULL", c("matrix", "NULL"))


#' Class union of numeric and character
#' @title numericOrCharacter
#' @aliases numericOrCharacter-class
#' @name numericOrCharacter
#' @description Contains either numeric or character object
#' @return numericOrCharacter class object
#' @exportClass numericOrCharacter
#' @export
numericOrCharacter <- setClassUnion("numericOrCharacter",
                                    c("numeric", "character"))


# ClustAllObject Class ---------------------------------------------------------
#' @title ClustAllObject
#' @aliases ClustAllObject-class
#' @description This class contains the original data and the imputed datasets
#' needed to run the ClustAll pipeline. The results of all the stratifications
#' are stored in summary_clusters. From the stratifications that pass the
#' bootstrapping, a matrix with the Jaccard distances is calculated and stored
#' in JACCARD_DISTANCE_F.
#' @slot data Data Frame of the input data after applying one-hot encoding to the
#' categorical variables and extracting the validation (true label) column.
#' @slot dataOriginal Data Frame of the input data.
#' @slot dataImputed  Mids object derived from the mice package that stores the
#' imputed data, in case imputation was applied. Otherwise NULL.
#' @slot dataValidation A vector in the case there is a validation (true label)
#' column in the input data. This information can be added later with
#' \code{\link{addValidationData}}. Otherwise NULL.
#' @slot nImputation Number of imputations performed.
#' @slot processed A boolean. TRUE if \code{\link{runClustAll}} has been
#' executed. Otherwise FALSE.
#' @slot summary_clusters List with the resulting stratifications
#' for each combination of clustering methods (distance + clustering algorithm)
#' and depth, in case \code{\link{runClustAll}} has been executed previously.
#' Otherwise NULL.
#' @slot JACCARD_DISTANCE_F Matrix containing the Jaccard distances derived from
#' the robust stratifications after applying the bootstrapping if
#' \code{\link{runClustAll}} has been executed previously. Otherwise NULL.
#' @return ClustAllObject class object.
#' @exportClass ClustAllObject
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
#' @param .Object initializing object
#' @param data Data Frame of the input data after applying one-hot encoding to the
#' categorical variables and extracting the validation (true label) column.
#' @param dataOriginal Data Frame of the input data.
#' @param dataImputed  Mids object derived from the mice package that stores the
#' imputed data, in case imputation was applied. Otherwise NULL.
#' @param dataValidation A vector in the case there is a validation (true label)
#' column in the input data. This information can be added later with
#' \code{\link{addValidationData}}. Otherwise NULL.
#' @param nImputation Number of imputations performed.
#' @param processed A boolean. TRUE if \code{\link{runClustAll}} has been
#' executed. Otherwise FALSE.
#' @param summary_clusters List with the resulting stratifications
#' for each combination of clustering methods (distance + clustering algorithm)
#' and depth, in case \code{\link{runClustAll}} has been executed previously.
#' Otherwise NULL.
#' @param JACCARD_DISTANCE_F Matrix containing the Jaccard distances derived from
#' the robust stratifications after applying the bootstrapping if
#' \code{\link{runClustAll}} has been executed previously. Otherwise NULL.
#' @return ClustAllObject class object.
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

#' Show method for a \code{\link{ClustAllObject-class}} object
#' @title show method for ClustAllObject
#' @param object \code{\link{ClustAllObject-class}} object
#' @return summarize information about the object
setMethod("show", "ClustAllObject", function(object) {

    cat(is(object)[[1]], "\n",
        "Data: Number of variables: ",ncol(object@data),
        ". Number of patients: ", nrow(object@data), "\n",
        "Imputated: ", ifelse(is.null(object@dataImputed), "NO.", "YES."),
        "\nNumber of imputations: ", object@nImputation, "\n",
        "Processed: ", object@processed, "\n",
        "Number of stratifications: ", ifelse(object@processed,
                                             nrow(object@JACCARD_DISTANCE_F),
                                             "NULL"), "\n", sep = ""
    )
  }
)

#' @title data accession method
#' @aliases showData,ClustAllObject-method
#' @description
#' Generic function to retrieve the initial data used for
#' \code{\link{createClustAll}} from a \code{\link{ClustAllObject-class}}
#' object.
#' @usage showData(Object)
#' @param Object \code{\link{ClustAllObject-class}} object.
#' @return The Data Frame with the initial data
#' @seealso \code{\link{createClustAll}}, \code{\link{ClustAllObject-class}},
#' \code{\link{runClustAll}}.
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc <- subset(wdbc,select=-ID)
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
  definition=function(Object) {Object@data}
)

#' @title dataOriginal accession method
#' @aliases dataOriginal,ClustAllObject-method
#' @description
#' Generic function to retrieve the initial data used for
#' \code{\link{createClustAll}} from a \code{\link{ClustAllObject-class}}
#' object.
#' @usage dataOriginal(Object)
#' @param Object \code{\link{ClustAllObject-class}} object.
#' @return The Data Frame with the initial data
#' @seealso \code{\link{createClustAll}}, \code{\link{ClustAllObject-class}},
#' \code{\link{runClustAll}}
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc <- subset(wdbc,select=-ID)
#' obj_noNA <- createClustAll(data = wdbc, colValidation = "Diagnosis")
#' dataOriginal(obj_noNA)
#' @export
setGeneric(
  name="dataOriginal",
  def=function(Object){standardGeneric("dataOriginal")}
)

setMethod(
  f="dataOriginal",
  signature=signature(
    Object="ClustAllObject"),
  definition=function(Object) {Object@dataOriginal}
)

#' @title dataImputed accession method
#' @aliases dataImputed,ClustAllObject-method
#' @description
#' Generic function to retrieve the imputed data obtained in
#' \code{\link{createClustAll}} from a \code{\link{ClustAllObject-class}} object
#' @usage dataImputed(Object)
#' @param Object \code{\link{ClustAllObject-class}} object
#' @return Mids class object with the imputed data or NULL.
#' @seealso \code{\link{createClustAll}}, \code{\link{ClustAllObject-class}},
#' \code{\link{runClustAll}}
#' @examples
#' data("BreastCancerWisconsinMISSING", package = "ClustAll")
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc <- subset(wdbc,select=-ID)
#' obj_NA <- createClustAll(data = wdbcNA, colValidation = "Diagnosis",
#'                          dataImputed = wdbcMIDS)
#' dataImputed(obj_NA)
#' @export
setGeneric(
  name="dataImputed",
  def=function(Object){standardGeneric("dataImputed")}
)

setMethod(
  f="dataImputed",
  signature=signature(
    Object="ClustAllObject"),
  definition=function(Object) {Object@dataImputed}
)


#' @title nImputation accession method
#' @aliases nImputation,ClustAllObject-method
#' @description
#' Generic function to retrieve the number of imputations in
#' \code{\link{createClustAll}} from a \code{\link{ClustAllObject-class}}
#' object.
#' @usage nImputation(Object)
#' @param Object \code{\link{ClustAllObject-class}} object.
#' @return Numeric vector that contains the number of imputations. 0 in the
#' case of no imputations were required.
#' @seealso \code{\link{createClustAll}}, \code{\link{ClustAllObject-class}},
#' \code{\link{runClustAll}}
#' @examples
#' data("BreastCancerWisconsinMISSING", package = "ClustAll")
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc <- subset(wdbc,select=-ID)
#' obj_NA <- createClustAll(data = wdbcNA, colValidation = "Diagnosis",
#'                          dataImputed = wdbcMIDS)
#' nImputation(obj_NA)
#' @export
setGeneric(
  name="nImputation",
  def=function(Object){standardGeneric("nImputation")}
)

setMethod(
  f="nImputation",
  signature=signature(
    Object="ClustAllObject"),
  definition=function(Object) {Object@nImputation}
)


#' @title summary_clusters accession method
#' @aliases summary_clusters,ClustAllObject-method
#' @description
#' Generic function to retrieve the resulting stratifications for each
#' combination of clusterings method (distance + clustering algorithm) and
#' depth of \code{\link{runClustAll}} from a \code{\link{ClustAllObject-class}}
#' object.
#' @usage summary_clusters(Object)
#' @param Object \code{\link{ClustAllObject-class}} object.
#' @return List with the resulting stratifications for each combination of
#' clusterings method (distance + clustering algorithm) and depth methods or
#' NULL if runClustAll method has not been executed yet.
#' @seealso \code{\link{runClustAll}}, \code{\link{ClustAllObject-class}}
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc <- subset(wdbc,select=c(-ID, -Diagnosis))
#' wdbc <- wdbc[1:15,1:8]
#' obj_noNA <- createClustAll(data = wdbc)
#' obj_noNA1 <- runClustAll(Object = obj_noNA, threads = 1, simplify = FALSE)
#' summary_clusters(obj_noNA1)
#' @export
setGeneric(
  name="summary_clusters",
  def=function(Object){standardGeneric("summary_clusters")}
)

setMethod(
  f="summary_clusters",
  signature=signature(
    Object="ClustAllObject"),
  definition=function(Object) {Object@summary_clusters}
)


#' @title JACCARD_DISTANCE_F accession method
#' @aliases JACCARD_DISTANCE_F,ClustAllObject-method
#' @description
#' Generic function to retrieve the matrix with the Jaccard distances derived
#' from the robust populations stratifications in\code{\link{runClustAll}} from
#' a \code{\link{ClustAllObject-class}} object.
#' @usage JACCARD_DISTANCE_F(Object)
#' @param Object \code{\link{ClustAllObject-class}} object.
#' @return Matrix containing the Jaccard distances derived from the robust
#' populations stratifications or NULL if runClustAll method has not been
#' executed yet.
#' @seealso \code{\link{runClustAll}}, \code{\link{ClustAllObject-class}}
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc <- subset(wdbc,select=c(-ID, -Diagnosis))
#' wdbc <- wdbc[1:15,1:8]
#' obj_noNA <- createClustAll(data = wdbc)
#' obj_noNA1 <- runClustAll(Object = obj_noNA, threads = 1, simplify = FALSE)
#' JACCARD_DISTANCE_F(obj_noNA1)
#' @export
setGeneric(
  name="JACCARD_DISTANCE_F",
  def=function(Object){standardGeneric("JACCARD_DISTANCE_F")}
)

setMethod(
  f="JACCARD_DISTANCE_F",
  signature=signature(
    Object="ClustAllObject"),
  definition=function(Object) {Object@JACCARD_DISTANCE_F}
)


#' @title processed accession method
#' @aliases processed,ClustAllObject-method
#' @description
#' Generic function to know if \code{\link{ClustAllObject-class}} has been
#' processed. TRUE if \code{\link{runClustAll}} has been executed. Otherwise
#' FALSE.
#' @usage processed(Object)
#' @param Object \code{\link{ClustAllObject-class}} object.
#' @return TRUE if runClustAll has been executed. Otherwise FALSE.
#' @seealso \code{\link{runClustAll}}, \code{\link{ClustAllObject-class}}
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc <- subset(wdbc,select=c(-ID, -Diagnosis))
#' wdbc <- wdbc[1:15,1:8]
#' obj_noNA <- createClustAll(data = wdbc)
#' processed(obj_noNA)
#' @export
setGeneric(
  name="processed",
  def=function(Object){standardGeneric("processed")}
)

setMethod(
  f="processed",
  signature=signature(
    Object="ClustAllObject"),
  definition=function(Object) {

    return(Object@processed)
  }
)


#' @title dataValidation accession method
#' @aliases dataValidation,ClustAllObject-method
#' @description
#' Generic function to retrieve numeric vector if it has been added with the
#' true labels from a \code{\link{ClustAllObject-class}} object.
#' @usage dataValidation(Object)
#' @param Object \code{\link{ClustAllObject-class}} object.
#' @return numeric vector with true labels if validation column has been added.
#' Otherwise NULL.
#' @seealso \code{\link{ClustAllObject-class}}
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc <- subset(wdbc,select=-ID)
#' obj_noNA <- createClustAll(data = wdbc, colValidation="Diagnosis")
#' dataValidation(obj_noNA)
#' @export
setGeneric(
  name="dataValidation",
  def=function(Object){standardGeneric("dataValidation")}
)

setMethod(
  f="dataValidation",
  signature=signature(
    Object="ClustAllObject"),
  definition=function(Object) {Object@dataValidation}
)

#' @title addValidationData
#' @name addValidationData
#' @docType methods
#' @rdname addValidationData
#' @aliases addValidationData,ClustAllObject,numericOrCharacter-method
#' @description
#' Generic function to add validation data to the
#' \code{\link{ClustAllObject-class}} object.
#' @usage addValidationData(Object, dataValidation)
#' @param Object \code{\link{ClustAllObject-class}} object.
#' @param dataValidation A numeric or character vector with the
#' validation data (true labels).
#' The length of the vector must be the same as input data used in
#' \code{\link{createClustAll}}.
#' @return \code{\link{ClustAllObject-class}} object.
#' @seealso \code{\link{ClustAllObject-class}}
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' label <- as.numeric(as.factor(wdbc$Diagnosis))
#' wdbc <- wdbc[,-c(1, 2)] # delete patients IDs & label
#' obj_noNA <- createClustAll(data = wdbc)
#' obj_noNA <- addValidationData(Object = obj_noNA,
#'                               dataValidation = label)
#' @exportMethod addValidationData
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
      message("The introduced data  and the original data labelling have different lengths.")
      message("Make sure the introduced data is correct.")
      stop()
    }

    Object@dataValidation <- dataValidation
    return(Object)
  }
)

#' @title extractData
#' @aliases extractData,ClustAllObject-method
#' @description
#' Generic function to retrieve all the data used in
#' \code{\link{createClustAll}} from a \code{\link{ClustAllObject-class}}
#' object.
#' @usage extractData(Object)
#' @param Object \code{\link{ClustAllObject-class}} object.
#' @return List with the information of original, modified and imputed data.
#' @seealso \code{\link{createClustAll}}, \code{\link{ClustAllObject-class}},
#' \code{\link{runClustAll}}
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc <- subset(wdbc,select=-ID)
#' \donttest{
#' obj_noNA <- createClustAll(data = wdbc, colValidation = "Diagnosis")
#' extractData(obj_noNA)
#' }
#' @export
setGeneric(
  name="extractData",
  def=function(Object){standardGeneric("extractData")}
)

setMethod(
  f="extractData",
  signature=signature(
    Object="ClustAllObject"),
  definition=function(Object) {
    out_list <- list(Object@data, Object@dataOriginal, Object@dataImputed)
    names(out_list) <- c("Data_modified", "Data_original", "Data_imputed")

    return(out_list)
  }
)

#' @title extractResults
#' @aliases extractResults,ClustAllObject-method
#' @description
#' Generic function to retrieve all the results after processing
#' \code{\link{runClustAll}} from a \code{\link{ClustAllObject-class}} object.
#' @usage extractResults(Object)
#' @param Object \code{\link{ClustAllObject-class}} object.
#' @return List with all the names of generated stratifications and the
#' statically robust ones.
#' @seealso \code{\link{createClustAll}}, \code{\link{ClustAllObject-class}},
#' \code{\link{runClustAll}}
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc <- subset(wdbc,select=-ID)
#' \donttest{
#' obj_noNA <- createClustAll(data = wdbc, colValidation = "Diagnosis")
#' obj_noNA1 <- runClustAll(Object = obj_noNA, threads = 1, simplify = TRUE)
#' extractResults(obj_noNA1)
#' }
#'
#' @export
setGeneric(
  name="extractResults",
  def=function(Object){standardGeneric("extractResults")}
)

setMethod(
  f="extractResults",
  signature=signature(
    Object="ClustAllObject"),
  definition=function(Object) {
    if (Object@processed)
    {
      stratification_names <- colnames(Object@JACCARD_DISTANCE_F)
      stratification_robust <- Object@summary_clusters[stratification_names]
      out_list <- list(Object@summary_clusters, stratification_robust)
      names(out_list) <- c("All_clusters", "Robust_clusters")
    } else {
      message("There are no results generated. Please run runClustAll before.")
      out_list <- list(NULL)
    }

    return(out_list)
  }
)


# Cocumenting DataSets ---------------------------------------------------------
#' wdbc: Diagnostic Wisconsin Breast Cancer Database.
#'
#' A dataset containing Features are computed from a digitized image of a fine
#' needle aspirate (FNA) of a breast mass.
#' They describe the characteristics of the cell nuclei present in the image.
#'
#' The dataset comprises two types of features —categorical and numerical—
#' derived from a digitized image of a fine needle aspirate (FNA) of a breast
#' mass from 659 patients. Each patient is characterized by 31 features (10x3)
#' and belongs to one of two target classes: ‘malignant’ or ‘benign’.
#' @source <https://archive.ics.uci.edu/dataset/17/breast+cancer+wisconsin+diagnostic>
#'
#' \itemize{
#'   \item Diagnosis Label says tumor is malingnant or benignant
#'   \item radius. Mean of distances from the center to points on the perimeter
#'   \item perimeter
#'   \item area
#'   \item smoothness. Local variation in radius lengths
#'   \item compactness. (Perimeter^2 / Area) - 1.0
#'   \item concavity. Severity of concave portions of the contour
#'   \item concave points. Number of concave portions of the contour
#'   \item symmetry.
#'   \item fractal dimension. “Coastline approximation” - 1.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name wdbc
#' @usage data("BreastCancerWisconsin", package = "ClustAll")
#' @format A data frame with 660 rows and 31 variables
#' @return wdbc dataset
NULL

#' wdbcNA: Diagnostic Wisconsin Breast Cancer Database with missing values
#'
#' We introduced random missing values to the wdbc dataset. \code{\link{wdbc}}
#'
#' @docType data
#' @keywords datasets
#' @name wdbcNA
#' @usage data("BreastCancerWisconsinMISSING", package = "ClustAll")
#' @format A data frame with 660 rows and 31 variables
#' @return wdbcNA dataset
NULL

#' wdbcMIDS: Diagnostic Wisconsin Breast Cancer Database with imputed values
#'
#' We introduced imputed random values to the wdbcNA dataset.
#' Using Mice. It is a mids object. \code{\link{wdbc}}
#'
#' @docType data
#' @keywords datasets
#' @name wdbcMIDS
#' @usage data("BreastCancerWisconsinMISSING", package = "ClustAll")
#' @format A data frame with 660 rows and 31 variables
#' @return wdbcMIDS dataset
NULL

#' obj_noNA1: Processed wdbc dataset for testing purposed
#'
#' Processed wdbc as appear in vignette
#'
#' @docType data
#' @keywords datasets
#' @name obj_noNA1
#' @usage data("testData", package = "ClustAll")
#' @format A processed ClustAllObject
#' @return ClustAllObject Object
NULL

#' obj_noNA1simplify: Processed wdbc dataset for testing purposed
#'
#' Processed wdbc as appear in vignette, with simplify TRUE parameter
#'
#' @docType data
#' @keywords datasets
#' @name obj_noNA1simplify
#' @usage data("testData", package = "ClustAll")
#' @format A processed ClustAllObject
#' @return ClustAllObject Object
NULL

#' obj_noNAno1Validation: Processed wdbc dataset for testing purposed
#'
#' Processed wdbc as appear in vignette, with no validation data
#'
#' @docType data
#' @keywords datasets
#' @name obj_noNAno1Validation
#' @usage data("testData", package = "ClustAll")
#' @format A processed ClustAllObject
#' @return ClustAllObject Object
NULL

# END OF ClustAll_ClustAllObject_Class.R
