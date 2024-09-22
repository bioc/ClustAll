# setClassUnion includes the new classes defined -------------------------------

#' @title Class Union: listOrNULL
#' @aliases listOrNULL-class
#' @description
#' This class union allows for flexibility in method signatures and slot
#' definitions by accepting either a list, NULL, or a missing value. It is
#' particularly useful when a slot or function parameter might contain a
#' list of elements but could also be empty or unspecified.
#'
#' @details
#' The listOrNULL class union includes:
#' \itemize{
#'   \item list: A standard R list object
#'   \item NULL: Representing an empty or unset value
#'   \item missing: Allowing for unspecified parameters in function calls
#' }
#'
#' This union is useful in scenarios where:
#' \itemize{
#'   \item A function might return a list of results or NULL if no results are available
#'   \item A slot in an S4 object could contain a list of elements or be empty
#'   \item A function parameter could accept a list of options, but also work with default settings if nothing is provided
#' }
#'
#' @seealso \code{\link{setClassUnion}}, \code{\link{ClustAllObject-class}}
setClassUnion("listOrNULL", c("list", "NULL", "missing"))


#' @title Class Union: numericOrNA
#' @aliases numericOrNA-class
#' @description
#' This class union allows for flexibility in method signatures and slot
#' definitions by accepting either a numeric value, NULL, or a missing value.
#' It is particularly useful when a slot or function parameter might contain
#' a numeric value but could also be empty, unspecified, or explicitly set to NULL.
#'
#' @details
#' The numericOrNA class union includes:
#' \itemize{
#'   \item numeric: A standard R numeric value or vector
#'   \item NULL: Representing an empty or unset value
#'   \item missing: Allowing for unspecified parameters in function calls
#' }
#'
#' This union is useful in scenarios where:
#' \itemize{
#'   \item A function might return a numeric result or NULL if no result is available
#'   \item A slot in an S4 object could contain a numeric value or be empty
#'   \item A function parameter could accept a numeric input, but also work with default settings if nothing is provided
#' }
#'
#' @seealso \code{\link{setClassUnion}}, \code{\link{ClustAllObject-class}}
setClassUnion("numericOrNA", c("numeric", "missing", "NULL"))



#' @title Class Union: characterOrNA
#' @aliases characterOrNA-class
#' @description
#' This class union allows for flexibility in method signatures and slot
#' definitions by accepting either a character value, NULL, or a missing value.
#' It is particularly useful when a slot or function parameter might contain
#' a character string but could also be empty, unspecified, or explicitly set to NULL.
#'
#' @details
#' The characterOrNA class union includes:
#' \itemize{
#'   \item character: A standard R character string or vector
#'   \item NULL: Representing an empty or unset value
#'   \item missing: Allowing for unspecified parameters in function calls
#' }
#'
#' This union is useful in scenarios where:
#' \itemize{
#'   \item A function might return a character result or NULL if no result is available
#'   \item A slot in an S4 object could contain a character value or be empty
#'   \item A function parameter could accept a character input, but also work with default settings if nothing is provided
#' }
#'
#' @seealso \code{\link{setClassUnion}}, \code{\link{ClustAllObject-class}}
setClassUnion("characterOrNA", c("character", "missing", "NULL"))


#' @title Class Union: logicalOrNA
#' @aliases logicalOrNA-class
#' @description
#' This class union allows for flexibility in method signatures and slot
#' definitions by accepting either a logical value, NULL, or a missing value.
#' It is particularly useful when a slot or function parameter might contain
#' a boolean flag but could also be empty, unspecified, or explicitly set to NULL.
#'
#' @details
#' The logicalOrNA class union includes:
#' \itemize{
#'   \item logical: A standard R logical value (TRUE or FALSE)
#'   \item NULL: Representing an empty or unset value
#'   \item missing: Allowing for unspecified parameters in function calls
#' }
#'
#' This union is useful in scenarios where:
#' \itemize{
#'   \item A function might return a logical result or NULL if no result is available
#'   \item A slot in an S4 object could contain a logical flag or be empty
#'   \item A function parameter could accept a logical input, but also work with default settings if nothing is provided
#' }
#'
#' @seealso \code{\link{setClassUnion}}, \code{\link{ClustAllObject-class}}
setClassUnion("logicalOrNA", c("logical", "missing", "NULL"))



#' @title Class Union: matrixOrNULL
#' @aliases matrixOrNULL-class
#' @description
#' This class union allows for flexibility in method signatures and slot
#' definitions by accepting either a matrix or NULL. It is particularly useful
#' when a slot or function parameter might contain a matrix of data but could
#' also be empty or explicitly set to NULL.
#'
#' @details
#' The matrixOrNULL class union includes:
#' \itemize{
#'   \item matrix: A standard R matrix object
#'   \item NULL: Representing an empty or unset value
#' }
#'
#' This union is useful in scenarios where:
#' \itemize{
#'   \item A function might return a matrix of results or NULL if no results are available
#'   \item A slot in an S4 object could contain a matrix of data or be empty
#'   \item A function parameter could accept a matrix input, but also work with default settings if nothing is provided
#' }
#'
#' @seealso \code{\link{setClassUnion}}, \code{\link{ClustAllObject-class}}
setClassUnion("matrixOrNULL", c("matrix", "NULL"))



#' @title Class Union: numericOrCharacter
#' @aliases numericOrCharacter-class
#' @description
#' This class union allows for flexibility in method signatures and slot
#' definitions by accepting either a numeric or character value. It is particularly
#' useful when a slot or function parameter might contain either numeric data
#' or character strings representing numbers or categories.
#'
#' @details
#' The numericOrCharacter class union includes:
#' \itemize{
#'   \item numeric: A standard R numeric value or vector
#'   \item character: A standard R character string or vector
#' }
#'
#' This union is useful in scenarios where:
#' \itemize{
#'   \item A function might accept or return either numeric values or their string representations
#'   \item A slot in an S4 object could contain either numeric data or categorical labels
#'   \item A function needs to handle both numeric and character input flexibly
#' }
#'
#' @seealso \code{\link{setClassUnion}}, \code{\link{ClustAllObject-class}}
numericOrCharacter <- setClassUnion("numericOrCharacter",
                                    c("numeric", "character"))

# ClustAllObject Class ---------------------------------------------------------
#' @title ClustAllObject
#' @aliases ClustAllObject-class
#' @description
#' The ClustAllObject class is the central data structure of the ClustAll
#' package, designed to store and manage data and results throughout the patient
#' stratification process. It encapsulates the original data, preprocessed data,
#' imputation results, and clustering outcomes, providing a cohesive framework
#' for the entire ClustALL workflow.
#' @slot data A data frame containing the preprocessed input data after applying
#' one-hot encoding to categorical variables and removing the validation column
#' (if present). This is the data used directly in the clustering process.
#' @slot dataOriginal A data frame containing the original, unmodified input data
#' as provided to \code{\link{createClustAll}}. This preserves the initial state
#' of the data for reference and validation purposes.
#' @slot dataImputed If imputation was applied, this slot contains a 'mids' object
#' from the mice package with the imputed datasets. If no imputation was performed,
#' this is NULL.
#' @slot dataValidation A numeric vector containing the reference labels (true labels)
#' of the original dataset, if provided. These labels are used for validation purposes
#' and are not used in the stratification process itself. NULL if no validation data
#' is available.
#' @slot nImputation An integer indicating the number of imputations to be
#' computed. It should be set to 0 if the dataset is already completed, or if it
#' the dataset has been imputed out of ClustAll framework.
#' @slot processed A logical flag. TRUE if \code{\link{runClustAll}} has been
#' executed on the object, FALSE otherwise. Indicates whether the object contains
#' stratification results.
#' @slot summary_clusters A list containing the resulting stratifications for each
#' combination of clustering methods (distance metric + clustering algorithm) and
#' embedding depth. This is populated after \code{\link{runClustAll}} has been
#' executed. NULL otherwise.
#' @slot JACCARD_DISTANCE_F A matrix of Jaccard distances between the robust
#' stratifications that passed the bootstrapping process. Used to assess similarity
#' between different stratification solutions. NULL if \code{\link{runClustAll}}
#' has not been executed.
#' @details
#' The ClustAllObject is designed to efficiently manage all aspects of the data
#' and results throughout the ClustAll pipeline:
#' \itemize{
#'   \item It preserves the original data while storing preprocessed versions for analysis.
#'   \item It handles missing data through multiple imputation, storing both original and imputed datasets.
#'   \item It maintains separation between data used for clustering and validation data.
#'   \item It stores all generated stratifications and identifies robust solutions.
#'   \item It provides a framework for comparing different stratification solutions.
#' }
#' This structure allows for a streamlined workflow from data input through
#' preprocessing, imputation (if needed), stratification, and final analysis of
#' results.
#'
#' @note
#' The ClustAllObject is typically created using the
#' \code{\link{createClustAll}} function and processed using
#' \code{\link{runClustAll}}. Direct manipulation of the object's slots is not
#' recommended as it may lead to inconsistencies in the analysis pipeline.
#'
#'
#' @return ClustAllObject class object.
#' @exportClass ClustAllObject
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
#'
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

#' @title Display Summary of ClustAllObject
#' @aliases show,ClustAllObject-method
#' @description
#' This method provides a concise summary of a ClustAllObject, displaying key
#' information about its contents and processing status. It offers a quick
#' overview of the object's characteristics without the need to inspect
#' individual slots.
#'
#' @param object A \code{\link{ClustAllObject-class}} object to be summarized.
#'
#' @return No return value, called for printing to the console.
#'
#' @details
#' The show method for ClustAllObject displays the following information:
#'
#' \enumerate{
#'   \item Object Class: Confirms that the object is of class ClustAllObject.
#'
#'   \item Data Dimensions:
#'   \itemize{
#'     \item Number of variables (columns) in the processed data.
#'     \item Number of patients (rows) in the dataset.
#'   }
#'
#'   \item Imputation Status:
#'   \itemize{
#'     \item Indicates whether the data has been imputed.
#'     \item If imputed, shows the number of imputations performed.
#'   }
#'
#'   \item Processing Status:
#'   \itemize{
#'     \item Indicates whether the ClustALL algorithm has been run on the object.
#'   }
#'
#'   \item Stratification Results:
#'   \itemize{
#'     \item If processed, displays the number of stratifications generated.
#'     \item If not processed, indicates that stratification results are not available.
#'   }
#' }
#'
#' This method is particularly useful for:
#' \itemize{
#'   \item Quick verification of object contents after creation or modification.
#'   \item Checking the processing status before running analyses.
#'   \item Confirming the number of stratifications generated after running the ClustALL algorithm.
#'   \item Easily sharing key object characteristics in reports or discussions.
#' }
#'
#' @note
#' \itemize{
#'   \item This method is automatically called when the object name is entered at the R console.
#'   \item It provides a high-level overview and does not display detailed data or results.
#'   \item For more detailed information about specific aspects of the object, use
#'     dedicated accessor methods or examine individual slots directly.
#' }
#'
#' @seealso \code{\link{createClustAll}}, \code{\link{runClustAll}},
#' \code{\link{ClustAllObject-class}}
#'
#' @export
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

#' @title Retrieve Processed Data from ClustAllObject
#' @aliases showData,ClustAllObject-method
#' @description
#' This method extracts and returns the processed data stored in a ClustAllObject.
#' The data returned is the version used for clustering analysis, which has undergone
#' preprocessing steps such as one-hot encoding for categorical variables and
#' removal of the validation column (if present).
#'
#' @usage showData(Object)
#'
#' @param Object A \code{\link{ClustAllObject-class}} object created by
#' \code{\link{createClustAll}}.
#'
#' @return A data frame containing the processed data used for clustering analysis.
#' This data reflects all preprocessing steps applied during object creation but
#' does not include any imputation that may have been performed.
#'
#' @details
#' The showData method provides access to the core dataset used in the ClustALL
#' analysis pipeline. Key aspects of this data include:
#'
#' \enumerate{
#'   \item Preprocessing Applied:
#'   \itemize{
#'     \item Categorical variables have been converted to numeric form via one-hot encoding.
#'     \item The validation column (if specified during object creation) has been removed.
#'   }
#'
#'   \item Data Structure:
#'   \itemize{
#'     \item All columns are numeric, suitable for use in clustering algorithms.
#'     \item Row order corresponds to the original input data.
#'   }
#'
#'   \item Missing Data:
#'   \itemize{
#'     \item The returned data may still contain missing values (NAs) if imputation
#'           was not performed.
#'     \item For imputed versions of the data, use the \code{\link{dataImputed}} method.
#'   }
#' }
#'
#' @seealso \code{\link{createClustAll}}, \code{\link{dataOriginal}},
#' \code{\link{dataImputed}}, \code{\link{ClustAllObject-class}}
#'
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

#' @title Retrieve Original Unprocessed Data from ClustAllObject
#' @aliases dataOriginal,ClustAllObject-method
#' @description
#' This method extracts and returns the original, unmodified data that was used
#' to create the ClustAllObject. It provides access to the raw input data before
#' any preprocessing steps were applied, including one-hot encoding, validation
#' column removal, or imputation.
#'
#' @usage dataOriginal(Object)
#'
#' @param Object A \code{\link{ClustAllObject-class}} object created by
#' \code{\link{createClustAll}}.
#'
#' @return A data frame containing the original, unprocessed input data exactly
#' as it was provided when creating the ClustAllObject.
#'
#' @details
#' The dataOriginal method serves as a reference point for the initial state of
#' the data in the ClustALL analysis pipeline. Key aspects of this data include:
#'
#' \enumerate{
#'   \item Data Integrity:
#'   \itemize{
#'     \item Contains all original variables, including any that may have been removed
#'           or transformed during preprocessing.
#'     \item Preserves original data types (e.g., factors for categorical variables).
#'     \item Includes the validation column if it was present in the original data.
#'   }
#'
#'   \item Missing Data:
#'   \itemize{
#'     \item Reflects the original state of missing values (NAs) in the dataset.
#'   }
#'
#'   \item Data Structure:
#'   \itemize{
#'     \item Maintains the original row and column order of the input data.
#'     \item No transformations or encodings have been applied.
#'   }
#' }
#'
#' @seealso \code{\link{createClustAll}}, \code{\link{showData}},
#' \code{\link{dataImputed}}, \code{\link{ClustAllObject-class}}
#'
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

#' @title Retrieve Imputed Data from ClustAllObject
#' @aliases dataImputed,ClustAllObject-method
#' @description
#' This method extracts and returns the imputed datasets stored in a ClustAllObject.
#' It provides access to the multiple imputed versions of the data generated to
#' handle missing values, if imputation was conducted and included in the object.
#'
#' @usage dataImputed(Object)
#'
#' @param Object A \code{\link{ClustAllObject-class}} object created by
#' \code{\link{createClustAll}}.
#'
#' @return A 'mids' object from the mice package containing the imputed datasets.
#' If no imputation was performed, the method returns NULL.
#'
#' @details
#' The dataImputed method provides access to the imputed data used in the ClustALL
#' analysis pipeline when missing values are present. Key aspects of this data include:
#'
#' \enumerate{
#'   \item Imputation Structure:
#'   \itemize{
#'     \item Returns a 'mids' object, which contains multiple imputed datasets.
#'     \item Each imputed dataset is a complete version of the original data with
#'           missing values filled in.
#'     \item The number of imputed datasets corresponds to the 'nImputation' parameter.
#'   }
#'
#'   \item Imputation Method:
#'   \itemize{
#'     \item Imputation is performed using the mice (Multivariate Imputation by Chained
#'           Equations) package.
#'     \item The specific imputation methods used for each variable can be examined
#'           in the returned 'mids' object.
#'   }
#'
#'   \item Data Consistency:
#'   \itemize{
#'     \item The imputed datasets maintain the same structure (rows and columns) as
#'           the original data.
#'     \item Only variables with missing values are imputed; complete variables remain unchanged.
#'   }
#' }#'
#'
#' @seealso \code{\link{createClustAll}}, \code{\link{showData}},
#' \code{\link{dataOriginal}}, \code{\link{ClustAllObject-class}},
#'
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


#' @title Retrieve Number of Imputations from ClustAllObject
#' @aliases nImputation,ClustAllObject-method
#' @description
#' This method returns the number of imputations performed when creating or
#' processing the ClustAllObject. It provides information about how missing data
#' was handled in the ClustALL pipeline.
#'
#' @usage nImputation(Object)
#'
#' @param Object A \code{\link{ClustAllObject-class}} object created by
#' \code{\link{createClustAll}}.
#'
#' @return An integer indicating the number of imputations performed. Returns 0
#' if no imputations were required or performed.
#'
#' @details
#' The nImputation method provides insight into the multiple imputation strategy
#' used in the ClustALL analysis pipeline:
#'
#' This method is particularly useful for:
#' \itemize{
#'   \item Verifying whether imputation was performed on the dataset.
#'   \item Understanding the extent of the imputation process.
#'   \item Assessing the potential impact of imputation on subsequent analyses.
#'   \item Reporting the methodology used in handling missing data.
#' }
#'
#' @note
#' \itemize{
#'   \item This method returns the value stored in the 'nImputation' slot of the ClustAllObject.
#'   \item A return value of 0 does not necessarily mean the original data had no
#'     missing values; it could also indicate that imputation was explicitly skipped.
#'   \item The number of imputations is typically set during the creation of the
#'     ClustAllObject with the \code{\link{createClustAll}} function.
#'   \item For accessing the actual imputed datasets, use the \code{\link{dataImputed}} method.
#' }#'
#' @seealso \code{\link{createClustAll}}, \code{\link{dataImputed}},
#' \code{\link{ClustAllObject-class}}, \code{\link[mice]{mice}}
#'
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


#' @title Retrieve Summary of Clustering Results from ClustAllObject
#' @aliases summary_clusters,ClustAllObject-method
#' @description
#' This method extracts and returns a comprehensive summary of all clustering
#' results (stratifications) generated by the ClustALL algorithm. It provides
#' access to the complete set of clustering solutions, including both robust
#' and non-robust stratifications.
#'
#' @usage summary_clusters(Object)
#'
#' @param Object A processed \code{\link{ClustAllObject-class}} object. The object
#' must have been processed by \code{\link{runClustAll}} before using this method.
#'
#' @return A list where each element represents a stratification. Each stratification
#' is a vector of cluster assignments for each sample in the dataset. Returns NULL
#' if \code{\link{runClustAll}} has not been executed on the object.
#'
#' @details
#' The summary_clusters method provides access to all clustering results generated
#' during the ClustALL analysis:
#'
#' \enumerate{
#'   \item Comprehensive Results:
#'   \itemize{
#'     \item Includes all stratifications generated, regardless of their robustness.
#'     \item Each stratification represents a unique combination of data embedding,
#'           distance metric, clustering algorithm, and number of clusters.
#'   }
#'
#'   \item Stratification Structure:
#'   \itemize{
#'     \item Each element in the returned list is named according to the parameters
#'           used to generate it (e.g., "cuts_a_1" for the first cut using method 'a').
#'     \item Each stratification is a vector of integers, where each integer represents
#'           a cluster assignment for a sample.
#'   }
#'
#'   \item Analysis Possibilities:
#'   \itemize{
#'     \item Allows for comparison of different clustering solutions.
#'     \item Enables examination of how different algorithm parameters affect clustering outcomes.
#'     \item Facilitates identification of consistent patterns across multiple stratifications.
#'   }
#' }
#'
#' This method is particularly useful for:
#' \itemize{
#'   \item Accessing the full range of clustering solutions for in-depth analysis.
#'   \item Comparing robust stratifications (identified by other methods) with the full set of results.
#'   \item Extracting specific stratifications for further analysis or visualization.
#' }
#'
#' @note
#' \itemize{
#'   \item This method returns the data stored in the 'summary_clusters' slot of the ClustAllObject.
#'   \item It will return NULL if \code{\link{runClustAll}} has not been executed on the object.
#'   \item The returned list includes all stratifications, not just those deemed robust.
#'     For accessing only robust stratifications, consider using the \code{\link{resStratification}} function.
#'   \item The order and naming of stratifications in the returned list correspond to
#'     the order in which they were generated during the ClustALL process.
#' }
#'
#' @seealso \code{\link{runClustAll}}, \code{\link{resStratification}},
#' \code{\link{JACCARD_DISTANCE_F}}, \code{\link{ClustAllObject-class}}
#'
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


#' @title Retrieve Jaccard Distance Matrix for Robust Stratifications
#' @aliases JACCARD_DISTANCE_F,ClustAllObject-method
#' @description
#' This method extracts and returns the matrix of Jaccard distances between
#' robust stratifications identified by the ClustALL algorithm. It provides a
#' quantitative measure of similarity between different clustering solutions
#' that have passed the bootstrapping process for population-based robustness.
#'
#' @usage JACCARD_DISTANCE_F(Object)
#'
#' @param Object A processed \code{\link{ClustAllObject-class}} object. The object
#' must have been processed by \code{\link{runClustAll}} before using this method.
#'
#' @return A square matrix where each element represents the Jaccard distance
#' between two robust stratifications. The row and column names correspond to
#' the names of the robust stratifications. Returns NULL if \code{\link{runClustAll}}
#' has not been executed on the object.
#'
#' @details
#' The JACCARD_DISTANCE_F method provides crucial information about the similarity
#' structure of robust clustering solutions:
#'
#' \enumerate{
#'   \item Jaccard Distance:
#'   \itemize{
#'     \item A measure of dissimilarity between sample sets, calculated as 1 minus the Jaccard coefficient.
#'     \item Ranges from 0 (identical stratifications) to 1 (completely different stratifications).
#'     \item Lower values indicate higher similarity between stratifications.
#'   }
#'
#'   \item Matrix Structure:
#'   \itemize{
#'     \item Symmetric matrix with stratification names as row and column labels.
#'     \item Diagonal elements are always 0 (each stratification is identical to itself).
#'     \item Off-diagonal elements represent pairwise Jaccard distances.
#'   }
#'
#'   \item Robust Stratifications:
#'   \itemize{
#'     \item Only includes stratifications that passed the bootstrapping process.
#'     \item Represents the most stable and reliable clustering solutions.
#'   }
#' }
#'
#' This method is particularly useful for:
#' \itemize{
#'   \item Identifying groups of similar stratifications.
#'   \item Assessing the diversity of robust clustering solutions.
#'   \item Selecting representative stratifications for further analysis.
#'   \item Visualizing the relationships between different clustering outcomes.
#'   \item Input for further clustering or dimensionality reduction of stratifications.
#' }
#'
#' @note
#' \itemize{
#'   \item This method returns the data stored in the 'JACCARD_DISTANCE_F' slot of the ClustAllObject.
#'   \item It will return NULL if \code{\link{runClustAll}} has not been executed on the object.
#'   \item The Jaccard distance is calculated based on the co-occurrence of samples in clusters,
#'     not on the specific cluster labels.
#'   \item This matrix is often used as input for the \code{\link{plotJACCARD}} function
#'     to visualize the similarity structure of stratifications.
#' }
#'
#' @seealso \code{\link{runClustAll}}, \code{\link{plotJACCARD}},
#' \code{\link{resStratification}}, \code{\link{ClustAllObject-class}}
#'
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


#' @title Check Processing Status of ClustAllObject
#' @aliases processed,ClustAllObject-method
#' @description
#' This method retrieves the processing status of a ClustAllObject, indicating
#' whether the ClustALL algorithm has been executed on the object. It provides a
#' quick way to verify if clustering results are available for analysis.
#'
#' @usage processed(Object)
#'
#' @param Object A \code{\link{ClustAllObject-class}} object created by
#' \code{\link{createClustAll}}.
#'
#' @return A logical value:
#' \itemize{
#'   \item TRUE if \code{\link{runClustAll}} has been executed on the object.
#'   \item FALSE if the object has not yet been processed by \code{\link{runClustAll}}.
#' }
#'
#' @details
#' The processed method serves as a crucial indicator in the ClustALL workflow:
#'
#' \enumerate{
#'   \item Processing Status:
#'   \itemize{
#'     \item Indicates whether the ClustALL algorithm has been applied to the object.
#'     \item A TRUE value means clustering results are available for analysis.
#'     \item A FALSE value indicates the object only contains input data and preprocessing.
#'   }
#'
#'   \item Workflow Implications:
#'   \itemize{
#'     \item Helps determine which methods and analyses can be performed on the object.
#'     \item Guides users in the correct sequence of operations in the ClustALL pipeline.
#'   }
#'
#'   \item Data Availability:
#'   \itemize{
#'     \item TRUE status implies availability of:
#'     \itemize{
#'       \item Stratification results (\code{\link{summary_clusters}})
#'       \item Jaccard distance matrix (\code{\link{JACCARD_DISTANCE_F}})
#'       \item Other clustering-related outputs
#'     }
#'     \item FALSE status means only input and preprocessed data are available.
#'   }
#' }
#'
#' @note
#' \itemize{
#'   \item This method checks the 'processed' slot of the ClustAllObject.
#'   \item The processed status is automatically set to TRUE when \code{\link{runClustAll}}
#'     completes successfully.
#'   \item Users should not manually modify this status to ensure consistency between
#'     the status and the actual content of the object.
#'   \item A FALSE status does not necessarily indicate an error; it simply means
#'     \code{\link{runClustAll}} needs to be executed before accessing clustering results.
#' }
#'
#' @seealso \code{\link{runClustAll}}, \code{\link{createClustAll}},
#' \code{\link{ClustAllObject-class}}, \code{\link{summary_clusters}},
#' \code{\link{JACCARD_DISTANCE_F}}
#'
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


#' @title Retrieve Validation Data from ClustAllObject
#' @aliases dataValidation,ClustAllObject-method
#' @description
#' This method extracts and returns the validation data (true labels) stored in
#' a ClustAllObject. These labels represent known classifications or groupings
#' of samples, which can be used to assess the performance of clustering results.
#'
#' @usage dataValidation(Object)
#'
#' @param Object A \code{\link{ClustAllObject-class}} object created by
#' \code{\link{createClustAll}} or modified by \code{\link{addValidationData}}.
#'
#' @return A numeric vector containing the validation data (true labels) if available.
#' Returns NULL if no validation data has been added to the object.
#'
#' @details
#' The dataValidation method provides access to the ground truth or reference
#' classifications for the samples in the dataset:
#'
#' \enumerate{
#'   \item Validation Data Content:
#'   \itemize{
#'     \item Contains known classifications or groupings of samples.
#'     \item Typically represents biological, clinical, or other meaningful categorizations.
#'     \item Used as benchmarking to evaluate clustering performance.
#'   }
#'
#'   \item Data Characteristics:
#'   \itemize{
#'     \item Returned as a numeric vector.
#'     \item Length matches the number of samples in the original dataset.
#'     \item Each element corresponds to a sample's true label or classification.
#'   }
#'
#'   \item Availability and Source:
#'   \itemize{
#'     \item May be added during object creation via the colValidation parameter in \code{\link{createClustAll}}.
#'     \item Can be added later using the \code{\link{addValidationData}} function.
#'     \item If not available, the method returns NULL.
#'   }
#' }
#'
#' @note
#' \itemize{
#'   \item This method returns the data stored in the 'dataValidation' slot of the ClustAllObject.
#'   \item The validation data is not used in the clustering process itself; it is solely for evaluation purposes.
#'   \item If validation data was not provided during object creation or added later, this method will return NULL.
#'   \item Always check for NULL before using the returned data in calculations or visualizations.
#'   \item The interpretation and use of validation data depend on the specific context of your study.
#' }
#'
#' @seealso \code{\link{createClustAll}}, \code{\link{addValidationData}},
#' \code{\link{validateStratification}}, \code{\link{ClustAllObject-class}}

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

#' @title Add Validation Data to ClustAllObject
#' @aliases addValidationData,ClustAllObject,numericOrCharacter-method
#' @description
#' This function adds or updates the validation data (true labels) in a
#' ClustAllObject. It allows users to incorporate known classifications or
#' groupings of samples after the object has been created, enabling subsequent
#' evaluation of clustering results against these true labels.
#'
#' @usage addValidationData(Object, dataValidation)
#'
#' @param Object A \code{\link{ClustAllObject-class}} object created by
#' \code{\link{createClustAll}}.
#' @param dataValidation A numeric or character vector containing the validation
#' data (true labels) for the samples. The length of this vector must match the
#' number of rows in the original input data used in
#' \code{\link{createClustAll}}.
#'
#' @return An updated \code{\link{ClustAllObject-class}} object with the new
#' validation data added to the dataValidation slot.
#'
#' @details
#' Adding validation data to a ClustAllObject is crucial for assessing the
#' performance of clustering results. This function allows for flexible
#' workflow where true labels can be incorporated at any stage of the analysis:
#' \itemize{
#'   \item It can be used to add validation data that was not available during
#'     initial object creation.
#'   \item It can also update existing validation data with new or corrected labels.
#'   \item The added data can be used with functions like
#'   \code{\link{validateStratification}}
#'     to evaluate clustering performance.
#' }
#' Key points to consider:
#' \itemize{
#'   \item The length of dataValidation must exactly match the number of samples
#'     in the original dataset.
#'   \item If the object already contains validation data, this function will
#'     overwrite it with the new data.
#'   \item Both numeric and character vectors are accepted, allowing for various
#'     types of classification schemes.
#' }
#'
#' @note
#' This function modifies the 'dataValidation' slot of the ClustAllObject in-place.
#' Always ensure that the provided validation data correctly corresponds to the
#' samples in your dataset to avoid misinterpretation of subsequent analyses.
#'
#' @seealso \code{\link{createClustAll}}, \code{\link{dataValidation}},
#' \code{\link{validateStratification}}, \code{\link{ClustAllObject-class}}
#'
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' label <- as.numeric(as.factor(wdbc$Diagnosis))
#' wdbc <- wdbc[,-c(1, 2)] # delete patients IDs & label
#' obj_noNA <- createClustAll(data = wdbc)
#' obj_noNA <- addValidationData(Object = obj_noNA,
#'                               dataValidation = label)
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
#' This function retrieves all data components stored in a
#' \code{\link{ClustAllObject-class}},
#' including the preprocessed data, original input data, and imputed datasets
#' (if applicable). It provides a comprehensive view of the data at different
#' stages of the ClustALL pipeline.
#' @usage extractData(Object)
#' @param Object A \code{\link{ClustAllObject-class}} object created by
#' \code{\link{createClustAll}}.
#' @return A list containing three elements:
#' \itemize{
#'   \item Data_modified: A data frame of the preprocessed data used for clustering.
#'   \item Data_original: A data frame of the original, unmodified input data.
#'   \item Data_imputed: A 'mids' object from the mice package containing imputed
#'     datasets, if imputation was performed. NULL otherwise.
#' }
#'
#' @details
#' The extractData function provides access to data at various stages of the
#' ClustALL pipeline:
#' \itemize{
#'   \item Data: Reflects preprocessing steps such as one-hot encoding
#'     for categorical variables and removal of the validation column.
#'   \item DataOriginal: The exact data as input to \code{\link{createClustAll}},
#'     useful for reference and verifying preprocessing steps.
#'   \item Data_imputed: Multiple imputed datasets if missing data was present
#'     and imputation was performed.
#' }
#' This function is particularly useful for:
#' \itemize{
#'   \item Verifying preprocessing steps and their effects on the data.
#'   \item Accessing original data for additional analyses or visualizations.
#'   \item Examining imputed datasets to understand how missing data was handled.
#'   \item Exporting data at different stages for use in other analyses or packages.
#' }
#'
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

#' @title Extract Clustering Results from ClustAllObject
#' @aliases extractResults,ClustAllObject-method
#' @description
#' This function retrieves the complete set of clustering results from a
#' processed ClustAllObject, including all generated stratifications and the
#' subset of statistically robust stratifications. It provides comprehensive
#' access to the outcomes of the ClustALL algorithm.
#'
#' @usage extractResults(Object)
#'
#' @param Object A processed \code{\link{ClustAllObject-class}} object. The object
#' must have been processed by \code{\link{runClustAll}} before using this function.
#'
#' @return A list containing two elements:
#' \itemize{
#'   \item All_clusters: A list of all stratifications generated by the ClustALL algorithm.
#'   \item Robust_clusters: A list of statistically robust stratifications that passed
#'     the bootstrapping process.
#' }
#' If \code{\link{runClustAll}} has not been executed, the function returns a list
#' with a single NULL element.
#'
#' @details
#' The extractResults function provides comprehensive access to the clustering
#' outcomes of the ClustALL algorithm:
#' \itemize{
#'   \item All_clusters: Contains every stratification generated, regardless of
#'     statistical robustness. Each element is a vector of cluster assignments
#'     for the samples.
#'   \item Robust_clusters: A subset of All_clusters, containing only those
#'     stratifications that passed the bootstrapping process for population-based
#'     robustness.
#' }
#' This function is particularly useful for:
#' \itemize{
#'   \item Comprehensive analysis of all generated clustering solutions.
#'   \item Comparing robust stratifications with non-robust ones.
#'   \item Exporting clustering results for further analysis in other tools.
#'   \item Assessing the impact of robustness criteria on stratification selection.
#' }
#'
#' @note
#' This function will return a list with a single NULL element if
#' \code{\link{runClustAll}} has not been executed on the object. Always check
#' if the returned list contains valid results before proceeding with analysis.
#'
#' @seealso \code{\link{runClustAll}}, \code{\link{summary_clusters}},
#' \code{\link{resStratification}}, \code{\link{ClustAllObject-class}}
#'
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
#' The Breast Cancer Wisconsin (Diagnostic) dataset, also known as "wdbc", contains
#' features computed from digitized images of fine needle aspirates (FNA) of breast
#' masses. The features describe characteristics of the cell nuclei present in each image.
#'
#' The dataset includes 569 patients, each characterized by 30 numerical features and one
#' categorical feature. The numerical features are computed from ten different measurements
#' of the cell nuclei, and each measurement is repeated three times, resulting in a total
#' of 30 features per patient (10 features x 3 measurements).
#'
#' The categorical feature is the diagnosis, which indicates whether the tumor is
#' malignant (M) or benign (B). This feature serves as the target class for
#' classification tasks.
#'
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
#' The "wdbcNA" dataset is a modified version of the Breast Cancer Wisconsin
#' (Diagnostic) dataset (\code{\link{wdbc}}), incorporating missing values at
#' random. This dataset is designed to demonstrate and evaluate the handling
#' of missing data in the context of breast cancer diagnosis.
#'
#' The dataset retains the same structure as the original "wdbc" dataset, with
#' 569 patients and 32 variables. However, some values in the numerical features
#' have been randomly replaced with missing values (NA).
#'
#' For a detailed description of the features and the original dataset, please
#' refer to the documentation of the "wdbc" dataset (\code{\link{wdbc}}).
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
#' The "wdbcMIDS" dataset is an imputed version of the "wdbcNA" dataset, which is
#' a modified version of the Breast Cancer Wisconsin (Diagnostic) dataset
#' (\code{\link{wdbc}}) with missing values introduced at random. The missing values
#' have been imputed using the Multiple Imputation by Chained Equations (MICE)
#' algorithm, resulting in a "mids" object manually.
#'
#' For more information on the original dataset and the version with missing values,
#' please refer to the documentation of the "wdbc" (\code{\link{wdbc}}).
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
#' The "obj_noNA1" dataset is a processed version of the Breast Cancer Wisconsin
#' (Diagnostic) dataset (\code{\link{wdbc}}), prepared for testing purposes and
#' used in the ClustAll package vignette. It has been processed using the ClustAll
#' package methods, which include data preprocessing, feature transformation, and
#' the application of clustering algorithms.
#'
#' The "obj_noNA1" dataset can be used as a reference for users who want to
#' understand the ClustAll package workflow and replicate the analysis presented
#' in the vignette.
#'
#' For more information on the original dataset, please refer to the
#' documentation of the "wdbc" (\code{\link{wdbc}}) dataset.
#'
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
#' The "obj_noNA1simplify" dataset is a processed version of the Breast Cancer Wisconsin
#' (Diagnostic) dataset (\code{\link{wdbc}}), prepared for testing purposes and used in
#' the ClustAll package vignette. It has been processed using the ClustAll package
#' methods with the `simplify` parameter set to TRUE, which reduces the computational
#' complexity of the clustering algorithms by considering only a subset of the
#' dendrogram depths.
#'
#' The "obj_noNA1simplify" dataset can be used as a reference for users who want to
#' understand the ClustAll package workflow and replicate the simplified analysis
#' presented in the vignette.
#'
#' For more information on the original dataset and the full processed version, please
#' refer to the documentation of the "wdbc" (\code{\link{wdbc}}).
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
#' The "obj_noNAno1Validation" dataset is a processed version of the Breast
#' Cancer Wisconsin (Diagnostic) dataset (\code{\link{wdbc}}), prepared for
#' testing purposes and used in the ClustAll package vignette. It has been
#' processed using the ClustAll package methods, but with the validation data
#' removed.
#'
#' The "obj_noNAno1Validation" dataset can be used as a reference for users who want
#' to understand the ClustAll package workflow and replicate the analysis presented
#' in the vignette, focusing on unsupervised learning aspects.
#'
#' For more information on the original dataset and the processed version with
#' validation data, please refer to the documentation of the "wdbc"
#' (\code{\link{wdbc}}).
#'
#' @docType data
#' @keywords datasets
#' @name obj_noNAno1Validation
#' @usage data("testData", package = "ClustAll")
#' @format A processed ClustAllObject
#' @return ClustAllObject Object
NULL

#' Heart Disease Dataset
#'
#' This dataset contains various medical and lifestyle attributes of patients,
#' along with their heart disease diagnosis status. It is commonly used for
#' predicting the presence of heart disease in patients.
#'
#' @description
#' The dataset comprises both categorical and numerical features derived from
#' medical examinations and patient history. Each row represents a patient,
#' characterized by 13 attributes, with the target variable indicating the
#' presence or absence of heart disease.
#'
#' @source Kaggle: https://www.kaggle.com/datasets/johnsmith88/heart-disease-dataset
#'
#' @format A data frame with 918 rows and 12 variables:
#' \describe{
#'   \item{Age}{Numeric. Age of the patient in years.}
#'   \item{Sex}{Categorical. Patient's gender (M = Male, F = Female).}
#'   \item{ChestPainType}{Categorical. Type of chest pain experienced (TA = Typical Angina, ATA = Atypical Angina, NAP = Non-Anginal Pain, ASY = Asymptomatic).}
#'   \item{RestingBP}{Numeric. Resting blood pressure in mm Hg.}
#'   \item{Cholesterol}{Numeric. Serum cholesterol in mg/dl.}
#'   \item{FastingBS}{Binary. Fasting blood sugar > 120 mg/dl (1 = true; 0 = false).}
#'   \item{RestingECG}{Categorical. Resting electrocardiogram results (Normal, ST = having ST-T wave abnormality, LVH = showing probable or definite left ventricular hypertrophy by Estes' criteria).}
#'   \item{MaxHR}{Numeric. Maximum heart rate achieved.}
#'   \item{ExerciseAngina}{Categorical. Exercise-induced angina (Y = Yes, N = No).}
#'   \item{Oldpeak}{Numeric. ST depression induced by exercise relative to rest.}
#'   \item{ST_Slope}{Categorical. The slope of the peak exercise ST segment (Up, Flat, Down).}
#'   \item{HeartDisease}{Binary. Output class (1 = heart disease, 0 = normal).}
#' }
#'
#' @details
#' This dataset is valuable for developing and testing machine learning models
#' for heart disease prediction. It includes a mix of demographic information,
#' vital signs, and results from various medical tests, making it a comprehensive
#' resource for studying factors associated with heart disease.
#'
#' @docType data
#' @keywords datasets
#' @name heart_data
#' @usage data("heart_data", package = "ClustAll")
#' @return heart dataset
NULL


# END OF ClustAll_ClustAllObject_Class.R
