# ClustAllObject_Methods--------------------------------------------------------
#' @import ComplexHeatmap
#' @import circlize
#' @import RColorBrewer
#' @import ggplot2
#' @import grid
#' @import grDevices
#' @title plotJACCARD: Heatmap of robust stratification distances based on
#'  Jaccard similarity
#' @aliases plotJACCARD,ClustAllObject,logicalOrNA,numericOrNA-method
#' @description
#' This function plots the correlation matrix heatmap showing the Jaccard
#' distances between robust stratifications.
#' @usage plotJACCARD(Object,
#'                    paint=TRUE,
#'                    stratification_similarity=0.7)
#'
#' @param Object \code{\link{ClustAllObject-class}} object.
#' @param paint TRUE for painting a square Logical vector with the annotation for the different
#' stratifications.
#' @param stratification_similarity Numeric value representing the minimum
#' Jaccard distance required to consider two stratifications as similar.
#' The default is 0.7.
#' @return Plot of a heatmap of the correlation matrix displaying Jaccard
#' distances between statistically robust stratifications.
#'
#' @seealso \code{\link{resStratification}},\code{\link{cluster2data}},
#' \code{\link{ClustAllObject-class}}
#'
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc <- subset(wdbc,select=c(-ID, -Diagnosis))
#' wdbc <- wdbc[1:15,1:8]
#' obj_noNA <- createClustAll(data = wdbc)
#' \donttest{
#' obj_noNA1 <- runClustAll(Object = obj_noNA, threads = 1, simplify = TRUE)
#' plotJACCARD(obj_noNA1, paint = TRUE, stratification_similarity = 0.9)
#' }
#' @export
setGeneric(
  name="plotJACCARD",
  def=function(Object, paint=TRUE, stratification_similarity=0.7)
  {standardGeneric("plotJACCARD")}
)

setMethod(
  f="plotJACCARD",
  signature=signature(
    Object="ClustAllObject",
    paint="logicalOrNA",
    stratification_similarity="numericOrNA"),
  definition=function(Object, paint=TRUE, stratification_similarity=0.7) {

    if (isProcessed(Object) == FALSE) {
      message("The object is not processed. You need to run runClustAll.")
      message("Note that the number of cores to use can be specified.")
      stop()
    }

    maxgo <- max(Object@JACCARD_DISTANCE_F[Object@JACCARD_DISTANCE_F < 1])
    ordergo <- hclust(1 - as.dist(Object@JACCARD_DISTANCE_F))$order
    m <- Object@JACCARD_DISTANCE_F[ordergo, ordergo]
    robust_stratification <- obtain_metadata(m)
    res <- resStratification(Object, population=1e-9, all=TRUE,
                             stratification_similarity)
    full_length <- ncol(Object@JACCARD_DISTANCE_F)

    # Correlation matrix heatmap with the robust stratifications
    col_fun <- colorRamp2(c(0, 1),
                          colors = grDevices::colorRampPalette(brewer.pal(9,"Blues"))(25),
                          breaks=seq(0, 1, length.out = 25))

    legend <- HeatmapAnnotation(JACCARD_index = seq(0, 1,
                                length.out = ncol(m)),
                                col = list(lg = col_fun),
                                annotation_name_side = "right")
    ra <- rowAnnotation(
            Distance=robust_stratification[,"Distance"],
            Clustering=robust_stratification[,"Clustering"],
            Depth=robust_stratification[,"Depth"],
            col = list(Distance=structure(names=c("Correlation","Gower"),
                                          c("#CCCCFF","blue4") ),
                       Clustering=structure(names=c("H-Clustering", "K-Means",
                                                    "K-Medoids"),
                                            c("forestgreen", "sandybrown",
                                              "tomato3")),
                       Depth=colorRamp2(c(1,
                                          round(max(robust_stratification[, "Depth"])/2),
                                          max(robust_stratification[, "Depth"])),
                                        c("darkcyan", "#F7DCCA", "#C75F97"))))

    hp <- Heatmap(as.matrix(m), name="hp", cluster_columns=FALSE,
                  cluster_rows=FALSE, left_annotation=ra, col=col_fun,
                  bottom_annotation = NULL, show_column_names = FALSE,
                  heatmap_legend_param = list(
                    direction = "vertical", title="JACCARD index", at=c(0, 1),
                    legend_width = unit(10, "cm")
                  ))

    if (is.null(res)) {
      paint <- FALSE
    }

    if (paint == TRUE) {
      ht_opt$message <- FALSE
      draw(hp)
      ngroup <- 0
      paint_names <- unlist(lapply(res, function(sublist) {
        c(sublist[[1]][[1]], sublist[[length(sublist)]][[1]])
      }))

      invisible(lapply(seq(from = 1, to = length(paint_names), by = 2), function(z) {
              ngroup <- ngroup + 1
              index <- which(rownames(m) %in% c(paint_names[z], paint_names[z + 1]))
              start <- index[1] - 1
              finish <- index[2]
              decorate_heatmap_body("hp", row_slice = 1, column_slice = 1, {
                grid::grid.rect(unit(start / full_length, "npc"), unit(1 - start / full_length, "npc"), # top left
                                width = (finish - start) / full_length,
                                height = (finish - start) / full_length,
                                gp = gpar(lwd = 2.5, lty = 2.5, fill = FALSE, col = "red"),
                                just = c("left", "top"), draw = TRUE
                )
              })
            }))

    } else {
      return(hp)
    }
  }
)


#' @title resStratification: Show the representative stratifications
#' @aliases resStratification,ClustAllObject,numericOrNA,logicalOrNA,numericOrNA-method
#' @description
#' This function returns a list of representative stratifications.
#' Stratifications that do not meet the minimum population threshold for each
#' group are discarded. The minimum population percentage can be set with a
#' default value of 0.05 (5%). When 'all' is TRUE, the function returns all
#' robust stratifications; otherwise, it returns the representative for each
#' group of stratification.
#' @usage resStratification(Object,
#'                          population=0.05,
#'                          all=FALSE,
#'                          stratification_similarity=0.7)
#'
#' @param Object \code{\link{ClustAllObject-class}} object.
#' @param population Numeric vector specifying the minimum percentage of the
#' total population that a stratification must have to be considered valid.
#' @param all Logical vector indicating whether to return all representative
#' stratifications for each group of clusters. If FALSE, only the centroid
#' (representative) stratification of each group is returned.
#' @param stratification_similarity Numeric value representing the minimum
#' Jaccard distance required to consider two stratifications as similar.
#' The default is 0.7.
#' @return List containing all statistically robust stratifications,
#' grouped by clusters formed based on the previously defined minimum cluster
#' similarity and population criteria. If all is set to TRUE, the function
#' returns all robust stratifications for each cluster. If all is set to FALSE,
#' only the centroid (representative) stratification for each cluster group is
#' returned.
#'
#' @seealso \code{\link{plotJACCARD}},\code{\link{cluster2data}},
#' \code{\link{ClustAllObject-class}}
#'
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc <- subset(wdbc,select=c(-ID, -Diagnosis))
#' wdbc <- wdbc[1:15,1:8]
#' obj_noNA <- createClustAll(data = wdbc)
#' \donttest{
#' obj_noNA1 <- runClustAll(Object = obj_noNA, threads = 1, simplify = TRUE)
#' resStratification(Object = obj_noNA1, population = 0.05,
#'                   stratification_similarity = 0.88, all = FALSE)
#' }
#'
#' @export
setGeneric(
  name="resStratification",
  def=function(Object, population=0.05, all=FALSE,
               stratification_similarity=0.7)
  {standardGeneric("resStratification")}
)

setMethod(
  f="resStratification",
  signature=signature(
    Object="ClustAllObject",
    population="numericOrNA",
    all="logicalOrNA",
    stratification_similarity="numericOrNA"),
  definition=function(Object, population=0.05, all=FALSE,
                      stratification_similarity=0.7) {

    if (isProcessed(Object) == FALSE) {
      message("The object has not been processed yet.")
      message(". You need to execute runClustAll.")
      message("Note that the number of cores to use can be specified.")
      stop()
    }

    maxgo <- max(Object@JACCARD_DISTANCE_F[Object@JACCARD_DISTANCE_F < 1])
    ordergo <- hclust(1 - as.dist(Object@JACCARD_DISTANCE_F))$order

    # obtain the definitive clusters
    definitive_clusters <- obtainDefCluster(Object@JACCARD_DISTANCE_F[ordergo,
                                                                      ordergo],
                                            stratification_similarity)

    if (length(definitive_clusters) > 0) {
      # choose the representative stratification considering at least a minimum
      # percentage of the total population in each cluster. Default is 0.05 (5%)
      res <- chooseClusters(definitive_clusters, Object@summary_clusters,
                            population, all)
      stratificationRep <- lapply(seq_along(res), function(i) {
        if (all == TRUE) {
          inner_list <- lapply(seq_along(res[[i]]), function(j) {
            list(res[[i]][j], table(Object@summary_clusters[[res[[i]][j]]]))
          })
          names(inner_list) <- paste0("Stratification_", seq_along(res[[i]]))
          inner_list
        } else {
          list(table(Object@summary_clusters[[res[[i]]]]))
        }
      })

      if (all == FALSE) {
        names(stratificationRep) <- res
      } else {
      names(stratificationRep) <- paste0("Cluster_",
                                         seq(length(stratificationRep)))
      }

      return(stratificationRep)

    } else {
      message("There are no robust groups of stratification for the selected parameters.")
      return(list(NULL))
    }
  }
)


#' @title cluster2data: Export selected stratification(s)
#' @aliases cluster2data,ClustAllObject,character-method
#' @description
#' Returns the original input data in a Data Frame, appending the selected
#' robust stratification(s) as additional columns. The names of the representative
#' stratifications can be obtained using the \code{\link{resStratification}}
#' method.
#' @usage cluster2data(Object,
#'                     stratificationName)
#'
#' @param Object \code{\link{ClustAllObject-class}} object.
#' @param stratificationName Name of the stratification(s) to be exported.
#'
#' @return Returns the original Data Frame with additional column(s)
#' corresponding to the selected stratification(s).
#'
#' @seealso \code{\link{resStratification}},\code{\link{plotJACCARD}},
#' \code{\link{ClustAllObject-class}}
#'
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc <- subset(wdbc,select=c(-ID, -Diagnosis))
#' wdbc <- wdbc[1:15,1:8]
#' obj_noNA <- createClustAll(data = wdbc)
#' \donttest{
#' obj_noNA1 <- runClustAll(Object = obj_noNA, threads = 1, simplify = TRUE)
#' resStratification(Object = obj_noNA1, population = 0.05,
#'                   stratification_similarity = 0.88, all = FALSE)
#' df <- cluster2data(Object = obj_noNA1,
#'                    stratificationName = c("cuts_a_1","cuts_b_5","cuts_a_5"))
#' }
#' @export
setGeneric(
  name="cluster2data",
  def=function(Object, stratificationName){standardGeneric("cluster2data")}
)

setMethod(
  f="cluster2data",
  signature=signature(
    Object="ClustAllObject",
    stratificationName="character"),
  definition=function(Object, stratificationName) {

    if (isProcessed(Object) == FALSE) {
      message("The object is not processed. You need to run runClustAll.")
      message("Note that the number of cores to use can be specified.")
      stop()
    }

    checkCluster(stratificationName, Object@summary_clusters)

    df <- Object@dataOriginal
    df[, stratificationName] <- Object@summary_clusters[stratificationName]

    return(df)
  }
)


#' @import networkD3
#' @import dplyr
#' @title plotSANKEY: Plots Sankey Diagram showing the cluster distribution
#' and shifts between a pair of stratifications derived from ClustAllObject
#' @aliases plotSANKEY,ClustAllObject,character,logicalOrNA-method
#' @description
#' This function generates a Sankey Diagram that visualizes the distribution of
#' stratifications and the shifts between two stratifications derived from a
#' ClustAllObject.
#' @usage plotSANKEY(Object,
#'                   clusters,
#'                   validationData=FALSE)
#' @param Object \code{\link{ClustAllObject-class}} object.
#' @param clusters Character vector with the names of a pair of
#' stratifications to compare. When validationData is TRUE, only include the
#' cluster names you want to compare with the validation information. Check
#' \code{\link{resStratification}} for stratification results.
#' @param validationData Logical value indicating whether to use true labels
#' (validation data) for comparison with the selected stratification.
#' @return Sankey plot showing the distribution and shifts between the
#' selected stratifications. When validationData is TRUE, the plot includes the
#' true labels (validation data) if available.
#' @seealso \code{\link{resStratification}},\code{\link{cluster2data}},
#' \code{\link{ClustAllObject-class}}
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' label <- as.numeric(as.factor(wdbc$Diagnosis))
#' wdbc <- subset(wdbc,select=c(-ID, -Diagnosis))
#' wdbc <- wdbc[1:15,1:8]
#' label <- label[16:30]
#' obj_noNA <- createClustAll(data = wdbc)
#' \donttest{
#' obj_noNA1 <- runClustAll(Object = obj_noNA, threads = 1, simplify = TRUE)
#' resStratification(Object = obj_noNA1, population = 0.05,
#'                   stratification_similarity = 0.88, all = FALSE)
#' plotSANKEY(Object = obj_noNA1, clusters = c("cuts_a_1","cuts_b_5"))
#'
#' obj_noNA1 <- addValidationData(obj_noNA1, label)
#' plotSANKEY(Object = obj_noNA1, clusters = "cuts_a_1", validationData=TRUE)
#' }
#'
#' @export
setGeneric(
  name="plotSANKEY",
  def=function(Object, clusters, validationData=FALSE)
  {standardGeneric("plotSANKEY")}
)

setMethod(
  f="plotSANKEY",
  signature=signature(
    Object="ClustAllObject",
    clusters="character",
    validationData="logicalOrNA"),
  definition=function(Object, clusters, validationData=FALSE) {
    if (isProcessed(Object) == FALSE) {
      message("The object has not been processed yet.")
      message("You need to execute runClustAll.")
      message("Note that the number of cores to use can be specified.")
      stop()
    }

    if (validationData) {
      if (is.null(Object@dataValidation)) {
        message("The ClustALL Object does not include labels.")
        message("Please create a new object or modidy the object with the original labelling data. \nFor that check addValidationData method.")
        stop()
      } else if (length(clusters) != 1) {
        message("More than one stratifications have been selected.")
        message("Only the first one will be considered to plot against the original labelling data.")
        clusters <- clusters[1]
      }
    } else if (length(clusters) != 2) {
      message("You need to include the name of a pair of stratifications.")
      message("To obtain the stratifications you may use resStratification.")
      stop()
    }

    checkCluster(clusters, Object@summary_clusters)

    df <- cluster2data(Object, clusters)

    if (validationData) {
      df[, "validation"] <- Object@dataValidation
      clusters <- c(clusters, "validation")
    }

    df <- df[, clusters]
    names1 <- paste0(clusters[1], "_Group_",
                     seq_len(length(unique(df[,clusters[1]]))))
    names2 <- paste0(clusters[2], "_Group_",
                     seq_len(length(unique(df[,clusters[2]]))))

    source <- sort(rep(names1, times=length(unique(df[,clusters[2]]))))
    target <- rep(names2, times=length(unique(df[,clusters[1]])))

    value <- unlist(lapply(unique(df[, 1]), function(i) {
      df_tmp <- df[df[, 1] == i, ]
      tmp <- nrow(df)
      unlist(lapply(unique(df[, 2]), function(j) {
        length(which(df_tmp[, 2] == j)) / tmp
      }))
    }))

    links <- data.frame(source = source,
                        target = target,
                        value = value)

    nodes <- data.frame(name=c(as.character(links$source),
                               as.character(links$target)) %>% unique()
    )

    links$IDsource <- match(links$source, nodes$name)-1
    links$IDtarget <- match(links$target, nodes$name)-1

    plot <- networkD3::sankeyNetwork(Links = links, Nodes = nodes,
                                     Source = "IDsource", Target = "IDtarget",
                                     Value = "value", NodeID = "name",
                                     sinksRight=FALSE)

    return(plot)
  }
)


#' @title validateStratification: calculates the sensitivity and specificity
#' of selected stratification
#' @aliases validateStratification,ClustAllObject,characterOrNA-method
#' @description
#' Returns the sensitivity and specificity of the selected stratification
#' compared to the true labels (validation data). The names of representative
#' stratifications can be obtained using the method
#' \code{\link{resStratification}}.
#' @usage validateStratification(Object,
#'                               stratificationName)
#'
#' @param Object \code{\link{ClustAllObject-class}} object.
#' @param stratificationName Character vector with the name of a stratification.
#' Check \code{\link{resStratification}} to obtain stratification names.
#'
#' @return the sensitivity and specificity values of the selected stratification
#' when validation data is available.
#'
#' @seealso \code{\link{resStratification}},\code{\link{plotJACCARD}},
#' \code{\link{ClustAllObject-class}}
#'
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' label <- as.numeric(as.factor(wdbc$Diagnosis))
#' wdbc <- subset(wdbc,select=c(-ID, -Diagnosis))
#' wdbc <- wdbc[1:15,1:8]
#' label <- label[16:30]
#' obj_noNA <- createClustAll(data = wdbc)
#' \donttest{
#' obj_noNA1 <- runClustAll(Object = obj_noNA, threads = 1, simplify = TRUE)
#' resStratification(Object = obj_noNA1, population = 0.05,
#'                   stratification_similarity = 0.88, all = FALSE)
#' obj_noNA1 <- addValidationData(Object = obj_noNA1,
#'                                dataValidation = label)
#' validateStratification(obj_noNA1, "cuts_a_1")
#' }
#'
#' @export
setGeneric(
  name="validateStratification",
  def=function(Object, stratificationName){
    standardGeneric("validateStratification")}
)

setMethod(
  f="validateStratification",
  signature=signature(
    Object="ClustAllObject",
    stratificationName="characterOrNA"),
  definition=function(Object, stratificationName) {

    if (is.null(Object@dataValidation)) {
      message("The ClustALL Object does not include labels.")
      message("Please create a new object or modify the object with the original labelling data. \nFor that check addValidationData method")
      stop()
    }

    if (isProcessed(Object) == FALSE) {
      message("The object has not been processed yet.")
      message("You need to execute runClustAll.")
      message("Note that the number of cores to use can be specified.")
      stop()
    }

    checkCluster(stratificationName, Object@summary_clusters)

    if (length(stratificationName) != 1) {
      message("More than one stratifications have been selected.")
      message("Only the first one will be considered.")
      stratificationName <- stratificationName[1]
    }

    df <- cluster2data(Object, stratificationName)
    res <- table(df[,stratificationName], Object@dataValidation)
    sensitivity <- res[3]/(res[3]+res[4])
    specifity <- res[2]/(res[2]+res[1])

    if (sensitivity < 0.5 & specifity < 0.5)
    {
      sensitivity <- 1 - sensitivity
      specifity <- 1 - specifity
    }

    showRes <- c(sensitivity, specifity)
    base::names(showRes) <- c("sensitivity", "specificity")

    return(showRes)
  }
)

# END OF ClustAll_ClustAllObject_Methods.R
