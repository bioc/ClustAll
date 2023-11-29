# ClustAllObject_Methods--------------------------------------------------------
#' @import ComplexHeatmap
#' @import circlize
#' @import RColorBrewer
#' @import ggplot2
#' @import grid
#' @title Plots correlation Jaccard Distances from ClustAllObject
#' @aliases plotJACCARD,ClustAllObject-method
#' @description
#' This function plots the correlation Jaccard Distances
#' @usage plotJACCARD(Object,
#'        paint=TRUE,
#'        stratification_similarity=0.7)
#'
#' @param Object \code{\link{ClustAllObject-class}} object
#' @param paint Logical vector to paint the different group of clusters in the plot
#' @param stratification_similarity the minimum values to consider two groups similar in Jaccard distances. As default 0.7.
#'
#' @return plot
#'
#' @seealso \code{\link{resStratification}},\code{\link{cluster2data}},\code{\link{ClustAllObject-class}}
#'
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc <- wdbc[,-c(1,2)]
#' obj_noNA <- createClustAll(data = wdbc)
#' obj_noNA1 <- runClustAll(Object = obj_noNA, threads = 8)
#' plotJACCARD(obj_noNA1, paint = TRUE, stratification_similarity = 0.88)
#'
#' @export
setGeneric(
  name="plotJACCARD",
  def=function(Object, paint=TRUE, stratification_similarity=0.7){standardGeneric("plotJACCARD")}
)

setMethod(
  f="plotJACCARD",
  signature=signature(
    Object="ClustAllObject",
    paint="logicalOrNA",
    stratification_similarity="numericOrNA"),
  definition=function(Object, paint=TRUE, stratification_similarity=0.7) {

    if (isProcessed(Object) == FALSE) {
      message("The object is not processed. You need to run runClustAll. Note that the number of cores to use can be specified.")
      stop()
    }

    maxgo <- max(Object@JACCARD_DISTANCE_F[Object@JACCARD_DISTANCE_F < 1])
    ordergo <- hclust(1 - as.dist(Object@JACCARD_DISTANCE_F))$order
    m <- Object@JACCARD_DISTANCE_F[ordergo, ordergo]
    robust_stratification <- obtain_metadata(m)
    res <- resStratification(Object, population=1e-9, all=TRUE, stratification_similarity)
    full_length <- ncol(Object@JACCARD_DISTANCE_F)

    # This plot contains the robust stratifications
    # col_fun <- colorRampPalette(brewer.pal(9,"Blues"))(25)
    col_fun <- colorRamp2(c(0, 1), colors = colorRampPalette(brewer.pal(9,"Blues"))(25),
                          breaks=seq(0, 1, length.out = 25))
    # legend <- Legend(title="JACCARD index", legend_width = unit(10, "cm"),
    #                  direction = "horizontal", at=c(0, 1), title_position="topcenter")
    legend <- HeatmapAnnotation(JACCARD_index = seq(0, 1, length.out = ncol(m)), col = list(lg = col_fun),
                                 annotation_name_side = "right")
    ra <- rowAnnotation(
      Distance=robust_stratification[,"Distance"],
      Clustering=robust_stratification[,"Clustering"],
      Depth=robust_stratification[,"Depth"],
      col = list(Distance=structure(names=c("Correlation","Gower"),c("#CCCCFF","blue4") ),
                 Clustering=structure(names=c("Hierachical", "k-means", "k-medoids"), c("forestgreen", "sandybrown", "tomato3") ),
                 Depth=colorRamp2(c(1,round(max(robust_stratification[, "Depth"])/2),
                                    max(robust_stratification[, "Depth"])),
                                  c("darkcyan", "#F7DCCA", "#C75F97"))))

    hp <- Heatmap(as.matrix(m), name="hp", cluster_columns=FALSE, cluster_rows=FALSE,
                  left_annotation=ra, col=col_fun, bottom_annotation = NULL,
                  show_column_names = FALSE,
                  heatmap_legend_param = list(
                    direction = "vertical", title="JACCARD index", at=c(0, 1),
                    legend_width = unit(10, "cm")
                  ))

    if (is.null(res)) {
      paint <- FALSE
    }

    if (paint == TRUE) {
      draw(hp)
      ngroup <- 0
      paint_names <- c()
      for (i in 1:length(res)) {
          paint_names <- c(paint_names, res[[i]][[1]][1])
          paint_names <- c(paint_names, res[[i]][[length(res[[i]])]][1])
      }

      for (z in seq(from=1, to=length(paint_names), by=2)) {
        ngroup <- ngroup + 1
        index <- which(rownames(m) %in% c(paint_names[z], paint_names[z+1]))
        start <- index[1] - 1
        finish <- index[2]
        decorate_heatmap_body("hp", row_slice = 1, column_slice = 1, {
          grid.rect(unit(start/full_length, "npc"), unit(1-start/full_length, "npc"), # top left
                    width = (finish-start)/full_length,
                    height = (finish-start)/full_length,
                    gp = gpar(lwd = 2.5, lty = 2.5, fill=FALSE, col="red"), just = c("left", "top"), draw = TRUE
          )
        })
      }
    } else {
      return(hp)
    }
  }
)


#' @import clusteval
#' @title Show stratification representatives from ClustAllObject
#' @aliases resStratification,ClustAllObject-method
#' @description
#' This function returns the stratifications representatives by filtering those clusters with a minimum percentage of the population. Either returns all the robust stratification or the representative one of each group of stratification
#' @usage resStratification(Object,
#'        population=0.05,
#'        all=FALSE,
#'        stratification_similarity=0.7)
#'
#' @param Object \code{\link{ClustAllObject-class}} object
#' @param population Numeric vector giving the minimum amount of population that a stratification must have to be considered representative
#' @param all Logical vector to return all the representative clusters per group of clusters. If it is FALSE, only the centroid stratification of every group of clusters is returned
#' @param stratification_similarity the minimum values to consider two groups similar in Jaccard distances. As default 0.7.

#' @return list
#'
#' @seealso \code{\link{plotJACCARD}},\code{\link{cluster2data}},\code{\link{ClustAllObject-class}}
#'
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc <- wdbc[,-c(1,2)]
#' obj_noNA <- createClustAll(data = wdbc)
#' obj_noNA1 <- runClustAll(Object = obj_noNA, threads = 8)
#' resStratification(Object = obj_noNA1, population = 0.05, stratification_similarity = 0.88, all = FALSE)
#'
#' @export
setGeneric(
  name="resStratification",
  def=function(Object, population=0.05, all=FALSE, stratification_similarity=0.7){standardGeneric("resStratification")}
)

setMethod(
  f="resStratification",
  signature=signature(
    Object="ClustAllObject",
    population="numericOrNA",
    all="logicalOrNA",
    stratification_similarity="numericOrNA"),
  definition=function(Object, population=0.05, all=FALSE, stratification_similarity=0.7) {

    if (isProcessed(Object) == FALSE) {
      message("The object is not processed. You need to run runClustAll. Note that the number of cores to use can be specified.")
      stop()
    }

    maxgo <- max(Object@JACCARD_DISTANCE_F[Object@JACCARD_DISTANCE_F < 1])
    ordergo <- hclust(1 - as.dist(Object@JACCARD_DISTANCE_F))$order

    # obtain the definitive clusters
    definitive_clusters <- obtainDefCluster(Object@JACCARD_DISTANCE_F[ordergo, ordergo], stratification_similarity)

    if (length(definitive_clusters) > 0) {
      # chose the representative stratification with minimum population in each stratification As default 0.05 (5%)
      res <- chooseClusters(definitive_clusters, Object@summary_clusters, population, all)
      stratificationRep <- list()

      if (all == TRUE) {
        for (i in 1:length(res)) {
          stratificationRep[[i]] <- list()

          for (j in 1:length(res[[i]])) {

            stratificationRep[[i]][[j]] <- c(res[[i]][j], table(Object@summary_clusters[[res[[i]][j]]]))
            names(stratificationRep)[[i]] <- paste("Cluster_", i)
          }
        }

      } else {
        for(i in 1:(length(res))) {
          stratificationRep[[i]] <- list(table(Object@summary_clusters[[res[[i]]]]))
          names(stratificationRep)[i] <- res[[i]]
        }
      }

      return(stratificationRep)

    } else {
      message(paste0("There are no robust groups of stratification for the selected parameters. Stratification_similarity: ", stratification_similarity, "."))
      return(NULL)
    }
  }
)


#' @title cluster2data
#' @aliases cluster2data,ClustAllObject-method
#' @description
#' Returns the data frame of the original data using which the clustering of the selected stratification(s) are included as varibles. The representative stratification names can be obtained using the method \code{\link{resStratification}}
#' @usage cluster2data(Object,
#'        clusterName)
#'
#' @param Object \code{\link{ClustAllObject-class}} object
#' @param clusterName Character vector with one or more stratification names
#'
#' @return data.frame
#'
#' @seealso \code{\link{resStratification}},\code{\link{plotJACCARD}},\code{\link{ClustAllObject-class}}
#'
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc <- wdbc[,-c(1,2)]
#' obj_noNA <- createClustAll(data = wdbc)
#' obj_noNA1 <- runClustAll(Object = obj_noNA, threads = 8)
#' resStratification(Object = obj_noNA1, population = 0.05, stratification_similarity = 0.88, all = FALSE)
#' df <- cluster2data(Object = obj_noNA1, clusterName = c("cuts_c_3","cuts_a_9","cuts_b_13"))
#' @export
setGeneric(
  name="cluster2data",
  def=function(Object, clusterName){standardGeneric("cluster2data")}
)

setMethod(
  f="cluster2data",
  signature=signature(
    Object="ClustAllObject",
    clusterName="character"),
  definition=function(Object, clusterName) {

    if (isProcessed(Object) == FALSE) {
      message("The object is not processed. You need to run runClustAll. Note that the number of cores to use can be specified.")
      stop()
    }

    checkCluster(clusterName, Object@summary_clusters)

    df <- Object@data
    df[, clusterName] <- Object@summary_clusters[clusterName]

    return(df)
  }
)


#' @import networkD3
#' @import dplyr
#' @title Plots Sankey diagram from two selected groups from ClustAllObject
#' @aliases plotSANKEY,ClustAllObject-method
#' @description
#' This function plots the Sankey diagram of two selected clusters
#' @usage plotSANKEY(Object,
#'                   clusters)
#'
#' @param Object \code{\link{ClustAllObject-class}} object
#' @param clusters Character vector with the names of two clusters. Check resStratification to obtain stratification names.
#' @param validationData Logical value to use validation data to compare with the selected stratification
#'
#' @return plot
#'
#' @seealso \code{\link{resStratification}},\code{\link{cluster2data}},\code{\link{ClustAllObject-class}}
#'
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc <- wdbc[,-c(1,2)]
#' obj_noNA <- createClustAll(data = wdbc)
#' obj_noNA1 <- runClustAll(Object = obj_noNA, threads = 8)
#' resStratification(Object = obj_noNA1, population = 0.05, stratification_similarity = 0.88, all = FALSE)
#' plotSANKEY(Object = obj_noNA1, clusters = c("cuts_c_3","cuts_a_9"))
#' plotSANKEY(Object = obj_noNA1, clusters = c("cuts_c_3","cuts_b_13"))
#'
#' @export
setGeneric(
  name="plotSANKEY",
  def=function(Object, clusters, validationData=FALSE){standardGeneric("plotSANKEY")}
)

setMethod(
  f="plotSANKEY",
  signature=signature(
    Object="ClustAllObject",
    clusters="character",
    validationData="logicalOrNA"),
  definition=function(Object, clusters, validationData=FALSE) {
    if (isProcessed(Object) == FALSE) {
      message("The object is not processed. You need to run runClustAll. Note that the number of cores to use can be specified.")
      stop()
    }

    if (validationData) {
      if (is.null(Object@dataValidation)) {
        message("The ClustALL Object does not contain validation data. Please create a new object or modidy the object wi the validation data. \nFor that check addValidationData method.")
        stop()
      } else if (length(clusters) != 1) {
        message("More than one stratifications have been selected. Only the first one will be use to plot with the validation data.")
        clusters <- clusters[1]
      }
    } else if (length(clusters) != 2) {
      message("You need to include the name of two clusters. To obtain the stratifications you may use resStratification.")
      stop()
    }

    checkCluster(clusters, Object@summary_clusters)

    df <- cluster2data(Object, clusters)

    if (validationData) {
      df[, "validation"] <- Object@dataValidation
      clusters <- c(clusters, "validation")
    }

    df <- df[, clusters]
    names1 <- paste0(clusters[1], "_Group_", 1:length(unique(df[,clusters[1]])))
    names2 <- paste0(clusters[2], "_Group_", 1:length(unique(df[,clusters[2]])))

    source <- sort(rep(names1, times=length(unique(df[,clusters[2]]))))
    target <- rep(names2, times=length(unique(df[,clusters[1]])))

    value <- c()

    for (i in unique(df[,1])) {
      df_tmp <- df[which(df[,1] == i),]
      tmp <- nrow(df)
      for (j in unique(df[,2])) {
        value <- c(value, length(which(df_tmp[,2] == j))/tmp)
      }
    }

    links <- data.frame(source = source,
                        target = target,
                        value=value)

    nodes <- data.frame(name=c(as.character(links$source),
                               as.character(links$target)) %>% unique()
    )

    links$IDsource <- match(links$source, nodes$name)-1
    links$IDtarget <- match(links$target, nodes$name)-1

    plot <- sankeyNetwork(Links = links, Nodes = nodes,
                          Source = "IDsource", Target = "IDtarget",
                          Value = "value", NodeID = "name",
                          sinksRight=FALSE)

    return(plot)
  }
)


#' @title validateStratification
#' @aliases validateStratification,ClustAllObject-method
#' @description
#' Returns the sensitivity and specifity of the selected stratification calculated using the validation data. The representative stratification names can be obtained using the method \code{\link{resStratification}}
#' @usage cluster2data(Object,
#'        stratificationName)
#'
#' @param Object \code{\link{ClustAllObject-class}} object
#' @param stratificationName Character vector with the name a stratification. Check resStratification to obtain stratification names.
#'
#' @return numeric
#'
#' @seealso \code{\link{resStratification}},\code{\link{plotJACCARD}},\code{\link{ClustAllObject-class}}
#'
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc$Diagnosis <- as.numeric(as.factor(wdbc$Diagnosis))
#' wdbc <- wdbc[,-c(1)] # delete patients IDs
#' obj_noNA <- createClustAll(data = wdbc, colValidation = "Diagnosis")
#' obj_noNA1 <- runClustAll(Object = obj_noNA, threads = 8)
#' resStratification(Object = obj_noNA1, population = 0.05, stratification_similarity = 0.88, all = FALSE)
#' @export
setGeneric(
  name="validateStratification",
  def=function(Object, stratificationName){standardGeneric("validateStratification")}
)

setMethod(
  f="validateStratification",
  signature=signature(
    Object="ClustAllObject",
    stratificationName="characterOrNA"),
  definition=function(Object, stratificationName) {

    if (is.null(Object@dataValidation)) {
      message("The ClustALL Object does not contain validation data. Please create a new object or add the validation data. \nFor that check addValidationData method.")
      stop()
    }

    if (isProcessed(Object) == FALSE) {
      message("The object is not processed. You need to run runClustAll. Note that the number of cores to use can be specified.")
      stop()
    }

    checkCluster(stratificationName, Object@summary_clusters)

    if (length(stratificationName) != 1) {
      message("More than one stratification have been detected. Only the first one will be use")
      stratificationName <- stratificationName[1]
    }

    df <- cluster2data(Object, stratificationName)
    res <- table(df[,stratificationName], Object@dataValidation)
    sensitivity <- res[3]/(res[3]+res[4])
    specifity <- res[2]/(res[2]+res[1])
    showRes <- c(sensitivity, specifity)
    names(showRes) <- c("sensitivity", "specifity")

    return(showRes)
  }
)

# END OF ClustAll_ClustAllObject_Methods.R
