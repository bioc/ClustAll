<<<<<<< HEAD
<<<<<<< HEAD
# ClustAllObject_Methods--------------------------------------------------------
#' @import corrplot
#' @import dplyr
#' @title Plots correlation Jaccard Distances from ClustAllObject
#' @aliases plotJACCARD,ClustAllObject-method
#' @description
#' This function plots the correlation Jaccard Distances
#' @usage plotJACCARD(Object,
#'        paint=TRUE,
#'        cluster_similarity=0.7)
#'
#' @param Object \code{\link{ClustAllObject-class}} object
#' @param paint Logical vector to paint the different group of clusters in the plot
#' @param cluster_similarity the minimum values to consider two groups similar in Jaccard distances. As default 0.7.
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
#' plotJACCARD(obj_noNA1, paint = TRUE, cluster_similarity = 0.88)
#'
#' @export
setGeneric(
  name="plotJACCARD",
  def=function(Object, paint=TRUE, cluster_similarity=0.7){standardGeneric("plotJACCARD")}
)

setMethod(
  f="plotJACCARD",
  signature=signature(
    Object="ClustAllObject",
    paint="logicalOrNA",
    cluster_similarity="numericOrNA"),
  definition=function(Object, paint="TRUE", cluster_similarity=0.7) {

    if (isProcessed(Object) == FALSE) {
      message("The object is not processed. You need to run runClustAll. Note that the number of cores to use can be specified.")
      stop()
    }

    maxgo <- max(Object@JACCARD_DISTANCE_F[Object@JACCARD_DISTANCE_F < 1])
    ordergo <- hclust(1 - as.dist(Object@JACCARD_DISTANCE_F))$order

    # This plot contains the robust stratifications
    resPlot <- corrplot(Object@JACCARD_DISTANCE_F[ordergo, ordergo], is.corr=F,
                        method="shade", main="", order="original", tl.col="black",
                        tl.srt=65, cl.pos="b", cl.length=4, sig.level = 0.7)

    if (paint == TRUE) {
      res <- resStratification(Object, population = 1e-9, all = T, cluster_similarity)

      paint_names <- c()
      for (i in 1:length(res)) {
          paint_names <- c(paint_names, res[[i]][[1]][1])
          paint_names <- c(paint_names, res[[i]][[length(res[[i]])]][1])
      }

      for (z in seq(from=1, to=length(paint_names), by=2)) {
        resPlot <- resPlot %>% corrRect(name = c(paint_names[z], paint_names[z+1]), lwd = 4, col="black")
      }
    }

    return(resPlot)
  }
)


#' @import clusteval
#' @title Show stratification representatives from ClustAllObject
#' @aliases resStratification,ClustAllObject-method
#' @description
#' This function returns the stratifications representatives by filtering those clusters with a minimum percentage of the population. Either returns all the robust cluster or the representative one of each group of cluster
#' @usage resStratification(Object,
#'        population=0.05,
#'        all=FALSE,
#'        cluster_similarity=0.7)
#'
#' @param Object \code{\link{ClustAllObject-class}} object
#' @param population Numeric vector giving the minimum amount of population that a cluster must have to be considered representative
#' @param all Logical vector to return all the representative clusters per group of clusters. If it is FALSE, only the centroid cluster of every group of clusters is returned
#' @param cluster_similarity the minimum values to consider two groups similar in Jaccard distances. As default 0.7.

#' @return list
#'
#' @seealso \code{\link{plotJACCARD}},\code{\link{cluster2data}},\code{\link{ClustAllObject-class}}
#'
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc <- wdbc[,-c(1,2)]
#' obj_noNA <- createClustAll(data = wdbc)
#' obj_noNA1 <- runClustAll(Object = obj_noNA, threads = 8)
#' resStratification(Object = obj_noNA1, population = 0.05, cluster_similarity = 0.88, all = F)
#'
#' @export
setGeneric(
  name="resStratification",
  def=function(Object, population=0.05, all=FALSE, cluster_similarity=0.7){standardGeneric("resStratification")}
)

setMethod(
  f="resStratification",
  signature=signature(
    Object="ClustAllObject",
    population="numericOrNA",
    all="logicalOrNA",
    cluster_similarity="numericOrNA"),
  definition=function(Object, population=0.05, all=FALSE, cluster_similarity=0.7) {

    if (isProcessed(Object) == FALSE) {
      message("The object is not processed. You need to run runClustAll. Note that the number of cores to use can be specified.")
      stop()
    }

    maxgo <- max(Object@JACCARD_DISTANCE_F[Object@JACCARD_DISTANCE_F < 1])
    ordergo <- hclust(1 - as.dist(Object@JACCARD_DISTANCE_F))$order

    # obtain the definitive clusters
    definitive_clusters <- obtainDefCluster(Object@JACCARD_DISTANCE_F[ordergo, ordergo], cluster_similarity)

    # chose the representative cluster with minimum population in each cluster. As default 0.05 (5%)
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
  }
)


#' @title cluster2data
#' @aliases cluster2data,ClustAllObject-method
#' @description
#' Returns the data frame of the original data using which the clustering of the selected cluster(s) are included as varibles. The representative cluster names can be obtained using the method \code{\link{resStratification}}
#' @usage cluster2data(Object,
#'        clusterName)
#'
#' @param Object \code{\link{ClustAllObject-class}} object
#' @param clusterName Character vector with one or more cluster names
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
#' resStratification(Object = obj_noNA1, population = 0.05, cluster_similarity = 0.88, all = F)
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
#' @param clusters Character vector with the names of two clusters. Check resStratification to obtain cluster names.
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
#' resStratification(Object = obj_noNA1, population = 0.05, cluster_similarity = 0.88, all = F)
#' plotSANKEY(Object = obj_noNA1, clusters = c("cuts_c_3","cuts_a_9"))
#' plotSANKEY(Object = obj_noNA1, clusters = c("cuts_c_3","cuts_b_13"))
#'
#' @export
setGeneric(
  name="plotSANKEY",
  def=function(Object, clusters){standardGeneric("plotSANKEY")}
)

setMethod(
  f="plotSANKEY",
  signature=signature(
    Object="ClustAllObject",
    clusters="character"),
  definition=function(Object, clusters) {
    if (isProcessed(Object) == FALSE) {
      message("The object is not processed. You need to run runClustAll. Note that the number of cores to use can be specified.")
      stop()
    }

    if (length(clusters) != 2) {
      message("You need to include the name of two clusters.")
      stop()
    }

    checkCluster(clusters, Object@summary_clusters)

    df <- cluster2data(Object, clusters)
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

    # With networkD3, connection must be provided using id
    links$IDsource <- match(links$source, nodes$name)-1
    links$IDtarget <- match(links$target, nodes$name)-1

    # Make the Network
    plot <- sankeyNetwork(Links = links, Nodes = nodes,
                          Source = "IDsource", Target = "IDtarget",
                          Value = "value", NodeID = "name",
                          sinksRight=FALSE)

    return(plot)
  }
)
# END OF ClustAll_ClustAllObject_Methods.R
=======
# ClustAllObject_Methods--------------------------------------------------------
#' @import corrplot
#' @import dplyr
#' @title Plots correlation Jaccard Distances from ClustAllObject
#' @aliases plotJACCARD,ClustAllObject-method
#' @description
#' This function plots the correlation Jaccard Distances
#' @usage plotJACCARD(Object,
#'        paint=TRUE,
#'        cluster_similarity=0.7)
#'
#' @param Object \code{\link{ClustAllObject-class}} object
#' @param paint Logical vector to paint the different group of clusters in the plot
#' @param cluster_similarity the minimum values to consider two groups similar in Jaccard distances. As default 0.7.
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
#' plotJACCARD(obj_noNA1, paint = TRUE, cluster_similarity = 0.88)
#'
#' @export
setGeneric(
  name="plotJACCARD",
  def=function(Object, paint=TRUE, cluster_similarity=0.7){standardGeneric("plotJACCARD")}
)

setMethod(
  f="plotJACCARD",
  signature=signature(
    Object="ClustAllObject",
    paint="logicalOrNA",
    cluster_similarity="numericOrNA"),
  definition=function(Object, paint="TRUE", cluster_similarity=0.7) {

    if (isProcessed(Object) == FALSE) {
      message("The object is not processed. You need to run runClustAll. Note that the number of cores to use can be specified.")
      stop()
    }

    maxgo <- max(Object@JACCARD_DISTANCE_F[Object@JACCARD_DISTANCE_F < 1])
    ordergo <- hclust(1 - as.dist(Object@JACCARD_DISTANCE_F))$order

    # This plot contains the robust stratifications
    resPlot <- corrplot(Object@JACCARD_DISTANCE_F[ordergo, ordergo], is.corr=F,
                        method="shade", main="", order="original", tl.col="black",
                        tl.srt=65, cl.pos="b", cl.length=4, sig.level = 0.7)

    if (paint == TRUE) {
      res <- resStratification(Object, population = 1e-9, all = T, cluster_similarity)

      paint_names <- c()
      for (i in 1:length(res)) {
          paint_names <- c(paint_names, res[[i]][[1]][1])
          paint_names <- c(paint_names, res[[i]][[length(res[[i]])]][1])
      }

      for (z in seq(from=1, to=length(paint_names), by=2)) {
        resPlot <- resPlot %>% corrRect(name = c(paint_names[z], paint_names[z+1]), lwd = 4, col="black")
      }
    }

    return(resPlot)
  }
)


#' @import clusteval
#' @title Show stratification representatives from ClustAllObject
#' @aliases resStratification,ClustAllObject-method
#' @description
#' This function returns the stratifications representatives by filtering those clusters with a minimum percentage of the population. Either returns all the robust cluster or the representative one of each group of cluster
#' @usage resStratification(Object,
#'        population=0.05,
#'        all=FALSE,
#'        cluster_similarity=0.7)
#'
#' @param Object \code{\link{ClustAllObject-class}} object
#' @param population Numeric vector giving the minimum amount of population that a cluster must have to be considered representative
#' @param all Logical vector to return all the representative clusters per group of clusters. If it is FALSE, only the centroid cluster of every group of clusters is returned
#' @param cluster_similarity the minimum values to consider two groups similar in Jaccard distances. As default 0.7.

#' @return list
#'
#' @seealso \code{\link{plotJACCARD}},\code{\link{cluster2data}},\code{\link{ClustAllObject-class}}
#'
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc <- wdbc[,-c(1,2)]
#' obj_noNA <- createClustAll(data = wdbc)
#' obj_noNA1 <- runClustAll(Object = obj_noNA, threads = 8)
#' resStratification(Object = obj_noNA1, population = 0.05, cluster_similarity = 0.88, all = F)
#'
#' @export
setGeneric(
  name="resStratification",
  def=function(Object, population=0.05, all=FALSE, cluster_similarity=0.7){standardGeneric("resStratification")}
)

setMethod(
  f="resStratification",
  signature=signature(
    Object="ClustAllObject",
    population="numericOrNA",
    all="logicalOrNA",
    cluster_similarity="numericOrNA"),
  definition=function(Object, population=0.05, all=FALSE, cluster_similarity=0.7) {

    if (isProcessed(Object) == FALSE) {
      message("The object is not processed. You need to run runClustAll. Note that the number of cores to use can be specified.")
      stop()
    }

    maxgo <- max(Object@JACCARD_DISTANCE_F[Object@JACCARD_DISTANCE_F < 1])
    ordergo <- hclust(1 - as.dist(Object@JACCARD_DISTANCE_F))$order

    # obtain the definitive clusters
    definitive_clusters <- obtainDefCluster(Object@JACCARD_DISTANCE_F[ordergo, ordergo], cluster_similarity)

    # chose the representative cluster with minimum population in each cluster. As default 0.05 (5%)
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
  }
)


#' @title cluster2data
#' @aliases cluster2data,ClustAllObject-method
#' @description
#' Returns the data frame of the original data using which the clustering of the selected cluster(s) are included as varibles. The representative cluster names can be obtained using the method \code{\link{resStratification}}
#' @usage cluster2data(Object,
#'        clusterName)
#'
#' @param Object \code{\link{ClustAllObject-class}} object
#' @param clusterName Character vector with one or more cluster names
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
#' resStratification(Object = obj_noNA1, population = 0.05, cluster_similarity = 0.88, all = F)
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
#' @param clusters Character vector with the names of two clusters. Check resStratification to obtain cluster names.
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
#' resStratification(Object = obj_noNA1, population = 0.05, cluster_similarity = 0.88, all = F)
#' plotSANKEY(Object = obj_noNA1, clusters = c("cuts_c_3","cuts_a_9"))
#' plotSANKEY(Object = obj_noNA1, clusters = c("cuts_c_3","cuts_b_13"))
#'
#' @export
setGeneric(
  name="plotSANKEY",
  def=function(Object, clusters){standardGeneric("plotSANKEY")}
)

setMethod(
  f="plotSANKEY",
  signature=signature(
    Object="ClustAllObject",
    clusters="character"),
  definition=function(Object, clusters) {
    if (isProcessed(Object) == FALSE) {
      message("The object is not processed. You need to run runClustAll. Note that the number of cores to use can be specified.")
      stop()
    }

    if (length(clusters) != 2) {
      message("You need to include the name of two clusters.")
      stop()
    }

    checkCluster(clusters, Object@summary_clusters)

    df <- cluster2data(Object, clusters)
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

    # With networkD3, connection must be provided using id
    links$IDsource <- match(links$source, nodes$name)-1
    links$IDtarget <- match(links$target, nodes$name)-1

    # Make the Network
    plot <- sankeyNetwork(Links = links, Nodes = nodes,
                          Source = "IDsource", Target = "IDtarget",
                          Value = "value", NodeID = "name",
                          sinksRight=FALSE)

    return(plot)
  }
)
# END OF ClustAll_ClustAllObject_Methods.R
>>>>>>> b1f783ee4d6e330596ce40ae5bfebb6c027e9530
=======
# ClustAllObject_Methods--------------------------------------------------------
#' @import corrplot
#' @import dplyr
#' @title Plots correlation Jaccard Distances from ClustAllObject
#' @aliases plotJACCARD,ClustAllObject-method
#' @description
#' This function plots the correlation Jaccard Distances
#' @usage plotJACCARD(Object,
#'        paint=TRUE,
#'        cluster_similarity=0.7)
#'
#' @param Object \code{\link{ClustAllObject-class}} object
#' @param paint Logical vector to paint the different group of clusters in the plot
#' @param cluster_similarity the minimum values to consider two groups similar in Jaccard distances. As default 0.7.
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
#' plotJACCARD(obj_noNA1, paint = TRUE, cluster_similarity = 0.88)
#'
#' @export
setGeneric(
  name="plotJACCARD",
  def=function(Object, paint=TRUE, cluster_similarity=0.7){standardGeneric("plotJACCARD")}
)

setMethod(
  f="plotJACCARD",
  signature=signature(
    Object="ClustAllObject",
    paint="logicalOrNA",
    cluster_similarity="numericOrNA"),
  definition=function(Object, paint="TRUE", cluster_similarity=0.7) {

    if (isProcessed(Object) == FALSE) {
      message("The object is not processed. You need to run runClustAll. Note that the number of cores to use can be specified.")
      stop()
    }

    maxgo <- max(Object@JACCARD_DISTANCE_F[Object@JACCARD_DISTANCE_F < 1])
    ordergo <- hclust(1 - as.dist(Object@JACCARD_DISTANCE_F))$order

    # This plot contains the robust stratifications
    resPlot <- corrplot(Object@JACCARD_DISTANCE_F[ordergo, ordergo], is.corr=F,
                        method="shade", main="", order="original", tl.col="black",
                        tl.srt=65, cl.pos="b", cl.length=4, sig.level = 0.7)

    if (paint == TRUE) {
      res <- resStratification(Object, population = 1e-9, all = T, cluster_similarity)

      paint_names <- c()
      for (i in 1:length(res)) {
          paint_names <- c(paint_names, res[[i]][[1]][1])
          paint_names <- c(paint_names, res[[i]][[length(res[[i]])]][1])
      }

      for (z in seq(from=1, to=length(paint_names), by=2)) {
        resPlot <- resPlot %>% corrRect(name = c(paint_names[z], paint_names[z+1]), lwd = 4, col="black")
      }
    }

    return(resPlot)
  }
)


#' @import clusteval
#' @title Show stratification representatives from ClustAllObject
#' @aliases resStratification,ClustAllObject-method
#' @description
#' This function returns the stratifications representatives by filtering those clusters with a minimum percentage of the population. Either returns all the robust cluster or the representative one of each group of cluster
#' @usage resStratification(Object,
#'        population=0.05,
#'        all=FALSE,
#'        cluster_similarity=0.7)
#'
#' @param Object \code{\link{ClustAllObject-class}} object
#' @param population Numeric vector giving the minimum amount of population that a cluster must have to be considered representative
#' @param all Logical vector to return all the representative clusters per group of clusters. If it is FALSE, only the centroid cluster of every group of clusters is returned
#' @param cluster_similarity the minimum values to consider two groups similar in Jaccard distances. As default 0.7.

#' @return list
#'
#' @seealso \code{\link{plotJACCARD}},\code{\link{cluster2data}},\code{\link{ClustAllObject-class}}
#'
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc <- wdbc[,-c(1,2)]
#' obj_noNA <- createClustAll(data = wdbc)
#' obj_noNA1 <- runClustAll(Object = obj_noNA, threads = 8)
#' resStratification(Object = obj_noNA1, population = 0.05, cluster_similarity = 0.88, all = F)
#'
#' @export
setGeneric(
  name="resStratification",
  def=function(Object, population=0.05, all=FALSE, cluster_similarity=0.7){standardGeneric("resStratification")}
)

setMethod(
  f="resStratification",
  signature=signature(
    Object="ClustAllObject",
    population="numericOrNA",
    all="logicalOrNA",
    cluster_similarity="numericOrNA"),
  definition=function(Object, population=0.05, all=FALSE, cluster_similarity=0.7) {

    if (isProcessed(Object) == FALSE) {
      message("The object is not processed. You need to run runClustAll. Note that the number of cores to use can be specified.")
      stop()
    }

    maxgo <- max(Object@JACCARD_DISTANCE_F[Object@JACCARD_DISTANCE_F < 1])
    ordergo <- hclust(1 - as.dist(Object@JACCARD_DISTANCE_F))$order

    # obtain the definitive clusters
    definitive_clusters <- obtainDefCluster(Object@JACCARD_DISTANCE_F[ordergo, ordergo], cluster_similarity)

    # chose the representative cluster with minimum population in each cluster. As default 0.05 (5%)
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
  }
)


#' @title cluster2data
#' @aliases cluster2data,ClustAllObject-method
#' @description
#' Returns the data frame of the original data using which the clustering of the selected cluster(s) are included as varibles. The representative cluster names can be obtained using the method \code{\link{resStratification}}
#' @usage cluster2data(Object,
#'        clusterName)
#'
#' @param Object \code{\link{ClustAllObject-class}} object
#' @param clusterName Character vector with one or more cluster names
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
#' resStratification(Object = obj_noNA1, population = 0.05, cluster_similarity = 0.88, all = F)
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
#' @param clusters Character vector with the names of two clusters. Check resStratification to obtain cluster names.
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
#' resStratification(Object = obj_noNA1, population = 0.05, cluster_similarity = 0.88, all = F)
#' plotSANKEY(Object = obj_noNA1, clusters = c("cuts_c_3","cuts_a_9"))
#' plotSANKEY(Object = obj_noNA1, clusters = c("cuts_c_3","cuts_b_13"))
#'
#' @export
setGeneric(
  name="plotSANKEY",
  def=function(Object, clusters){standardGeneric("plotSANKEY")}
)

setMethod(
  f="plotSANKEY",
  signature=signature(
    Object="ClustAllObject",
    clusters="character"),
  definition=function(Object, clusters) {
    if (isProcessed(Object) == FALSE) {
      message("The object is not processed. You need to run runClustAll. Note that the number of cores to use can be specified.")
      stop()
    }

    if (length(clusters) != 2) {
      message("You need to include the name of two clusters.")
      stop()
    }

    checkCluster(clusters, Object@summary_clusters)

    df <- cluster2data(Object, clusters)
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

    # With networkD3, connection must be provided using id
    links$IDsource <- match(links$source, nodes$name)-1
    links$IDtarget <- match(links$target, nodes$name)-1

    # Make the Network
    plot <- sankeyNetwork(Links = links, Nodes = nodes,
                          Source = "IDsource", Target = "IDtarget",
                          Value = "value", NodeID = "name",
                          sinksRight=FALSE)

    return(plot)
  }
)
# END OF ClustAll_ClustAllObject_Methods.R
>>>>>>> b1f783ee4d6e330596ce40ae5bfebb6c027e9530
