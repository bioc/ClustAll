# ClustAllObject_Methods--------------------------------------------------------
#' @import corrplot
#' @import dplyr
#' @title Plots correlation Jaccard Distances from ClustAllObject
#' @aliases plotJACCARD,ClustAllObject-method
#' @description 
#' This function plots the correlation Jaccard Distances 
#' @usage plotJACCARD(Object,
#'                    paint=TRUE)
#'
#' @param Object \code{\link{ClustAllObject-class}} object
#' @param paint Logical vector to paint the different group of clusters in the plot
#' 
#' @return plot
#' 
#' @seealso \code{\link{resStratification}},\code{\link{cluster2data}},\code{\link{ClustAllObject-class}}
#' 
#' @examples
#' MISSING EXAMPLES
#' data("data_use")
#' 
#' 
#' 
#' 
#' 
#' 
#' @export
setGeneric(
  name="plotJACCARD",
  def=function(Object, paint=TRUE){standardGeneric("plotJACCARD")}
)

setMethod(
  f="plotJACCARD",
  signature=signature(
    Object="ClustAllObject", 
    paint="logicalOrNA"),
  definition=function(Object, paint="TRUE") {
    
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
      res <- resStratification(Object, population = 1e-9, all = T)
      
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
#'                          population=0.05,
#'                          all=FALSE)
#'
#' @param Object \code{\link{ClustAllObject-class}} object
#' @param population Numeric vector giving the minimum amount of population that a cluster must have to be considered representative
#' @param all Logical vector to return all the representative clusters per group of clusters. If it is FALSE, only the centroid cluster of every group of clusters is returned
#' 
#' @return list
#' 
#' @seealso \code{\link{plotJACCARD}},\code{\link{cluster2data}},\code{\link{ClustAllObject-class}}
#' 
#' @examples
#' MISSING EXAMPLES
#' data("data_use")
#' 
#' 
#' 
#' 
#' @export
setGeneric(
  name="resStratification",
  def=function(Object, population=NA, all=NA){standardGeneric("resStratification")}
)

setMethod(
  f="resStratification",
  signature=signature(
    Object="ClustAllObject",
    population="numericOrNA",
    all="logicalOrNA"),
  definition=function(Object, population=0.05, all=FALSE) {
    
    if (isProcessed(Object) == FALSE) {
      message("The object is not processed. You need to run runClustAll. Note that the number of cores to use can be specified.")
      stop()
    }
    
    maxgo <- max(Object@JACCARD_DISTANCE_F[Object@JACCARD_DISTANCE_F < 1])
    ordergo <- hclust(1 - as.dist(Object@JACCARD_DISTANCE_F))$order
    
    # obtain the definitive clusters
    definitive_clusters <- obtainDefCluster(Object@JACCARD_DISTANCE_F[ordergo, ordergo])
    
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
#'                     clusterName)
#'
#' @param Object \code{\link{ClustAllObject-class}} object
#' @param clusterName Character vector with one or more cluster names
#' 
#' @return data.frame
#' 
#' @seealso \code{\link{resStratification}},\code{\link{plotJACCARD}},\code{\link{ClustAllObject-class}}
#' 
#' @examples
#' MISSING EXAMPLES
#' data("data_use")
#' 
#' 
#' 
#' 
#' 
#' 
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
# END OF ClustAll_ClustAllObject_Methods.R