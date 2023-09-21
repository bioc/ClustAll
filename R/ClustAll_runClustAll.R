# runClustAll ------------------------------------------------------------------
#' @import clValid
#' @import mice
#' @import bigstatsr
#' @import FactoMineR
#' @import modeest
#' @import dplyr
#' @import fpc
#' @import clValid
#' @import doSNOW
#' @import foreach
#' @import clusteval
#' @import flock
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom methods is new validObject
#' @importFrom stats as.dist cor cutree hclust median quantile sd
#' @importFrom utils txtProgressBar capture.output
#' @title ClustAll: pipeline a data driven strategy in order to find hidden subgroups.
#' @aliases runClustAll,ClustAllObject-method,numericOrNA
#' @description This method runs the ClustALL algorithm
#'
#'
#' @usage runClustAll(Object,
#'        threads=1)
#'
#' @param Object \code{\link{ClustAllObject-class}} object
#' @param threads Numeric vector giving the number of cores to use
#'
#' @return An object of class \code{\link{ClustAllObject-class}}
#'
#' @seealso \code{\link{resStratification}},\code{\link{plotJACCARD}},\code{\link{cluster2data}},\code{\link{ClustAllObject-class}}
#'
#' @examples
#' data("BreastCancerWisconsin", package = "ClustAll")
#' wdbc <- wdbc[,-c(1,2)]
#' obj_noNA <- createClustAll(data = wdbc)
#' obj_noNA1 <- runClustAll(Object = obj_noNA, threads = 8)
#'
#' @export

setGeneric(
  name="runClustAll",
  def=function(Object, threads=1) {
    standardGeneric("runClustAll")
    }
)


setMethod(
  f="runClustAll",
  signature=signature(
    Object="ClustAllObject",
    threads="numericOrNA"),
  definition=function(Object, threads=1) {

    if (Object@processed == TRUE) {
      message("ClustAll pipeline have been already processed. Stopping the process...")
      message("You may want to use resStratification to see the stratification results.")
      stop()
    }

    packages.parallel <- c("clValid", "mice", "bigstatsr", "FactoMineR", "modeest", "dplyr","fpc")
    functions.parallel <- c("obtainImputationData", "obtainDataPCA", "cstats.table_PAM",
                            "cstats.table_hclust", "generatePCA_derived")
    nvariables <- ncol(Object@data)

    if (Object@nImputation == 0) {
      nRow <- 1
    } else {
      nRow <- Object@nImputation
    }

    # create empty arrays
    summary_clusters_a <- as_FBM(matrix(0, ncol(Object@data), nRow))
    summary_clusters_b <- as_FBM(matrix(0, ncol(Object@data), nRow))
    summary_clusters_c <- as_FBM(matrix(0, ncol(Object@data), nRow))
    summary_clusters_d <- as_FBM(matrix(0, ncol(Object@data), nRow))

    summary_matrices_a <- createEmptyArrays(ncol(Object@data), Object@data)
    summary_matrices_b <- createEmptyArrays(ncol(Object@data), Object@data)
    summary_matrices_c <- createEmptyArrays(ncol(Object@data), Object@data)
    summary_matrices_d <- createEmptyArrays(ncol(Object@data), Object@data)

    # set parallelization parameters
    lock <- tempfile() # for synchronization between R processes

    if (Object@nImputation == 0) {
      nimp <- 1
    } else {
      nimp <- Object@nImputation
    }

    if (threads > detectCores()) {
      message("A greater number of cores than detected have been included as argument.")
      message(paste("The maximum number of cores will be used: "), detectCores(), ".")
      threads <- detectCores()
    }

    printLogo()

    message("Running Data Complexity Reduction and Stratification Process. This step may take some time...\n")

    if (threads == 1) {
      message("Only one core is being used. Consider selecting more than one core for process parallelization (threads parameter).\n")
    }

    cl <- makeCluster(threads)
    registerDoSNOW(cl)

    pb <- txtProgressBar(max = nimp, style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)

    ## Step 1. Data Complexity Reduction
    foreach(impgo=1:nimp, .packages=packages.parallel, .export=functions.parallel, .options.snow=opts) %dopar% { # for every imputation

      if (Object@nImputation == 0) {
        impData <- Object@data
        impData <- apply(impData, 2, as.numeric)
      } else {
        impData <- obtainImputationData(Object@dataImputed, impgo)
      }

      ### Step 1.1. Dendogram
      cor_est_var <- cor(impData, method="spearman") # obtain correlations between variables


      variables_clust <- hclust(as.dist(cor_est_var)) # hierarchical clustering
      possible_heights <- variables_clust$height


      ### Step 1.2. Reprocessing
      for (heights_cut in (length(possible_heights)-(ncol(Object@data)-2)):(length(possible_heights)-1)) { # compute for all possible heights

        data_PCA <- obtainDataPCA(impData, variables_clust, possible_heights, heights_cut)
        ## END OF Step 1. Data Complexity Reduction


        ## Step 2. Statrification Process
        ### Step 2.1. Conducting statrifications
        data_PCA_scaled <- apply(data_PCA, 2, scale) # scale PCA values for correlation distances: (a) K-MEANS and (b) H-CLUST clustering
        rownames(data_PCA_scaled) <- 1:nrow(data_PCA_scaled) # rename otherwise clValid function does not detect rownames of the df


        #### Step 2.1.1. a) K-Means method
        data_PCA.clValid_internal_kmeans <- clValid(data_PCA_scaled, 2:6, clMethods=c("kmeans"),
                                                    validation="internal",maxitems=nrow(data_PCA_scaled),metric="correlation")
        oS_kmeans <- as.numeric(mlv(optimalScores(data_PCA.clValid_internal_kmeans)[,3], method = "mfv"))

        if (length(oS_kmeans)>1) {
          oS_kmeans <- median(oS_kmeans) # choose the median
        }

        kmeans_res <- clusters(data_PCA.clValid_internal_kmeans, "kmeans")
        kmeans_res_c <- kmeans_res[as.character(oS_kmeans)][[1]]$cluster

        for (t in 1:as.numeric(oS_kmeans)) { # for every k
          induse <- as.numeric(names(kmeans_res_c[kmeans_res_c==t]))

          locked = flock::lock(lock) # synchronization between processes
          summary_matrices_a[[heights_cut]][induse, induse] <- summary_matrices_a[[heights_cut]][induse, induse] + 1
          flock::unlock(locked)
        }

        summary_clusters_a[heights_cut, impgo] <- oS_kmeans


        #### Step 2.1.2. b) H-Clust method
        data_PCA.clValid_internal_hclust <- clValid(data_PCA_scaled, 2:6, clMethods=c("hierarchical"),
                                                    validation="internal",maxitems=nrow(data_PCA_scaled),metric="correlation")
        oS_hclust <- as.numeric(mlv(optimalScores(data_PCA.clValid_internal_hclust)[,3], method = "mfv"))

        if (length(oS_hclust) > 1) {
          oS_hclust <- median(oS_hclust) # choose the median
        }

        hclust_res <- clusters(data_PCA.clValid_internal_hclust, "hierarchical")
        hclust_res_c <- cutree(hclust_res, k=oS_hclust)

        for (t in 1:oS_hclust) {
          induse <- as.numeric(names(hclust_res_c[hclust_res_c==t]))
          locked = flock::lock(lock) # synchronization between processes
          summary_matrices_b[[heights_cut]][induse, induse] <- summary_matrices_b[[heights_cut]][induse, induse] + 1 # add result
          flock::unlock(locked)
        }
        summary_clusters_b[heights_cut, impgo] <- oS_hclust


        # Gower distances: (c) K-Medoids and (d) H-Clust clustering
        PCA_gower <- as.data.frame(data_PCA)
        colnames(PCA_gower) <- c(1:dim(PCA_gower)[2])
        binary <- which(apply(PCA_gower,2,function(x) { all(x %in% 0:1) })) # those variables with binary values

        if (length(binary)==1) {
          PCA_gower[,binary] <- factor(PCA_gower[,binary])

        } else {
          PCA_gower[,binary] <- data.frame(apply(PCA_gower[,binary], 2, as.logical))
        }

        PCA_gower <- PCA_gower %>% mutate_if(is.character, as.factor)
        gower_dist <- daisy(PCA_gower, metric="gower")


        #### Step 2.1.3. c) K-Medoids method
        # apply the previous formula to get the optimal K for each cut with PAM method and gower distance
        summary_clusters_c[heights_cut,impgo] <- cstats.table_PAM(gower_dist, 6)
        pam_res <- pam(gower_dist, k=summary_clusters_c[heights_cut,impgo])
        pam_res_c <- pam_res$clustering

        # calculate TREE for each cut and each imputation and its corresponding optimal K
        for (t in 1:summary_clusters_c[heights_cut, impgo]) {
          induse <- as.numeric(names(pam_res_c[pam_res_c==t]))

          locked = flock::lock(lock) # synchronization between processes
          summary_matrices_c[[heights_cut]][induse, induse] <- summary_matrices_c[[heights_cut]][induse, induse] + 1
          flock::unlock(locked)
        }


        #### Step 2.1.4. d) H-Clust method
        divisive.clust <- diana(as.matrix(gower_dist), diss = TRUE, keep.diss = TRUE)
        summary_clusters_d[heights_cut,impgo] <- cstats.table_hclust(gower_dist, divisive.clust, 6)
        hclustgow_res_c <- cutree(divisive.clust, k=summary_clusters_d[heights_cut,impgo])
        names(hclustgow_res_c) <- 1:dim(Object@data)[1]

        for (t in 1:summary_clusters_d[heights_cut, impgo])  {
          induse <- as.numeric(names(hclustgow_res_c[hclustgow_res_c==t]))

          locked = flock::lock(lock) # synchronization between processes
          summary_matrices_d[[heights_cut]][induse, induse] <- summary_matrices_d[[heights_cut]][induse, induse] + 1
          flock::unlock(locked)
        }
      }
    }

    stopCluster(cl) # end of parallelization
    ### END OF Step 2.1. Conducting stratifications

    if (Object@nImputation != 0) {
      # put all the clusters together
      summary_n_clust <- obtainSummaryCluster(summary_clusters_a, summary_clusters_b,
                                              summary_clusters_c, summary_clusters_d)
    } else { # remove 0s and put all together
      summary_n_clust <- c(summary_clusters_a[][summary_clusters_a[]!=0], summary_clusters_b[][summary_clusters_b[]!=0],
                           summary_clusters_c[][summary_clusters_c[]!=0], summary_clusters_d[][summary_clusters_d[]!=0])
    }

    # renaming the matrices
    names(summary_matrices_a) <- paste("cuts_a_",1:length(summary_matrices_a), sep="")
    names(summary_matrices_b) <- paste("cuts_b_",1:length(summary_matrices_b), sep="")
    names(summary_matrices_c) <- paste("cuts_c_",1:length(summary_matrices_c), sep="")
    names(summary_matrices_d) <- paste("cuts_d_",1:length(summary_matrices_d), sep="")

    summary_matrices_MEASURES <- c(summary_matrices_a, summary_matrices_b,
                                   summary_matrices_c, summary_matrices_d)

    # remove those matrices with only 0s
    tmp <- lapply(summary_matrices_MEASURES, function(x) as.matrix(x[]))
    if (length(which(lapply(tmp, sum)==0)) > 0) {
      summary_matrices_MEASURES = summary_matrices_MEASURES[-which(lapply(summary_matrices_MEASURES, function(x) sum(x[]))==0)]
    }


    ### Step 2.2. Filtering non-robust stratifications
    summary_clusters <- vector("list", length(summary_matrices_MEASURES))

    for(i in 1:length(summary_clusters)) {
      hclustgo <- hclust(1-as.dist(summary_matrices_MEASURES[[i]][]))
      summary_clusters[[i]] <- cutree(hclustgo, k=summary_n_clust[i])
    }
    names(summary_clusters) <- names(summary_matrices_MEASURES)

    # Jaccard Distance
    JACCARD_DISTANCE <- matrix(NA, length(summary_matrices_MEASURES), length(summary_matrices_MEASURES))
    rownames(JACCARD_DISTANCE) <- names(summary_matrices_MEASURES)
    colnames(JACCARD_DISTANCE) <- rownames(JACCARD_DISTANCE)

    # For two clusterings of the same data set, this function calculates the similarity statistic specified of the clusterings from the comemberships of the observations.
    # Basically, the comembership is defined as the pairs of observations that are clustered together.
    for(i in 1:nrow(JACCARD_DISTANCE)) {
      for(j in 1:nrow(JACCARD_DISTANCE)) {
        JACCARD_DISTANCE[i,j] <- cluster_similarity(summary_clusters[[i]], summary_clusters[[j]],
                                                    similarity="jaccard")
      }
    }

    message("")
    message("\nFiltering non-robust statifications...\n")

    pb <- txtProgressBar(min = 0, max = length(summary_matrices_MEASURES), initial = 0, style = 3)

    # Robustness of the clustering
    summary_matrices_STABILITY <- matrix(NA, length(summary_matrices_MEASURES), 3)
    for(i in 1:length(summary_matrices_MEASURES)){
      invisible(capture.output( # avoid printing function messages
        r1 <- clusterboot(data=as.dist(1000-summary_matrices_MEASURES[[i]][]),
                          B=100, distances=TRUE, bootmethod="boot",
                          bscompare=TRUE, multipleboot=FALSE,
                          jittertuning=0.05, noisetuning=c(0.05,4),
                          subtuning=floor(nrow(data)/2),
                          clustermethod=disthclustCBI,noisemethod=FALSE,count=TRUE,
                          showplots=FALSE,dissolution=0.5,
                          recover=0.75,method="complete",k=2)
      ))

      summary_matrices_STABILITY[i,1:2] <- as.numeric(r1$bootmean[1:2])
      summary_matrices_STABILITY[i,3] <- mean(summary_matrices_STABILITY[i,1:2])

      setTxtProgressBar(pb,i)
    }

    quantileuse <- 0.85 # Stratifications with less than 85% stability are excluded
    qgo <- quantile(summary_matrices_STABILITY[,3], quantileuse)
    JACCARD_DISTANCE_F <- JACCARD_DISTANCE[summary_matrices_STABILITY[, 3] >= qgo,summary_matrices_STABILITY[, 3]>=qgo]
    ### END OF Step 2.2. Filtering non-robust stratifications

    Object@summary_clusters <- summary_clusters
    Object@JACCARD_DISTANCE_F <- JACCARD_DISTANCE_F
    Object@processed <- TRUE

    message("\nClustAll pipeline finished successfully!\n")

    return(Object)
  }
)
# END OF ClustAll_runClustAll.R
