# ClustAllObject_Methods_internal functions definition -------------------------
# These functions are not meant to be invoked directly by the user.
# See the ClustAllObject_Methods functions instead.

# This function returns a vector with the definitive clusters based on distances matrix
obtainDefCluster <- function(mat_using, cluster_similarity=0.7){

  candidates_clustering <- c()
  definitive_clusters <- list()
  j <- 1

  for (i in 1:(ncol(mat_using)-1)) {
    if (i < j-1 | names(mat_using[i,])[j] %in% candidates_clustering) {  ## Avoid repeating same cluster
      next
    }

    if (i == ncol(mat_using)-1) {
      if (mat_using[i,i+1] >= cluster_similarity) {
        candidates_clustering <- c(names(mat_using[i,])[i], names(mat_using[i,])[i+1])
        definitive_clusters <- append(definitive_clusters, list(candidates_clustering))
      }
    } else {

      if (mat_using[i,i+1] >= cluster_similarity) {
        candidates_clustering <- c(names(mat_using[i,])[i])

        for (j in (i+1):(ncol(mat_using))) {

          if (j==ncol(mat_using)) {
            candidates_clustering <- c(candidates_clustering, names(mat_using[i,])[j])
            definitive_clusters <- append(definitive_clusters, list(candidates_clustering))
            break
          }

          if(mat_using[i,j] >= cluster_similarity) {
            candidates_clustering <- c(unique(candidates_clustering), names(mat_using[i,])[j])
            if (j == (ncol(mat_using)-2)) {
              append(definitive_clusters, list(candidates_clustering))
            }

          } else if (mat_using[i+1,j] >= cluster_similarity){ # The lower this value is, the larger group of clusters is obtained
            candidates_clustering <- c(unique(candidates_clustering), names(mat_using[i,])[j])
            if (j == (ncol(mat_using)-2)) {
              append(definitive_clusters, list(candidates_clustering))
            }

          } else {
            definitive_clusters <- append(definitive_clusters, list(candidates_clustering))
            break
          }
        }
      }
    }
  }

  return(definitive_clusters)
}


# This function filter the definitive cluster based on population and returns those clusters
chooseClusters <- function(definitive_clusters, summary_clusters, population=0.05, all=T) {

  if (0 > population | population >= 1) {
    message("Not a valid value for population. Should introduce a value from 0 to 1.")
    message("Working with at least 5% of the population.")
    population <- 0.05
  }

  possible_stratifications <- list()
  chosen_clusters <- list()

  for (i in 1:(length(names(summary_clusters)))) {
    possible_stratifications[i] <- list(as.numeric(table(summary_clusters[[i]])))
  }

  names(possible_stratifications) <- names(summary_clusters)

  for (i in 1:length(definitive_clusters)) {
    for (j in definitive_clusters[[i]]) {
      if (min(unlist(possible_stratifications[j])) < (length(summary_clusters[[1]]))*population) { # at least %X of the population in a cluster
        definitive_clusters[[i]] <- definitive_clusters[[i]][definitive_clusters[[i]] != j]
      }
    }

    if (length(definitive_clusters[[i]]) == 0) {
      next

    } else if ((length(definitive_clusters[[i]]) %% 2) == 0) {
      chosen_clusters <- c(chosen_clusters, definitive_clusters[[i]][length(definitive_clusters[[i]])/2])

    } else {
      chosen_clusters <- c(chosen_clusters, definitive_clusters[[i]][ceiling(length(definitive_clusters[[i]])/2)])
    }
  }

  if (all == TRUE) {
    return(definitive_clusters)

  } else {
    return(chosen_clusters)
  }
}


# This function checks if the introduced cluster exists in the list of clusters
checkCluster <- function(cluster, lCluster) {
  stopProcess <- FALSE
  booleans <- cluster %in% names(lCluster)

  for (i in 1:length(booleans)) {
    if (booleans[i] == FALSE) {
      message(paste("The cluster "), cluster[i], " does not exist. You may want to check the clusters using resStratification.")
      stopProcess <- TRUE
    }
  }

  if (stopProcess){
    stop()
  }
}
# END OF ClustAll_ClustAllObject_Methods_internal.R
=======
# ClustAllObject_Methods_internal functions definition -------------------------
# These functions are not meant to be invoked directly by the user.
# See the ClustAllObject_Methods functions instead.

# This function returns a vector with the definitive clusters based on distances matrix
obtainDefCluster <- function(mat_using, cluster_similarity=0.7){

  candidates_clustering <- c()
  definitive_clusters <- list()
  j <- 1

  for (i in 1:(ncol(mat_using)-1)) {
    if (i < j-1 | names(mat_using[i,])[j] %in% candidates_clustering) {  ## Avoid repeating same cluster
      next
    }

    if (i == ncol(mat_using)-1) {
      if (mat_using[i,i+1] >= cluster_similarity) {
        candidates_clustering <- c(names(mat_using[i,])[i], names(mat_using[i,])[i+1])
        definitive_clusters <- append(definitive_clusters, list(candidates_clustering))
      }
    } else {

      if (mat_using[i,i+1] >= cluster_similarity) {
        candidates_clustering <- c(names(mat_using[i,])[i])

        for (j in (i+1):(ncol(mat_using))) {

          if (j==ncol(mat_using)) {
            candidates_clustering <- c(candidates_clustering, names(mat_using[i,])[j])
            definitive_clusters <- append(definitive_clusters, list(candidates_clustering))
            break
          }

          if(mat_using[i,j] >= cluster_similarity) {
            candidates_clustering <- c(unique(candidates_clustering), names(mat_using[i,])[j])
            if (j == (ncol(mat_using)-2)) {
              append(definitive_clusters, list(candidates_clustering))
            }

          } else if (mat_using[i+1,j] >= cluster_similarity){ # The lower this value is, the larger group of clusters is obtained
            candidates_clustering <- c(unique(candidates_clustering), names(mat_using[i,])[j])
            if (j == (ncol(mat_using)-2)) {
              append(definitive_clusters, list(candidates_clustering))
            }

          } else {
            definitive_clusters <- append(definitive_clusters, list(candidates_clustering))
            break
          }
        }
      }
    }
  }

  return(definitive_clusters)
}


# This function filter the definitive cluster based on population and returns those clusters
chooseClusters <- function(definitive_clusters, summary_clusters, population=0.05, all=T) {

  if (0 > population | population >= 1) {
    message("Not a valid value for population. Should introduce a value from 0 to 1.")
    message("Working with at least 5% of the population.")
    population <- 0.05
  }

  possible_stratifications <- list()
  chosen_clusters <- list()

  for (i in 1:(length(names(summary_clusters)))) {
    possible_stratifications[i] <- list(as.numeric(table(summary_clusters[[i]])))
  }

  names(possible_stratifications) <- names(summary_clusters)

  for (i in 1:length(definitive_clusters)) {
    for (j in definitive_clusters[[i]]) {
      if (min(unlist(possible_stratifications[j])) < (length(summary_clusters[[1]]))*population) { # at least %X of the population in a cluster
        definitive_clusters[[i]] <- definitive_clusters[[i]][definitive_clusters[[i]] != j]
      }
    }

    if (length(definitive_clusters[[i]]) == 0) {
      next

    } else if ((length(definitive_clusters[[i]]) %% 2) == 0) {
      chosen_clusters <- c(chosen_clusters, definitive_clusters[[i]][length(definitive_clusters[[i]])/2])

    } else {
      chosen_clusters <- c(chosen_clusters, definitive_clusters[[i]][ceiling(length(definitive_clusters[[i]])/2)])
    }
  }

  if (all == TRUE) {
    return(definitive_clusters)

  } else {
    return(chosen_clusters)
  }
}


# This function checks if the introduced cluster exists in the list of clusters
checkCluster <- function(cluster, lCluster) {
  stopProcess <- FALSE
  booleans <- cluster %in% names(lCluster)

  for (i in 1:length(booleans)) {
    if (booleans[i] == FALSE) {
      message(paste("The cluster "), cluster[i], " does not exist. You may want to check the clusters using resStratification.")
      stopProcess <- TRUE
    }
  }

  if (stopProcess){
    stop()
  }
}
# END OF ClustAll_ClustAllObject_Methods_internal.R
>>>>>>> b1f783ee4d6e330596ce40ae5bfebb6c027e9530
=======
# ClustAllObject_Methods_internal functions definition -------------------------
# These functions are not meant to be invoked directly by the user.
# See the ClustAllObject_Methods functions instead.

# This function returns a vector with the definitive clusters based on distances matrix
obtainDefCluster <- function(mat_using, cluster_similarity=0.7){

  candidates_clustering <- c()
  definitive_clusters <- list()
  j <- 1

  for (i in 1:(ncol(mat_using)-1)) {
    if (i < j-1 | names(mat_using[i,])[j] %in% candidates_clustering) {  ## Avoid repeating same cluster
      next
    }

    if (i == ncol(mat_using)-1) {
      if (mat_using[i,i+1] >= cluster_similarity) {
        candidates_clustering <- c(names(mat_using[i,])[i], names(mat_using[i,])[i+1])
        definitive_clusters <- append(definitive_clusters, list(candidates_clustering))
      }
    } else {

      if (mat_using[i,i+1] >= cluster_similarity) {
        candidates_clustering <- c(names(mat_using[i,])[i])

        for (j in (i+1):(ncol(mat_using))) {

          if (j==ncol(mat_using)) {
            candidates_clustering <- c(candidates_clustering, names(mat_using[i,])[j])
            definitive_clusters <- append(definitive_clusters, list(candidates_clustering))
            break
          }

          if(mat_using[i,j] >= cluster_similarity) {
            candidates_clustering <- c(unique(candidates_clustering), names(mat_using[i,])[j])
            if (j == (ncol(mat_using)-2)) {
              append(definitive_clusters, list(candidates_clustering))
            }

          } else if (mat_using[i+1,j] >= cluster_similarity){ # The lower this value is, the larger group of clusters is obtained
            candidates_clustering <- c(unique(candidates_clustering), names(mat_using[i,])[j])
            if (j == (ncol(mat_using)-2)) {
              append(definitive_clusters, list(candidates_clustering))
            }

          } else {
            definitive_clusters <- append(definitive_clusters, list(candidates_clustering))
            break
          }
        }
      }
    }
  }

  return(definitive_clusters)
}


# This function filter the definitive cluster based on population and returns those clusters
chooseClusters <- function(definitive_clusters, summary_clusters, population=0.05, all=T) {

  if (0 > population | population >= 1) {
    message("Not a valid value for population. Should introduce a value from 0 to 1.")
    message("Working with at least 5% of the population.")
    population <- 0.05
  }

  possible_stratifications <- list()
  chosen_clusters <- list()

  for (i in 1:(length(names(summary_clusters)))) {
    possible_stratifications[i] <- list(as.numeric(table(summary_clusters[[i]])))
  }

  names(possible_stratifications) <- names(summary_clusters)

  for (i in 1:length(definitive_clusters)) {
    for (j in definitive_clusters[[i]]) {
      if (min(unlist(possible_stratifications[j])) < (length(summary_clusters[[1]]))*population) { # at least %X of the population in a cluster
        definitive_clusters[[i]] <- definitive_clusters[[i]][definitive_clusters[[i]] != j]
      }
    }

    if (length(definitive_clusters[[i]]) == 0) {
      next

    } else if ((length(definitive_clusters[[i]]) %% 2) == 0) {
      chosen_clusters <- c(chosen_clusters, definitive_clusters[[i]][length(definitive_clusters[[i]])/2])

    } else {
      chosen_clusters <- c(chosen_clusters, definitive_clusters[[i]][ceiling(length(definitive_clusters[[i]])/2)])
    }
  }

  if (all == TRUE) {
    return(definitive_clusters)

  } else {
    return(chosen_clusters)
  }
}


# This function checks if the introduced cluster exists in the list of clusters
checkCluster <- function(cluster, lCluster) {
  stopProcess <- FALSE
  booleans <- cluster %in% names(lCluster)

  for (i in 1:length(booleans)) {
    if (booleans[i] == FALSE) {
      message(paste("The cluster "), cluster[i], " does not exist. You may want to check the clusters using resStratification.")
      stopProcess <- TRUE
    }
  }

  if (stopProcess){
    stop()
  }
}
# END OF ClustAll_ClustAllObject_Methods_internal.R
>>>>>>> b1f783ee4d6e330596ce40ae5bfebb6c027e9530
