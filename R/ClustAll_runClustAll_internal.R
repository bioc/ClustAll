# runClustAll_internal functions definition ------------------------------------
# These functions are not meant to be invoked directly by the user.
# See the createClusteAll function instead.


# This function creates an empty matrix to store the clustering summaries 
createEmptyMatrix <- function(nvariables, data_use) {
  summary_matrices <- vector("list", nvariables)
  for(i in 1:(nvariables)) {
    summary_matrices[[i]] <- matrix(0,nrow(data_use),nrow(data_use))
  }
  
  return(summary_matrices)
}


# This function creates a list with FMB objects to store the results
createEmptyArrays <- function(nvariables, data_use) {
  summary_matrices <- vector("list", nvariables)
  for(i in 1:(nvariables)) {
    summary_matrices[[i]] <- as_FBM(matrix(0,nrow(data_use),nrow(data_use)))
  }
  
  return(summary_matrices)
}


# This function generates the PCA
generatePCA_derived <- function(data, variability, maxvar) {
  pca_go <- PCA(data, graph = FALSE, ncp=min(c(maxvar,ncol(data))))
  variab <- pca_go$eig[,3]
  whichgo <- min(which(variab>50))
  varigo <- min(c(whichgo,maxvar))
  result <- pca_go$ind$coord[,c(1:varigo)]
  
  return(result)
}


# This functions fills the missings values , and returns the completed data in a specified format
obtainImputationData <- function(imp, impgo) {
  impData <- complete(imp,impgo)
  impData[is.na(impData)] <- 0
  impData <- data.frame(apply(impData, 2, as.numeric))
  impData <- impData[,apply(impData,2,sd)>0]
  
  return(impData)
} 

# This function reduces the dimensions, grouping the introduced variables and generating PCA data
obtainDataPCA <- function(impData, variables_clust, possible_heights, heights_cut) {
  treego <- cutree(variables_clust, h=possible_heights[heights_cut]) 
  groups_go <- unique(treego)
  variables_use <- names(treego[treego==groups_go[1]]) 
  
  if (length(variables_use)==1) {
    data_PCA <- impData[,variables_use]
    
  }else{
    data_PCA <- generatePCA_derived(data=impData[,variables_use],
                                    variability=50, maxvar=3)
  }
  
  if (length(groups_go)>1) {
    for (h in 2:length(groups_go)) {
      
      variables_use <- names(treego[treego==groups_go[h]])
      if (length(variables_use)==1) {
        data_PCA <- cbind(data_PCA,impData[,variables_use])
        
      } else {
        data_PCA <- cbind(data_PCA, generatePCA_derived(data=impData[,variables_use],
                                                        variability=50,maxvar=3))
      }
    } 
  }
  
  return(data_PCA)
}


# This functions applies the Partitioning (clustering) of the data into k clusters (PAM) and return the results
cstats.table_PAM <- function(dist, k) {
  clust.assess <- c("cluster.number", "wb.ratio", "dunn", "avg.silwidth")
  output.stats <- matrix(ncol = length(clust.assess), nrow = k-1)
  
  for(i in 2:k) {
    pam_fit <- pam(dist, diss = TRUE, k = i)
    pam.clust.num <- (pam_fit$clustering)
    
    output.stats[i-1,] <- unlist(cluster.stats(d = dist, clustering = pam.clust.num)[clust.assess])
  }
  
  colnames(output.stats) <- clust.assess
  rownames(output.stats) <- c(2:k)
  
  #return(output.stats)
  output.stats.df <- as.data.frame(output.stats)
  
  resultado <- median(c(output.stats.df[which.max(output.stats.df$avg.silwidth),]$cluster.number,
                        output.stats.df[which.max(output.stats.df$dunn),]$cluster.number,
                        output.stats.df[which.min(output.stats.df$wb.ratio),]$cluster.number))
  
  return(resultado)
}


# This function returs the results of the H-clust method (d)
cstats.table_hclust <- function(dist, tree,k) {
  clust.assess <- c("cluster.number","wb.ratio","dunn","avg.silwidth")
  output.stats <- matrix(ncol = length(clust.assess), nrow = k-1)
  
  for (i in 2:k) {
    output.stats[i-1,] <- unlist(cluster.stats(d = dist, clustering = cutree(tree, k = i))[clust.assess])
  }
  
  colnames(output.stats) <- clust.assess
  rownames(output.stats) <- c(2:k)
  
  output.stats.df <- as.data.frame(output.stats)
  
  result <- median(c(output.stats.df[which.max(output.stats.df$avg.silwidth),]$cluster.number,
                     output.stats.df[which.max(output.stats.df$dunn),]$cluster.number,
                     output.stats.df[which.min(output.stats.df$wb.ratio),]$cluster.number))
  
  return(result)
}


# This function filters the summary clusters with 0s and joins the four of the into a vector
obtainSummaryCluster <-  function(summary_clusters_a, summary_clusters_b,
                                  summary_clusters_c, summary_clusters_d) {
  # remove those rows with only 0s 
  nclust_a <- as.data.frame(summary_clusters_a[which(apply(summary_clusters_a[], 1, sum)>0),])
  nclust_b <- as.data.frame(summary_clusters_b[which(apply(summary_clusters_b[], 1, sum)>0),])
  nclust_c <- as.data.frame(summary_clusters_c[which(apply(summary_clusters_c[], 1, sum)>0),]) 
  nclust_d <- as.data.frame(summary_clusters_d[which(apply(summary_clusters_d[], 1, sum)>0),])
  
  # put all the clusters together
  summary_n_clust <- c(apply(nclust_a, 1, function(x) median(x, na.rm=T)),
                       apply(nclust_b, 1, function(x) median(x, na.rm=T)),
                       apply(nclust_c, 1, function(x) median(x, na.rm=T)),
                       apply(nclust_d, 1, function(x) median(x, na.rm=T)))
  
  return(summary_n_clust)
}


# This function prints the logo
printLogo <- function() {
  message("      ______ __              __   ___     __     __    ")
  message("     / ____// /__  __ _____ / /_ /   |   / /    / /    ")
  message("    / /    / // / / // ___// __// /| |  / /    / /     ")
  message("   / /___ / // /_/ /(__  )/ /_ / ___ | / /___ / /___   ")
  message("  /_____//_/ |__,_//____/ |__//_/  |_|/_____//_____/   ")
}
# END OF ClustAll_runClustAll_internal.R