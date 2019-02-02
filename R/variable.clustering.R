#' @title Variable Clustering
#'
#' @description
#' This function allows you to perform variable clustering for a given data set. Number of clusters can be determined optimally or manually.The clusGap function is used for optimal clustering.
#'
#' @param data A data set needs to be defined.
#' @param default_flag Default flag must be specified as a string.
#' @param cluster_number (Default = "optimal"), If the optimal option is selected, clustering is performed over the optimal number of clustering determined by function.
#' @keywords creditR
#' @import cluster
#' @export
#' @examples
#' variable.clustering(credit_data, "default_flag", cluster_number = "optimal")



variable.clustering <- function(data, default_flag, cluster_number = "optimal"){
  optimal.cluster <- function(data,default_flag){
    number <- which( colnames(data)==default_flag )
    data1 <- data[,-number]
    gs.pam.RU <- clusGap(t(data1), FUN = pam, K.max = ncol(data1)-1, B = 100)
    cluster_number <- with(gs.pam.RU,maxSE(Tab[,"gap"],Tab[,"SE.sim"]))
    kmm = kmeans(t(data1),centers = cluster_number,iter.max = 1000, nstart = 1000)
    variable_clusters <- as.data.frame(kmm$cluster)
    variable_clusters$Variable <- rownames(variable_clusters)
    colnames(variable_clusters) <- c("Group", "Variable")
    return(variable_clusters)
  }
  given.cluster <- function(data, default_flag, cluster_number){
    number <- which( colnames(data)==default_flag )
    data1 <- data[,-number]
    clus_num <- cluster_number
    kmm = kmeans(t(data1),centers = cluster_number,iter.max = 1000, nstart = 1000)
    variable_clusters <- as.data.frame(kmm$cluster)
    variable_clusters$Variable <- rownames(variable_clusters)
    colnames(variable_clusters) <- c("Group", "Variable")
    return(variable_clusters)
  }
  number <- which( colnames(data)==default_flag )
  data1 <- data[,-number]
  groups_s <- as.data.frame(ifelse(cluster_number == "optimal", optimal.cluster(data,default_flag),given.cluster(data,default_flag,cluster_number)))
  colnames(groups_s) <- c("Group")
  groups_s$variable <- colnames(data1)
  return(groups_s)
  }


