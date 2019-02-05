#' @title Average Correlations of Clusters
#'
#' @description
#' This function allows the calculation of correlations for clusters. This function should be used with clustering function.
#'
#' @param data A raw data set needs to be specified.
#' @param clustering_data Output of the clustering function needs to be specified.
#' @param variables The column name of "variables" of clustering function output needs to be specified.
#' @param clusters The column name of "clusters" of clustering function output needs to be specified.
#' @keywords creditR
#' @export
#' @examples
#' correlation.cluster(my_data,clustering_output, "variable_names","Groups")

correlation.cluster <- function(data, clustering_data, variables, clusters){
  number_cluster <- which( colnames(clustering_data)==clusters)
  number_variables <- which( colnames(clustering_data)==variables)
  cluster_v <- matrix(data=NA,nrow=1,ncol=length(unique(clustering_data[,clusters])))
  cor_v <- matrix(data=NA,nrow=1,ncol=length(unique(clustering_data[,clusters])))
  for(i in 1:length(unique(clustering_data[,clusters]))){
    cor_data <-   data[clustering_data[clustering_data[,number_cluster] ==i,][,number_variables]]
    korelasyon_degerleri <- cor(cor_data)
    lower <- korelasyon_degerleri
    lower[lower.tri(korelasyon_degerleri, diag = TRUE)] = 99
    lower <- as.data.frame(lower)
    lower1 <- as.list(lower)
    lowerlist <- mean(lower[lower!=99])
    cluster_v[,i] <- i
    cor_v[,i] <- lowerlist
  }
  cor_summary<-as.data.frame(cbind(t(cluster_v), t(cor_v)))
  colnames(cor_summary) <- c("Clusters","Correlation")
  return(cor_summary)

}

