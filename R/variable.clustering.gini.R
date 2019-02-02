#' @title Variable Clustering Gini
#'
#' @description
#' This function allows you to perform variable clustering and calculate the gini values for a given data set. Number of clusters can be determined optimally or manually.The clusGap function is used for optimal clustering.
#'
#' @param data A data set needs to be defined.
#' @param default_flag Default flag must be specified as a string.
#' @param cluster_number (Default = "optimal"), If the optimal option is selected, clustering is performed over the optimal number of clustering determined by function.
#' @keywords creditR
#' @import cluster
#' @import MLmetrics
#' @export
#' @examples
#' variable.clustering.gini(credit_data, "default_flag", cluster_number = "optimal")


variable.clustering.gini <- function(data, default_flag, cluster_number = "optimal"){
  Gini.univariate.data <- function(data, default_flag){
    number <- which( colnames(data)==default_flag)
    gini_column_names <- matrix(data=NA,nrow=1,ncol=length(data))
    gini_values <- matrix(data=NA,nrow=1,ncol=length(data))
    gini<-matrix(data=NA,nrow=1,ncol=length(data))

    for(i in 1:length(data)){
      univ_model <- glm(formula = data[,number] ~ data[,i], family = binomial(link = "logit"), data = data)
      gini_values[,i]<- Gini(y_pred = univ_model$fitted.values, y_true = as.numeric(as.character(data[,default_flag])))
      gini_column_names[,i]<-colnames(data[i])
    }

    gini<-cbind(t(gini_column_names), t(gini_values))
    gini_df <- as.data.frame(gini)
    colnames(gini_df) <- c("Variable","Gini")
    gini_df$Gini <- as.numeric(as.character(gini_df$Gini))
    ordered_gini_df<-gini_df[order(-gini_df$Gini),]
    ordered_gini_df <- ordered_gini_df[ordered_gini_df[,1] != default_flag,]
    rownames(ordered_gini_df) <- c(1:length(ordered_gini_df[,1]))
    return(ordered_gini_df)


  }
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
    groups_s$Variable <- colnames(data1)
    return(groups_s)
  }
  gini_data <-Gini.univariate.data(data, default_flag)
  cluster_data<- variable.clustering(data,default_flag, cluster_number)
  merged_data <- merge(cluster_data, gini_data, by = "Variable")
  merged_data <- merged_data[order(merged_data$Group),]
  max_gini <- aggregate(merged_data$Gini, by = list(merged_data[,2]), max)
  colnames(max_gini) <- c("Group", "Gini")
  max_gini <- merge(max_gini, merged_data, by = "Gini")
  final_list <- as.vector(max_gini$Variable)
  final_shortlist <- data[,c(final_list,default_flag)]
  return(final_shortlist)
}


