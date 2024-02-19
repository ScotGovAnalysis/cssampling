#' @title Add SHeS clusters to data frame
#'
#' @description This function merges the sampling frame with the SHeS clusters
#' information and creates a new variable 'shes_clustersize'.
#'
#' @param df Name of the data frame which is the sample frame.
#' 
#' @param clusters Name of the data frame which includes the clusters for 
#' the SHeS.
#'
#' @returns Data frame which includes the cleaned sampling frame and the SHeS
#' clusters. The data frame needs to include a column which contains 
#' 'shes_y' + shes.surveysweep in its name.
#'
#' @examples
#' add_clusters(df = shes.sframe, clusters = shes_clusters)

add_clusters <- function(df, clusters){
  df %>%
    
    # remove all columns that contain "shes_y" 
    select(-contains("shes_y")) %>%
    
    # add SHeS cluster information
    left_join(clusters) %>%
    
    # create 'shes_clustersive' column based on shes.surveysweep
    mutate(across(
      .cols = contains(paste0("shes_y", 
                              shes.surveysweep), ignore.case = FALSE),
      .names = 'shes_clustersize'))
}
