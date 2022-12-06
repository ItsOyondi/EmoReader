#import libraries
#' @imports dplyr
#' @imports stringr
#' @imports tidytext
#' @imports ggplot2
#' @imports tidyr
#' @imports syuzhet
#' @imports tidyverse
#' @imports cluster    # clustering algorithms
#' @imports factoextra # clustering algorithms & visualization
#' @imports MASS #dimension reduction
#' @imports singlet
#' @imports magrittr
#' @imports tibble
#' @import  Matrix


# library(dplyr)
# library(stringr)
# library(ggplot2)
# library(tidyr)
# library(syuzhet)
# library(tidyverse)
# library(cluster)    # clustering algorithms
# library(factoextra) # clustering algorithms & visualization
# library(MASS) #dimension reduction
# library(singlet)
# library(dendextend)
# library(Matrix)



read_inbuilt_data <- function(data){

  data$review <- gsub("shouldn't","should not",data$review)
  data$review <- gsub("didn't","did not",data$review)
  data$review <- gsub("don't","do not",data$review)
  data$review <- gsub("can't","can not",data$review)
  data$review <- gsub("couldn't","could not",data$review)
  data$review <- gsub("'ll"," will",data$review)
  data$review <- gsub("'ve"," have",data$review)
  data$review <- gsub("i'm"," I am",data$review)

  return (data)
}

read_input_csv_data <- function(csv_file){

  data = data.table::fread(csv_file)

  data$review_body <- gsub("shouldn't","should not",data$review_body)
  data$review_body <- gsub("didn't","did not",data$review_body)
  data$review_body <- gsub("don't","do not",data$review_body)
  data$review_body <- gsub("can't","can not",data$review_body)
  data$review_body <- gsub("couldn't","could not",data$review_body)
  data$review_body <- gsub("'ll"," will",data$review_body)
  data$review_body <- gsub("'ve"," have",data$review)
  data$review_body <- gsub("i'm"," I am",data$review)

  df <- data.frame(star_rating = data$star_rating, review = data$review_body) #convert to a dataframe

  return (df)
}

amazon_data <- read_input_csv_data('small.csv')


get_emotion <- function(csv_file){
  library(syuzhet)
  csv_file$review <- tolower(csv_file$review)
  emotions <- get_nrc_sentiment(csv_file$review)
  return (emotions)
}

emo_mat <- get_emotion(amazon_data)


count_emotions <-function(emotion_file){
  #Getting the total frequency of the ten emotions
  emosbar <- colSums(emotion_file)
  emosum <- data.frame(count = emosbar, emotion = names(emosbar))
  return(emosum)
}


matrix_conversion <- function(data_file){
  #building a matrix of emotion dataframe
  my_mat <- as.matrix(data_file)
  return(my_mat)
}


convert_to_sparse_matrix <- function(){
  my_mat = matrix_conversion(data_file = emo_mat)
  #Building the Sparse matrix from the emotions matrix
  sparsematrix <- as(my_mat, "sparseMatrix")
  return(sparsematrix)
}


#NMF - Dimension Reduction
nmf_func <- function(nmfdim){
  sparsematrix = convert_to_sparse_matrix()
  data_nmf <- run_nmf(sparsematrix, nmfdim) #nmfdim = rank
  hist(sparsematrix@x)
  plot(density(sparsematrix@x))

  return(plot)
}


#Normalizing the data
norm_sparse_matrix <- function(){
  norm_data <- convert_to_sparse_matrix()
  norm_data <- Seurat::LogNormalize(norm_data)
  return(norm_data)
}


modeling_nmf <- function(rank){
  norm_data <- convert_to_sparse_matrix()
  nmf_model <- run_nmf(norm_data, rank=rank)
  return (nmf_model)
}


#create a hteamap for the nmf model
heatmap_visualize <- function(){
  nmf_model = modeling_nmf(3)
  sparsematrix = convert_to_sparse_matrix()
  h <- nmf_model$h
  colnames(nmf_model$h) <- colnames(sparsematrix)
  return(heatmap(h))
}


#PCA - Dimension Reduction
pca_func <- function (){
  #data(emotions, package = "MASS")
  pca_out <- prcomp (emo_mat, scale = T)
  return (pca_out)
}



#Biplotting to see how the features are related
visualizing_pca <- function (){
  pca_out = pca_func()
  par(mar=c(4,4,2,2))
  biplot(pca_out, cex = 0.5, cex.axis = 0.5) #each number is the row in the dataset and the points in the red are the columns
  return(biplot)
}


set.seed(1234) # Setting seed

cluster_kmeans <- function(data_matrix){

  ##reference https://www.geeksforgeeks.org/k-means-clustering-in-r-programming/
  dataframe_data=as.data.frame(data_matrix)
  #clean df
  c_df = na.omit(dataframe_data)
  ################################ k-means clustering approach 1
  #fit the k-means clustering model
  kms <- kmeans(c_df, centers = 5, nstart = 20)
  if (kms$ifault==4) { kms = kmeans(c_df, kms$centers, algorithm="MacQueen") }
  #kms


  #identifying the clusters
  kms$cluster

  # Confusion Matrix
  cm <- table(c_df$positive, kms$cluster)

  #evaluate the model and visualize
  plot(c_df[c("negative", "positive")], col = kms$cluster,
       main = "K-means with 5 clusters")


  #plot the cluster centers
  kms$centers
  kms$centers[, c("positive", "negative")]


  #visualize the clusters
  y_kmeans <- kms$cluster
  clusplot(c_df[, c("positive", "negative")],
           y_kmeans,
           lines = 0,
           shade = TRUE,
           color = TRUE,
           labels = 4,
           plotchar = FALSE,
           span = TRUE,
           main = paste("Cluster Amazon reviews"),
           xlab = 'positive',
           ylab = 'negative')
}



# Hierarchical Clustering
# https://www.datacamp.com/tutorial/hierarchical-clustering-R
h_cluster<- function(emo){
  #normailizaing the data
  emo_data_sc <- as.data.frame(scale(emo))

  #transposing the data
  dist_mat_emo <- dist(t(emo_data_sc))


  #Hierarchical Clustering
  hclust_avg_emo <- hclust(dist_mat_emo, method = 'average')


  #Cutting the tree at 3
  cut_clust_norm <- cutree(hclust_avg_emo, k = 3)


  plot(hclust_avg_emo)
  #seperating the cluster by color
  rect.hclust(hclust_avg_emo , k = 3, border = 2:6)
  abline(h = 280, col = 'red')# cutting line for the cluster.


}




