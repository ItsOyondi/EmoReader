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


read_data <- function(csv_file){

  data = data.table::fread(csv_file)

  data <- tibble::rowid_to_column(data, "id")

  data$review_body <- gsub("shouldn't","should not",data$review_body)
  data$review_body <- gsub("didn't","did not",data$review_body)
  data$review_body <- gsub("don't","do not",data$review_body)
  data$review_body <- gsub("can't","can not",data$review_body)
  data$review_body <- gsub("couldn't","could not",data$review_body)
  data$review_body <- gsub("'ll"," will",data$review_body)
  data$review_body <- gsub("'ve"," have",data$review)
  data$review_body <- gsub("i'm"," I am",data$review)

  df <- data.frame(id = data$id, star_rating = data$star_rating, review = data$review_body) #convert to a dataframe
  return (df)
}
# reading the data
#tidy_data <- read_data("amazon.csv")

stats_analysis <- function(data){
  #Statistical analysis of  the data
  output <- summary(data)
  return(output)
}



#stats <- function(data){
#Total number of instances having positive and negative reviews
#count_no <- data %>%
# count(star_rating)
#return(count_no)
#}

#stats(tidy_data)


#sentiments(tidy_data)

count_emotions <-function(emotion_file){
  #read the amazon emotion csv file
  emo <- data.table::fread(emotion_file)
  #adding id to the dataset
  #emotions <- tibble::rowid_to_column(emotions, "id")
  #tidy_data_emotion= tidy_data %>% inner_join(emotions,by="id")

  #Loading the emotion csv and assigning it into a dataframe
  emotions <- data.frame(anger = emo$anger, anticipation = emo$anticipation, disgust = emo$disgust, fear = emo$fear, joy = emo$joy, sadness = emo$sadness, surprise = emo$surprise, trust = emo$trust, negative = emo$negative, positive = emo$positive )
  emosbar <- colSums(emotions)
  emosum <- data.frame(count = emosbar, emotion = names(emosbar))
  return(emosum)
}


matrix_conversion <- function(data_file){
  emo <- data.table::fread(data_file)
  emotions <- data.frame(anger = emo$anger, anticipation = emo$anticipation, disgust = emo$disgust, fear = emo$fear, joy = emo$joy, sadness = emo$sadness, surprise = emo$surprise, trust = emo$trust, negative = emo$negative, positive = emo$positive )
  #building a matrix of emotion dataframe
  my_mat <- as.matrix(emotions)

  #Saving the matrix as csv
  write.csv(emotions, file ="my_mat.csv",row.names = TRUE)

  return(my_mat)
}



sparse_matrix <- function(){
  my_mat = matrix_conversion(data_file = "amazonemotion.csv")
  #Building the Sparse matrix from the emotions matrix
  sparsematrix <- as(my_mat, "sparseMatrix")
  return(sparsematrix)
}


#nmf
#NMF - Dimension Reduction
nmf_func <- function(nmfdim){
  sparsematrix = sparse_matrix()
  data_nmf <- run_nmf(sparsematrix, nmfdim) #nmfdim = rank
  hist(sparsematrix@x)
  plot(density(sparsematrix@x))

  return(plot)
}


#Normalizing the data

norm_fun <- function(){
  norm_data <- sparse_matrix()
  norm_data <- Seurat::LogNormalize(norm_data)
  return(norm_data)
}


modeling_nmf <- function(rank){
  norm_data <- sparse_matrix()
  nmf_model <- run_nmf(norm_data, rank=rank)
  str(nmf_model)
  return (nmf_model)
}

#create a hteamap for the nmf model

heatmap_visualize <- function(){
  nmf_model = modeling_nmf(3)
  sparsematrix = sparse_matrix()
  h <- nmf_model$h
  colnames(nmf_model$h) <- colnames(sparsematrix)
  return(heatmap(h))
}


#PCA - Dimension Reduction

pca_func <- function (){

  emo <- data.table::fread("amazonemotion.csv")
  emotions <- data.frame(anger = emo$anger, anticipation = emo$anticipation, disgust = emo$disgust, fear = emo$fear, joy = emo$joy, sadness = emo$sadness, surprise = emo$surprise, trust = emo$trust, negative = emo$negative, positive = emo$positive )

  #data(emotions, package = "MASS")
  pca_out <- prcomp (emotions, scale = T)
  return (pca_out)
}


#emofunc<- function(){
#  emotions_pc <- pca_out$x
 # emotions_pc
 # return(emotions_pc)
#}

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
  kmeans.re <- kmeans(c_df, centers = 5, nstart = 20, iter.max = 100)
  if (kmeans.re$ifault==4) { kmeans.re = kmeans(c_df, kmeans.re$centers, algorithm="MacQueen") }
  kmeans.re


  #identifying the clusters
  kmeans.re$cluster

  # Confusion Matrix
  cm <- table(c_df$positive, kmeans.re$cluster)

  #evaluate the model and visualize
  plot(c_df[c("negative", "positive")], col = kmeans.re$cluster,
       main = "K-means with 5 clusters")


  #plot the cluster centers
  kmeans.re$centers
  kmeans.re$centers[, c("positive", "negative")]


  #visualize the clusters
  y_kmeans <- kmeans.re$cluster
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




