#import libraries
#' @import syuzhet
#' @import cluster
#' @import singlet
#' @import Matrix
#' @import dplyr




#' @title Read inbuilt data set
#' @name read_inbuilt_data
#' @param data csv file as a parameter
#' @return A data frame
#' @description The function read_inbuilt_data is used for reading our data set.
#' Before beginning data analysis, it may be best to investigate and clean the data.
#' Due to improper writing, some words might not be recognized by sentiment analysis. As a result,
#' in this function, once the data is loaded the function also pre-process the data before converting it to the dataframe.
#' @export
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




#' @title get emotions
#' @name get_emotion
#' @param csv_file csv file as a parameter
#' @return A data frame
#' @description This function works by dividing emotions in an column and they are giving us a total of 10
#' different emotions, for example anger, eagerness, contempt, fear, joy, sadness, surprise, trust, negativity, and positivity,
#' by using get nrc function the above 10 emotions will divide based on the positive and negative sentiments
#' @references https://medium.com/swlh/exploring-sentiment-analysis-a6b53b026131
#' @export
get_emotion <- function(csv_file){
  csv_file$review <- tolower(csv_file$review)
  emotions <- get_nrc_sentiment(csv_file$review)
  return (emotions)
}


#' @title counting emotions
#' @name count_emotions
#' @param emotion_file emo_mat file as a parameter to count how many words will fall under each emotion
#' @return A data frame
#' @description This  function is used to count the total number of words are present
#' in each emotion after the words have been divided by 10 emotions
#' @export

count_emotions <-function(emotion_file){
  #Getting the total frequency of the ten emotions
  emosbar <- colSums(emotion_file)
  emosum <- data.frame(count = emosbar, emotion = names(emosbar))
  return(emosum)
}

#' @title matrix conversion
#' @name matrix_conversion
#' @param data_file csv file as a parameter
#' @return matrix
#' @description Converting each of the 10 emotions data frames into a matrix data frame since
#' dimension reduction will become easy to perform after the conversion of matrix data frame
#' @references https://statisticsglobe.com/convert-data-frame-to-matrix-in-r
#' @export
matrix_conversion <- function(data_file){
  #building a matrix of emotion dataframe
  my_mat <- as.matrix(data_file)
  return(my_mat)
}

#' @title converting to sparse matrix
#' @name convert_to_sparse_matrix
#' @return sparsematrix
#' @description It converts emotion matrix into sparse matrix, sparse matrices can be
#' useful for computing large-scale applications that dense matrices cannot handle.
#' @export
convert_to_sparse_matrix <- function(){
  my_mat = matrix_conversion(data_file = emo_mat)
  #Building the Sparse matrix from the emotions matrix
  sparsematrix <- as(my_mat, "sparseMatrix")
  return(sparsematrix)
}

#' @title NMF dimension reduction
#' @name nmf_func
#' @param nmfdim sparsematrix as a parameter
#' @return nmf data
#' @description Nonnegative matrix factorization (NMF) is an dimension reduction method.
#' It has become a widely used tool for the analysis of high dimensional data as
#' it automatically extracts sparse and meaningful features from a set of nonnegative data vectors.
#' @export

#NMF - Dimension Reduction
nmf_func <- function(nmfdim){
  sparsematrix = convert_to_sparse_matrix()
  data_nmf <- run_nmf(sparsematrix, nmfdim) #nmfdim = rank
  hist(sparsematrix@x)
  plot(density(sparsematrix@x))
  return(plot)
}

#' @title Normalizing the sparsematrix
#' @name norm_sparse_matrix
#' @return normalized data
#' @description normalization for sparse matrix which is needed for log convert the data to make it as "normal" as feasible,
#' improving the validity of the statistical analysis results. In other words, the skewness of our
#' initial data is reduced or eliminated via the log transformation.
#' @references https://satijalab.org/seurat/articles/dim_reduction_vignette.html
#' @export
#Normalizing the data
norm_sparse_matrix <- function(){
  norm_data <- convert_to_sparse_matrix()
  norm_data <- Seurat::LogNormalize(norm_data)
  return(norm_data)
}


#' @title modeling nmf
#' @name modeling_nmf
#' @param rank sparsematrix as a parameter
#' @return modeling data
#' @description modeling nmf to discover hidden semantic patterns in unstructured collection of documents.
#' Modeling by using Rank where the value or range of ranks for which NMF is performed.
#' @export

modeling_nmf <- function(rank){
  norm_data <- convert_to_sparse_matrix()
  nmf_model <- run_nmf(norm_data, rank=rank)
  return (nmf_model)
}
#' @title visualizing nmf
#' @name heatmap_visualize
#' @return graph
#' @description A heatmap uses a warm-to-cool color scheme to graphically represent visitor behavior data as hot and cold regions.
#' Red is the location with the maximum visitor contact that’s means which emotion is strong (repeated more), and warm colors point to those areas,
#' while cool colors denote those areas with the least visitor interaction which means which emotion weak (repeated less)
#' @export

#create a hteamap for the nmf model
heatmap_visualize <- function(){
  nmf_model = modeling_nmf(3)
  sparsematrix = convert_to_sparse_matrix()
  h <- nmf_model$h
  colnames(nmf_model$h) <- colnames(sparsematrix)
  return(heatmap(h))
}
#' @title PCA dimension reduction
#' @name pca_func
#' @return pca data
#' @description PCA dimension reduction method which is used to reduce the number of variables in your data by extracting important ones from a large pool.
#' It reduces the dimension of the data with the aim of retaining as much information as possible.
#' @references https://rpubs.com/JanpuHou/278584
#' @export

#PCA - Dimension Reduction
pca_func <- function (){
  #data(emotions, package = "MASS")
  pca_out <- prcomp (emo_mat, scale = T)
  return (pca_out)
}

#' @title visualizing pca
#' @name visualizing_pca
#' @return graph
#' @description biplot visualizes the data into two-dimensional chart that represents the relationship between the rows and columns of a table.
#' optimally represent any two of the following characteristics: distances between observations. relationships between variables.
#' @export


#Biplotting to see how the features are related
visualizing_pca <- function (){
  pca_out = pca_func()
  par(mar=c(4,4,2,2))
  biplot(pca_out, cex = 0.5, cex.axis = 0.5) #each number is the row in the dataset and the points in the red are the columns
  return(biplot)
}


set.seed(1234) # Setting seed
#' @title Clustering by using Kmeans
#' @name cluster_kmeans
#' @param data_matrix emo_mat file as a parameter
#' @return graph
#' @description K-means clustering is an Unsupervised Non-linear algorithm that cluster data based on similarity or similar groups. It chooses the number K clusters
#' It Select at random K points, and forms centroids and it will Assign each data point to closest centroid that forms K clusters.
#' from the 10 emotions the k means clustering performs cluster based on positive and negative emotions.
#' @references https://www.geeksforgeeks.org/k-means-clustering-in-r-programming/
#' @export

cluster_kmeans <- function(data_matrix){

  ##reference https://www.geeksforgeeks.org/k-means-clustering-in-r-programming/
  dataframe_data=as.data.frame(data_matrix)
  #clean df
  c_df = stats::na.omit(dataframe_data)
  ################################ k-means clustering approach 1
  #fit the k-means clustering model
  kms <- stats::kmeans(c_df, centers = 5, nstart = 20)
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


#' @title Hirarchial clustering
#' @name h_cluster
#' @param emo emo_mat file as a parameter
#' @return graph
#' @description Hierarchical clustering is mostly used for combining nearest clusters into one large cluster here in our package we are combing nearest emotions into one
#' large cluster to find the nearest clusters h cluster firstly calculate the distance between every pair of observation points and store it in a distance matrix. By plotting our data,
#' we can see that two emotions like “anger” and “negative” are the nearest emotions they were combined and form a large cluster, after that, it updates its distance calculation
#' and stores the results in a fresh distance matrix. The process then repeats stages 2 and 3 until all clusters have been combined into a single cluster.
#' @references https://www.datacamp.com/tutorial/hierarchical-clustering-R
#' @export
# Hierarchical Clustering
# https://www.datacamp.com/tutorial/hierarchical-clustering-R
h_cluster<- function(emo){
  #normailizaing the data
  emo_data_sc <- as.data.frame(scale(emo))

  #transposing the data
  dist_mat_emo <- stats::dist(t(emo_data_sc))


  #Hierarchical Clustering
  hclust_avg_emo <- stats::hclust(dist_mat_emo, method = 'average')


  #Cutting the tree at 3
  cut_clust_norm <- stats::cutree(hclust_avg_emo, k = 3)


  plot(hclust_avg_emo)
  #seperating the cluster by color
  rect.hclust(hclust_avg_emo , k = 3, border = 2:6)
  abline(h = 280, col = 'red')# cutting line for the cluster.


}


