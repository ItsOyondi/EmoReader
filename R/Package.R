#import libraries
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(tidyr)
library(syuzhet)
library(tidyverse)
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(MASS) #dimension reduction

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
tidy_data <- read_data("amazon.csv")

stats_analysis <- function(data){
  #Statistical analysis of  the data
  output <- summary(data)
  return(output)
}
stats_analysis(tidy_data)

stats <- function(data){
  #Total number of instances having positive and negative reviews
  count_no <- data %>%
    count(star_rating)
  return(count_no)
}
  
stats(tidy_data)

#Analyzing the sentiments using the syuzhet package

text <- tibble(review = str_to_lower(tidy_data$review))


# THIS CODE BELOW WILL TAKE LOT OF TIME TO EXECUTE 
# emotions <- get_nrc_sentiment(text$review)

#adding id to the dataset
#emotions <- tibble::rowid_to_column(emotions, "id")
#tidy_data_emotion= tidy_data %>% inner_join(emotions,by="id")

#Loading the emotion csv and assigning it into a dataframe
emo <- read.csv("amazonemotion.csv")
emotions <- data.frame(anger = emo$anger, anticipation = emo$anticipation, disgust = emo$disgust, fear = emo$fear, joy = emo$joy, sadness = emo$sadness, surprise = emo$surprise, trust = emo$trust, negative = emo$negative, positive = emo$positive )
emosbar <- colSums(emotions)
emosum <- data.frame(count = emosbar, emotion = names(emosbar))

#building a matrix of emotion dataframe
my_mat <- as.matrix(emotions)
my_mat

#Saving the matrix as csv
write.csv(emotions, file ="my_mat.csv",row.names = TRUE)

#Building the Sparse matrix from the emotions matrix
sparsematrix <- as(my_mat, "sparseMatrix")
sparsematrix

#For NMF - Dimension Reduction
library(singlet)
data <- ard_nmf(sparsematrix)
hist(sparsematrix@x)
plot(density(sparsematrix@x))

#Normalizing the data
norm_data <- sparsematrix
norm_data <- Seurat::LogNormalize(norm_data)

nmf_model <- ard_nmf(norm_data)
str(nmf_model)
str(sparsematrix)

h <- nmf_model$h
colnames(nmf_model$h) <- colnames(sparsematrix)
heatmap(h)
dim(nmf_model$w)

stars <- emo$star_rating
plot(nmf_model$w[,1], stars)
df <- cbind(stars, nmf_model$w)
df <- as.data.frame(df)
ggplot(df, aes(stars, NMF_1, group = stars)) + geom_boxplot()


#Creating a bar plot showing the counts for each different emotions
ggplot(emosum, aes(x = reorder(emotion, -count), y = count))+
  geom_bar(stat = "identity")

#Sentiment Analysis using the "bing" lexicon
bing_word_counts <- text %>% unnest_tokens(output = word, input = review) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)

#Finding our top words as per the sentiments
top_word <- bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(order_by = n, n= 30) %>%
  ungroup() %>%
  mutate(word = reorder(word,n))
top_word

#Creating visualization of the positive/negative sentiments
top_word%>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiments", x = NULL)+
  coord_flip()

#Visualizing how the positive and negative sentiments are located
bing_word_counts %>%
  ggplot(aes(word, n, color = sentiment))+
  geom_point()


cluster_kmeans <- function(data_matrix){

  dataframe_data=as.data.frame(data_matrix)
  #clean df
  c_df = na.omit(dataframe_data)
  ################################ k-means clustering approach 1
  #fit the k-means clustering model
  set.seed(1234) # Setting seed
  kmeans.re <- kmeans(c_df, centers = 8, nstart = 20)
  kmeans.re


  #identifying the clusters
  kmeans.re$cluster

  # Confusion Matrix
  cm <- table(c_df$positive, kmeans.re$cluster)
  cm

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
           labels = 2,
           plotchar = FALSE,
           span = TRUE,
           main = paste("Cluster Amazon reviews"),
           xlab = 'positive',
           ylab = 'negative')
}

cluster_kmeans(my_mat)




