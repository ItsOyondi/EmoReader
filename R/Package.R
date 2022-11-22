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

read_data <- function(){
  data = read.csv("amazon.csv")
  
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


tidy_data = read_data()

#Statistical analysis of  the data
output <- summary(tidy_data)
view(output)

# seperating the files by their rating
Five_star<-tidy_data %>%filter(star_rating==5)
Four_star<-tidy_data %>% filter(star_rating==4)
Three_star<-tidy_data %>% filter(star_rating==3)
Two_star<-tidy_data %>% filter(star_rating==2)
One_star<-tidy_data %>% filter(star_rating==1)

#Total number of instances having positive and negative reviews
count_no <- tidy_data %>%
  count(star_rating)

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

#Dimension Reduction of emotions - PCA
data(emotions, package = "MASS")
pca_out <- prcomp (emotions, scale = T)
pca_out

emotions_pc <- pca_out$x
emotions_pc

#Running the summary of the PCA dimension reduction - shows cumulative variance explained by the PCA
summary(pca_out)

#Plotting the summary statistics of PCA
plot(pca_out) # x-axis represents the number of PCA

#Biplotting to see how the features are related
par(mar=c(4,4,2,2))
biplot(pca_out, cex = 0.5, cex.axis = 0.5) #each number is the row in the dataset and the points in the red are the columns


