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

  data <- data.frame(star_rating = data$star_rating, review = data$review_body) #convert to a dataframe
  return (data)
}


tidy_data_clean = read_data()

#Statistical analysis of  the data
output <- summary(tidy_data_clean)
view(output)

# seperating the files by their rating
Five_star<-tidy_data_clean %>%filter(star_rating==5)
Four_star<-tidy_data_clean %>% filter(star_rating==4)
Three_star<-tidy_data_clean %>% filter(star_rating==3)
Two_star<-tidy_data_clean %>% filter(star_rating==2)
One_star<-tidy_data_clean %>% filter(star_rating==1)

#Total number of instances having positive and negative reviews
count_no <- tidy_data_clean %>%
  count(star_rating)

#Analyzing the sentiments using the syuzhet package

text.df <- tibble(review = str_to_lower(data$review))

#Selecting only 1% of the data first to test if we can get the emotions
text <- sample_frac(text.df, 0.01)

emotions <- get_nrc_sentiment(text$review)
emosbar <- colSums(emotions)
emosum <- data.frame(count = emosbar, emotion = names(emosbar))

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


