

#import libraries
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(tidyr)
library(syuzhet)

read_data <- function(){
  data = read.csv("amazon.csv")

  data <- data.frame(star_rating = data$star_rating, review = data$review_body) #convert to a dataframe
  return (data)
}

data_preprocess <- function(){
  #data preprocessing
  data(stop_words)

  data = read_data()

  tidy_data <- data %>%
    unnest_tokens(word, review) %>%
    anti_join(stop_words) %>%  # remove stop words

    group_by(star_rating) %>%  #group the term frequencies by star ratings
    count(word, sort = TRUE) #get the word count for each term


  tidy_data %>%
    filter(n > 100) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    xlab(NULL) +
    coord_flip()

  df <- tidy_data %>%
    pivot_wider(names_from = word, values_from = n)

  df <- subset(df, select = -c(br)) #remove uncessary column - br
  df #return the data frame
}

data = data_preprocess()



write.csv(data,"amazonlong.csv", row.names = TRUE)

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

