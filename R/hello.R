# Hello, world!
#
# This is an example function named 'hello' 
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(tidyr)

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
  tidy_data %>%
    pivot_wider(names_from = word, values_from = n)
}

data=data_preprocess()


write.csv(data,"amazonlong.csv", row.names = TRUE)

