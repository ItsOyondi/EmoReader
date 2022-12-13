# EmoReader

## Introduction

```EmoReader``` is a R package that generates the sentiments from the text document. It generates 10 different emotion from the text document and all the emotions are scored accordingly which is then coverted to a dgc matrix for the dimension reduction and clustering.


## Installation of the package

We need to first install the devtools package. You can install the ```devtools ``` using the below code:

```
install.packages("devtools")
```

You would need to install the singlet package that will be used to run the nmf_func in the EmoReader package. You can install the  ```singlet``` using the below code:

```
library(devtools)
devtools::install_github("zdebruine/singlet")
library(singlet)
```

You can install the ```EmoReader``` package from the GitHub: 

```
library(devtools)
devtools::install_github("dikshashrestha/EmoReader")
library (EmoReader)
```
## Link to the pkgdown site

The link to the pkgdown site:
https://dikshashrestha.github.io/EmoReader/

## Link to the package vignette

The link to the package vignette is below:
https://dikshashrestha.github.io/EmoReader/articles/amazon_vig.html


## Example Code Block

### Loading the in-built dataset 
```
df <- read_inbuilt_data(amazon_data)
str(df)
# 'data.frame':	500 obs. of  2 variables:
#  $ star_rating: int  5 5 5 4 2 3 3 5 4 5 ...
#  $ review     : chr  "As advertised. Everything works perfectly, I'm very happy with the camera. As a matter of fact I'm going to buy"| __truncated__ # #  "it's great" "These work great and fit my life proof case for the iPhone 6" "Work very well but could not get used to not hearing anything out of the #  ear they v were plugged into." ...
```


```
my_mat <-matrix_conversion(emo_mat)
head(my_mat)

#      anger anticipation disgust fear joy sadness surprise trust negative positive
# [1,]     0            1       0    0   1       0        0     2        0        1
# [2,]     0            0       0    0   0       0        0     0        0        0
# [3,]     0            0       0    1   0       1        0     1        1        0
# [4,]     0            0       0    1   0       0        0     0        1        0
# [5,]     1            1       0    0   1       0        0     1        1        3
```


```
sparse_mat <- convert_to_sparse_matrix()
head(sparse_mat)

# 6 x 10 sparse Matrix of class "dgCMatrix"
#   [[ suppressing 10 column names ‘anger’, ‘anticipation’, ‘disgust’ ... ]]
                        
# [1,] . 1 . . 1 . . 2 . 1
# [2,] . . . . . . . . . .
# [3,] . . . 1 . 1 . 1 1 .
# [4,] . . . 1 . . . . 1 .
# [5,] 1 1 . . 1 . . 1 1 3
# [6,] . 1 . . 1 . 1 1 . 3
```

### Loading the csv file and passing it to a dataframe 

The below function reads the csv file and it is then passed to a dataframe afte renaming the columns. 
```
library(readr)
data <- read.csv('Flipkart_Customer_Review.csv')
data <- data.frame(star_rating = data$rating, review = data$review)
```

Our read_inbuilt_data function then pre-process the dataframe which is passed to the data.

```
df_flip <- read_inbuilt_data(data)
str(df_flip)

# 'data.frame':	1000 obs. of  2 variables:
#  $ star_rating: int  5 5 4 5 5 5 4 4 5 5 ...
#  $ review     : chr  "It was nice produt. I like it's design a lot.  It's easy to carry. And.   Looked stylish.READ MORE" "awesome sound....very pretty #  to see this nd the sound quality was too good I wish to take this product loved ...
```

```
emo_mat <- get_emotion(df_flip)
head(emo_mat)

#      anger anticipation disgust fear joy sadness surprise trust negative positive
# 1      0            0       0    0   0       0        0     0        0        0
# 2      0            2       0    0   2       0        1     2        0        2
# 3      1            3       0    1   1       0        1     3        1        4
# 4      0            2       0    1   3       1        1     4        2        5
# 5      0            2       0    0   1       0        1     1        1        2
# 6      2            4       1    1   5       1        0     4        1        6
```
