# EmoReader

## Introduction

EmoReader is a R package that generates the sentiments from the text document. It generates 10 different emotion from the text document and all the emotions are scored accordingly which is then coverted to a dgc matrix for the dimension reduction and clustering.


## Installation of the package

Install the package from the GitHub:


```
library(devtools)
devtools::install_github("dikshashrestha/EmoReader")
library (EmoReader)
```
## Link to the pkgdown site

The link to the pkgdown site:
https://itsoyondi.github.io/EmoReader/index.html

## Link to the package vignette

The link to the package vignette is below:


## Example Code Block

```
df <- read_inbuilt_data(amazon_data)
str(df)
# 'data.frame':	500 obs. of  2 variables:
#  $ star_rating: int  5 5 5 4 2 3 3 5 4 5 ...
#  $ review     : chr  "As advertised. Everything works perfectly, I'm very happy with the camera. As a matter of fact I'm going to buy"| __truncated__ # #  "it's great" "These work great and fit my life proof case for the iPhone 6" "Work very well but could not get used to not hearing anything out of the #  ear they v were plugged into." ...
```



```
emo_mat <- get_emotion(df)
head(emo_mat)
#  anger anticipation disgust fear joy sadness surprise trust negative
#1	 0	      1       	0	      0 	1 	0       	0     	2     	0	
#2	 0	      0	        0	      0 	0	  0	        0	      0	      0	
#3	 0	      0	        0	      1	  0	  1	        0	      1	      1	
#4	 0	      0	        0	      1	  0	  0	        0	      0     	1	
#5	 1	      1	        0	      0	  1	  0	        0	      1	      1

```


```
count_emotions(emo_mat)
 
#              count   emotion
# anger	        164	    anger		
# anticipation	389	    anticipation		
# disgust	      90	    disgust		
# fear	        152	    fear		
# joy	          348	    joy		
# sadness	      194	    sadness		
# surprise	    216   	surprise		
# trust	        401	    trust		
# negative	    350	    negative		
# positive	    715	    positive
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
