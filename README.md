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
str(amazon_data)
#> 'data.frame':	500 obs. of  2 variables:
#>  $ star_rating: int  5 5 5 4 2 3 3 5 4 5 ...
#>  $ review     : chr  "As advertised. Everything works perfectly, I'm very happy with the camera. As a matter of fact I'm going to buy"| __truncated__ #> "it's great" "These work great and fit my life proof case for the iPhone 6" "Work very well but could not get used to not hearing anything out of the #>  ear they v were plugged into." ...
```
