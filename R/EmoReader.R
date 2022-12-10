#' EmoReader Emotion Clustering Library
#'
#' @description EmoReader is a package that enables users to create emotions from existing text documents. The data is first loaded and preprocessed by the EmoReader package.
#' It generates 10 various emotions based on the text document, scores each one appropriately, and then turns the results into a matrix for dimension reductions and clustering.
#' Emo Reader
#'
#' @import knitr Matrix singlet cluster syuzhet stats
#' @importFrom methods as
#' @importFrom syuzhet get_nrc_sentiment
#' @importFrom cluster clusplot
#' @importFrom singlet run_nmf
#' @importFrom Matrix colSums
#' @importFrom readr read_csv
#' @importFrom graphics abline hist par
#'
#'
#' @docType package
#' @name EmoReader
#' @author Diksha Shrestha, Roshan Shrestha, Josephat Oyondi, Akhila
#' @references 
#' http://rstudio-pubs-static.s3.amazonaws.com/529538_417d7feffb254fbe9fc3e5294623485e.html
#' https://medium.com/swlh/exploring-sentiment-analysis-a6b53b026131
#' https://statisticsglobe.com/convert-data-frame-to-matrix-in-r
#' https://satijalab.org/seurat/articles/dim_reduction_vignette.html
#' https://rpubs.com/JanpuHou/278584
#' https://www.geeksforgeeks.org/k-means-clustering-in-r-programming/
#' https://www.datacamp.com/tutorial/hierarchical-clustering-R
#' https://cran.r-project.org/web/packages/syuzhet/vignettes/syuzhet-vignette.html
#' https://rdrr.io/cran/syuzhet/man/get_nrc_sentiment.html


#' @aliases EmoReader-package
#' @md
#'
utils::globalVariables(c("emo_mat", "anger", "anticipation" , "disgust", "fear", "joy", "sadness", "surprise","trust", "negative","positive"))
NULL
