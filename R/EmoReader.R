#' EmoReader Emotion Clustering Library
#'
#' @description
#' Emo Reader
#'
#' @import knitr singlet cluster syuzhet stats
#' @importFrom methods as
#' @importFrom syuzhet get_nrc_sentiment
#' @importFrom cluster clusplot
#' @importFrom singlet run_nmf
#' @importFrom Matrix colSums
#' @importFrom readr read_csv
#' @importFrom graphics abline hist par
#'
#' @docType package
#' @name EmoReader
#' @author Diskha Shrestha, Roshan Shrestha, Josephat Oyondi, Akhila
#' @aliases EmoReader-package
#' @md
#'
utils::globalVariables(c("emo_mat", "anger", "anticipation" , "disgust", "fear", "joy", "sadness", "surprise","trust", "negative","positive"))
NULL
