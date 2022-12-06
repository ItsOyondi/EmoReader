test_that("reading data", {
  expected <- read_data(csv_file = "small.csv")
  expect_s3_class(expected, "data.frame")
})

test_that("Creating Emotions", {

  expected <- read_data(csv_file = "small.csv")
  expect_error(get_emotion(expected), NA)

})


test_that("Count Emotions", {
  val <- count_emotions("emo_mat.csv")
  expect_equal(dim(val), c(10,2))
})

test_that("converting data to a matrix", {
  e_val <- matrix_conversion(data_file = "amazonemotion.csv")
  expect_error(e_val, NA)
})

test_that("Creating a sparsematrix", {
  expect_error(sparse_matrix(), NA)

})
test_that("NMF dimention reduction", {
  library(singlet)
  expect_error(nmf_func(nmfdim = 3), NA)

})

test_that("normalizing the data", {
  expect_error(norm_fun(),NA)
})

test_that("Modeling nmf", {
  expect_error(modeling_nmf(rank = 3), NA)
})

test_that("creating heatmap for nmf", {
  expect_error(heatmap_visualize(), NA)
})

test_that("PCA dimention reduction", {

  expect_error(pca_func(), NA)
})

test_that("visualizing pca", {

  expect_error(visualizing_pca(), NA)
})


test_that("kmeans clustering", {
  expected <- read.csv("emo_mat.csv")
  expect_error(cluster_kmeans(expected), NA)
})


test_that("H clustering", {
  expected <- read.csv("emo_mat.csv")
  expect_error(h_cluster(expected), NA)
})
