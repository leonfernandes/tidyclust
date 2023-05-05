test_that("bad input", {
  expect_snapshot(
    error = TRUE,
    hier_clust(mode = "bogus")
  )
  expect_snapshot(
    error = TRUE,
    {
      bt <- hier_clust(linkage_method = "bogus") %>% set_engine("stats")
      fit(bt, mpg ~ ., mtcars)
    }
  )
  expect_snapshot(
    error = TRUE,
    translate_tidyclust(hier_clust(), engine = NULL)
  )
  expect_snapshot(
    error = TRUE,
    translate_tidyclust(hier_clust(formula = ~x))
  )
})

test_that("predictions", {
  set.seed(1234)
  hclust_fit <- hier_clust(num_clusters = 4) %>%
    set_engine("stats") %>%
    fit(~., mtcars)

  set.seed(1234)
  ref_res <- cutree(hclust(dist(mtcars)), k = 4)

  ref_predictions <- ref_res %>% unname()

  relevel_preds <- function(x) {
    factor(unname(x), unique(unname(x))) %>% as.numeric()
  }

  expect_equal(
    relevel_preds(predict(hclust_fit, mtcars)$.pred_cluster),
    predict(hclust_fit, mtcars)$.pred_cluster %>% as.numeric()
  )

  expect_equal(
    relevel_preds(ref_predictions),
    extract_cluster_assignment(hclust_fit)$.cluster %>% as.numeric()
  )
})

test_that("extract_cluster_assignment works if you don't set num_clusters", {
  set.seed(1234)
  hclust_fit <- hier_clust(num_clusters = 4) %>%
    set_engine("stats") %>%
    fit(~., mtcars)

  set.seed(1234)
  hclust_fit_no_args <- hier_clust() %>%
    set_engine("stats") %>%
    fit(~., mtcars)

  expect_identical(
    extract_cluster_assignment(hclust_fit, mtcars),
    extract_cluster_assignment(hclust_fit_no_args, mtcars, num_clusters = 4)
  )
})

test_that("predict works if you don't set num_clusters", {
  set.seed(1234)
  hclust_fit <- hier_clust(num_clusters = 4) %>%
    set_engine("stats") %>%
    fit(~., mtcars)

  set.seed(1234)
  hclust_fit_no_args <- hier_clust() %>%
    set_engine("stats") %>%
    fit(~., mtcars)

  expect_identical(
    predict(hclust_fit, mtcars),
    predict(hclust_fit_no_args, mtcars, num_clusters = 4)
  )
})

test_that("extract_centroids work", {
  set.seed(1234)
  hclust_fit <- hier_clust(num_clusters = 4) %>%
    set_engine("stats") %>%
    fit(~., mtcars)

  set.seed(1234)
  ref_res <- cutree(hclust(dist(mtcars)), k = 4)

  ref_predictions <- ref_res %>% unname()

  expect_identical(
    extract_centroids(hclust_fit) %>%
      dplyr::mutate(.cluster = as.integer(.cluster)),
    mtcars %>%
      dplyr::group_by(.cluster = ref_predictions) %>%
      dplyr::summarize(dplyr::across(dplyr::everything(), mean))
  )
})

test_that("extract_centroids work if you don't set num_clusters", {
  set.seed(1234)
  hclust_fit <- hier_clust() %>%
    set_engine("stats") %>%
    fit(~., mtcars)

  set.seed(1234)
  ref_res <- cutree(hclust(dist(mtcars)), k = 4)

  ref_predictions <- ref_res %>% unname()

  expect_identical(
    extract_centroids(hclust_fit, num_clusters = 4) %>%
      dplyr::mutate(.cluster = as.integer(.cluster)),
    mtcars %>%
      dplyr::group_by(.cluster = ref_predictions) %>%
      dplyr::summarize(dplyr::across(dplyr::everything(), mean))
  )
})

test_that("predictions with new data", {
  set.seed(1234)
  hclust_fit <- hier_clust(num_clusters = 4) %>%
    set_engine("stats") %>%
    fit(~., mtcars)

  set.seed(1234)
  ref_res <- cutree(hclust(dist(mtcars)), k = 4)

  ref_predictions <- ref_res %>% unname()

  relevel_preds <- function(x) {
    factor(unname(x), unique(unname(x))) %>% as.numeric()
  }

  expect_equal(
    relevel_preds(predict(hclust_fit, mtcars[1:10, ])$.pred_cluster),
    predict(hclust_fit, mtcars[1:10, ])$.pred_cluster %>% as.numeric()
  )
})

test_that("Right classes", {
  expect_equal(
    class(hier_clust()),
    c("hier_clust", "cluster_spec", "unsupervised_spec")
  )
})

test_that("printing", {
  expect_snapshot(
    hier_clust()
  )
  expect_snapshot(
    hier_clust(num_clusters = 10)
  )
})

test_that("updating", {
  expect_snapshot(
    hier_clust(num_clusters = 5) %>%
      update(num_clusters = tune())
  )
})
