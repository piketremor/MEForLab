mark_stand <- function() {
  set.seed(3)
  n <- 40
  ht <- c(runif(15, 70, 85), runif(15, 45, 60), runif(10, 15, 30))
  data.frame(stand = "S", plot = rep(1:4, 10), fvs = sample(c("SM", "RM", "AB", "YB"), n, TRUE),
             dbh = pmax(5, ht / 4 + rnorm(n, 0, 1)), tpa = 6.018, ht_cp = ht,
             hcb_cp = ht * (1 - runif(n, 0.35, 0.55)))
}

test_that("carved target empties the gap band and sums to one", {
  t <- cp_target_carve(rep(1, 100), gaps = list(c(0.45, 0.60)))
  expect_equal(sum(t), 1)
  expect_true(all(t[45:60] == 0))
  expect_length(attr(t, "centers"), 2)
})

test_that("cp_printsplit is 1 for an empty band and 0 for a flat profile", {
  o <- rep(1, 100); expect_equal(cp_printsplit(o), 0)
  o[45:60] <- 0; expect_equal(cp_printsplit(o), 1)
  expect_lt(cp_printsplit(c(rep(1, 44), rep(3, 16), rep(1, 40))), 0)
  expect_equal(cp_printsplit(o * 7), cp_printsplit(o))   # independent of stocking
  expect_warning(expect_equal(cp_gap_depth(o), 1), "deprecated")
})

test_that("cp_mark hits the residual BA and improves the profile", {
  d <- mark_stand()
  ba0 <- sum(0.005454154 * d$dbh^2 * d$tpa)
  m <- cp_mark(d, residual_ba = 0.65 * ba0, pieces = 1)
  expect_s3_class(m, "cp_mark")
  expect_lte(m$ba_after, 0.65 * ba0 + 1e-8)
  expect_gt(m$ba_after, 0.65 * ba0 - max(0.005454154 * d$dbh^2 * d$tpa))
  expect_lt(m$hellinger[["after"]], m$hellinger[["before"]])
  expect_gt(m$printsplit[["after"]], m$printsplit[["before"]])
  expect_true(all(m$trees$mark_reason[m$trees$tpa_removed > 0] != ""))
  expect_output(print(m), "Crown-profile marking")
})

test_that("protected trees are never marked", {
  d <- mark_stand(); d$keep <- d$ht_cp > 40 & d$ht_cp < 65
  m <- cp_mark(d, residual_ba = 0.65 * sum(0.005454154 * d$dbh^2 * d$tpa), pieces = 1, protect = "keep")
  expect_true(all(m$trees$tpa_removed[d$keep] == 0))
})
