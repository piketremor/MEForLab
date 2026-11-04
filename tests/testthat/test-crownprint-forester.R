fstand <- function() {
  set.seed(7)
  n <- 48
  ht <- c(runif(16, 70, 88), runif(16, 45, 62), runif(16, 18, 35))
  data.frame(stand = "S", fvs = sample(c("WP", "EH", "RM", "RS", "RO"), n, TRUE),
             dbh = pmax(5, ht / 4 + rnorm(n, 0, 1)), tpa = 6.018, ht = ht, cr = runif(n, 0.35, 0.6))
}

test_that("species groups map FVS codes", {
  expect_equal(cp_species_group(c("WP", "BF", "SM", "QA", "ZZ")),
               c("White pine", "Spruce-fir", "Tolerant hardwood", "Aspen-birch", "Other"))
})

test_that("view, plan and guide are consistent", {
  d <- fstand(); ba0 <- sum(0.005454154 * d$dbh^2 * d$tpa)
  v <- cp_view_stand(d, "S")
  expect_equal(sum(v$summary$ba), ba0)
  expect_equal(diff(v$suggested_gap), 0.15)
  p <- cp_plan(v, 0.65 * ba0, favor = list(upper = c(`White pine` = 2), lower = c(Hemlock = 2)))
  ba1 <- sum(0.005454154 * p$trees$dbh^2 * p$trees$tpa_leave)
  expect_lte(ba1, 0.65 * ba0 + 1e-8)
  expect_gt(p$printsplit[["after"]], p$printsplit[["before"]])
  g <- cp_dbh_guide(p)
  expect_equal(sum(g$guide$tpa_now), sum(d$tpa), tolerance = 0.1)
  expect_true(all(g$guide$dbh_class %% 2 == 0 & g$guide$dbh_class >= 6))
  expect_equal(g$totals$ba_leave, ba1)
})

test_that("three strata and the optimizer run", {
  d <- fstand(); ba0 <- sum(0.005454154 * d$dbh^2 * d$tpa)
  v <- cp_view_stand(d, "S")
  p3 <- cp_plan(v, 0.7 * ba0, strata = 3)
  expect_length(p3$gaps, 2)
  po <- cp_plan(v, 0.7 * ba0, method = "optimize", pieces = 1)
  expect_lte(sum(0.005454154 * po$trees$dbh^2 * po$trees$tpa_leave), 0.7 * ba0 + 1e-8)
})

test_that("cp_mark_species favors weighted groups", {
  d <- fstand(); g <- cp_species_group(d$fvs); ba0 <- sum(0.005454154 * d$dbh^2 * d$tpa)
  m1 <- cp_mark_species(d, 0.65 * ba0, g)
  m2 <- cp_mark_species(d, 0.65 * ba0, g, weights = list(upper = c(`White pine` = 4), lower = c(`White pine` = 4)))
  wp <- g == "White pine"
  expect_lte(sum(m2$removed[wp]), sum(m1$removed[wp]))
})

test_that("workflow writes the guide and sheet", {
  d <- fstand(); out <- tempfile()
  w <- cp_workflow(d, "S", residual_ba = 0.65 * sum(0.005454154 * d$dbh^2 * d$tpa), out_dir = out)
  expect_true(file.exists(file.path(out, "marking_guide_S.csv")))
  expect_true(file.exists(file.path(out, "marking_sheet_S.png")))
})
