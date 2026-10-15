test_that("vicary.height inverts vicary.site", {
  si <- c(45, 55, 65)
  for (sp in c("RS", "BF"))
    expect_equal(vicary.site(sp, vicary.height(sp, 40, si), 40), si)
})

test_that("MCW follows Russell & Weiskittel metric form", {
  expect_equal(MCW("RS", 10), 1.80 * (25.4)^0.461 * 3.28084)
  expect_true(all(diff(MCW("SM", c(4, 8, 12, 16))) > 0))
})

test_that("basal.area.larger counts only strictly larger trees, in input order", {
  bal <- basal.area.larger(c(1, 1, 1), c(14, 6, 10), c(20, 100, 50))
  ba <- 0.005454154 * c(14, 6, 10)^2 * c(20, 100, 50)
  expect_equal(bal, c(0, ba[1] + ba[3], ba[1]))
})

test_that("vectorised equations accept vectors and flag unknown species", {
  expect_length(wykoff.ht(c("RS", "BF", "SM"), c(8, 10, 12)), 3)
  expect_length(vol.calc(c("RS", "BF"), c(8, 10), c(50, 55)), 2)
  expect_warning(v <- vol.calc("ZZ", 10, 50))
  expect_true(is.na(v))
})

test_that("Kozak_merchR no longer errors on undefined cr", {
  out <- Kozak_merchR(data.frame(dbh = c(14.1, 18), tht = c(58, 80)))
  expect_true(all(out$vol > 0))
})
