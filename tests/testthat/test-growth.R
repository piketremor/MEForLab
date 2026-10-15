tl <- data.frame(stand = "A", fvs = c("RS", "BF", "RM", "WP"), spcd = c(97, 12, 316, 129),
                 dbh = c(10, 8, 12, 4), ht = c(60, 50, 65, 35), cr = c(.5, .45, .4, .6),
                 tpa = c(80, 120, 30, 200))

test_that("species crosswalk round-trips", {
  expect_equal(species_lookup(c(3, 4, 8), "tt", "fvs"), c("RS", "BF", "RM"))
  expect_equal(species_lookup("BR", "fvs", "spcd"), 823)
})

test_that("point cruise expansion uses BAF / (0.005454 DBH^2) per point", {
  cr <- data.frame(Type = "A", Plot = c(1, 1, 2), Spp = c(3, 4, 0), DBH = c(10, 8, 5),
                   Tree_Notes = c("40", "35 forked", "no tally"))
  x <- expand_point_cruise(cr)
  expect_equal(nrow(x), 2)
  expect_equal(sum(0.005454154 * x$dbh^2 * x$tpa), 20)   # 2 trees x BAF 20 / 2 points
  expect_equal(x$cr, c(.40, .35))
})

test_that("CONUS components return sensible annual values", {
  cmp <- conus_competition(tl$dbh, tl$ht, tl$cr, tl$tpa, tl$fvs)
  expect_equal(cmp$bal[which.max(tl$dbh)], 0)
  dg <- conus_dg(tl$spcd, tl$dbh, tl$cr, tl$ht, cmp$bal, 150, -32)
  expect_true(all(dg > 0 & dg < 1))
  hg <- conus_hg(tl$spcd, tl$ht, tl$cr, cmp$ccfl, cmp$cch, 150, 29.5, -32)
  expect_true(all(hg >= 0 & hg < 4))
  ps <- conus_survival(tl$spcd, tl$cr, cmp$cch)
  expect_true(all(ps > 0.8 & ps <= 1))
})

test_that("projection grows the stand and respects the density backstop", {
  r <- conus_project(tl, years = 30)
  s <- r$summary
  expect_equal(s$year, seq(0, 30, 5))
  expect_true(all(diff(s$qmd) > 0))
  dense <- tl; dense$tpa <- dense$tpa * 10
  s2 <- conus_project(dense, years = 30)$summary
  expect_true(all(s2$sdi[-1] <= 0.85 * s2$sdi_max[-1] * 1.01))  # stand SDImax shifts slightly as mortality reweights species
})

test_that("thinning from below hits the residual target and records removals", {
  r <- conus_project(tl, years = 10, regime = list(treatment(0, "thin_below", 60)))
  s0 <- r$summary[r$summary$year == 0, ]
  expect_equal(s0$ba_after, 60, tolerance = 1e-8)
  expect_equal(s0$ba - s0$removed_ba, 60, tolerance = 1e-8)
})

test_that("FVS keyword file is well formed", {
  f <- tempfile(fileext = ".key")
  fvs_write_keywords(c(A = "s1.tre"), f, regime = list(treatment(0, "thin_below", 80)))
  k <- readLines(f)
  expect_true(all(c("STDIDENT", "PROCESS", "STOP", "TREEFMT") %in% k))
  expect_true(any(grepl("^THINBBA\\s+2025", k)))
})

test_that("compiled FVS runs when available", {
  skip_if(is.na(fvs_find("ne")), "FVSne not installed")
  r <- fvs_project(transform(tl, ht_imputed = FALSE), years = 10)
  expect_true(nrow(r$summary) >= 3)
})
