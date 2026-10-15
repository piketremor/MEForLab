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

test_that("read_inventory takes any data frame or file: fixed plots, common names, custom maps", {
  fx <- data.frame(Stand = rep(c("N", "S"), each = 3), PLOT = c(1, 1, 2, 1, 2, 2),
                   Spp = c("red spruce", "balsam fir", "eastern hemlock", "red maple",
                           "eastern white pine", "red maple"),
                   dbh = c(9, 6, 14, 11, 20, 8), HT = c(55, NA, 70, NA, 90, 40),
                   CR = c(.4, .5, .45, .4, .35, .5))
  f <- tempfile(fileext = ".csv"); utils::write.csv(fx, f, row.names = FALSE)
  tl <- read_inventory(f, design = "fixed", plot_acres = 0.1, species_codes = "common",
                       cols = list(stand = "stand", plot = "plot", species = "spp",
                                   dbh = "DBH", ht = "ht", cr = "cr"))
  expect_equal(tl$fvs, c("RS", "BF", "EH", "RM", "WP", "RM"))
  expect_equal(unique(tl$tpa), 10 / 2)                      # 1/0.1 ac over 2 plots
  expect_equal(tl$ht[!tl$ht_imputed], c(55, 70, 90, 40))   # measured heights kept
  expect_equal(tl$cr, fx$CR)                                # fractions detected
  tl2 <- read_inventory(fx, cols = list(stand = "Stand", plot = "PLOT", species = "Spp", dbh = "dbh"),
                        species_map = c("red spruce" = "RS", "balsam fir" = "BF",
                                        "eastern hemlock" = "EH", "red maple" = "RM",
                                        "eastern white pine" = "WP"))
  expect_equal(sum(0.005454154 * tl2$dbh^2 * tl2$tpa), 2 * (20 * 3 / 2))  # per stand: BAF x 3 trees / 2 points
})
