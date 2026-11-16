test_that("window parsing handles years, months, dates and Excel US dates", {
  p <- .nfws_parse_ym
  expect_equal(p("1995"), 1995L * 12L)
  expect_equal(p("1995", end = TRUE), 1995L * 12L + 11L)
  expect_equal(p("1995-06"), 1995L * 12L + 5L)
  expect_equal(p("1995-06-17", end = TRUE), 1995L * 12L + 5L)
  expect_equal(p("6/1/2010"), 2010L * 12L + 5L)
  expect_equal(p(as.Date("2003-09-30")), 2003L * 12L + 8L)
  expect_equal(p(2001), 2001L * 12L)
  expect_true(is.na(p("spring 95")))
  expect_true(is.na(p("1995-13")))
})

test_that("points are validated without stopping", {
  df <- data.frame(id = c("A", "B", "C", "D", "A"), lat = c(46, 44.9, 46, NA, 45),
                   lon = c(-69.8, -71.6, 69.8, -70, -70),
                   s = c("1985", "1995-06", "2001", "", "2010"),
                   e = c("2025", "2003-09", "1999", "", "junk"))
  p <- .nfws_points(df, "lat", "lon", "id", "s", "e")
  expect_equal(p$use, c(TRUE, TRUE, FALSE, FALSE, TRUE))
  expect_match(p$note[1], "clamped")
  expect_match(p$note[3], "outside region")
  expect_match(p$note[4], "outside region")
  expect_match(p$note[5], "end unparsed")
  expect_match(p$note[5], "duplicate id")
  expect_equal(.nfws_ym(p$s[2]), "1995-06")
  expect_error(.nfws_points(df, "latitude", "lon", NULL, NULL, NULL), "not found")
})

synth <- function(pts) {
  yrs <- 1991:2020; ok <- which(pts$use); mo <- rep(1:12, 30)
  wave <- 8 - 14 * cos(2 * pi * (mo - 0.5) / 12)
  mon <- do.call(rbind, lapply(ok, function(i) data.frame(
    pt = i, year = rep(yrs, each = 12), month = mo, ppt = 90,
    tmax = wave + 4, tmin = wave - 6, eto_flat = pmax(0, 60 * sin(pi * (mo - 0.5) / 12)), trr = 1.02)))
  list(site = data.frame(pt = seq_len(nrow(pts)), lon_c = pts$lon, lat_c = pts$lat, ok = pts$use),
       mon = .nfws_derive(mon))
}

test_that("staggered windows sum the right months and flag partial seasons", {
  p <- .nfws_points(data.frame(lat = c(46, 44.9), lon = c(-69.8, -71.6),
                               s = c("1991", "1995-06"), e = c("2020", "2003-09")),
                    "lat", "lon", NULL, "s", "e")
  X <- synth(p); r <- .nfws_summarise(p, X, 5)
  expect_equal(r$summary$n_months, c(360L, 100L))
  expect_equal(r$summary$n_gs_partial, c(0L, 2L))
  m <- X$mon[X$mon$pt == 2 & X$mon$k >= p$s[2] & X$mon$k <= p$e[2], ]
  expect_equal(r$summary$WS_mm[2], sum(m$ws))
  g <- r$gs[r$gs$id == "2", ]
  expect_true(g$partial[g$year == 1995] && g$partial[g$year == 2003])
  expect_lt(g$gs_days_in_window[g$year == 1995], g$gs_days[g$year == 1995])
  expect_equal(r$summary$gs_ws_mean_mm[2], mean(g$ws_gs[!g$partial]))
})

test_that("a window containing no growing season returns no season rows", {
  p <- .nfws_points(data.frame(lat = 45, lon = -70, s = "2000-11", e = "2001-02"),
                    "lat", "lon", NULL, "s", "e")
  r <- .nfws_summarise(p, synth(p), 5)
  expect_equal(r$summary$n_months, 4L)
  expect_equal(r$summary$n_gs, 0L)
  expect_null(r$gs)
})

test_that("flat ETo swaps both ws and eto", {
  p <- .nfws_points(data.frame(lat = 45, lon = -70), "lat", "lon", NULL, NULL, NULL)
  X <- synth(p); f <- .nfws_swap(X$mon, "flat")
  expect_equal(f$ws, f$ppt - f$eto_flat)
  expect_equal(f$eto_terr, f$eto_flat)
})

test_that("extraction reproduces the reference point (needs input rasters)", {
  dir <- getOption("nfws.data_dir", Sys.getenv("NFWS_DATA"))
  skip_if(!nzchar(dir) || !dir.exists(dir), "NFWS_DATA not set")
  r <- nf_water_surplus(data.frame(lat = 46.0, lon = -69.8), data_dir = dir)
  expect_equal(r$summary$WS_mm, 10652.67, tolerance = 0.01, scale = 1)
  expect_equal(r$summary$gs_ws_mean_mm, -44.34, tolerance = 0.01, scale = 1)
})
