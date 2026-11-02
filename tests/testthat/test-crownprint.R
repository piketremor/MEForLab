sim_stand <- function(n_plots = 8, seed = 42) {
  set.seed(seed)
  do.call(rbind, lapply(seq_len(n_plots), function(p) {
    # overstory species "OV": tall, short crowns high up; understory "UN"
    ov <- data.frame(plot = p, sp = "OV", dbh = runif(8, 25, 40))
    un <- data.frame(plot = p, sp = "UN", dbh = runif(8, 6, 14))
    d <- rbind(ov, un)
    d$ht <- 1.37 + exp(3.3 - 10 / (d$dbh + 0.1)) + rnorm(nrow(d), 0, 0.8)
    d$cr <- ifelse(d$sp == "OV", 0.35, 0.7) + rnorm(nrow(d), 0, 0.03)
    d$tph <- 25
    d
  }))
}
cols_m <- list(stand = NULL, plot = "plot", species = "sp", dbh = "dbh",
               ht = "ht", cr = "cr", weight = "tph")

test_that("occupancy tally follows eq. 3-4 (Premer 2026 Fig. 2 logic)", {
  # canopy 1-21 m; one crown 5-18 m at 25 tph -> Cs 0.20..0.85 get 25
  tr <- data.frame(plot = 1, sp = c("A", "B"), dbh = c(30, 10),
                   ht_cp = c(21, 18), hcb_cp = c(1, 5), w = c(10, 25))
  pr <- cp_profile(tr, units = "metric",
                   cols = list(plot = "plot", species = "sp", dbh = "dbh", weight = "w"))
  b <- pr$profile[pr$profile$group == "B", ]
  expect_equal(b$occupancy[b$i %in% 20:85], rep(25, 66))
  expect_true(all(b$occupancy[!b$i %in% 20:85] == 0))
  a <- pr$profile[pr$profile$group == "A", ]
  expect_equal(range(a$i[a$occupancy > 0]), c(1, 100))   # crown spans all of Cs
  expect_equal(pr$plots$top_ht, 21); expect_equal(pr$plots$base_hcb, 1)
})

test_that("weighted beta MLE recovers known shape", {
  x <- (1:100) / 100; x[100] <- 0.995
  f <- MEForLab:::.cp_beta_mle(x, dbeta(x, 3, 1.5))
  expect_equal(unname(f[1:2]), c(3, 1.5), tolerance = 0.05)
})

test_that("local eq. 1 height fit recovers coefficients", {
  d <- sim_stand(12)
  h <- cp_height(d, method = "local", cols = cols_m, units = "metric", random = FALSE)
  fit <- attr(h, "ht_fit")
  expect_true(all(fit$method == "local-nls"))
  expect_equal(mean(fit$a), 3.3, tolerance = 0.05)
  expect_false(anyNA(h$ht_cp))
  expect_true(all(h$ht_source == "measured"))
})

test_that("paper HCB coefficients reproduce eq. 2 / Table 5", {
  d <- data.frame(plot = 1, sp = "RS", dbh = 20, ht = 15)
  h <- cp_height(d, cols = list(stand = NULL, plot = "plot", species = "sp",
                                dbh = "dbh", ht = "ht", cr = NULL, weight = NULL),
                 units = "metric")
  x <- cp_hcb(h, method = "paper", units = "metric",
              cols = list(stand = NULL, plot = "plot", species = "sp", dbh = "dbh",
                          ht = "ht", cr = NULL, weight = NULL))
  expect_equal(x$hcb_cp, 15 * (1 - 0.51 * exp(-9.52 * 20^-1.66)))
  expect_equal(x$hcb_source, "paper")
})

test_that("crownprint separates an overstory from an understory group", {
  d <- sim_stand(8)
  cp <- crownprint(d, cols = cols_m, units = "metric", boot = 50, seed = 1)
  g <- cp$summary$groups
  expect_s3_class(cp, "crownprint")
  expect_true(g$alpha[g$group == "OV"] > g$beta[g$group == "OV"])
  expect_true(g$alpha[g$group == "UN"] < g$beta[g$group == "UN"])
  pw <- cp$summary$pairwise
  expect_equal(pw$n_shared_plots, 8)
  expect_true(abs(pw$diff_mean_pos) > 0.2 && pw$p_value < 0.001)
  expect_true(any(grepl("upper canopy", cp$interpretation)))
  expect_true(all(c("alpha_se", "beta_se", "log_alpha_se") %in% names(cp$betas)))
  expect_output(print(cp), "CrownPrint")
})

test_that("units do not change canopy-space results", {
  d <- sim_stand(4)
  m <- crownprint(d, cols = cols_m, units = "metric", boot = 0)
  di <- d; di$dbh <- di$dbh / 2.54; di$ht <- di$ht / 0.3048
  i <- crownprint(di, cols = cols_m, units = "imperial", boot = 0)
  expect_equal(m$betas$alpha, i$betas$alpha, tolerance = 1e-6)
  expect_equal(i$trees$ht_cp, di$ht)
})

test_that("crownprint runs on read_inventory output", {
  tl <- read_inventory(maine_cruise, cols = list(stand = "Stand", plot = "Plot",
                       species = "Species", dbh = "DBH", ht = "Ht", cr = "CR"))
  cp <- crownprint(tl, boot = 20, seed = 1,
                   group = c(RS = "spruce-fir", BF = "spruce-fir", WS = "spruce-fir"))
  expect_true(all(cp$betas$alpha > 0, na.rm = TRUE))
  expect_setequal(cp$summary$groups$group, c("spruce-fir", "Other"))
  expect_equal(cp$settings$random_effect, "stand")
  expect_equal(cp$trees$ht_cp[!tl$ht_imputed], tl$ht[!tl$ht_imputed])
})
