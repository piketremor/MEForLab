## Synthetic example inventory shipped with MEForLab (not real plot data).
## Six Maine-like stands, BAF-20 prism points with nested 5.3-ft sapling plots.
set.seed(2026)
library(MEForLab)
types <- list(
  `SF-1` = list(sp = c(RS = .45, BF = .25, RM = .12, WS = .08, PB = .10), ba = 150, shape = 6, scale = 1.6, n = 7, acres = 42),
  `NH-2` = list(sp = c(SM = .35, AB = .25, YB = .20, RM = .12, WA = .08), ba = 110, shape = 5, scale = 2.4, n = 6, acres = 35),
  `WP-3` = list(sp = c(WP = .55, RM = .15, RO = .15, EH = .15),          ba = 170, shape = 4, scale = 3.6, n = 6, acres = 28),
  `HM-4` = list(sp = c(EH = .60, RS = .15, YB = .15, RM = .10),          ba = 190, shape = 5, scale = 2.6, n = 5, acres = 19),
  `MX-5` = list(sp = c(BF = .35, RM = .25, PB = .15, QA = .15, RS = .10), ba = 95, shape = 7, scale = 1.0, n = 8, acres = 55),
  `CE-6` = list(sp = c(WC = .55, BF = .20, RM = .10, TA = .08, BS = .07), ba = 130, shape = 6, scale = 1.4, n = 5, acres = 16))
over <- list(); sap <- list(); pid <- 0
for (st in names(types)) {
  ty <- types[[st]]
  for (p in seq_len(ty$n)) {
    pid <- pid + 1
    k <- rpois(1, ty$ba / 20)
    if (k == 0) { over[[length(over) + 1]] <- data.frame(Stand = st, Plot = pid, Tree = 0, Species = NA,
                     DBH = NA, Ht = NA, CR = NA, Acres = ty$acres); next }
    spp <- sample(names(ty$sp), k, TRUE, ty$sp)
    dbh <- round(pmax(5, rgamma(k, ty$shape, 1 / ty$scale)))
    ht <- round(wykoff.ht(spp, dbh) * exp(rnorm(k, 0, .08)))
    ht[runif(k) > .25] <- NA                       # ~25% measured heights
    cr <- round(pmin(85, pmax(15, rnorm(k, ifelse(spp %in% c("WP", "RO", "SM"), 40, 50), 10))) / 5) * 5
    over[[length(over) + 1]] <- data.frame(Stand = st, Plot = pid, Tree = seq_len(k), Species = spp,
                                           DBH = dbh, Ht = ht, CR = cr, Acres = ty$acres)
    ns <- rpois(1, 1.2)
    if (ns > 0) {
      ssp <- sample(names(ty$sp), ns, TRUE, ty$sp)
      for (s in unique(ssp)) {
        cnt <- rpois(4, c(.9, .5, .3, .15) * sum(ssp == s))
        sap[[length(sap) + 1]] <- data.frame(Stand = st, Plot = pid, Species = s,
                                             `1in` = cnt[1], `2in` = cnt[2], `3in` = cnt[3], `4in` = cnt[4],
                                             check.names = FALSE)
      }
    }
  }
}
maine_cruise <- do.call(rbind, over)
# one point with no tally (all stems below the prism threshold)
maine_cruise <- rbind(maine_cruise, data.frame(Stand = "MX-5", Plot = pid + 1, Tree = 0, Species = NA,
                                               DBH = NA, Ht = NA, CR = NA, Acres = 55))
maine_saplings <- do.call(rbind, sap)
rownames(maine_cruise) <- rownames(maine_saplings) <- NULL
save(maine_cruise, file = "data/maine_cruise.rda", compress = "xz")
save(maine_saplings, file = "data/maine_saplings.rda", compress = "xz")
