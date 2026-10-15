## CONUS variant-free growth model (FVS remodeling project) ---------------------
##
## Equations and species parameters from the FVS Remodeling Project
## (Johnson, Marshall & Weiskittel; https://github.com/holoros/fvs_remodeling),
## snapshot of commit 9ead428 (3 Jul 2026). These are *preliminary* equations
## fit to FIA remeasurement data for all of CONUS with one annual time step:
##
##   diameter growth   dDBH = exp(b0 + b1 ln((DBH+1)^2 / (CR*HT+1)^b3)
##                                + b2 BAL^b4 / ln(DBH+2.7) + b5 ELEV + b6 EMT)
##   height growth     dHT  = HTmax b1 b2 CR^b3 exp(-b1 HT - b4 CCFL - b8 CCH^0.5
##                                - b5 ELEV + b6 TD^0.5 + b7 EMT) (1-exp(-b1 HT))^(b2-1)
##   survival          P    = 1 - exp(-exp(b0 + b1 (CR+0.01)^b2 + b3 CCH^b4))
##   crown recession   dHTLC = (HT - HTLC)(1 - exp(b0 + b1 dHT + b2 CCH))
##
## Units: inches, feet, ft2/ac; CR is a fraction; ELEV in feet; EMT (extreme
## minimum temperature) and TD (MWMT - MCMT) in deg C from ClimateNA
## 1991-2020 normals.

#' CONUS growth model parameters
#'
#' @param component one of "dg", "hg", "mort", "htlc"
#' @return data.frame of species parameters (one row per FIA SPCD)
#' @export
conus_params <- function(component = c("dg", "hg", "mort", "htlc")) {
  component <- match.arg(component)
  if (is.null(.mfl[[component]])) {
    f <- system.file("extdata", paste0("conus_", component, ".csv"), package = "MEForLab")
    .mfl[[component]] <- utils::read.csv(f, stringsAsFactors = FALSE)
  }
  .mfl[[component]]
}

#' Which species have CONUS parameters, and which are borrowed
#'
#' Species without parameters for a component borrow from a surrogate:
#' red spruce (97) for softwoods and red maple (316) for hardwoods, unless a
#' surrogate is given explicitly.
#'
#' @param spcd FIA species codes
#' @param surrogate named vector, e.g. `c("91" = 94)` to grow Norway spruce as
#'   white spruce
#' @return data.frame: spcd and the code actually used for each component
#' @examples
#' conus_coverage(c(97, 12, 91, 601, 998))
#' @export
conus_coverage <- function(spcd, surrogate = NULL) {
  spcd <- unique(spcd)
  res <- data.frame(spcd = spcd)
  for (k in c("dg", "hg", "mort", "htlc"))
    res[[k]] <- .conus_spcd(spcd, k, surrogate)
  res
}

.conus_spcd <- function(spcd, component, surrogate = NULL) {
  p <- conus_params(component)
  use <- spcd
  if (!is.null(surrogate)) {
    s <- surrogate[as.character(spcd)]
    use[!is.na(s)] <- s[!is.na(s)]
  }
  miss <- !use %in% p$spcd
  if (any(miss)) {
    soft <- species_lookup(spcd[miss], "spcd", "softwood")
    soft[is.na(soft)] <- spcd[miss][is.na(soft)] < 300
    use[miss] <- ifelse(soft, 97L, 316L)
  }
  use
}

.conus_coef <- function(spcd, component, surrogate = NULL) {
  p <- conus_params(component)
  p[match(.conus_spcd(spcd, component, surrogate), p$spcd), , drop = FALSE]
}

## competition ---------------------------------------------------------------

# sum of x over trees strictly larger (by key) than each tree
.sum_larger <- function(key, x) {
  u <- sort(unique(key))
  idx <- match(key, u)
  agg <- as.vector(rowsum(x, idx, reorder = TRUE))  # one value per unique key, ascending
  tot <- rev(cumsum(rev(agg)))                      # sum over keys >= k
  larger <- c(tot[-1], 0)                           # sum over keys > k
  larger[idx]
}

#' Competition measures used by the CONUS equations
#'
#' @param dbh,ht,cr,tpa tree attributes (in., ft, fraction, trees/ac)
#' @param fvs FVS alpha species code (for maximum crown width)
#' @param lcw_exp exponent relating largest to maximum crown width,
#'   LCW = MCW * CR^lcw_exp (an approximation; see Details)
#' @return data.frame with `bal` (ft2/ac), `ccfl` (crown competition factor in
#'   larger trees, percent) and `cch` (crown closure at the tree tip, fraction
#'   of an acre)
#' @details CCFL uses the FVS form CCF_i = 0.001803 MCW^2 TPA with MCW from
#'   [MCW()]. CCH sums the horizontal crown area of every tree at the height
#'   of the subject tree's tip: zero above a tree's top, the largest crown
#'   width at or below its crown base, linear in between. The remodeling
#'   project computes CCH with its own crown profile, which is not published
#'   in the repository, so absolute CCH values here are an approximation.
#' @export
conus_competition <- function(dbh, ht, cr, tpa, fvs, lcw_exp = 0.5) {
  ba <- 0.005454154 * dbh^2 * tpa
  mcw <- MCW(fvs, dbh)
  bal <- .sum_larger(dbh, ba)
  ccfl <- .sum_larger(dbh, 0.001803 * mcw^2 * tpa)
  lcw <- mcw * cr^lcw_exp
  hcb <- ht * (1 - cr)
  # crown width of tree j (columns) at the tip height of tree i (rows)
  H <- matrix(ht, length(ht), length(ht))                  # H[i, j] = ht_i
  rel <- (rep(ht, each = length(ht)) - H) / rep(pmax(ht - hcb, 1e-6), each = length(ht))
  rel <- pmin(pmax(rel, 0), 1)
  cw <- rel * rep(lcw, each = length(ht))
  cch <- as.vector((cw^2 * (pi / 4)) %*% tpa) / 43560
  data.frame(bal = bal, ccfl = ccfl, cch = cch)
}

## component equations ----------------------------------------------------------

#' Annual diameter growth (in./yr), CONUS equation
#' @param spcd FIA species code
#' @param dbh tree DBH (in.)
#' @param cr crown ratio (fraction)
#' @param ht total height (ft)
#' @param bal basal area in larger trees (ft2/ac)
#' @param elev elevation (ft)
#' @param emt extreme minimum temperature (deg C)
#' @param surrogate see [conus_coverage()]
#' @export
conus_dg <- function(spcd, dbh, cr, ht, bal, elev, emt, surrogate = NULL) {
  b <- .conus_coef(spcd, "dg", surrogate)
  exp(b$B0 + b$B1 * log((dbh + 1)^2 / (cr * ht + 1)^b$B3) +
        b$B2 * bal^b$B4 / log(dbh + 2.7) + b$B5 * elev + b$B6 * emt)
}

#' Annual height growth (ft/yr), CONUS equation
#' @inheritParams conus_dg
#' @param ccfl crown competition factor in larger trees
#' @param cch crown closure at tree tip (fraction)
#' @param td temperature difference MWMT - MCMT (deg C)
#' @export
conus_hg <- function(spcd, ht, cr, ccfl, cch, elev, td, emt, surrogate = NULL) {
  b <- .conus_coef(spcd, "hg", surrogate)
  g <- b$B0 * b$B1 * b$B2 * cr^b$B3 *
    exp(-b$B1 * ht - b$B4 * ccfl - b$B8 * sqrt(cch) - b$B5 * elev +
          b$B6 * sqrt(td) + b$B7 * emt) *
    (1 - exp(-b$B1 * ht))^(b$B2 - 1)
  pmax(g, 0)
}

#' Annual survival probability, CONUS equation
#' @inheritParams conus_hg
#' @export
conus_survival <- function(spcd, cr, cch, surrogate = NULL) {
  b <- .conus_coef(spcd, "mort", surrogate)
  p <- 1 - exp(-exp(b$b0 + b$b1 * (cr + 0.01)^b$b2 + b$b3 * cch^b$b4))
  ifelse(cr > 0, p, 0)
}

#' Annual rise in height to live crown (ft/yr), CONUS equation
#' @inheritParams conus_hg
#' @param htlc height to live crown base (ft)
#' @param dht height growth this year (ft)
#' @export
conus_dhtlc <- function(spcd, ht, htlc, dht, cch, surrogate = NULL) {
  b <- .conus_coef(spcd, "htlc", surrogate)
  pmax((ht - htlc) * (1 - exp(b$b0 + b$b1 * dht + b$b2 * cch)), 0)
}

## the projection ------------------------------------------------------------------

#' Project a tree list with the CONUS variant-free model
#'
#' Grows every stand in `trees` on an annual step. Each tree record keeps its
#' identity and carries a declining TPA (deterministic mortality); there is
#' no regeneration or ingrowth, so include saplings in the starting tree list.
#'
#' @param trees tree list from [read_demeritt_inventory()] or
#'   [expand_point_cruise()] + [complete_trees()]: needs `stand`, `fvs`,
#'   `spcd`, `dbh`, `ht`, `cr`, `tpa`
#' @param years projection length
#' @param site list or one-row-per-stand data.frame with `elev` (ft), `emt`,
#'   `td` (deg C). The default is an approximation for Orono, Maine; replace
#'   it with ClimateNA values for real work.
#' @param regime list of treatments (see [treatment()]); applied at the start
#'   of the year they name
#' @param mult growth/mortality multipliers, e.g. `list(dg = 1.1)` for
#'   sensitivity analysis
#' @param surrogate see [conus_coverage()]
#' @param sdi_limit density backstop: when stand SDI (summation method)
#'   exceeds `sdi_limit` x the stand's maximum SDI, extra mortality brings it
#'   back to that line, falling mostly on the shortest trees. The stand
#'   maximum is the basal-area-weighted mean of the FVS-NE species SDI maxima.
#'   Set to NA to turn it off. See Details.
#' @param report_every summary interval (years)
#' @param keep_trees return the tree list at each reporting year
#' @return list with `summary` (stand x year) and, if requested, `trees`
#' @details The CONUS survival equation depends only on crown ratio and crown
#'   closure at the tree tip. A tree record that represents a dense cohort
#'   (e.g. 500 saplings/ac of one species and size) has no taller neighbours,
#'   so its predicted survival stays near 1 and the stand can grow far past
#'   any biologically possible density. Compiled FVS handles this with an
#'   SDI-based density mortality model; the `sdi_limit` backstop plays the
#'   same role here. It is an addition to, not part of, the remodeling
#'   project's equations: report it when you use it, and compare runs with
#'   `sdi_limit = NA` to see how much it matters.
#' @examples
#' tl <- data.frame(stand = "A", fvs = c("RS", "BF", "RM"), spcd = c(97, 12, 316),
#'                  dbh = c(10, 8, 12), ht = c(60, 50, 65), cr = c(.5, .45, .4),
#'                  tpa = c(80, 120, 30))
#' res <- conus_project(tl, years = 20)
#' res$summary
#' @export
conus_project <- function(trees, years = 50,
                          site = list(elev = 150, emt = -32, td = 29.5),
                          regime = list(), mult = list(),
                          surrogate = c("91" = 94, "601" = 762),
                          sdi_limit = 0.85, report_every = 5, keep_trees = FALSE) {
  need <- c("stand", "fvs", "spcd", "dbh", "ht", "cr", "tpa")
  miss <- setdiff(need, names(trees))
  if (length(miss)) stop("tree list is missing: ", paste(miss, collapse = ", "),
                         " (run complete_trees() first?)")
  if (anyNA(trees[, need])) stop("NA values in ", paste(need, collapse = "/"),
                                 "; run complete_trees() first")
  m <- utils::modifyList(list(dg = 1, hg = 1, mort = 1), mult)
  stands <- split(trees, trees$stand)
  res <- lapply(names(stands), function(s) {
    st <- .site_for(site, s)
    .conus_one(stands[[s]], s, years, st, regime, m, surrogate, report_every, keep_trees,
               sdi_limit)
  })
  out <- list(summary = do.call(rbind, lapply(res, `[[`, "summary")))
  if (keep_trees) out$trees <- do.call(rbind, lapply(res, `[[`, "trees"))
  attr(out, "engine") <- "conus"
  out
}

.site_for <- function(site, stand) {
  if (is.data.frame(site)) {
    r <- site[as.character(site$stand) == stand, , drop = FALSE]
    if (!nrow(r)) stop("no site row for stand ", stand)
    return(list(elev = r$elev[1], emt = r$emt[1], td = r$td[1]))
  }
  site
}

.conus_one <- function(t, stand, years, site, regime, m, surrogate, every, keep,
                       sdi_limit = NA) {
  t$htlc <- t$ht * (1 - t$cr)
  summ <- list(); tl <- list()
  removed <- c(ba = 0, tpa = 0, vol = 0)
  record <- function(yr) {
    s <- stand_summary(t[t$tpa > 0, , drop = FALSE])
    if (is.null(s) || !nrow(s)) s <- data.frame(stand = stand, tpa = 0, ba = 0, qmd = NA, sdi = 0,
                                  rd = NA, top_ht = NA, vol_cuft = 0,
                                  lead_spp = NA, lead_share = NA)
    s$year <- yr
    s$sdi_max <- .stand_sdimax(t)
    s$removed_tpa <- removed[["tpa"]]; s$removed_ba <- removed[["ba"]]
    s$removed_vol <- removed[["vol"]]
    summ[[length(summ) + 1]] <<- s
    if (keep) tl[[length(tl) + 1]] <<- transform(t, year = yr)
    removed[] <<- 0
  }
  for (yr in 0:years) {
    # reporting year rows describe the stand *before* that year's treatment
    # (as FVS does); removals and after-treatment BA go on the same row
    pre <- t
    for (a in regime) if (identical(as.numeric(a$year), as.numeric(yr))) {
      r <- apply_treatment(t, a)
      t <- r$trees
      removed <- removed + r$removed
    }
    if (yr %% every == 0 || yr == years) {
      after <- t; t <- pre; record(yr); t <- after
      summ[[length(summ)]]$ba_after <- sum(0.005454154 * t$dbh^2 * t$tpa)
    }
    if (yr == years) break
    t <- t[t$tpa > 1e-4, , drop = FALSE]
    if (!nrow(t)) next
    cmp <- conus_competition(t$dbh, t$ht, t$cr, t$tpa, t$fvs)
    dg <- m$dg * conus_dg(t$spcd, t$dbh, t$cr, t$ht, cmp$bal, site$elev, site$emt, surrogate)
    hg <- m$hg * conus_hg(t$spcd, t$ht, t$cr, cmp$ccfl, cmp$cch, site$elev, site$td,
                          site$emt, surrogate)
    ps <- conus_survival(t$spcd, t$cr, cmp$cch, surrogate)
    ps <- 1 - pmin(m$mort * (1 - ps), 1)
    dl <- conus_dhtlc(t$spcd, t$ht, t$htlc, hg, cmp$cch, surrogate)
    t$dbh <- t$dbh + dg
    t$ht <- t$ht + hg
    t$htlc <- pmin(t$htlc + dl, t$ht * 0.95)        # keep at least 5% live crown
    t$cr <- (t$ht - t$htlc) / t$ht
    t$tpa <- t$tpa * ps
    if (is.finite(sdi_limit)) t$tpa <- .sdi_backstop(t, sdi_limit)
  }
  list(summary = do.call(rbind, summ),
       trees = if (keep) do.call(rbind, tl) else NULL)
}

.stand_sdimax <- function(t) {
  smax <- species_lookup(t$fvs, "fvs", "sdimax_ne")
  smax[is.na(smax)] <- 400
  w <- 0.005454154 * t$dbh^2 * t$tpa
  if (sum(w) <= 0) return(NA_real_)
  sum(w * smax) / sum(w)
}

# Bring stand SDI down to sdi_limit * SDImax, taking proportionally more from
# shorter trees. Returns the adjusted TPA vector.
.sdi_backstop <- function(t, sdi_limit) {
  if (!nrow(t)) return(t$tpa)
  lim <- sdi_limit * .stand_sdimax(t)
  sdi_i <- (t$dbh / 10)^1.605
  if (sum(t$tpa * sdi_i) <= lim) return(t$tpa)
  w <- 1 - 0.75 * t$ht / max(t$ht)                  # 0.25 (tallest) .. 1 (shortest)
  f <- function(k) sum(t$tpa * (1 - pmin(k * w, 1)) * sdi_i) - lim
  k <- if (f(1 / min(w)) > 0) 1 / min(w) else stats::uniroot(f, c(0, 1 / min(w)), tol = 1e-8)$root
  t$tpa * (1 - pmin(k * w, 1))
}
