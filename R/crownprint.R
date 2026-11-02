#' CrownPrint: crown-profile analysis of vertical canopy structure
#'
#' A nested workflow that turns a plot inventory into species- (or group-)
#' specific vertical crown occupancy profiles and summarises each with a beta
#' distribution, following Premer (2026). The steps can be run one at a time
#' or all at once with [crownprint()]:
#'
#' 1. [cp_height()]: complete total height (HT) with the FVS-NE Wykoff
#'    equation, the paper's Table 4 coefficients, or a local fit of
#'    eq. 1, `HT = 1.37 + exp(a + b / (DBH + 0.1))`.
#' 2. [cp_hcb()]: complete height to live crown base (HCB) with a local fit of
#'    eq. 2, `HCB = HT * (1 - a * exp(-b * DBH^c))`, the paper's Table 5
#'    coefficients, or observed crown ratios.
#' 3. [cp_profile()]: build the normalized canopy space for each plot
#'    (eq. 3, `Cs(i) = 0.01 i`, 0 = lowest live crown, 1 = tallest tree) and
#'    tally expansion-weighted crown occupancy per group (eq. 4).
#' 4. [cp_beta()]: fit a beta distribution (eq. 5) to each plot x group
#'    profile, with bootstrap standard errors.
#' 5. [cp_summary()]: estimate group-level alpha and beta on the log scale
#'    (eq. 6; random stand effect when there are 3 or more stands), plus
#'    paired between-group comparisons.
#' 6. [cp_interpret()]: plain-language interpretation.
#'
#' @references Premer, M.I. 2026. CrownPrint: a crown-profile approach to
#'   quantifying canopy structure. Canadian Journal of Forest Research 56:
#'   1-14. doi:10.1139/cjfr-2025-0239
#' @name crownprint-package
#' @keywords internal
NULL

## coefficient tables ---------------------------------------------------------

# Premer (2026) Table 4 (eq. 1; DBH cm, HT m) and Table 5 (eq. 2; DBH cm).
# SMC = Douglas-fir / western hemlock (Washington); CTRN = balsam fir / red
# spruce (Maine). BF and RS share b and c in Table 5.
.cp_paper <- data.frame(
  sp    = c("DF", "WH", "BF", "RS"),
  ht_a  = c(3.33, 3.27, 3.01, 2.89),
  ht_b  = c(-10.11, -8.53, -9.69, -9.39),
  cr_a  = c(0.65, 0.65, 0.52, 0.51),
  cr_b  = c(10.85, 7.73, 9.52, 9.52),
  cr_c  = c(-3.12, -4.38, -1.66, -1.66),
  stringsAsFactors = FALSE)

.cp_bh <- 1.37   # breast height (m)
.cp_k  <- 0.1    # DBH offset in eq. 1 (cm)

.cp_cols_default <- list(stand = "stand", plot = "plot", species = "fvs",
                         dbh = "dbh", ht = "ht", hcb = NULL, cr = "cr",
                         weight = "tpa")

## standardise input ----------------------------------------------------------

# Internal frame (metric): .stand .plot .pid .sp .grp .by .dbh (cm)
# .ht_obs .hcb_obs (m) .cr_obs .w, plus .ht / .hcb when already completed.
.cp_std <- function(trees, cols = list(), units = c("imperial", "metric"),
                    group = NULL, by = NULL) {
  units <- match.arg(units)
  d <- as.data.frame(trees, stringsAsFactors = FALSE)
  if (!nrow(d)) stop("crownprint: no trees")
  cl <- .cp_cols_default
  for (k in names(cols)) cl[k] <- list(cols[[k]])
  user <- names(cols)
  pick <- function(key) {
    nm <- cl[[key]]
    if (is.null(nm)) return(NULL)
    hit <- names(d)[tolower(names(d)) == tolower(nm)]
    if (!length(hit)) {
      if (key %in% user || key %in% c("plot", "species", "dbh"))
        stop("crownprint: column '", nm, "' (", key, ") not found; available: ",
             paste(names(d), collapse = ", "))
      return(NULL)
    }
    d[[hit[1]]]
  }
  num <- function(v) suppressWarnings(as.numeric(v))
  cf <- if (units == "imperial") c(d = 2.54, h = 0.3048) else c(d = 1, h = 1)

  stand <- pick("stand"); if (is.null(stand)) stand <- rep("all", nrow(d))
  plot <- pick("plot")
  sp <- trimws(as.character(pick("species")))
  dbh <- num(pick("dbh")) * cf[["d"]]
  w <- pick("weight")
  if (is.null(w)) {
    message("crownprint: no expansion-factor column; every tree gets weight 1")
    w <- rep(1, nrow(d))
  }
  w <- num(w)

  ht <- num(pick("ht")) * cf[["h"]]
  if (!length(ht)) ht <- rep(NA_real_, nrow(d))
  if (!is.null(d$ht_imputed)) ht[as.logical(d$ht_imputed) %in% TRUE] <- NA
  ht[!is.na(ht) & ht <= .cp_bh] <- NA
  hcb <- num(pick("hcb")) * cf[["h"]]
  if (!length(hcb)) hcb <- rep(NA_real_, nrow(d))
  hcb[!is.na(hcb) & hcb < 0] <- NA
  crv <- pick("cr")
  cr <- if (is.null(crv)) rep(NA_real_, nrow(d)) else .parse_cr(crv, "auto")
  if (!is.null(d$cr_imputed)) cr[as.logical(d$cr_imputed) %in% TRUE] <- NA
  if (!is.null(d$source)) cr[d$source %in% "sapling"] <- NA   # cr_default, not measured
  from_hcb <- !is.na(hcb) & !is.na(ht) & hcb < ht
  cr[from_hcb] <- 1 - hcb[from_hcb] / ht[from_hcb]

  grp <- if (is.null(group)) sp
         else if (length(group) == 1 && is.null(names(group))) {
           g <- names(d)[tolower(names(d)) == tolower(group)]
           if (!length(g)) stop("crownprint: group column '", group, "' not found")
           as.character(d[[g[1]]])
         } else {
           g <- unname(group[sp]); g[is.na(g)] <- "Other"; g
         }
  byv <- if (is.null(by)) rep("all", nrow(d)) else {
    b <- names(d)[tolower(names(d)) == tolower(by)]
    if (!length(b)) stop("crownprint: by column '", by, "' not found")
    as.character(d[[b[1]]])
  }

  s <- data.frame(.row = seq_len(nrow(d)),
                  .stand = as.character(stand), .plot = as.character(plot),
                  .sp = sp, .grp = grp, .by = byv, .dbh = dbh,
                  .ht_obs = ht, .hcb_obs = hcb, .cr_obs = cr, .w = w,
                  stringsAsFactors = FALSE)
  s$.pid <- paste(s$.stand, s$.plot, sep = "/")
  if (!is.null(d$ht_cp))  s$.ht  <- num(d$ht_cp)  * cf[["h"]]
  if (!is.null(d$hcb_cp)) s$.hcb <- num(d$hcb_cp) * cf[["h"]]
  keep <- !is.na(s$.dbh) & s$.dbh > 0 & !is.na(s$.w) & s$.w > 0 &
          !is.na(s$.sp) & nzchar(s$.sp)
  if (any(!keep))
    message("crownprint: dropped ", sum(!keep),
            " rows with missing species, DBH or weight")
  attr(s, "units") <- units
  attr(s, "cf") <- cf
  attr(s, "dropped") <- which(!keep)
  s[keep, , drop = FALSE]
}

## step 1: heights ------------------------------------------------------------

# FVS-NE Wykoff (imperial) returned in metres; species without NE
# coefficients use the generic softwood ("OS") or hardwood ("OH") curve.
.cp_wykoff_m <- function(sp, dbh_cm) {
  h <- suppressWarnings(wykoff.ht(rep(sp, length(dbh_cm)), dbh_cm / 2.54))
  src <- rep("wykoff", length(h))
  if (anyNA(h)) {
    soft <- species_lookup(sp, "fvs", "softwood")
    alt <- if (!is.na(soft) && !soft) "OH" else "OS"
    h[is.na(h)] <- wykoff.ht(alt, dbh_cm[is.na(h)] / 2.54)
    src[] <- paste0("wykoff-", alt)
  }
  list(ht = h * 0.3048, src = src)
}

.cp_try <- function(expr) tryCatch(expr, error = function(e) NULL,
                                   warning = function(w) NULL)

# Fit eq. 1 for one species. Tries nlme (random a and b by plot, then random
# a only), then nls, then the linearised least-squares start values.
.cp_fit_eq1 <- function(dbh, ht, pid, random) {
  dd <- data.frame(dbh = dbh, ht = ht, pid = factor(pid),
                   bh = .cp_bh, k = .cp_k)
  y <- log(pmax(dd$ht - .cp_bh, 0.05)); x <- 1 / (dd$dbh + .cp_k)
  st <- unname(stats::coef(stats::lm(y ~ x)))
  st <- c(a = st[1], b = st[2])
  out <- function(a, b, re, method, sd_re = NA_real_)
    list(a = a, b = b, re = re, method = method, sd_re = sd_re)
  if (random && nlevels(dd$pid) >= 3 && requireNamespace("nlme", quietly = TRUE)) {
    for (rf in list(nlme::pdDiag(a + b ~ 1), stats::as.formula(a ~ 1))) {
      f <- .cp_try(nlme::nlme(ht ~ bh + exp(a + b / (dbh + k)), data = dd,
                              fixed = a + b ~ 1, random = rf, groups = ~pid,
                              start = st))
      if (!is.null(f)) {
        fe <- nlme::fixef(f); re <- nlme::ranef(f)
        re <- data.frame(pid = rownames(re),
                         a = if (!is.null(re$a)) re$a else 0,
                         b = if (!is.null(re$b)) re$b else 0,
                         stringsAsFactors = FALSE)
        return(out(fe[["a"]], fe[["b"]], re, "local-nlme", stats::sd(re$a)))
      }
    }
  }
  f <- .cp_try(stats::nls(ht ~ bh + exp(a + b / (dbh + k)), data = dd,
                          start = as.list(st)))
  if (!is.null(f)) { cf <- stats::coef(f); return(out(cf[["a"]], cf[["b"]], NULL, "local-nls")) }
  out(st[["a"]], st[["b"]], NULL, "local-linearized")
}

.cp_pred_eq1 <- function(f, dbh, pid) {
  a <- rep(f$a, length(dbh)); b <- rep(f$b, length(dbh))
  if (!is.null(f$re)) {
    j <- match(pid, f$re$pid); h <- !is.na(j)
    a[h] <- a[h] + f$re$a[j[h]]; b[h] <- b[h] + f$re$b[j[h]]
  }
  .cp_bh + exp(a + b / (dbh + .cp_k))
}

.cp_ht <- function(s, method, random, min_n, keep_measured) {
  pred <- rep(NA_real_, nrow(s)); src <- rep(NA_character_, nrow(s))
  rows <- list()
  for (g in sort(unique(s$.sp))) {
    i <- which(s$.sp == g)
    obs <- i[!is.na(s$.ht_obs[i])]
    m <- method; f <- NULL
    if (m == "local") {
      if (length(obs) >= min_n)
        f <- .cp_fit_eq1(s$.dbh[obs], s$.ht_obs[obs], s$.pid[obs], random)
      else m <- "wykoff"
    }
    if (m == "paper") {
      j <- match(toupper(g), .cp_paper$sp)
      if (is.na(j)) m <- "wykoff"
      else f <- list(a = .cp_paper$ht_a[j], b = .cp_paper$ht_b[j], re = NULL,
                     method = "paper", sd_re = NA_real_)
    }
    if (is.null(f)) {
      w <- .cp_wykoff_m(g, s$.dbh[i]); p <- w$ht; ms <- w$src[1]
      ab <- c(NA_real_, NA_real_)
    } else {
      p <- .cp_pred_eq1(f, s$.dbh[i], s$.pid[i]); ms <- f$method
      ab <- c(f$a, f$b)
    }
    pred[i] <- p; src[i] <- ms
    res <- if (length(obs)) s$.ht_obs[obs] - p[match(obs, i)] else numeric(0)
    rows[[g]] <- data.frame(species = g, method = ms, n_obs = length(obs),
                            n_plots = length(unique(s$.pid[obs])),
                            a = ab[1], b = ab[2],
                            sd_plot_a = if (is.null(f)) NA_real_ else f$sd_re,
                            rmse_m = if (length(res)) sqrt(mean(res^2)) else NA_real_,
                            bias_m = if (length(res)) mean(res) else NA_real_,
                            stringsAsFactors = FALSE)
  }
  if (keep_measured) {
    h <- !is.na(s$.ht_obs); pred[h] <- s$.ht_obs[h]; src[h] <- "measured"
  }
  s$.ht <- pred; s$.ht_src <- src
  attr(s, "ht_fit") <- do.call(rbind, c(rows, make.row.names = FALSE))
  s
}

## step 2: height to crown base -------------------------------------------------

# Eq. 2 written on the crown-ratio scale: CR = 1 - HCB/HT = a exp(-b DBH^c).
.cp_fit_eq2 <- function(dbh, cr, pid, random) {
  dd <- data.frame(dbh = dbh, cr = cr, pid = factor(pid))
  starts <- list(c(a = 0.6, b = 10, c = -2),
                 c(a = min(1, max(cr)), b = 1, c = -1),
                 c(a = mean(cr), b = 0.1, c = 0.5))
  lo <- c(0.01, 1e-8, -10); hi <- c(1.5, 1e4, 5)
  at_bound <- function(f) {
    cf <- stats::coef(f)
    any(abs(cf - lo) <= 1e-6 * pmax(1, abs(lo)) | abs(cf - hi) <= 1e-6 * pmax(1, abs(hi)))
  }
  f <- NULL
  for (st in starts) {
    f <- .cp_try(stats::nls(cr ~ a * exp(-b * dbh^c), data = dd, start = as.list(st),
                            algorithm = "port", lower = lo, upper = hi))
    if (!is.null(f) && at_bound(f)) f <- NULL   # not identified
    if (!is.null(f)) break
  }
  if (is.null(f))
    return(list(a = mean(cr), b = 0, c = 0, re = NULL, method = "local-mean",
                sd_re = NA_real_))
  cf <- stats::coef(f)
  if (random && nlevels(dd$pid) >= 3 && requireNamespace("nlme", quietly = TRUE)) {
    fm <- .cp_try(nlme::nlme(cr ~ a * exp(-b * dbh^c), data = dd,
                             fixed = a + b + c ~ 1, random = a ~ 1,
                             groups = ~pid, start = cf))
    if (!is.null(fm)) {
      fe <- nlme::fixef(fm); re <- nlme::ranef(fm)
      re <- data.frame(pid = rownames(re), a = re$a, stringsAsFactors = FALSE)
      return(list(a = fe[["a"]], b = fe[["b"]], c = fe[["c"]], re = re,
                  method = "local-nlme", sd_re = stats::sd(re$a)))
    }
  }
  list(a = cf[["a"]], b = cf[["b"]], c = cf[["c"]], re = NULL,
       method = "local-nls", sd_re = NA_real_)
}

.cp_pred_eq2 <- function(f, dbh, pid) {
  a <- rep(f$a, length(dbh))
  if (!is.null(f$re)) {
    j <- match(pid, f$re$pid); h <- !is.na(j); a[h] <- a[h] + f$re$a[j[h]]
  }
  a * exp(-f$b * dbh^f$c)
}

# median observed CR: plot x species, stand x species, species, all, default
.cp_cr_median <- function(s, i, cr_default) {
  v <- rep(NA_real_, length(i))
  for (key in list(paste(s$.pid, s$.sp), paste(s$.stand, s$.sp), s$.sp,
                   rep("all", nrow(s)))) {
    m <- tapply(s$.cr_obs, key, stats::median, na.rm = TRUE)
    fill <- is.na(v); v[fill] <- m[key[i][fill]]
  }
  v[is.na(v)] <- cr_default
  v
}

.cp_hcb <- function(s, method, random, min_n, keep_measured, cr_default) {
  if (is.null(s$.ht) || anyNA(s$.ht)) stop("crownprint: complete heights first (cp_height)")
  cr <- rep(NA_real_, nrow(s)); src <- rep(NA_character_, nrow(s))
  rows <- list()
  for (g in sort(unique(s$.sp))) {
    i <- which(s$.sp == g)
    obs <- i[!is.na(s$.cr_obs[i])]
    m <- method; f <- NULL
    if (m == "local") {
      if (length(obs) >= min_n)
        f <- .cp_fit_eq2(s$.dbh[obs], s$.cr_obs[obs], s$.pid[obs], random)
      else m <- "paper"
    }
    if (m == "paper") {
      j <- match(toupper(g), .cp_paper$sp)
      if (is.na(j)) m <- "observed"
      else f <- list(a = .cp_paper$cr_a[j], b = .cp_paper$cr_b[j],
                     c = .cp_paper$cr_c[j], re = NULL, method = "paper",
                     sd_re = NA_real_)
    }
    if (is.null(f)) {
      p <- .cp_cr_median(s, i, cr_default); ms <- "observed-median"
      abc <- rep(NA_real_, 3)
    } else {
      p <- .cp_pred_eq2(f, s$.dbh[i], s$.pid[i]); ms <- f$method
      abc <- c(f$a, f$b, f$c)
    }
    cr[i] <- p; src[i] <- ms
    res <- if (length(obs)) s$.cr_obs[obs] - p[match(obs, i)] else numeric(0)
    rows[[g]] <- data.frame(species = g, method = ms, n_obs = length(obs),
                            n_plots = length(unique(s$.pid[obs])),
                            a = abc[1], b = abc[2], c = abc[3],
                            sd_plot_a = if (is.null(f)) NA_real_ else f$sd_re,
                            rmse_cr = if (length(res)) sqrt(mean(res^2)) else NA_real_,
                            bias_cr = if (length(res)) mean(res) else NA_real_,
                            stringsAsFactors = FALSE)
  }
  cr <- pmin(pmax(cr, 0.01), 1)
  hcb <- s$.ht * (1 - cr)
  if (keep_measured) {
    h1 <- !is.na(s$.cr_obs)
    hcb[h1] <- s$.ht[h1] * (1 - s$.cr_obs[h1]); src[h1] <- "measured"
    h2 <- !h1 & !is.na(s$.hcb_obs) & s$.hcb_obs < s$.ht   # HCB measured, HT not
    hcb[h2] <- s$.hcb_obs[h2]; src[h2] <- "measured"
  }
  s$.hcb <- hcb; s$.hcb_src <- src
  attr(s, "hcb_fit") <- do.call(rbind, c(rows, make.row.names = FALSE))
  s
}

## step 3: canopy space and occupancy -------------------------------------------

.cp_prof <- function(s, step) {
  if (is.null(s$.ht) || is.null(s$.hcb) || anyNA(s$.ht) || anyNA(s$.hcb))
    stop("crownprint: complete HT and HCB first (cp_height, cp_hcb)")
  K <- round(1 / step)
  cs <- step * seq_len(K)
  top <- tapply(s$.ht, s$.pid, max); base <- tapply(s$.hcb, s$.pid, min)
  rng <- top - base
  flat <- names(rng)[rng <= 0]
  if (length(flat)) {
    message("crownprint: skipping plots with no vertical canopy range: ",
            paste(flat, collapse = ", "))
    s <- s[!s$.pid %in% flat, , drop = FALSE]
  }
  b <- base[s$.pid]; r <- rng[s$.pid]
  s$.cb <- unname((s$.hcb - b) / r); s$.ct <- unname((s$.ht - b) / r)
  tol <- 1e-9
  ind <- outer(s$.cb, cs, function(lo, x) x >= lo - tol) &
         outer(s$.ct, cs, function(hi, x) x <= hi + tol)
  key <- paste(s$.pid, s$.grp, sep = "\r")
  occ <- rowsum(ind * s$.w, key, reorder = TRUE)
  kk <- strsplit(rownames(occ), "\r", fixed = TRUE)
  pid <- vapply(kk, `[`, "", 1); grp <- vapply(kk, `[`, "", 2)
  first <- s[match(pid, s$.pid), ]
  prof <- data.frame(stand = rep(first$.stand, each = K),
                     plot = rep(first$.plot, each = K),
                     pid = rep(pid, each = K), group = rep(grp, each = K),
                     i = rep(seq_len(K), length(pid)),
                     cs = rep(cs, length(pid)),
                     occupancy = as.vector(t(occ)), stringsAsFactors = FALSE)
  cf <- attr(s, "cf")
  pl <- s[!duplicated(s$.pid), c(".pid", ".stand", ".plot", ".by")]
  pl$top_ht <- unname(top[pl$.pid]) / cf[["h"]]
  pl$base_hcb <- unname(base[pl$.pid]) / cf[["h"]]
  pl$n_trees <- as.vector(table(s$.pid)[pl$.pid])
  names(pl)[1:4] <- c("pid", "stand", "plot", "by")
  list(profile = prof, crowns = s, ind = ind, plots = pl, cs = cs, step = step)
}

## step 4: beta fits ---------------------------------------------------------

# Weighted beta MLE on the profile: the sufficient statistics are the
# occupancy-weighted means of log(x) and log(1 - x).
.cp_beta_mle <- function(x, f) {
  f <- f / sum(f)
  lx <- sum(f * log(x)); l1 <- sum(f * log1p(-x))
  m <- sum(f * x); v <- sum(f * (x - m)^2)
  phi <- if (v > 0) m * (1 - m) / v - 1 else 50
  if (!is.finite(phi) || phi <= 0) phi <- 2
  nll <- function(p) { a <- exp(p[1]); b <- exp(p[2])
    lbeta(a, b) - (a - 1) * lx - (b - 1) * l1 }
  gr <- function(p) { a <- exp(p[1]); b <- exp(p[2]); dab <- digamma(a + b)
    c(a * (digamma(a) - dab - lx), b * (digamma(b) - dab - l1)) }
  o <- stats::optim(log(c(m * phi, (1 - m) * phi)), nll, gr, method = "BFGS",
                    control = list(maxit = 500))
  c(alpha = exp(o$par[1]), beta = exp(o$par[2]), conv = o$convergence)
}

.cp_beta_moments <- function(a, b) {
  mu <- a / (a + b)
  data.frame(mean_pos = mu,
             mode_pos = ifelse(a > 1 & b > 1, (a - 1) / (a + b - 2), NA_real_),
             sd_pos = sqrt(a * b / ((a + b)^2 * (a + b + 1))))
}

.cp_betas <- function(pr, nudge, boot, level) {
  s <- pr$crowns; ind <- pr$ind
  x <- pmin(pmax(pr$cs, nudge), 1 - nudge)
  key <- paste(s$.pid, s$.grp, sep = "\r")
  totals <- tapply(s$.w, s$.pid, sum)
  q <- (1 - level) / 2
  out <- lapply(split(seq_len(nrow(s)), key), function(j) {
    W <- ind[j, , drop = FALSE] * s$.w[j]
    occ <- colSums(W)
    r <- data.frame(stand = s$.stand[j[1]], plot = s$.plot[j[1]], pid = s$.pid[j[1]],
                    by = s$.by[j[1]], group = s$.grp[j[1]], n_trees = length(j),
                    density = sum(s$.w[j]),
                    density_share = sum(s$.w[j]) / totals[[s$.pid[j[1]]]],
                    alpha = NA_real_, beta = NA_real_, alpha_se = NA_real_,
                    beta_se = NA_real_, log_alpha_se = NA_real_,
                    log_beta_se = NA_real_, alpha_lo = NA_real_, alpha_hi = NA_real_,
                    beta_lo = NA_real_, beta_hi = NA_real_, converged = NA,
                    stringsAsFactors = FALSE)
    if (sum(occ) <= 0) return(r)
    fit <- .cp_beta_mle(x, occ)
    r$alpha <- fit[["alpha"]]; r$beta <- fit[["beta"]]; r$converged <- fit[["conv"]] == 0
    if (boot > 0 && length(j) >= 2) {
      bb <- t(vapply(seq_len(boot), function(k) {
        idx <- sample.int(length(j), replace = TRUE)
        o <- colSums(W[idx, , drop = FALSE])
        if (sum(o) <= 0) return(c(NA_real_, NA_real_))
        .cp_beta_mle(x, o)[1:2]
      }, numeric(2)))
      bb <- bb[stats::complete.cases(bb), , drop = FALSE]
      if (nrow(bb) >= 10) {
        r$alpha_se <- stats::sd(bb[, 1]); r$beta_se <- stats::sd(bb[, 2])
        r$log_alpha_se <- stats::sd(log(bb[, 1])); r$log_beta_se <- stats::sd(log(bb[, 2]))
        r$alpha_lo <- stats::quantile(bb[, 1], q, names = FALSE)
        r$alpha_hi <- stats::quantile(bb[, 1], 1 - q, names = FALSE)
        r$beta_lo <- stats::quantile(bb[, 2], q, names = FALSE)
        r$beta_hi <- stats::quantile(bb[, 2], 1 - q, names = FALSE)
      }
    }
    r
  })
  b <- do.call(rbind, c(out, make.row.names = FALSE))
  b <- cbind(b, .cp_beta_moments(b$alpha, b$beta))
  b$log_alpha <- log(b$alpha); b$log_beta <- log(b$beta)
  b[order(b$pid, b$group), ]
}

## step 5: group summaries -----------------------------------------------------

.cp_summ <- function(b, random, level, min_trees) {
  b <- b[!is.na(b$alpha) & b$n_trees >= min_trees, , drop = FALSE]
  if (!nrow(b)) stop("crownprint: no plot x group beta fits to summarise")
  b$cell <- paste(b$group, b$by, sep = "\r")
  b$rand <- if (is.null(random)) "all" else as.character(b[[random]])
  b$log_ratio <- b$log_alpha - b$log_beta
  z <- stats::qnorm(1 - (1 - level) / 2)
  use_lme <- !is.null(random) && length(unique(b$rand)) >= 3 &&
             requireNamespace("nlme", quietly = TRUE)
  est <- function(y, boot_se) {
    cells <- sort(unique(b$cell))
    if (use_lme) {
      dd <- data.frame(y = b[[y]], cell = factor(b$cell, levels = cells), rand = b$rand)
      f <- if (length(cells) == 1)
        .cp_try(nlme::lme(y ~ 1, random = ~1 | rand, data = dd))
      else .cp_try(nlme::lme(y ~ 0 + cell, random = ~1 | rand, data = dd))
      if (!is.null(f)) {
        tt <- summary(f)$tTable
        return(data.frame(cell = cells, est = tt[, "Value"], se = tt[, "Std.Error"],
                          method = "lme", stringsAsFactors = FALSE))
      }
    }
    do.call(rbind, lapply(cells, function(cc) {
      v <- b[[y]][b$cell == cc]
      se <- if (length(v) >= 2) stats::sd(v) / sqrt(length(v))
            else if (!is.null(boot_se)) b[[boot_se]][b$cell == cc] else NA_real_
      data.frame(cell = cc, est = mean(v), se = se,
                 method = if (length(v) >= 2) "plot-mean" else "bootstrap",
                 stringsAsFactors = FALSE)
    }))
  }
  la <- est("log_alpha", "log_alpha_se"); lb <- est("log_beta", "log_beta_se")
  lr <- est("log_ratio", NULL); mp <- est("mean_pos", NULL)
  kk <- strsplit(la$cell, "\r", fixed = TRUE)
  g <- data.frame(group = vapply(kk, `[`, "", 1), by = vapply(kk, `[`, "", 2),
                  n_plots = as.vector(table(b$cell)[la$cell]),
                  n_trees = as.vector(tapply(b$n_trees, b$cell, sum)[la$cell]),
                  n_stands = as.vector(tapply(b$stand, b$cell,
                                              function(v) length(unique(v)))[la$cell]),
                  log_alpha = la$est, log_alpha_se = la$se,
                  log_beta = lb$est, log_beta_se = lb$se,
                  stringsAsFactors = FALSE)
  g$alpha <- exp(g$log_alpha); g$beta <- exp(g$log_beta)
  g$alpha_lo <- exp(g$log_alpha - z * g$log_alpha_se)
  g$alpha_hi <- exp(g$log_alpha + z * g$log_alpha_se)
  g$beta_lo <- exp(g$log_beta - z * g$log_beta_se)
  g$beta_hi <- exp(g$log_beta + z * g$log_beta_se)
  g$log_ratio <- lr$est; g$log_ratio_se <- lr$se
  g$p_skew <- 2 * stats::pnorm(-abs(g$log_ratio / g$log_ratio_se))
  g <- cbind(g, .cp_beta_moments(g$alpha, g$beta))
  g$mean_pos_plots <- mp$est; g$mean_pos_plots_se <- mp$se
  g$method <- la$method
  g <- g[order(g$by, -g$mean_pos), ]
  rownames(g) <- NULL

  # paired between-group comparisons on plots where both groups occur
  pw <- list()
  for (lv in unique(g$by)) {
    gg <- g$group[g$by == lv]
    if (length(gg) < 2) next
    for (p in utils::combn(gg, 2, simplify = FALSE)) {
      A <- b[b$by == lv & b$group == p[1], c("pid", "mean_pos")]
      B <- b[b$by == lv & b$group == p[2], c("pid", "mean_pos")]
      m <- merge(A, B, by = "pid")
      dlt <- m$mean_pos.x - m$mean_pos.y
      n <- length(dlt)
      se <- if (n >= 2) stats::sd(dlt) / sqrt(n) else NA_real_
      ga <- g[g$by == lv & g$group == p[1], ]; gb <- g[g$by == lv & g$group == p[2], ]
      xx <- (seq_len(1000) - 0.5) / 1000
      ovl <- mean(pmin(stats::dbeta(xx, ga$alpha, ga$beta), stats::dbeta(xx, gb$alpha, gb$beta)))
      pw[[length(pw) + 1]] <- data.frame(
        by = lv, group_1 = p[1], group_2 = p[2], n_shared_plots = n,
        diff_mean_pos = if (n) mean(dlt) else NA_real_, diff_se = se,
        p_value = if (n >= 2 && se > 0) 2 * stats::pt(-abs(mean(dlt) / se), n - 1) else NA_real_,
        overlap = ovl, stringsAsFactors = FALSE)
    }
  }
  pw <- if (length(pw)) do.call(rbind, pw) else
    data.frame(by = character(), group_1 = character(), group_2 = character(),
               n_shared_plots = integer(), diff_mean_pos = numeric(),
               diff_se = numeric(), p_value = numeric(), overlap = numeric())
  list(groups = g, pairwise = pw, level = level,
       random = if (use_lme) random else NULL)
}

## step 6: interpretation ------------------------------------------------------

#' Interpret CrownPrint beta estimates
#'
#' Turns group-level alpha / beta estimates into plain-language statements:
#' where in canopy space each group's crowns are concentrated, how spread
#' they are, and how groups differ. Following Premer (2026), alpha > beta
#' means occupancy concentrated toward the upper canopy, alpha < beta toward
#' the lower canopy, and alpha = beta a profile symmetric about mid-canopy.
#' The spread and overlap class boundaries are descriptive conventions of
#' this function, not values from the paper; change them with `spread_cut`
#' and `overlap_cut`.
#'
#' @param x a [crownprint()] result or the list returned by [cp_summary()]
#' @param sig significance level for the skew (log alpha/beta) and paired
#'   between-group tests
#' @param spread_cut SD of canopy position below which a profile is called a
#'   "narrow band" and above which it is "broadly spread" (a uniform profile
#'   has SD 0.289)
#' @param overlap_cut overlap of two groups' fitted profiles above which they
#'   "largely share" vertical space and below which they are "strongly
#'   stratified"
#' @param pairs which pairwise comparisons to describe: "significant"
#'   (paired test p < `sig`), "shared" (all pairs sharing at least
#'   `min_shared` plots) or "none". The full table is always in
#'   `summary$pairwise`.
#' @param min_shared minimum shared plots for a pair to be described
#' @return character vector, one statement per element
#' @export
cp_interpret <- function(x, sig = 0.05, spread_cut = c(0.15, 0.22),
                         overlap_cut = c(0.5, 0.8),
                         pairs = c("significant", "shared", "none"),
                         min_shared = 2) {
  pairs <- match.arg(pairs)
  sm <- if (inherits(x, "crownprint")) x$summary else x
  g <- sm$groups; pw <- sm$pairwise
  pct <- round(100 * sm$level)
  f2 <- function(v) formatC(v, format = "f", digits = 2)
  fp <- function(p) if (is.na(p)) "p = NA" else if (p < 0.001) "p < 0.001" else paste("p =", formatC(p, format = "f", digits = 3))
  out <- character()
  for (lv in unique(g$by)) {
    if (length(unique(g$by)) > 1) out <- c(out, paste0("[", lv, "]"))
    gl <- g[g$by == lv, ]
    for (k in seq_len(nrow(gl))) {
      r <- gl[k, ]
      skew <- if (is.na(r$p_skew) || r$p_skew >= sig)
        "alpha and beta are not distinguishable (alpha ~ beta): occupancy is roughly symmetric about mid-canopy"
      else if (r$log_ratio > 0)
        "alpha > beta: crown occupancy is concentrated toward the upper canopy"
      else "alpha < beta: crown occupancy is concentrated toward the lower canopy"
      spread <- if (r$sd_pos < spread_cut[1]) "a narrow vertical band"
                else if (r$sd_pos > spread_cut[2]) "broadly spread through the canopy"
                else "moderately concentrated"
      out <- c(out, paste0(
        r$group, " (", r$n_plots, " plots, ", r$n_trees, " trees): alpha = ",
        f2(r$alpha), " [", f2(r$alpha_lo), "-", f2(r$alpha_hi), "], beta = ",
        f2(r$beta), " [", f2(r$beta_lo), "-", f2(r$beta_hi), "] (", pct, "% CI). ",
        skew, " (log(alpha/beta) = ", f2(r$log_ratio), " +/- ", f2(r$log_ratio_se),
        ", ", fp(r$p_skew), "). Mean position ", f2(r$mean_pos), " of canopy space",
        if (!is.na(r$mode_pos)) paste0(", peak at ", f2(r$mode_pos)) else "",
        "; SD ", f2(r$sd_pos), " (", spread, ").",
        if (r$n_plots < 3) " Few plots: treat as tentative." else ""))
    }
    pl <- pw[pw$by == lv & pw$n_shared_plots >= min_shared, ]
    sg <- !is.na(pl$p_value) & pl$p_value < sig
    if (nrow(pl) && pairs != "none")
      out <- c(out, paste0("Paired comparisons: ", sum(sg), " of ", nrow(pl),
                           " group pairs sharing >= ", min_shared,
                           " plots differ in mean canopy position at p < ", sig,
                           if (pairs == "significant" && sum(sg)) ":" else "."))
    if (pairs == "significant") pl <- pl[sg, , drop = FALSE]
    if (pairs == "none") pl <- pl[0, , drop = FALSE]
    for (k in seq_len(nrow(pl))) {
      r <- pl[k, ]
      hi <- if (r$diff_mean_pos >= 0) r$group_1 else r$group_2
      lo <- if (r$diff_mean_pos >= 0) r$group_2 else r$group_1
      ov <- if (r$overlap > overlap_cut[2]) "largely share the same vertical space"
            else if (r$overlap < overlap_cut[1]) "are strongly stratified"
            else "are partially stratified"
      sigtxt <- if (!is.na(r$p_value) && r$p_value < sig) "" else " (not significant)"
      out <- c(out, paste0(
        hi, " sits higher than ", lo, " by ", f2(abs(r$diff_mean_pos)),
        " canopy-space units on the ", r$n_shared_plots, " shared plot(s)",
        if (!is.na(r$diff_se)) paste0(" (SE ", f2(r$diff_se), ", ", fp(r$p_value), ")") else "",
        sigtxt, "; profile overlap ", f2(r$overlap), ", so the two ", ov, "."))
    }
  }
  c(out, paste("Note: occupancy is a stand-scaled index of where crowns are,",
               "not leaf area; HCB is a proxy for the lower limit of active foliage (Premer 2026)."))
}

## exported step functions -------------------------------------------------

.cp_back <- function(trees, s, add) {
  d <- as.data.frame(trees, stringsAsFactors = FALSE)
  for (nm in names(add)) {
    v <- rep(if (is.character(add[[nm]])) NA_character_ else NA_real_, nrow(d))
    v[s$.row] <- add[[nm]]
    d[[nm]] <- v
  }
  d
}

#' Complete tree heights for CrownPrint (step 1)
#'
#' @param trees tree list, one row per tree (e.g. from [read_inventory()])
#' @param method "wykoff" (FVS-NE Wykoff equation, [wykoff.ht()]; species
#'   without NE coefficients use the generic "OS"/"OH" curve), "local" (fit
#'   eq. 1 per species to your measured heights; species with fewer than
#'   `min_n` heights fall back to "wykoff"), or "paper" (Premer 2026 Table 4
#'   coefficients for DF, WH, BF, RS; other species fall back to "wykoff")
#' @param cols named list mapping roles to column names (matched
#'   case-insensitively): `stand`, `plot`, `species`, `dbh`, `ht`, `hcb`,
#'   `cr`, `weight` (expansion factor). Defaults follow [read_inventory()]
#'   output: `stand`, `plot`, `fvs`, `dbh`, `ht`, `cr`, `tpa`. Rows flagged
#'   `ht_imputed` / `cr_imputed`, and crown ratios of sapling-tally rows
#'   (`source == "sapling"`), are treated as unmeasured.
#' @param units "imperial" (DBH in., heights ft) or "metric" (cm, m). Models
#'   are fit in metric internally; outputs are in the input units.
#' @param random fit plot random effects (nlme) for local fits when at least
#'   3 plots have data; falls back to nls if nlme fails
#' @param min_n minimum measured trees per species for a local fit
#' @param keep_measured keep measured values instead of predictions
#' @return `trees` with `ht_cp` (completed height) and `ht_source`; attribute
#'   `ht_fit` holds per-species coefficients, RMSE and bias (m) against the
#'   measured heights
#' @export
cp_height <- function(trees, method = c("wykoff", "local", "paper"), cols = list(),
                      units = c("imperial", "metric"), random = TRUE,
                      min_n = 10, keep_measured = TRUE) {
  method <- match.arg(method); units <- match.arg(units)
  s <- .cp_ht(.cp_std(trees, cols, units), method, random, min_n, keep_measured)
  cf <- attr(s, "cf")
  out <- .cp_back(trees, s, list(ht_cp = s$.ht / cf[["h"]], ht_source = s$.ht_src))
  attr(out, "ht_fit") <- attr(s, "ht_fit")
  out
}

#' Complete height to live crown base for CrownPrint (step 2)
#'
#' Eq. 2 of Premer (2026), `HCB = HT (1 - a exp(-b DBH^c))`, is fit on the
#' equivalent crown-ratio scale, `CR = a exp(-b DBH^c)` (DBH in cm), so trees
#' with a crown ratio but no measured height still contribute.
#'
#' @inheritParams cp_height
#' @param trees output of [cp_height()] (needs `ht_cp`)
#' @param method "local" (fit eq. 2 per species with nls, plus a random plot
#'   effect on `a` when `random`; species with fewer than `min_n` crown
#'   records fall back to "paper", then "observed"; if the three-parameter
#'   fit fails or lands on a parameter bound, the species' mean observed
#'   crown ratio is used, reported as "local-mean"), "paper" (Table 5
#'   coefficients for DF, WH, BF, RS; others fall back to "observed"), or
#'   "observed" (median observed crown ratio by plot x species, then stand x
#'   species, species, all trees, then `cr_default`)
#' @param cr_default crown ratio used when nothing else is available
#' @return `trees` with `hcb_cp`, `cr_cp` and `hcb_source`; attribute
#'   `hcb_fit` holds per-species coefficients and crown-ratio RMSE / bias
#' @export
cp_hcb <- function(trees, method = c("local", "paper", "observed"), cols = list(),
                   units = c("imperial", "metric"), random = TRUE, min_n = 10,
                   keep_measured = TRUE, cr_default = 0.4) {
  method <- match.arg(method); units <- match.arg(units)
  s <- .cp_std(trees, cols, units)
  if (is.null(s$.ht)) stop("cp_hcb: run cp_height() first (no ht_cp column)")
  s <- .cp_hcb(s, method, random, min_n, keep_measured, cr_default)
  cf <- attr(s, "cf")
  out <- .cp_back(trees, s, list(hcb_cp = s$.hcb / cf[["h"]],
                                 cr_cp = 1 - s$.hcb / s$.ht,
                                 hcb_source = s$.hcb_src))
  attr(out, "hcb_fit") <- attr(s, "hcb_fit")
  out
}

#' Build normalized canopy space and crown occupancy profiles (step 3)
#'
#' For each plot, canopy space runs from the lowest live crown base (0) to
#' the tallest tree (1) in steps of `step` (eq. 3). Each tree's crown is the
#' interval from its HCB to its tip in that space; occupancy at level
#' `Cs(i)` is the sum of expansion factors of the group's crowns that span it
#' (eq. 4).
#'
#' @inheritParams cp_height
#' @param trees output of [cp_hcb()] (needs `ht_cp` and `hcb_cp`)
#' @param group NULL (species), a column name, or a named vector mapping
#'   species codes to groups (unmapped species become "Other"), e.g.
#'   `c(RS = "spruce-fir", BF = "spruce-fir", RM = "hardwood")`
#' @param by optional column (e.g. treatment) carried to the plot level and
#'   used to split the summaries
#' @param step canopy-space resolution (0.01 = 100 layers)
#' @return object of class `cp_profile`: `profile` (long data frame of
#'   occupancy by plot, group and layer), `plots` (plot canopy limits in input
#'   height units), and internals used by [cp_beta()]
#' @export
cp_profile <- function(trees, cols = list(), units = c("imperial", "metric"),
                       group = NULL, by = NULL, step = 0.01) {
  units <- match.arg(units)
  s <- .cp_std(trees, cols, units, group, by)
  if (is.null(s$.ht) || is.null(s$.hcb))
    stop("cp_profile: run cp_height() and cp_hcb() first (needs ht_cp, hcb_cp)")
  structure(.cp_prof(s, step), class = "cp_profile")
}

#' Fit beta distributions to crown occupancy profiles (step 4)
#'
#' Fits `f(x) = x^(alpha-1) (1-x)^(beta-1) / B(alpha, beta)` (eq. 5) to each
#' plot x group profile by occupancy-weighted maximum likelihood. Layers at
#' 0 or 1 are nudged inward by `nudge`. Standard errors and percentile
#' intervals come from resampling the group's trees within the plot (canopy
#' limits held fixed); they are NA for groups with one tree.
#'
#' @param profile a `cp_profile` from [cp_profile()]
#' @param nudge distance layers at the canopy limits are moved inward
#' @param boot bootstrap replicates (0 to skip)
#' @param level confidence level for intervals
#' @param seed optional RNG seed for the bootstrap
#' @return data frame, one row per plot x group: tree count, density and
#'   share, `alpha`, `beta`, their SEs (also on the log scale) and intervals,
#'   and the implied mean, mode and SD of canopy position
#' @export
cp_beta <- function(profile, nudge = 0.005, boot = 200, level = 0.95, seed = NULL) {
  if (!inherits(profile, "cp_profile")) stop("cp_beta: needs a cp_profile")
  if (!is.null(seed)) set.seed(seed)
  .cp_betas(profile, nudge, boot, level)
}

#' Summarise CrownPrint beta parameters by group (step 5)
#'
#' Group estimates are made on the log scale, as in Premer (2026) eq. 6:
#' with `random` given and at least 3 levels, `lme(log(alpha) ~ 0 + group,
#' random = ~1 | random)` (nlme); otherwise the mean across plots with
#' SE = SD / sqrt(n), or the plot's bootstrap SE when a group occurs on a
#' single plot. Estimates are back-transformed with log-scale intervals.
#' Groups are also compared pairwise on the plots they share (paired
#' difference in mean canopy position) and by the overlap of their fitted
#' profiles.
#'
#' @param betas output of [cp_beta()]
#' @param random column of `betas` holding the random grouping, usually
#'   "stand"; NULL for none
#' @param level confidence level
#' @param min_trees drop plot x group fits based on fewer trees
#' @return list with `groups` (one row per group x `by` level) and `pairwise`
#' @export
cp_summary <- function(betas, random = "stand", level = 0.95, min_trees = 1) {
  .cp_summ(betas, random, level, min_trees)
}

## wrapper --------------------------------------------------------------------

#' CrownPrint: quantify vertical canopy structure from a plot inventory
#'
#' Runs the full workflow of Premer (2026): complete heights and crown bases,
#' build normalized canopy space for each plot, tally crown occupancy by
#' group in 0.01 layers, fit beta distributions, summarise alpha and beta
#' with standard errors, and interpret the result. See the step functions
#' ([cp_height()], [cp_hcb()], [cp_profile()], [cp_beta()],
#' [cp_summary()], [cp_interpret()]) for details.
#'
#' @inheritParams cp_height
#' @inheritParams cp_profile
#' @inheritParams cp_beta
#' @param trees tree list, one row per tree with plot, species, DBH and an
#'   expansion factor; measured heights and crown ratios / HCB on a subsample
#' @param ht_method,hcb_method passed to [cp_height()] / [cp_hcb()] `method`
#' @param random_effect column used as the random effect in [cp_summary()];
#'   "auto" uses stand when there are at least 3 stands
#' @param cr_default see [cp_hcb()]
#' @param min_trees see [cp_summary()]
#' @return object of class `crownprint`: `trees` (completed tree list),
#'   `ht_fit`, `hcb_fit`, `profile`, `plots`, `betas` (plot x group),
#'   `summary` (`groups`, `pairwise`), `interpretation`, `settings`. Has
#'   `print`, `summary` and `plot` methods.
#' @references Premer, M.I. 2026. CrownPrint: a crown-profile approach to
#'   quantifying canopy structure. Can. J. For. Res. 56: 1-14.
#'   doi:10.1139/cjfr-2025-0239
#' @examples
#' tl <- read_inventory(maine_cruise, cols = list(stand = "Stand", plot = "Plot",
#'                      species = "Species", dbh = "DBH", ht = "Ht", cr = "CR"))
#' sf <- tl[tl$stand == "SF-1", ]
#' cp <- crownprint(sf, group = c(RS = "RS", BF = "BF"), boot = 50, seed = 1)
#' cp
#' plot(cp)
#' @export
crownprint <- function(trees, cols = list(), units = c("imperial", "metric"),
                       ht_method = c("wykoff", "local", "paper"),
                       hcb_method = c("local", "paper", "observed"),
                       group = NULL, by = NULL, random_effect = "auto",
                       step = 0.01, nudge = 0.005, boot = 200, level = 0.95,
                       seed = NULL, random = TRUE, min_n = 10,
                       keep_measured = TRUE, cr_default = 0.4, min_trees = 1) {
  units <- match.arg(units); ht_method <- match.arg(ht_method)
  hcb_method <- match.arg(hcb_method)
  d <- as.data.frame(trees, stringsAsFactors = FALSE)
  d$ht_cp <- NULL; d$hcb_cp <- NULL
  s <- .cp_std(d, cols, units, group, by)
  s <- .cp_ht(s, ht_method, random, min_n, keep_measured)
  s <- .cp_hcb(s, hcb_method, random, min_n, keep_measured, cr_default)
  pr <- .cp_prof(s, step)
  if (!is.null(seed)) set.seed(seed)
  b <- .cp_betas(pr, nudge, boot, level)
  re <- if (identical(random_effect, "auto"))
    (if (length(unique(b$stand)) >= 3) "stand" else NULL) else random_effect
  sm <- .cp_summ(b, re, level, min_trees)
  cf <- attr(s, "cf")
  tr <- .cp_back(d, s, list(ht_cp = s$.ht / cf[["h"]], ht_source = s$.ht_src,
                            hcb_cp = s$.hcb / cf[["h"]],
                            cr_cp = 1 - s$.hcb / s$.ht, hcb_source = s$.hcb_src,
                            cp_group = s$.grp))
  res <- list(trees = tr, ht_fit = attr(s, "ht_fit"), hcb_fit = attr(s, "hcb_fit"),
              profile = pr$profile, plots = pr$plots, betas = b, summary = sm,
              settings = list(units = units, ht_method = ht_method,
                              hcb_method = hcb_method, step = step, nudge = nudge,
                              boot = boot, level = level, random_effect = re,
                              by = by,
                              share_ht_measured = mean(s$.ht_src == "measured"),
                              share_hcb_measured = mean(s$.hcb_src == "measured")))
  res$interpretation <- cp_interpret(sm)
  class(res) <- "crownprint"
  res
}

#' @export
print.crownprint <- function(x, ...) {
  st <- x$settings
  cat("CrownPrint canopy profile (Premer 2026)\n")
  cat(sprintf("  %d plots, %d trees; layer step %.2f\n", nrow(x$plots),
              sum(x$plots$n_trees), st$step))
  cat(sprintf("  HT: %s (%.0f%% measured); HCB: %s (%.0f%% measured)\n",
              st$ht_method, 100 * st$share_ht_measured, st$hcb_method,
              100 * st$share_hcb_measured))
  cat(sprintf("  Group estimates: %s\n\n", if (is.null(x$summary$random))
    "mean of plot log(alpha), log(beta)" else
      paste0("lme on log scale, random = ~1|", x$summary$random)))
  g <- x$summary$groups
  show <- g[, c("by", "group", "n_plots", "n_trees", "alpha", "alpha_lo", "alpha_hi",
                "beta", "beta_lo", "beta_hi", "mean_pos", "sd_pos", "p_skew")]
  if (length(unique(show$by)) == 1) show$by <- NULL
  num <- vapply(show, is.numeric, TRUE)
  show[num] <- lapply(show[num], round, 3)
  print(show, row.names = FALSE)
  cat("\n"); cat(strwrap(x$interpretation, exdent = 2), sep = "\n")
  invisible(x)
}

#' @export
summary.crownprint <- function(object, ...) object$summary

#' Plot CrownPrint profiles
#'
#' Fitted beta densities (x) over canopy space (y), as in Premer (2026)
#' Figs. 5-6: one curve per group from the group-level estimates, with
#' optional thin curves for each plot.
#'
#' @param x a [crownprint()] result
#' @param plots also draw each plot x group fit
#' @param col colours, one per group
#' @param ... passed to [graphics::plot()]
#' @return `x`, invisibly
#' @export
plot.crownprint <- function(x, plots = FALSE, col = NULL, ...) {
  g <- x$summary$groups; b <- x$betas
  lv <- unique(g$by); grp <- unique(g$group)
  if (is.null(col)) col <- grDevices::hcl.colors(max(3, length(grp)), "Dark 3")[seq_along(grp)]
  names(col) <- grp
  yy <- (seq_len(199)) / 200
  op <- graphics::par(mfrow = grDevices::n2mfrow(length(lv)))
  on.exit(graphics::par(op))
  for (l in lv) {
    gl <- g[g$by == l, ]
    dens <- vapply(seq_len(nrow(gl)), function(k) stats::dbeta(yy, gl$alpha[k], gl$beta[k]),
                   numeric(length(yy)))
    xmax <- max(dens[is.finite(dens)], 1)
    graphics::plot(NA, xlim = c(0, xmax * 1.05), ylim = c(0, 1),
                   xlab = "Crown point density", ylab = "Canopy space (0-1)",
                   main = if (length(lv) > 1) l else "", ...)
    if (plots) {
      bl <- b[b$by == l & !is.na(b$alpha), ]
      for (k in seq_len(nrow(bl)))
        graphics::lines(stats::dbeta(yy, bl$alpha[k], bl$beta[k]), yy,
                        col = grDevices::adjustcolor(col[bl$group[k]], 0.25))
    }
    for (k in seq_len(nrow(gl)))
      graphics::lines(dens[, k], yy, col = col[gl$group[k]], lwd = 2.5)
    graphics::legend("topright", legend = gl$group, col = col[gl$group], lwd = 2.5,
                     bty = "n")
  }
  invisible(x)
}
