# CrownPrint forester workflow: view a stand, plan the strata, get a DBH x species
# marking guide and a one-page sheet.
#   cp_view_stand() -> cp_plan() -> cp_dbh_guide() -> cp_guide_sheet()   (or cp_workflow())
# Inputs follow read_inventory() / read_demeritt_inventory(): stand, fvs, dbh (in),
# tpa, ht (ft), cr (fraction). Heights may be imputed (cp_height()).

.cp_ba <- function(dbh, tpa) 0.005454154 * dbh^2 * tpa

#' Default species groups for crown-profile marking guides
#'
#' Maps FVS species codes to the groups used by the forester workflow
#' ([cp_view_stand()]): White pine, Hemlock, Spruce-fir, Cedar, Red maple,
#' Tolerant hardwood, Aspen-birch, Oak, Other.
#' @param fvs character vector of FVS alpha codes
#' @return character vector of group labels
#' @examples
#' cp_species_group(c("WP", "RS", "SM", "RM", "QA", "XX"))
#' @export
cp_species_group <- function(fvs) {
  out <- rep("Other", length(fvs))
  out[fvs == "WP"] <- "White pine"; out[fvs == "EH"] <- "Hemlock"
  out[fvs %in% c("RS", "BF", "WS", "BS", "NS")] <- "Spruce-fir"; out[fvs == "WC"] <- "Cedar"
  out[fvs == "RM"] <- "Red maple"; out[fvs %in% c("SM", "YB", "AB", "WA", "BC")] <- "Tolerant hardwood"
  out[fvs %in% c("QA", "BT", "PB", "GB")] <- "Aspen-birch"; out[fvs %in% c("RO", "WO")] <- "Oak"
  out
}

# group colours (conifers greens/teal, hardwoods warm), and marking colours
.cp_pal <- c(`White pine` = "#123F2B", `Spruce-fir` = "#3F7A52", Hemlock = "#8DBF9B", Cedar = "#2E8B8B",
             Oak = "#9C5A1E", `Tolerant hardwood` = "#E3B061", `Red maple` = "#C2412D",
             `Aspen-birch` = "#EAD9A6", Other = "#B3B3B3")
.cp_cut <- "#E8662A"; .cp_cut_dark <- "#8A3412"; .cp_band <- "#FCE8D6"

# species x layer crown area (ft2/ac) on a 100-layer canopy space between base and top
.cp_layers <- function(d, w, base, top, grp, step = 0.01) {
  cs <- step * seq_len(round(1 / step))
  cb <- (d$ht * (1 - d$cr) - base) / (top - base); ct <- (d$ht - base) / (top - base)
  ind <- outer(cb, cs, function(a, x) x >= a - 1e-9) & outer(ct, cs, function(b, x) x <= b + 1e-9)
  r <- ind * (w * pi * (MCW(d$fvs, d$dbh) / 2)^2)
  lv <- unique(c(names(.cp_pal), sort(unique(grp))))
  G <- lv[lv %in% unique(grp)]
  M <- vapply(G, function(g) colSums(r[grp == g, , drop = FALSE]), numeric(length(cs)))
  if (is.null(dim(M))) M <- matrix(M, ncol = length(G), dimnames = list(NULL, G))
  M
}

.cp_strata <- function(gaps) {
  bnd <- sort(unlist(gaps)); lims <- c(0, bnd, 1)
  lo <- lims[c(TRUE, FALSE)]; hi <- lims[c(FALSE, TRUE)]
  list(lo = lo, hi = hi, names = if (length(lo) == 2) c("lower", "upper") else c("lower", "middle", "upper"))
}

#' Species-aware crown-profile marking
#'
#' Greedy marking that allocates growing space by species and stratum. The
#' target is a species x layer matrix \eqn{T(s, i) = t(i) p(s | i)}, where
#' \eqn{t(i)} is the stand's own crown-area profile with the gap band(s)
#' emptied ([cp_target_carve()]) and \eqn{p(s | i)} is the current species share
#' of crown area in that stratum times a forester's favor weight, renormalized.
#' Each step removes the record that most reduces the Hellinger distance between
#' the residual species x layer matrix and the target, until the residual basal
#' area is reached.
#'
#' @param d one stand: columns `fvs`, `dbh` (in), `tpa`, `ht` (ft), `cr` (fraction)
#' @param residual_ba residual basal area (ft2/ac)
#' @param group species-group label per row (e.g. from [cp_species_group()])
#' @param weights named list of strata (`"upper"`, `"lower"`, and `"middle"`
#'   with three strata), each a named numeric vector of multipliers by group;
#'   >1 favors (keeps) a group, <1 disfavors it. Default 1.
#' @param gaps list of gap bands in canopy space (see [cp_target_carve()])
#' @param min_dbh smallest markable DBH (in)
#' @param pieces each record is split into this many removal units
#' @param step canopy-space layer width
#' @param protect optional logical vector: records never marked
#' @param favor_cost exponent on the favor weight in the removal cost
#' @return list: `removed`, `residual` (trees/ac per record), species x layer
#'   matrices `O_before`, `O_after`, `target`, `printsplit` and `hellinger`
#'   before/after, `strata`, `gap_mask`, crown limits in canopy space and
#'   `canopy` (base, top in ft)
#' @examples
#' d <- data.frame(fvs = rep(c("WP", "EH", "RM", "RS"), each = 6),
#'                 dbh = c(18, 20, 22, 16, 24, 19, 8, 10, 12, 9, 11, 7,
#'                         10, 12, 9, 14, 11, 8, 6, 7, 9, 8, 6, 10),
#'                 tpa = 10, cr = 0.45)
#' d$ht <- 20 + 3 * d$dbh
#' m <- cp_mark_species(d, 0.65 * sum(0.005454154 * d$dbh^2 * d$tpa),
#'                      cp_species_group(d$fvs),
#'                      weights = list(upper = c(`White pine` = 2, `Red maple` = 0.5)))
#' m$printsplit
#' @export
cp_mark_species <- function(d, residual_ba, group, weights = list(), gaps = list(c(0.45, 0.60)),
                            min_dbh = 5, pieces = 1, step = 0.01, protect = NULL, favor_cost = 0.5) {
  K <- round(1 / step); cs <- step * seq_len(K)
  hcb <- d$ht * (1 - d$cr); top <- max(d$ht); base <- min(hcb)
  cb <- (hcb - base) / (top - base); ct <- (d$ht - base) / (top - base)
  ind <- outer(cb, cs, function(a, x) x >= a - 1e-9) & outer(ct, cs, function(b, x) x <= b + 1e-9)
  ca <- pi * (MCW(d$fvs, d$dbh) / 2)^2
  bt <- 0.005454154 * d$dbh^2
  G <- sort(unique(group)); gi <- match(group, G)
  occ_mat <- function(w) { M <- matrix(0, length(G), K); r <- ind * (w * ca)
                           for (g in seq_along(G)) M[g, ] <- colSums(r[gi == g, , drop = FALSE]); M }
  O0 <- occ_mat(d$tpa)
  t_i <- cp_target_carve(colSums(O0), gaps = gaps)
  S <- .cp_strata(gaps); lo <- S$lo; hi <- S$hi; snames <- S$names
  strat <- vapply(cs, function(x) { k <- which(x >= lo - 1e-9 & x <= hi + 1e-9); if (length(k)) snames[k[1]] else NA_character_ }, "")
  p <- matrix(0, length(G), K)
  for (s in snames) {
    L <- which(strat %in% s)
    sh <- rowSums(O0[, L, drop = FALSE]); if (sum(sh) == 0) next
    w <- stats::setNames(rep(1, length(G)), G)
    ww <- weights[[s]]; if (!is.null(ww)) w[intersect(names(ww), G)] <- ww[intersect(names(ww), G)]
    v <- sh * w; p[, L] <- v / sum(v)
  }
  Tm <- sweep(p, 2, as.numeric(t_i), `*`); Tm <- Tm / sum(Tm)
  hell <- function(M) sqrt(0.5 * sum((sqrt(M / sum(M)) - sqrt(Tm))^2))
  mid_s <- vapply((cb + ct) / 2, function(x) { k <- which(x >= lo - 1e-9 & x <= hi + 1e-9); if (length(k)) snames[k[1]] else snames[length(snames)] }, "")
  fw <- vapply(seq_along(group), function(j) { ww <- weights[[mid_s[j]]]; if (!is.null(ww) && group[j] %in% names(ww)) ww[[group[j]]] else 1 }, 0)
  cost <- fw^favor_cost
  w <- d$tpa; pw <- w / pieces
  cand <- d$dbh >= min_dbh; if (!is.null(protect)) cand <- cand & !protect
  left <- ifelse(cand, pieces, 0); removed <- numeric(nrow(d))
  O <- O0; ba <- sum(bt * w)
  contrib <- lapply(seq_len(nrow(d)), function(j) ind[j, ] * ca[j] * pw[j])
  while (ba > residual_ba && any(left > 0)) {
    j <- which(left > 0)
    Dj <- vapply(j, function(k) { M <- O; M[gi[k], ] <- pmax(M[gi[k], ] - contrib[[k]], 0); hell(M) * cost[k] }, 0)
    k <- j[which.min(Dj)]
    O[gi[k], ] <- pmax(O[gi[k], ] - contrib[[k]], 0); ba <- ba - bt[k] * pw[k]
    left[k] <- left[k] - 1; removed[k] <- removed[k] + pw[k]
  }
  rownames(O0) <- rownames(O) <- rownames(Tm) <- G
  gm <- Reduce(`|`, lapply(gaps, function(z) cs >= z[1] & cs <= z[2]))
  list(removed = removed, residual = w - removed, O_before = O0, O_after = O, target = Tm,
       printsplit = c(before = cp_printsplit(colSums(O0), gaps, step), after = cp_printsplit(colSums(O), gaps, step)),
       hellinger = c(before = hell(O0), after = hell(O)), strata = strat, gap_mask = gm,
       crown_base_cs = cb, crown_top_cs = ct, canopy = c(base = base, top = top))
}

#' Crown-profile marking guide for foresters
#'
#' A four-step workflow that turns a cruise into a DBH-pinned marking guide:
#' \enumerate{
#'   \item `cp_view_stand()`: species crown-area profile and a suggested gap band;
#'   \item `cp_plan()`: choose strata (2 or 3), gap band(s), residual basal area
#'         and species favor weights, and mark;
#'   \item `cp_dbh_guide()`: trees/ac to cut and leave by species x 2-in. DBH
#'         class, with a crown tie-breaker within a class;
#'   \item `cp_guide_sheet()`: one-page PNG (profiles before/after, the guide,
#'         and the before/after diameter distribution).
#' }
#' `cp_workflow()` runs all four and writes the guide CSV and the sheet.
#'
#' @param trees tree list: `stand`, `fvs`, `dbh` (in), `tpa`, `ht` (ft),
#'   `cr` (fraction), e.g. from [read_inventory()] or
#'   [read_demeritt_inventory()]
#' @param stand stand identifier
#' @param min_dbh smallest tree included (in)
#' @param group_fun function mapping FVS codes to species groups
#' @return `cp_view_stand()`: list with `trees`, `layers` (species x layer crown
#'   area, ft2/ac), `base`, `top` (ft), `suggested_gap`, `printsplit` and a
#'   `summary` table by group (trees/ac, BA, crown share)
#' @examples
#' tl <- read_inventory(maine_cruise, cols = list(stand = "Stand", plot = "Plot",
#'                      species = "Species", dbh = "DBH", ht = "Ht", cr = "CR"))
#' tl <- tl[!is.na(tl$ht) & !is.na(tl$cr), ]
#' v <- cp_view_stand(tl, "NH-2")
#' v$summary
#' p <- cp_plan(v, residual_ba = 0.65 * sum(v$summary$ba))
#' g <- cp_dbh_guide(p)
#' head(g$guide)
#' @name cp_forester
NULL

#' @rdname cp_forester
#' @export
cp_view_stand <- function(trees, stand, min_dbh = 5, group_fun = cp_species_group) {
  d <- as.data.frame(trees)[trees$stand == stand & trees$dbh >= min_dbh, , drop = FALSE]
  if (!nrow(d)) stop("cp_view_stand: no trees >= min_dbh in stand ", stand)
  if (anyNA(d$ht) || anyNA(d$cr)) stop("cp_view_stand: ht and cr must be complete (see cp_height())")
  d$grp <- group_fun(d$fvs)
  base <- min(d$ht * (1 - d$cr)); top <- max(d$ht)
  M <- .cp_layers(d, d$tpa, base, top, d$grp); tot <- rowSums(M)
  sm <- stats::filter(tot, rep(1 / 9, 9), sides = 2); sm[is.na(sm)] <- tot[is.na(sm)]
  # suggested two-strata gap: the 0.15-wide band within 0.30-0.70 holding the least crown area
  starts <- 30:55; i0 <- starts[which.min(vapply(starts, function(i) sum(sm[i:(i + 15)]), 0))]
  ba <- tapply(.cp_ba(d$dbh, d$tpa), d$grp, sum); tp <- tapply(d$tpa, d$grp, sum)
  cs <- colSums(M)
  s <- data.frame(group = names(ba), tpa = as.numeric(tp[names(ba)]), ba = as.numeric(ba),
                  crown_share = round(100 * as.numeric(cs[names(ba)]) / sum(M), 1))
  s$crown_share[is.na(s$crown_share)] <- 0
  list(trees = d, layers = M, base = base, top = top, suggested_gap = c(i0, i0 + 15) / 100,
       printsplit = cp_printsplit(tot), summary = s[order(-s$ba), ], stand = stand)
}

#' @rdname cp_forester
#' @param view output of `cp_view_stand()`
#' @param residual_ba residual basal area (ft2/ac)
#' @param strata 2 or 3
#' @param gaps list of gap bands; `NULL` uses `c(0.45, 0.60)` (two strata) or
#'   `c(0.30, 0.42)`, `c(0.62, 0.72)` (three); `"suggested"` uses the view's band
#' @param favor named list of species weights by stratum (see [cp_mark_species()])
#' @param pieces removal units per record
#' @param protect optional function of the tree table returning a logical
#'   vector of trees never marked
#' @param method `"rule"` (transparent priority rule, default) or `"optimize"`
#'   ([cp_mark_species()])
#' @details The rule marks in order of
#'   `(share of crown length inside the gap band(s) + 0.1 x (1 - relative crown top)) / favor weight`:
#'   crowns bridging the strata first, then lower crowns, disfavored species
#'   before favored ones, until the residual basal area is reached.
#' @return `cp_plan()`: list with `view`, `trees` (records split into pieces,
#'   with `tpa_cut`, `tpa_leave`), `printsplit` before/after, `gaps`,
#'   `residual_ba`, `favor`, `method`
#' @export
cp_plan <- function(view, residual_ba, strata = 2, gaps = NULL, favor = list(), pieces = 4, protect = NULL,
                    method = c("rule", "optimize")) {
  method <- match.arg(method)
  d <- view$trees
  if (identical(gaps, "suggested")) gaps <- list(view$suggested_gap)
  if (is.null(gaps)) gaps <- if (strata == 2) list(c(0.45, 0.60)) else list(c(0.30, 0.42), c(0.62, 0.72))
  dp <- d[rep(seq_len(nrow(d)), each = pieces), , drop = FALSE]; dp$tpa <- dp$tpa / pieces
  prot <- if (is.null(protect)) NULL else rep(protect(d), each = pieces)
  base <- view$base; top <- view$top
  if (method == "optimize") {
    m <- cp_mark_species(dp, residual_ba, dp$grp, weights = favor, gaps = gaps, pieces = 1, protect = prot)
    dp$tpa_cut <- m$removed; dp$tpa_leave <- m$residual
  } else {
    hcb <- dp$ht * (1 - dp$cr)
    cb <- (hcb - base) / (top - base); ct <- (dp$ht - base) / (top - base)
    len <- pmax(ct - cb, 1e-6)
    gshare <- Reduce(`+`, lapply(gaps, function(z) pmax(0, pmin(ct, z[2]) - pmax(cb, z[1])))) / len
    S <- .cp_strata(gaps); mid <- (cb + ct) / 2
    ms <- vapply(mid, function(x) { k <- which(x >= S$lo - 1e-9 & x <= S$hi + 1e-9); if (length(k)) S$names[k[1]] else "gap" }, "")
    fw <- vapply(seq_len(nrow(dp)), function(j) {
      s <- if (ms[j] == "gap") names(favor)[1] else ms[j]
      ww <- if (is.null(s)) NULL else favor[[s]]
      if (!is.null(ww) && dp$grp[j] %in% names(ww)) ww[[dp$grp[j]]] else 1 }, 0)
    pr <- (gshare + 0.1 * (1 - ct)) / fw
    ok <- dp$dbh >= 5; if (!is.null(prot)) ok <- ok & !prot
    ba <- sum(.cp_ba(dp$dbh, dp$tpa)); cut <- numeric(nrow(dp))
    for (i in order(-pr)) { if (ba <= residual_ba) break; if (!ok[i]) next
      cut[i] <- dp$tpa[i]; ba <- ba - .cp_ba(dp$dbh[i], dp$tpa[i]) }
    dp$tpa_cut <- cut; dp$tpa_leave <- dp$tpa - cut
  }
  tot <- function(w) rowSums(.cp_layers(dp, w, base, top, dp$grp))
  ps <- c(before = cp_printsplit(tot(dp$tpa), gaps), after = cp_printsplit(tot(dp$tpa_leave), gaps))
  list(view = view, trees = dp, printsplit = ps, gaps = gaps, residual_ba = residual_ba, favor = favor,
       method = method, strata = length(gaps) + 1)
}

#' @rdname cp_forester
#' @param plan output of `cp_plan()`
#' @param class_width DBH class width (in); classes are centred on even
#'   inches (6 = 5.0-6.9 in.), smallest class 6
#' @return `cp_dbh_guide()`: list with `guide` (group, dbh_class, tpa_now,
#'   tpa_cut, tpa_leave, pct_cut, ba_cut, mean crown base of trees to cut and
#'   to leave, tie_breaker) and `totals` (BA and trees/ac before and after)
#' @export
cp_dbh_guide <- function(plan, class_width = 2) {
  t <- plan$trees
  t$dbh_class <- pmax(6, floor((t$dbh + class_width / 2) / class_width) * class_width)
  t$hcb <- t$ht * (1 - t$cr)
  key <- interaction(t$grp, t$dbh_class, drop = TRUE, lex.order = TRUE)
  g <- do.call(rbind, lapply(split(t, key), function(x) {
    wm <- function(v, w) if (sum(w) > 0) stats::weighted.mean(v, w) else NA_real_
    data.frame(group = x$grp[1], dbh_class = x$dbh_class[1], tpa_now = sum(x$tpa), tpa_cut = sum(x$tpa_cut),
               tpa_leave = sum(x$tpa_leave), ba_cut = sum(.cp_ba(x$dbh, x$tpa_cut)),
               cut_crown_base_ft = wm(x$hcb, x$tpa_cut), leave_crown_base_ft = wm(x$hcb, x$tpa_leave))
  }))
  g <- g[order(g$group, g$dbh_class), ]; rownames(g) <- NULL
  g$pct_cut <- round(100 * g$tpa_cut / g$tpa_now)
  tb <- rep("", nrow(g))
  ok <- !is.na(g$cut_crown_base_ft) & !is.na(g$leave_crown_base_ft) & !(g$pct_cut %in% c(0, 100))
  tb[ok] <- "no crown preference"
  tb[ok & g$cut_crown_base_ft > g$leave_crown_base_ft + 2] <- "cut the shorter-crowned (higher crown base) trees"
  tb[ok & g$cut_crown_base_ft < g$leave_crown_base_ft - 2] <- "cut the longer-crowned (lower crown base) trees"
  g$tie_breaker <- tb
  for (k in c("tpa_now", "tpa_cut", "tpa_leave", "ba_cut")) g[[k]] <- round(g[[k]], 1)
  for (k in c("cut_crown_base_ft", "leave_crown_base_ft")) g[[k]] <- round(g[[k]])
  totals <- data.frame(ba_now = sum(.cp_ba(t$dbh, t$tpa)), ba_leave = sum(.cp_ba(t$dbh, t$tpa_leave)),
                       tpa_now = sum(t$tpa), tpa_leave = sum(t$tpa_leave))
  list(guide = g[, c("group", "dbh_class", "tpa_now", "tpa_cut", "tpa_leave", "pct_cut", "ba_cut",
                     "cut_crown_base_ft", "leave_crown_base_ft", "tie_breaker")], totals = totals)
}

#' @rdname cp_forester
#' @param guide output of `cp_dbh_guide()`
#' @param file PNG path
#' @param title sheet title
#' @return `cp_guide_sheet()`: `file`, invisibly
#' @export
cp_guide_sheet <- function(plan, guide, file, title = "") {
  v <- plan$view; d <- plan$trees
  Mb <- .cp_layers(d, d$tpa, v$base, v$top, d$grp) / 1e3; Ma <- .cp_layers(d, d$tpa_leave, v$base, v$top, d$grp) / 1e3
  pal <- .cp_pal; extra <- setdiff(colnames(Mb), names(pal)); pal[extra] <- "#B3B3B3"
  ink <- "black"; ink2 <- "grey30"
  yy <- seq(0.01, 1, 0.01); xm <- max(rowSums(Mb)) * 1.05
  g <- guide$guide; groups <- colnames(Mb)[colnames(Mb) %in% g$group]
  cls <- sort(unique(g$dbh_class))
  grDevices::png(file, 3000, 2000, res = 230)
  on.exit(grDevices::dev.off())
  graphics::layout(matrix(c(1, 2, 3, 1, 2, 4), 2, byrow = TRUE), widths = c(1, 1, 2.4), heights = c(1.1, 1))
  graphics::par(bg = "white", oma = c(1.5, 0, 3, 0))
  for (k in 1:2) {
    M <- if (k == 1) Mb else Ma; cum <- t(apply(M, 1, cumsum)); if (ncol(M) == 1) cum <- t(cum)
    prev <- cbind(0, cum[, -ncol(cum), drop = FALSE])
    graphics::par(mar = c(4, if (k == 1) 4 else 1.5, 3, 1))
    graphics::plot(NA, xlim = c(0, xm), ylim = c(0, 1), xaxs = "i", yaxs = "i", axes = FALSE, xlab = "", ylab = "")
    for (gp in plan$gaps) { graphics::rect(0, gp[1], xm, gp[2], col = .cp_band, border = NA)
                            graphics::abline(h = gp, col = .cp_cut, lty = 2, lwd = 0.6) }
    for (j in seq_len(ncol(M))) graphics::polygon(c(prev[, j], rev(cum[, j])), c(yy, rev(yy)),
                                                  col = pal[colnames(M)[j]], border = "white", lwd = 0.4)
    graphics::box(lwd = 0.8)
    graphics::axis(1, cex.axis = 0.8, tcl = -0.3)
    if (k == 1) { graphics::axis(2, at = seq(0, 1, 0.2), labels = round(v$base + seq(0, 1, 0.2) * (v$top - v$base)),
                                 las = 1, cex.axis = 0.8, tcl = -0.3)
                  graphics::mtext("Height (ft)", side = 2, line = 2.8, cex = 0.7) }
    graphics::mtext("Crown area (1,000 ft2/ac)", side = 1, line = 2.5, cex = 0.7)
    ps <- plan$printsplit[[k]]
    graphics::mtext(sprintf("%s: %.0f ft2/ac, PrintSplit %.2f", if (k == 1) "Before" else "After",
                            if (k == 1) guide$totals$ba_now else guide$totals$ba_leave, ps),
                    side = 3, adj = 0, line = 0.6, font = 2, cex = 0.75, col = if (k == 2) "#1D6B44" else ink)
  }
  graphics::legend("bottomright", legend = colnames(Mb), fill = pal[colnames(Mb)], border = NA, bty = "n", cex = 0.7)
  # guide grid: rows groups, columns DBH class; cell = trees/ac to cut / trees/ac now (percent)
  graphics::par(mar = c(3, 9.5, 3, 1))
  graphics::plot(NA, xlim = c(0.5, length(cls) + 0.5), ylim = c(0.5, length(groups) + 0.5), axes = FALSE, xlab = "", ylab = "")
  ramp <- grDevices::colorRampPalette(c("#F4F1EC", "#F6C9A8", .cp_cut, .cp_cut_dark))(101)
  for (i in seq_along(groups)) for (j in seq_along(cls)) {
    r <- g[g$group == groups[i] & g$dbh_class == cls[j], ]
    y <- length(groups) - i + 1
    if (!nrow(r)) next
    graphics::rect(j - 0.48, y - 0.45, j + 0.48, y + 0.45, col = ramp[r$pct_cut + 1], border = "white")
    tc <- if (r$pct_cut > 55) "white" else ink
    graphics::text(j, y + 0.13, sprintf("%.0f/%.0f", r$tpa_cut, r$tpa_now), cex = 0.6, col = tc)
    if (r$pct_cut > 0) graphics::text(j, y - 0.2, paste0(r$pct_cut, "%"), cex = 0.55, col = tc)
  }
  graphics::axis(1, at = seq_along(cls), labels = cls, cex.axis = 0.8, tcl = -0.3)
  graphics::axis(2, at = rev(seq_along(groups)), labels = groups, las = 1, cex.axis = 0.8, tick = FALSE)
  graphics::mtext("Marking guide: trees/ac to cut / trees/ac now (percent of class)", side = 3, adj = 0, line = 0.6, font = 2, cex = 0.75)
  # diameter distribution: residual trees/ac by group, removals stacked on top
  dc <- pmax(6, floor((d$dbh + 1) / 2) * 2); lev <- cls
  L <- sapply(groups, function(gr) tapply(d$tpa_leave[d$grp == gr], factor(dc[d$grp == gr], lev), sum))
  L[is.na(L)] <- 0; if (is.null(dim(L))) L <- matrix(L, ncol = length(groups))
  C <- tapply(d$tpa_cut, factor(dc, lev), sum); C[is.na(C)] <- 0
  H <- rbind(t(L), Cut = C)
  graphics::par(mar = c(4, 9.5, 2.5, 1))
  graphics::barplot(H, col = c(pal[groups], .cp_cut), border = "white", space = 0.15, names.arg = lev,
                    las = 1, cex.axis = 0.8, cex.names = 0.8, ylab = "")
  graphics::mtext("Trees/ac", side = 2, line = 2.8, cex = 0.7)
  graphics::mtext("DBH class (in., 2-in. classes)", side = 1, line = 2.5, cex = 0.7)
  graphics::mtext("Diameter distribution: residual by species, cut in orange", side = 3, adj = 0, line = 0.6, font = 2, cex = 0.75)
  fav <- if (length(plan$favor)) paste(vapply(names(plan$favor), function(s) paste0(s, ": ",
           paste(names(plan$favor[[s]]), plan$favor[[s]], sep = " x", collapse = ", ")), ""), collapse = "; ") else "none"
  graphics::mtext(title, outer = TRUE, adj = 0, line = 1.4, font = 2, cex = 0.9)
  graphics::mtext(sprintf("Gap band(s): %s of canopy height range   |   Species weights - %s",
                          paste(vapply(plan$gaps, function(z) sprintf("%.2f-%.2f", z[1], z[2]), ""), collapse = ", "), fav),
                  outer = TRUE, adj = 0, line = 0.2, col = ink2, cex = 0.65)
  graphics::mtext("Tie-breaker within a species and DBH class: see the guide CSV (crown base of trees to cut vs leave).",
                  side = 1, outer = TRUE, adj = 0, line = 0.3, col = ink2, cex = 0.6)
  invisible(file)
}

#' @rdname cp_forester
#' @param out_dir folder for `marking_guide_<stand>.csv` and `marking_sheet_<stand>.png`
#' @return `cp_workflow()`: list with `view`, `plan`, `guide`, invisibly
#' @export
cp_workflow <- function(trees, stand, residual_ba, strata = 2, gaps = NULL, favor = list(), out_dir = ".",
                        protect = NULL, method = "rule") {
  v <- cp_view_stand(trees, stand)
  p <- cp_plan(v, residual_ba, strata, gaps, favor, protect = protect, method = method)
  g <- cp_dbh_guide(p)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  utils::write.csv(g$guide, file.path(out_dir, paste0("marking_guide_", stand, ".csv")), row.names = FALSE)
  cp_guide_sheet(p, g, file.path(out_dir, paste0("marking_sheet_", stand, ".png")),
                 title = sprintf("Stand %s: CrownPrint marking guide (%d strata, residual %.0f ft2/ac; PrintSplit %.2f to %.2f)",
                                 stand, p$strata, residual_ba, p$printsplit[1], p$printsplit[2]))
  invisible(list(view = v, plan = p, guide = g))
}
