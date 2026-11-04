#' Target crown profiles for crown-profile marking
#'
#' `cp_target_carve()` keeps a stand's own crown-occupancy profile and empties
#' one or more gap bands between strata (the default target in [cp_mark()]).
#' `cp_target()` builds a target from scratch as a mixture of beta densities,
#' one per stratum.
#'
#' @param occ crown occupancy by canopy-space layer (length 1/step)
#' @param gaps list of `c(lower, upper)` gap bands in canopy space (0-1).
#'   Two strata: `list(c(0.45, 0.60))`; three strata: `list(c(0.30, 0.42),
#'   c(0.62, 0.72))`.
#' @param smooth width (layers) of the running mean applied before carving
#' @param step canopy-space layer width
#' @return numeric vector summing to 1, with attributes `gaps` and `centers`
#'   (stratum midpoints, top first)
#' @references Premer, M.I. 2026. CrownPrint: a crown-profile approach to
#'   quantifying canopy structure. Can. J. For. Res. 56: 1-14.
#' @export
cp_target_carve <- function(occ, gaps = list(c(0.45, 0.60)), smooth = 5, step = 0.01) {
  x <- seq(step, 1, by = step)
  o <- as.numeric(stats::filter(occ, rep(1 / smooth, smooth), sides = 2))
  o[is.na(o)] <- occ[is.na(o)]
  for (g in gaps) o[x >= g[1] & x <= g[2]] <- 0
  b <- sort(unlist(gaps)); lims <- c(0, b, 1)
  cen <- rev((lims[c(TRUE, FALSE)] + lims[c(FALSE, TRUE)]) / 2)
  structure(o / sum(o), centers = cen, gaps = gaps)
}

#' @rdname cp_target_carve
#' @param strata 2 or 3
#' @param shares share of crown occupancy per stratum, top first
#' @param centers stratum centers in canopy space, top first
#' @param phi beta concentration (larger = narrower strata)
#' @export
cp_target <- function(strata = 2, shares = NULL, centers = NULL, phi = 25, step = 0.01) {
  if (is.null(centers)) centers <- switch(as.character(strata), `2` = c(0.78, 0.25),
                                          `3` = c(0.85, 0.52, 0.18))
  if (is.null(shares)) shares <- switch(as.character(strata), `2` = c(0.6, 0.4),
                                        `3` = c(0.5, 0.3, 0.2))
  x <- seq(step, 1, by = step); x[length(x)] <- 1 - step / 2
  dens <- Reduce(`+`, Map(function(m, s) s * stats::dbeta(x, m * phi, (1 - m) * phi),
                          centers, shares))
  structure(dens / sum(dens), centers = centers, shares = shares, phi = phi)
}

#' PrintSplit: vertical separation between canopy strata
#'
#' PrintSplit (S) is the relative degree of vertical separation between the
#' canopy strata above and below one or more gap bands of a CrownPrint
#' crown-occupancy profile:
#' `S = 1 - mean(occupancy in gap bands) / mean(occupancy elsewhere)`.
#' S = 1 when the bands hold no crown (strata fully separated), 0 when they hold
#' as much crown per layer as the rest of canopy space (no separation), and
#' negative when crowns concentrate in the bands (strata merged). Because it is
#' a ratio of the stand's own occupancies, S does not depend on stocking.
#' Values are comparable across stands only when the bands are defined the same
#' way.
#'
#' @param occ crown occupancy by canopy layer (e.g. from [cp_profile()] or
#'   `cp_mark()$occ_before`)
#' @param gaps list of `c(lower, upper)` gap bands in canopy space (0-1)
#' @param step layer width
#' @return numeric scalar
#' @references Premer, M.I. 2026. CrownPrint: a crown-profile approach to
#'   quantifying canopy structure. Can. J. For. Res. 56: 1-14.
#' @examples
#' o <- rep(1, 100); cp_printsplit(o)   # flat profile: 0
#' o[45:60] <- 0; cp_printsplit(o)      # empty band: 1
#' @export
cp_printsplit <- function(occ, gaps = list(c(0.45, 0.60)), step = 0.01) {
  cs <- step * seq_along(occ)
  g <- Reduce(`|`, lapply(gaps, function(z) cs >= z[1] & cs <= z[2]))
  1 - mean(occ[g]) / mean(occ[!g])
}

#' @rdname cp_printsplit
#' @description `cp_gap_depth()` is a deprecated alias of `cp_printsplit()`.
#' @export
cp_gap_depth <- function(occ, gaps = list(c(0.45, 0.60)), step = 0.01) {
  .Deprecated("cp_printsplit", package = "MEForLab")
  cp_printsplit(occ, gaps = gaps, step = step)
}

.cp_hell <- function(o, t) sqrt(0.5 * sum((sqrt(o / sum(o)) - sqrt(t))^2))

#' Crown-profile marking for single-tree selection
#'
#' Marks trees for removal so that the residual stand's crown occupancy in
#' canopy space approaches a target profile (by default the stand's own profile
#' with a gap band emptied between two strata), stopping at a residual basal
#' area. Crowns are weighted by open-grown crown area ([MCW()]) and canopy
#' limits are held at their pre-harvest values. Trees are removed greedily:
#' each step removes the candidate (or piece of a tree record) whose removal
#' minimizes the Hellinger distance to the target.
#'
#' @param trees one stand's tree list with completed heights: `ht_cp` and
#'   `hcb_cp` from [cp_height()] / [cp_hcb()]
#' @param residual_ba residual basal area (ft2/ac if imperial, m2/ha if metric)
#' @param target a target vector (see [cp_target_carve()]) or a function of the
#'   stand's pre-harvest occupancy returning one
#' @param cols,units see [cp_height()]; species must be FVS codes for [MCW()]
#' @param protect optional name of a logical column: trees never marked
#' @param min_dbh smallest markable DBH (input units)
#' @param pieces each record is split into this many removal units (use 1 when
#'   each record is one tree, larger for expanded cruise records)
#' @param weight "crown_area" (default) or "tpa" (stems, as in CrownPrint)
#' @param step canopy-space layer width
#' @return object of class `cp_mark`: `trees` (input plus `tpa_removed`,
#'   `tpa_residual`, crown limits in canopy space, `gap_share`, `mark_reason`),
#'   occupancy before/after, target, basal area and stems before/after,
#'   Hellinger distance and PrintSplit before/after
#' @examples
#' tl <- read_inventory(maine_cruise, cols = list(stand = "Stand", plot = "Plot",
#'                      species = "Species", dbh = "DBH", ht = "Ht", cr = "CR"))
#' x <- tl[tl$stand == "NH-2" & tl$dbh >= 5, ]
#' x <- cp_hcb(cp_height(x), method = "observed")
#' m <- cp_mark(x, residual_ba = 70)
#' m
#' @export
cp_mark <- function(trees, residual_ba, target = function(o) cp_target_carve(o),
                    cols = list(), units = c("imperial", "metric"), protect = NULL,
                    min_dbh = 5, pieces = 4, weight = c("crown_area", "tpa"), step = 0.01) {
  units <- match.arg(units); weight <- match.arg(weight)
  s <- .cp_std(trees, cols, units)
  if (is.null(s$.ht) || is.null(s$.hcb))
    stop("cp_mark: run cp_height() and cp_hcb() first (needs ht_cp, hcb_cp)")
  if (length(unique(s$.stand)) > 1) stop("cp_mark: pass one stand at a time")
  d <- as.data.frame(trees)[s$.row, , drop = FALSE]
  cs <- step * seq_len(round(1 / step))
  top <- max(s$.ht); base <- min(s$.hcb)
  cb <- (s$.hcb - base) / (top - base); ct <- (s$.ht - base) / (top - base)
  ind <- outer(cb, cs, function(a, x) x >= a - 1e-9) & outer(ct, cs, function(b, x) x <= b + 1e-9)
  dbh_in <- s$.dbh / 2.54
  ba_tree <- if (units == "imperial") 0.005454154 * dbh_in^2 else pi * (s$.dbh / 200)^2
  w <- s$.w
  ca <- if (weight == "crown_area") pi * (MCW(s$.sp, dbh_in) / 2)^2 else rep(1, length(w))
  dmin <- if (units == "imperial") min_dbh * 2.54 else min_dbh
  cand <- s$.dbh >= dmin
  if (!is.null(protect)) cand <- cand & !as.logical(d[[protect]])
  occ <- colSums(ind * (w * ca)); occ0 <- occ
  if (is.function(target)) target <- target(occ0)
  ba <- sum(ba_tree * w); ba0 <- ba
  left <- ifelse(cand, pieces, 0); pw <- w / pieces
  removed <- numeric(length(w))
  while (ba > residual_ba && any(left > 0)) {
    j <- which(left > 0)
    Dj <- vapply(j, function(i) .cp_hell(pmax(occ - ind[i, ] * pw[i] * ca[i], 0), target), 0)
    k <- j[which.min(Dj)]
    occ <- pmax(occ - ind[k, ] * pw[k] * ca[k], 0); ba <- ba - ba_tree[k] * pw[k]
    left[k] <- left[k] - 1; removed[k] <- removed[k] + pw[k]
  }
  gaps <- attr(target, "gaps")
  gm <- if (!is.null(gaps)) Reduce(`|`, lapply(gaps, function(z) cs >= z[1] & cs <= z[2]))
        else target < 0.25 * max(target)
  share <- rowSums(ind[, gm, drop = FALSE]) / pmax(rowSums(ind), 1)
  cen <- attr(target, "centers")
  lab <- if (length(cen) == 3) c("upper", "middle", "lower") else c("upper", "lower")
  stratum <- lab[apply(abs(outer((cb + ct) / 2, cen, "-")), 1, which.min)]
  d$crown_base_cs <- cb; d$crown_top_cs <- ct; d$stratum <- stratum; d$gap_share <- share
  d$tpa_removed <- removed; d$tpa_residual <- w - removed
  d$mark_reason <- ifelse(removed > 0, ifelse(share >= 0.4, "crown spans a gap",
                                              paste("excess in", stratum, "stratum")), "")
  gdep <- function(o) 1 - mean(o[gm]) / mean(o[!gm])
  structure(list(trees = d, occ_before = occ0, occ_after = occ, target = target,
                 ba_before = ba0, ba_after = ba, tpa_before = sum(w), tpa_removed = sum(removed),
                 hellinger = c(before = .cp_hell(occ0, target), after = .cp_hell(occ, target)),
                 printsplit = c(before = gdep(occ0), after = gdep(occ)), gap_mask = gm,
                 canopy = c(base = base, top = top) / attr(s, "cf")[["h"]], units = units,
                 weight = weight), class = "cp_mark")
}

#' @export
print.cp_mark <- function(x, ...) {
  u <- if (x$units == "imperial") c("ft2/ac", "trees/ac") else c("m2/ha", "trees/ha")
  cat("Crown-profile marking (CrownPrint)\n")
  cat(sprintf("  Basal area %.1f -> %.1f %s; stems removed %.1f of %.1f %s\n",
              x$ba_before, x$ba_after, u[1], x$tpa_removed, x$tpa_before, u[2]))
  cat(sprintf("  PrintSplit %.2f -> %.2f; Hellinger distance to target %.3f -> %.3f\n",
              x$printsplit[1], x$printsplit[2], x$hellinger[1], x$hellinger[2]))
  r <- table(x$trees$mark_reason[x$trees$tpa_removed > 0])
  if (length(r)) { cat("  Marked records by reason:\n"); print(r) }
  invisible(x)
}

#' Plot crown-occupancy profiles before and after crown-profile marking
#'
#' @param x a [cp_mark()] result
#' @param ... passed to [graphics::plot()]
#' @return `x`, invisibly
#' @export
plot.cp_mark <- function(x, ...) {
  yy <- seq_along(x$occ_before) / length(x$occ_before)
  nb <- function(o) 100 * o / sum(o)
  xm <- max(nb(x$occ_before), nb(x$occ_after), 100 * x$target)
  graphics::plot(NA, xlim = c(0, xm * 1.05), ylim = c(0, 1), xlab = "Share of crown area (%)",
                 ylab = "Canopy space (0-1)", ...)
  g <- which(x$gap_mask)
  if (length(g)) {
    runs <- split(g, cumsum(c(1, diff(g) != 1)))
    for (r in runs) graphics::rect(0, min(yy[r]), xm * 2, max(yy[r]), col = "grey92", border = NA)
  }
  graphics::lines(nb(x$occ_before), yy, lty = 2, lwd = 2, col = "grey40")
  graphics::lines(100 * x$target, yy, lwd = 2, col = "#1baf7a")
  graphics::lines(nb(x$occ_after), yy, lwd = 2.5, col = "#2a78d6")
  graphics::legend("topright", bty = "n", lwd = c(2, 2, 2.5), lty = c(2, 1, 1),
                   col = c("grey40", "#1baf7a", "#2a78d6"), legend = c("Before", "Target", "After"))
  invisible(x)
}
