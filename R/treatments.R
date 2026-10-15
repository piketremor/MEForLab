## Silvicultural treatments, shared by both engines ----------------------------

#' Define a silvicultural treatment
#'
#' Treatments are passed to [project_stands()] (or [conus_project()] /
#' [fvs_project()]) as a list in the `regime` argument.
#'
#' @param year projection year (0 = inventory year) of the entry
#' @param type "thin_below" (remove smallest trees first), "thin_above"
#'   (largest first), "thin_proportional" (same fraction from every
#'   diameter class), or "clearcut"
#' @param residual_ba target residual basal area (ft2/ac) among the trees
#'   eligible for cutting (inside `min_dbh`-`max_dbh` and `species`), the same
#'   convention FVS uses for THINBBA/THINABA; ignored for clearcut
#' @param min_dbh,max_dbh only trees in this DBH range (in.) can be cut
#' @param species optional FVS codes; if given, only these species are cut
#' @return a treatment (list)
#' @examples
#' regime <- list(treatment(0, "thin_below", residual_ba = 80, min_dbh = 4.5),
#'                treatment(30, "thin_below", residual_ba = 80, min_dbh = 4.5))
#' @export
treatment <- function(year, type = c("thin_below", "thin_above",
                                     "thin_proportional", "clearcut"),
                      residual_ba = NA, min_dbh = 0, max_dbh = 999, species = NULL) {
  type <- match.arg(type)
  if (type != "clearcut" && !is.finite(residual_ba))
    stop("residual_ba is required for ", type)
  structure(list(year = year, type = type, residual_ba = residual_ba,
                 min_dbh = min_dbh, max_dbh = max_dbh, species = species),
            class = "mfl_treatment")
}

#' @export
print.mfl_treatment <- function(x, ...) {
  cat(sprintf("year %g: %s%s (cut %g-%g in.%s)\n", x$year, x$type,
              if (is.finite(x$residual_ba)) paste0(" to ", x$residual_ba, " ft2/ac") else "",
              x$min_dbh, x$max_dbh,
              if (length(x$species)) paste0(", species ", paste(x$species, collapse = "/")) else ""))
  invisible(x)
}

#' Apply one treatment to a single stand's tree list
#'
#' @param trees one stand's tree list (`dbh`, `tpa`, `fvs`, `ht`)
#' @param a a [treatment()]
#' @return list(trees = residual tree list, removed = c(ba, tpa, vol))
#' @export
apply_treatment <- function(trees, a) {
  ba_i <- 0.005454154 * trees$dbh^2 * trees$tpa
  elig <- trees$dbh >= a$min_dbh & trees$dbh <= a$max_dbh
  if (length(a$species)) elig <- elig & trees$fvs %in% a$species
  cut_frac <- numeric(nrow(trees))
  if (a$type == "clearcut") {
    cut_frac[elig] <- 1
  } else {
    excess <- sum(ba_i[elig]) - a$residual_ba     # FVS convention: target counts eligible trees only
    avail <- sum(ba_i[elig])
    if (excess > 0 && avail > 0) {
      if (a$type == "thin_proportional") {
        cut_frac[elig] <- min(excess / avail, 1)
      } else {
        o <- which(elig)
        o <- o[order(trees$dbh[o], decreasing = a$type == "thin_above")]
        left <- excess
        for (i in o) {
          if (left <= 0) break
          f <- min(left / ba_i[i], 1)
          cut_frac[i] <- f
          left <- left - f * ba_i[i]
        }
      }
    }
  }
  cut_tpa <- trees$tpa * cut_frac
  v <- if (!is.null(trees$ht)) suppressWarnings(vol.calc(trees$fvs, trees$dbh, trees$ht)) else NA
  removed <- c(ba = sum(ba_i * cut_frac), tpa = sum(cut_tpa),
               vol = sum(v * cut_tpa, na.rm = TRUE))
  trees$tpa <- trees$tpa - cut_tpa
  list(trees = trees[trees$tpa > 1e-6, , drop = FALSE], removed = removed)
}
