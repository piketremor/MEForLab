#' Project stands with either growth engine
#'
#' One entry point for the two engines so results can be compared directly:
#'
#' * `"conus"` -- the FVS Remodeling Project's variant-free CONUS equations,
#'   implemented in R ([conus_project()]). Runs anywhere, no install.
#' * `"fvs"` -- a compiled FVS variant (NE by default) driven through keyword
#'   files ([fvs_project()]). Needs FVS installed.
#'
#' @param trees tree list (see [read_demeritt_inventory()])
#' @param engine "conus" or "fvs"
#' @param years projection length (years)
#' @param regime list of [treatment()]s
#' @param ... passed to the engine
#' @return data.frame, one row per stand x reporting year, with at least
#'   `stand`, `year`, `tpa`, `ba`, `qmd`, `sdi`, `top_ht`, `vol_cuft`,
#'   `removed_ba`, `removed_vol`, `ba_after` and `engine`. Rows describe
#'   the stand at the start of each reporting year, before any treatment in
#'   that year; `ba_after` is basal area after the treatment.
#' @examples
#' tl <- data.frame(stand = "A", fvs = c("RS", "BF", "RM"), spcd = c(97, 12, 316),
#'                  dbh = c(10, 8, 12), ht = c(60, 50, 65), cr = c(.5, .45, .4),
#'                  tpa = c(80, 120, 30))
#' project_stands(tl, "conus", years = 20)
#' @export
project_stands <- function(trees, engine = c("conus", "fvs"), years = 50,
                           regime = list(), ...) {
  engine <- match.arg(engine)
  r <- switch(engine,
              conus = conus_project(trees, years = years, regime = regime, ...),
              fvs   = fvs_project(trees, years = years, regime = regime, ...))
  s <- r$summary
  s$engine <- attr(r, "engine")
  cols <- c("stand", "year", "engine", "tpa", "ba", "qmd", "sdi", "top_ht",
            "vol_cuft", "removed_tpa", "removed_ba", "removed_vol", "ba_after")
  s[, c(cols, setdiff(names(s), cols))]
}
