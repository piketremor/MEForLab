#' Example prism cruise (synthetic)
#'
#' A made-up BAF-20 point cruise of six Maine-like stands, for examples and
#' the vignette. It is not real plot data. Stands: `SF-1` spruce-fir, `NH-2`
#' northern hardwood, `WP-3` white pine, `HM-4` hemlock, `MX-5` young
#' mixedwood, `CE-6` cedar lowland. One point in `MX-5` had no tally and is
#' kept as a placeholder row (missing species), which is how
#' [expand_plots()] expects empty points to be recorded.
#'
#' @format data.frame, one row per tallied tree:
#' \describe{
#'   \item{Stand}{stand ID}
#'   \item{Plot}{point ID}
#'   \item{Tree}{tree number on the point}
#'   \item{Species}{FVS alpha species code}
#'   \item{DBH}{diameter at breast height (in.), 1-in classes}
#'   \item{Ht}{total height (ft), measured on about a quarter of trees, else NA}
#'   \item{CR}{crown ratio (percent)}
#'   \item{Acres}{stand area (ac)}
#' }
#' @source Simulated by `data-raw/make_example_data.R`.
#' @examples
#' head(maine_cruise)
#' read_inventory(maine_cruise, cols = list(stand = "Stand", plot = "Plot",
#'                species = "Species", dbh = "DBH", ht = "Ht", cr = "CR"))
"maine_cruise"

#' Example sapling tally (synthetic)
#'
#' Fixed-radius (5.3 ft) sapling counts nested on the [maine_cruise] points,
#' in wide format: one row per point x species, one count column per 1-in.
#' diameter class.
#'
#' @format data.frame with `Stand`, `Plot`, `Species` (FVS code) and counts
#'   `1in`, `2in`, `3in`, `4in`.
#' @source Simulated by `data-raw/make_example_data.R`.
"maine_saplings"
