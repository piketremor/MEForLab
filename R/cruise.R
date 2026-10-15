## Turning field inventory into per-acre tree lists ---------------------------

#' Expand a variable-radius (prism) point cruise to a per-acre tree list
#'
#' Each tallied tree represents `BAF / (0.005454 * DBH^2)` trees per acre on
#' its point; stand-level TPA divides by the number of points in the stand.
#' Points with no tally (all stems below the prism threshold) still count as
#' points, so keep their placeholder rows in `data` -- rows with a missing or
#' zero species code are used for the point count and then dropped.
#'
#' @param data data.frame, one row per tallied tree
#' @param baf basal area factor (ft2/ac per tree)
#' @param stand,plot,species,dbh column names in `data`
#' @param cr column holding crown ratio in percent (may contain notes after
#'   the number, e.g. "35 burn plot"); NULL if not recorded
#' @param reps column with the tally count for each record (e.g. 2 for a
#'   walk-through double count); NULL if every record is one tree
#' @param species_codes coding of `species`: "tt" (TallTimber numeric),
#'   "fvs" (NE alpha) or "spcd" (FIA)
#' @return data.frame tree list with columns `stand`, `plot`, `tree`, `fvs`,
#'   `spcd`, `dbh`, `cr` (0-1, NA if not recorded), `tpa`, `source`. The
#'   number of points per stand is attached as attribute `n_plots`.
#' @examples
#' cruise <- data.frame(Type = c("A1", "A1", "A1", "B2"), Plot = c(1, 1, 2, 3),
#'                      Spp = c(3, 4, 8, 0), DBH = c(10, 8, 12, 5),
#'                      Tree_Notes = c("40", "35", "50 forked", "no tally"))
#' expand_point_cruise(cruise)
#' @export
expand_point_cruise <- function(data, baf = 20, stand = "Type", plot = "Plot",
                                species = "Spp", dbh = "DBH", cr = "Tree_Notes",
                                reps = NULL, species_codes = c("tt", "fvs", "spcd")) {
  species_codes <- match.arg(species_codes)
  d <- as.data.frame(data)
  d <- d[!is.na(d[[stand]]) & !is.na(d[[plot]]), , drop = FALSE]
  st <- as.character(d[[stand]])
  pk <- paste(st, d[[plot]])
  n_plots <- tapply(pk, st, function(p) length(unique(p)))

  sp_raw <- d[[species]]
  keep <- !is.na(sp_raw) & !(sp_raw %in% c(0, "0", "")) & !is.na(d[[dbh]])
  d <- d[keep, , drop = FALSE]; st <- st[keep]

  fvs <- .to_fvs(d[[species]], species_codes)
  if (anyNA(fvs)) {
    bad <- unique(d[[species]][is.na(fvs)])
    warning("dropping ", sum(is.na(fvs)), " trees with unknown species code(s): ",
            paste(bad, collapse = ", "))
    d <- d[!is.na(fvs), , drop = FALSE]; st <- st[!is.na(fvs)]; fvs <- fvs[!is.na(fvs)]
  }
  D <- as.numeric(d[[dbh]])
  k <- if (is.null(reps)) 1 else { r <- as.numeric(d[[reps]]); ifelse(is.na(r) | r < 1, 1, r) }
  tpa <- baf / (0.005454154 * D^2) * k / as.numeric(n_plots[st])

  out <- data.frame(stand = st, plot = d[[plot]], tree = seq_len(nrow(d)),
                    fvs = fvs, spcd = species_lookup(fvs, "fvs", "spcd"),
                    dbh = D, cr = if (is.null(cr)) NA_real_ else .parse_cr(d[[cr]]),
                    tpa = tpa, source = "overstory", stringsAsFactors = FALSE)
  attr(out, "n_plots") <- n_plots
  out
}

#' Expand a fixed-radius sapling dot tally to a per-acre tree list
#'
#' @param data data.frame, one row per plot x species with counts in 1-inch
#'   diameter-class columns
#' @param n_plots named vector of points per stand (use
#'   `attr(overstory, "n_plots")` so plots with no saplings count as zeros)
#' @param radius plot radius (ft)
#' @param class_cols named vector mapping column name -> class midpoint DBH
#'   (in.). Classes below 1 in. are normally left out because the growth
#'   equations were fit to trees >= 1 in. DBH.
#' @param stand,plot,species column names
#' @param cr_default crown ratio assigned to saplings (not measured)
#' @inheritParams expand_point_cruise
#' @return tree list in the same format as [expand_point_cruise()]
#' @export
expand_sapling_tally <- function(data, n_plots, radius = 5.3,
                                 class_cols = c(`0.6" to 1.5"` = 1, `1.6" to 2.5"` = 2,
                                                `2.6" to 3.5"` = 3, `3.6" to 4.5"` = 4),
                                 stand = "Stand_ID", plot = "Plot", species = "Species",
                                 species_codes = c("tt", "fvs", "spcd"),
                                 cr_default = 0.6) {
  species_codes <- match.arg(species_codes)
  d <- as.data.frame(data)
  expf <- 43560 / (pi * radius^2)
  rows <- list()
  for (cc in names(class_cols)) {
    if (!cc %in% names(d)) stop("column not found: ", cc)
    n <- suppressWarnings(as.numeric(d[[cc]]))
    ok <- !is.na(n) & n > 0
    if (!any(ok)) next
    rows[[cc]] <- data.frame(stand = as.character(d[[stand]][ok]), plot = d[[plot]][ok],
                             sp = d[[species]][ok], dbh = unname(class_cols[cc]),
                             n = n[ok], stringsAsFactors = FALSE)
  }
  if (!length(rows)) return(NULL)
  s <- do.call(rbind, rows)
  s$fvs <- .to_fvs(suppressWarnings(if (species_codes == "fvs") s$sp else as.numeric(s$sp)),
                   species_codes)
  drop <- is.na(s$fvs) | !(s$stand %in% names(n_plots))
  if (any(drop))
    message("expand_sapling_tally: dropped ", sum(drop),
            " rows (unknown species code, or stand not in the overstory cruise)")
  s <- s[!drop, , drop = FALSE]
  data.frame(stand = s$stand, plot = s$plot, tree = seq_len(nrow(s)) + 1e6,
             fvs = s$fvs, spcd = species_lookup(s$fvs, "fvs", "spcd"),
             dbh = s$dbh, cr = cr_default,
             tpa = s$n * expf / as.numeric(n_plots[s$stand]),
             source = "sapling", stringsAsFactors = FALSE)
}

#' Fill in missing tree heights and crown ratios
#'
#' Heights come from the FVS-NE Wykoff equations ([wykoff.ht()]); species
#' without coefficients borrow the generic "OS" or "OH" curve. Missing crown
#' ratios get the median of the same species in the same stand, then the
#' species median overall, then `cr_default`. Existing values are kept.
#'
#' @param trees tree list (needs `stand`, `fvs`, `dbh`; `ht` and `cr` optional)
#' @param cr_default fallback crown ratio (fraction)
#' @return `trees` with complete `ht` (ft) and `cr` (fraction) columns, plus
#'   `ht_imputed` / `cr_imputed` flags
#' @export
complete_trees <- function(trees, cr_default = 0.4) {
  if (is.null(trees$ht)) trees$ht <- NA_real_
  if (is.null(trees$cr)) trees$cr <- NA_real_
  trees$ht_imputed <- is.na(trees$ht)
  if (any(trees$ht_imputed)) {
    i <- trees$ht_imputed
    h <- suppressWarnings(wykoff.ht(trees$fvs[i], trees$dbh[i]))
    soft <- species_lookup(trees$fvs[i], "fvs", "softwood")
    alt <- ifelse(!is.na(soft) & soft, "OS", "OH")
    h[is.na(h)] <- wykoff.ht(alt[is.na(h)], trees$dbh[i][is.na(h)])
    trees$ht[i] <- h
  }
  trees$cr_imputed <- is.na(trees$cr)
  if (any(trees$cr_imputed)) {
    key <- paste(trees$stand, trees$fvs)
    m1 <- tapply(trees$cr, key, stats::median, na.rm = TRUE)
    m2 <- tapply(trees$cr, trees$fvs, stats::median, na.rm = TRUE)
    i <- trees$cr_imputed
    v <- m1[key[i]]
    v[is.na(v)] <- m2[trees$fvs[i]][is.na(v)]
    v[is.na(v)] <- cr_default
    trees$cr[i] <- v
  }
  trees$cr <- pmin(pmax(trees$cr, 0.05), 0.95)
  trees
}

#' Read the Demeritt Forest 2025 inventory into a tree list
#'
#' Convenience wrapper for the UMaine Demeritt Forest (DBD) Inventory Project
#' 66 files: BAF-20 overstory points (`DBD_66_Tree_DataBAF20_StandsOK.xlsx`)
#' and the nested 5.3-ft radius sapling plots
#' (`Sapling_MASTER_DBDWith2026.xlsx`). Crown ratio (percent) is in the
#' `Tree_Notes` field; species are TallTimber numeric codes.
#'
#' @param overstory path to the overstory workbook
#' @param saplings path to the sapling workbook, or NULL to skip saplings
#' @return complete tree list (see [complete_trees()]); attribute `n_plots`
#'   gives points per stand and `stand_acres` the stand areas
#' @examples
#' \dontrun{
#' dir <- "G:/My Drive/Research/UMaine_Demerritt/UF_Demeritt_MgtPlan2026/2025_DBD_Inventory"
#' trees <- read_demeritt_inventory(file.path(dir, "DBD_66_Tree_DataBAF20_StandsOK.xlsx"),
#'                                  file.path(dir, "Sapling_MASTER_DBDWith2026.xlsx"))
#' }
#' @export
read_demeritt_inventory <- function(overstory, saplings = NULL) {
  if (!requireNamespace("readxl", quietly = TRUE))
    stop("install.packages('readxl') to read the inventory workbooks")
  ov <- suppressWarnings(as.data.frame(readxl::read_excel(overstory)))
  tr <- expand_point_cruise(ov, baf = 20, stand = "Type", plot = "Plot",
                            species = "Spp", dbh = "DBH", cr = "Tree_Notes",
                            reps = "Reps", species_codes = "tt")
  np <- attr(tr, "n_plots")
  if (!is.null(saplings)) {
    sp <- suppressWarnings(as.data.frame(readxl::read_excel(saplings)))
    sp$Stand_ID <- trimws(sp$Stand_ID)
    st <- expand_sapling_tally(sp, n_plots = np)
    if (!is.null(st)) tr <- rbind(tr, st)
  }
  acres <- tapply(ov$Acres_int, ov$Type, function(a) stats::median(a, na.rm = TRUE))
  tr <- complete_trees(tr)
  attr(tr, "n_plots") <- np
  attr(tr, "stand_acres") <- acres
  tr
}

#' Stand-level summary of a tree list
#'
#' @param trees tree list with `stand`, `dbh`, `tpa` and (optionally) `ht`,
#'   `fvs`
#' @param by grouping column
#' @return one row per stand: TPA, BA (ft2/ac), QMD (in.), SDI (Reineke,
#'   summation method), RD (Curtis), top height (mean height of the largest 40
#'   TPA, ft), total cubic volume (ft3/ac, [vol.calc()]), and the leading
#'   species by basal area with its share
#' @examples
#' tl <- data.frame(stand = "A", fvs = c("RS", "BF", "RM"), dbh = c(10, 8, 12),
#'                  ht = c(60, 50, 65), tpa = c(80, 120, 30))
#' stand_summary(tl)
#' @export
stand_summary <- function(trees, by = "stand") {
  if (!nrow(trees)) return(NULL)
  sp <- split(trees, trees[[by]])
  out <- lapply(names(sp), function(s) {
    t <- sp[[s]]
    ba <- sum(0.005454154 * t$dbh^2 * t$tpa)
    tpa <- sum(t$tpa)
    q <- if (tpa > 0) sqrt(ba / tpa / 0.005454154) else NA_real_
    r <- data.frame(stand = s, tpa = tpa, ba = ba, qmd = q,
                    sdi = sum(t$tpa * (t$dbh / 10)^1.605),
                    rd = if (is.finite(q) && q > 0) ba / sqrt(q) else NA_real_,
                    stringsAsFactors = FALSE)
    if (!is.null(t$ht)) {
      r$top_ht <- .top_height(t$dbh, t$ht, t$tpa)
      if (!is.null(t$fvs))
        r$vol_cuft <- sum(suppressWarnings(vol.calc(t$fvs, t$dbh, t$ht)) * t$tpa, na.rm = TRUE)
    }
    if (!is.null(t$fvs) && ba > 0) {
      bsp <- tapply(0.005454154 * t$dbh^2 * t$tpa, t$fvs, sum)
      r$lead_spp <- names(which.max(bsp))
      r$lead_share <- max(bsp) / ba
    }
    r
  })
  out <- do.call(rbind, out)
  names(out)[1] <- by
  out
}

## internals ------------------------------------------------------------------

.parse_cr <- function(x) {
  v <- suppressWarnings(as.numeric(sub("^\\s*([0-9]+(\\.[0-9]+)?).*$", "\\1", as.character(x))))
  v[!grepl("^\\s*[0-9]", as.character(x))] <- NA
  v <- v / 100
  v[!is.na(v) & (v <= 0 | v > 1)] <- NA
  v
}

.to_fvs <- function(x, codes) {
  switch(codes,
         tt   = species_lookup(as.numeric(x), "tt", "fvs"),
         spcd = species_lookup(as.numeric(x), "spcd", "fvs"),
         fvs  = { f <- toupper(trimws(x)); f[!f %in% meforlab_species()$fvs] <- NA; f })
}

.top_height <- function(dbh, ht, tpa, n = 40) {
  o <- order(dbh, decreasing = TRUE)
  w <- pmin(tpa[o], pmax(n - c(0, cumsum(tpa[o]))[seq_along(o)], 0))
  if (sum(w) <= 0) return(NA_real_)
  sum(w * ht[o]) / sum(w)
}
