## Turning field inventory into per-acre tree lists ---------------------------

#' Expand plot or point inventory data to a per-acre tree list
#'
#' Works for any tree-level inventory where each row is one tallied tree.
#' Under `design = "prism"` (variable-radius points) each tree represents
#' `BAF / (0.005454 * DBH^2)` trees per acre. Under `design = "fixed"` it
#' represents `1 / plot_acres` trees per acre. Stand-level TPA then divides
#' by the number of plots in the stand. Plots with no tally still count:
#' keep a placeholder row for them (missing or 0 species, or missing DBH).
#' The row is counted as a plot and then dropped.
#'
#' Column names are matched case-insensitively.
#'
#' @param data data.frame, one row per tallied tree
#' @param design "prism" (variable radius, uses `baf`) or "fixed" (fixed
#'   area, uses `plot_acres` or `plot_radius`)
#' @param baf basal area factor (ft2/ac per tree) for prism plots
#' @param plot_acres,plot_radius fixed-plot size, as acres or radius in ft
#'   (give one)
#' @param stand,plot,species,dbh column names in `data`
#' @param ht column with measured total height (ft), or NULL. Missing values
#'   are imputed later by [complete_trees()].
#' @param cr column with crown ratio, or NULL. Text after a leading number is
#'   ignored (e.g. "35 burn plot").
#' @param cr_scale "percent" (0-100), "fraction" (0-1), or "auto" (percent
#'   if any value is above 1)
#' @param reps column with the number of trees each record represents
#'   (e.g. 2 for a double count), or NULL
#' @param species_codes coding of `species`: "fvs" (NE alpha, e.g. "RS"),
#'   "spcd" (FIA numeric), "tt" (TallTimber numeric) or "common" (e.g.
#'   "red spruce"). Ignored when `species_map` is given.
#' @param species_map optional named vector mapping your own species codes
#'   to FVS alpha codes, e.g. `c(RedSpr = "RS", Fir = "BF")`
#' @param source label stored in the `source` column
#' @return data.frame tree list with columns `stand`, `plot`, `tree`, `fvs`,
#'   `spcd`, `dbh`, `ht`, `cr` (fraction), `tpa`, `source`. Attribute
#'   `n_plots` holds the number of plots per stand.
#' @examples
#' # fixed 1/10-acre plots, FVS codes, heights measured on some trees
#' fx <- data.frame(Stand = "S1", Plot = c(1, 1, 2), Species = c("RS", "BF", "RM"),
#'                  DBH = c(9.2, 6.1, 11.4), Height = c(55, NA, 62))
#' expand_plots(fx, design = "fixed", plot_acres = 0.1, ht = "height",
#'              species_codes = "fvs")
#' @export
expand_plots <- function(data, design = c("prism", "fixed"), baf = 20,
                         plot_acres = NULL, plot_radius = NULL,
                         stand = "stand", plot = "plot", species = "species",
                         dbh = "dbh", ht = NULL, cr = NULL,
                         cr_scale = c("auto", "percent", "fraction"), reps = NULL,
                         species_codes = c("fvs", "spcd", "tt", "common"),
                         species_map = NULL, source = "overstory") {
  design <- match.arg(design); cr_scale <- match.arg(cr_scale)
  species_codes <- match.arg(species_codes)
  d <- as.data.frame(data)
  col <- function(x) .find_col(d, x)
  stand <- col(stand); plot <- col(plot); species <- col(species); dbh <- col(dbh)
  if (!is.null(ht)) ht <- col(ht)
  if (!is.null(cr)) cr <- col(cr)
  if (!is.null(reps)) reps <- col(reps)
  if (design == "fixed") {
    if (is.null(plot_acres) && is.null(plot_radius))
      stop("fixed-area plots need plot_acres or plot_radius")
    if (is.null(plot_acres)) plot_acres <- pi * plot_radius^2 / 43560
  }

  d <- d[!is.na(d[[stand]]) & !is.na(d[[plot]]), , drop = FALSE]
  st <- trimws(as.character(d[[stand]]))
  n_plots <- tapply(paste(st, d[[plot]]), st, function(p) length(unique(p)))

  sp_raw <- d[[species]]
  D <- suppressWarnings(as.numeric(d[[dbh]]))
  keep <- !is.na(sp_raw) & !(trimws(as.character(sp_raw)) %in% c("0", "")) & !is.na(D) & D > 0
  d <- d[keep, , drop = FALSE]; st <- st[keep]; D <- D[keep]

  fvs <- .to_fvs(d[[species]], species_codes, species_map)
  if (anyNA(fvs)) {
    bad <- unique(d[[species]][is.na(fvs)])
    warning("dropping ", sum(is.na(fvs)), " trees with unknown species code(s): ",
            paste(utils::head(bad, 20), collapse = ", "),
            ". Supply species_map = c(yourcode = \"FVS\") to keep them.")
    k <- !is.na(fvs)
    d <- d[k, , drop = FALSE]; st <- st[k]; fvs <- fvs[k]; D <- D[k]
  }
  mult <- if (is.null(reps)) 1 else {
    r <- suppressWarnings(as.numeric(d[[reps]])); ifelse(is.na(r) | r < 1, 1, r) }
  per_tree <- if (design == "prism") baf / (0.005454154 * D^2) else 1 / plot_acres
  tpa <- per_tree * mult / as.numeric(n_plots[st])

  crv <- if (is.null(cr)) rep(NA_real_, nrow(d)) else .parse_cr(d[[cr]], cr_scale)
  htv <- if (is.null(ht)) rep(NA_real_, nrow(d)) else {
    h <- suppressWarnings(as.numeric(d[[ht]])); h[!is.na(h) & h <= 4.5] <- NA; h }

  out <- data.frame(stand = st, plot = d[[plot]], tree = seq_len(nrow(d)),
                    fvs = fvs, spcd = species_lookup(fvs, "fvs", "spcd"),
                    dbh = D, ht = htv, cr = crv, tpa = tpa, source = source,
                    stringsAsFactors = FALSE)
  attr(out, "n_plots") <- n_plots
  out
}

#' Expand a prism (variable-radius) point cruise
#'
#' Shortcut for [expand_plots()] with `design = "prism"` and the TallTimber
#' column names used by UMaine cruises (`Type`, `Plot`, `Spp`, `DBH`,
#' `Tree_Notes`).
#'
#' @inheritParams expand_plots
#' @examples
#' cruise <- data.frame(Type = c("A1", "A1", "A1", "B2"), Plot = c(1, 1, 2, 3),
#'                      Spp = c(3, 4, 8, 0), DBH = c(10, 8, 12, 5),
#'                      Tree_Notes = c("40", "35", "50 forked", "no tally"))
#' expand_point_cruise(cruise)
#' @export
expand_point_cruise <- function(data, baf = 20, stand = "Type", plot = "Plot",
                                species = "Spp", dbh = "DBH", cr = "Tree_Notes",
                                reps = NULL, species_codes = c("tt", "fvs", "spcd", "common"),
                                species_map = NULL) {
  expand_plots(data, design = "prism", baf = baf, stand = stand, plot = plot,
               species = species, dbh = dbh, cr = cr, cr_scale = "percent",
               reps = reps, species_codes = match.arg(species_codes),
               species_map = species_map)
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
#' @param stand,plot,species column names (case-insensitive)
#' @param cr_default crown ratio assigned to saplings (not measured)
#' @inheritParams expand_plots
#' @return tree list in the same format as [expand_plots()]
#' @export
expand_sapling_tally <- function(data, n_plots, radius = 5.3,
                                 class_cols = c(`0.6" to 1.5"` = 1, `1.6" to 2.5"` = 2,
                                                `2.6" to 3.5"` = 3, `3.6" to 4.5"` = 4),
                                 stand = "Stand_ID", plot = "Plot", species = "Species",
                                 species_codes = c("tt", "fvs", "spcd", "common"),
                                 species_map = NULL, cr_default = 0.6) {
  species_codes <- match.arg(species_codes)
  d <- as.data.frame(data)
  stand <- .find_col(d, stand); plot <- .find_col(d, plot); species <- .find_col(d, species)
  names(class_cols) <- vapply(names(class_cols), function(x) .find_col(d, x), "")
  expf <- 43560 / (pi * radius^2)
  rows <- list()
  for (cc in names(class_cols)) {
    n <- suppressWarnings(as.numeric(d[[cc]]))
    ok <- !is.na(n) & n > 0
    if (!any(ok)) next
    rows[[cc]] <- data.frame(stand = trimws(as.character(d[[stand]][ok])), plot = d[[plot]][ok],
                             sp = d[[species]][ok], dbh = unname(class_cols[cc]),
                             n = n[ok], stringsAsFactors = FALSE)
  }
  if (!length(rows)) return(NULL)
  s <- do.call(rbind, rows)
  s$fvs <- .to_fvs(s$sp, species_codes, species_map)
  drop <- is.na(s$fvs) | !(s$stand %in% names(n_plots))
  if (any(drop))
    message("expand_sapling_tally: dropped ", sum(drop),
            " rows (unknown species code, or stand not in the overstory cruise)")
  s <- s[!drop, , drop = FALSE]
  data.frame(stand = s$stand, plot = s$plot, tree = seq_len(nrow(s)) + 1e6,
             fvs = s$fvs, spcd = species_lookup(s$fvs, "fvs", "spcd"),
             dbh = s$dbh, ht = NA_real_, cr = cr_default,
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

#' Build a complete tree list from any inventory
#'
#' The one-call route from field data to a tree list both growth engines
#' accept. `overstory` (and optionally `saplings`) can be a data.frame or a
#' path to a .csv/.xlsx/.xls file. Columns are named through `cols`, matched
#' case-insensitively. Steps:
#'
#' 1. [expand_plots()]: expand to trees per acre (prism or fixed plots).
#' 2. [expand_sapling_tally()] (optional): add a fixed-radius sapling tally
#'    in wide format, one count column per diameter class.
#' 3. [complete_trees()]: fill missing heights and crown ratios.
#'
#' @param overstory data.frame or file path; one row per tallied tree
#' @param saplings optional data.frame or file path of sapling counts
#' @param design,baf,plot_acres,plot_radius,cr_scale,species_codes,species_map
#'   see [expand_plots()]
#' @param cols named list of overstory column names: `stand`, `plot`,
#'   `species`, `dbh` (required) and `ht`, `cr`, `reps`, `acres` (optional;
#'   `acres` is stand area, used for forest totals)
#' @param sapling_cols named list of sapling column names: `stand`, `plot`,
#'   `species`
#' @param sapling_classes named vector, sapling count column -> class
#'   midpoint DBH (in.)
#' @param sapling_radius sapling plot radius (ft)
#' @param sheet worksheet to read when a path is an Excel file
#' @return tree list (see [expand_plots()]) with `ht_imputed` and
#'   `cr_imputed` flags; attributes `n_plots` and, if `cols$acres` is given,
#'   `stand_acres`
#' @examples
#' plots <- data.frame(
#'   Stand = rep(c("North", "South"), each = 4), Plot = c(1, 1, 2, 2, 1, 1, 2, 2),
#'   Spp = c("RS", "BF", "RS", "EH", "WP", "RM", "WP", "RO"),
#'   DBH = c(10.1, 7.3, 12.4, 15.0, 18.2, 8.8, 21.5, 11.0),
#'   CR  = c(45, 50, 40, 55, 35, 40, 30, 45))
#' tl <- read_inventory(plots, design = "fixed", plot_acres = 1/10,
#'                      cols = list(stand = "Stand", plot = "Plot", species = "Spp",
#'                                  dbh = "DBH", cr = "CR"))
#' stand_summary(tl)
#' @export
read_inventory <- function(overstory, saplings = NULL,
                           design = c("prism", "fixed"), baf = 20,
                           plot_acres = NULL, plot_radius = NULL,
                           cols = list(stand = "stand", plot = "plot",
                                       species = "species", dbh = "dbh"),
                           cr_scale = c("auto", "percent", "fraction"),
                           species_codes = c("fvs", "spcd", "tt", "common"),
                           species_map = NULL,
                           sapling_cols = cols[c("stand", "plot", "species")],
                           sapling_classes = c(`1` = 1, `2` = 2, `3` = 3, `4` = 4),
                           sapling_radius = 5.3, sheet = 1) {
  design <- match.arg(design); cr_scale <- match.arg(cr_scale)
  species_codes <- match.arg(species_codes)
  need <- c("stand", "plot", "species", "dbh")
  if (length(miss <- setdiff(need, names(cols))))
    stop("cols is missing: ", paste(miss, collapse = ", "))
  ov <- .read_any(overstory, sheet)
  tr <- expand_plots(ov, design = design, baf = baf, plot_acres = plot_acres,
                     plot_radius = plot_radius, stand = cols$stand, plot = cols$plot,
                     species = cols$species, dbh = cols$dbh, ht = cols$ht, cr = cols$cr,
                     cr_scale = cr_scale, reps = cols$reps,
                     species_codes = species_codes, species_map = species_map)
  np <- attr(tr, "n_plots")
  if (!is.null(saplings)) {
    sp <- .read_any(saplings, sheet)
    st <- expand_sapling_tally(sp, n_plots = np, radius = sapling_radius,
                               class_cols = sapling_classes,
                               stand = sapling_cols$stand, plot = sapling_cols$plot,
                               species = sapling_cols$species,
                               species_codes = species_codes, species_map = species_map)
    if (!is.null(st)) tr <- rbind(tr, st[, names(tr)])
  }
  tr <- complete_trees(tr)
  attr(tr, "n_plots") <- np
  if (!is.null(cols$acres)) {
    a <- .find_col(ov, cols$acres)
    attr(tr, "stand_acres") <- tapply(suppressWarnings(as.numeric(ov[[a]])),
                                      trimws(as.character(ov[[.find_col(ov, cols$stand)]])),
                                      function(x) stats::median(x, na.rm = TRUE))
  }
  tr
}

#' Read the Demeritt Forest 2025 inventory into a tree list
#'
#' [read_inventory()] preset for the UMaine Demeritt Forest (DBD)
#' Inventory Project 66 files: BAF-20 overstory points
#' (`DBD_66_Tree_DataBAF20_StandsOK.xlsx`) and nested 5.3-ft radius sapling
#' plots (`Sapling_MASTER_DBDWith2026.xlsx`). Species are TallTimber codes and
#' crown ratio (percent) is in `Tree_Notes`. Use it as a template for other
#' TallTimber-style cruises.
#'
#' @param overstory,saplings paths (or data.frames) for the two workbooks
#' @return see [read_inventory()]
#' @examples
#' \dontrun{
#' dir <- "G:/My Drive/Research/UMaine_Demerritt/UF_Demeritt_MgtPlan2026/2025_DBD_Inventory"
#' trees <- read_demeritt_inventory(file.path(dir, "DBD_66_Tree_DataBAF20_StandsOK.xlsx"),
#'                                  file.path(dir, "Sapling_MASTER_DBDWith2026.xlsx"))
#' }
#' @export
read_demeritt_inventory <- function(overstory, saplings = NULL) {
  read_inventory(overstory, saplings, design = "prism", baf = 20,
                 cols = list(stand = "Type", plot = "Plot", species = "Spp", dbh = "DBH",
                             cr = "Tree_Notes", reps = "Reps", acres = "Acres_int"),
                 cr_scale = "percent", species_codes = "tt",
                 sapling_cols = list(stand = "Stand_ID", plot = "Plot", species = "Species"),
                 sapling_classes = c(`0.6" to 1.5"` = 1, `1.6" to 2.5"` = 2,
                                     `2.6" to 3.5"` = 3, `3.6" to 4.5"` = 4),
                 sapling_radius = 5.3)
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

.parse_cr <- function(x, scale = c("percent", "fraction", "auto")) {
  scale <- match.arg(scale)
  txt <- as.character(x)
  v <- suppressWarnings(as.numeric(sub("^\\s*([0-9]*\\.?[0-9]+).*$", "\\1", txt)))
  v[!grepl("^\\s*[0-9.]", txt)] <- NA
  if (scale == "auto") scale <- if (any(v > 1, na.rm = TRUE)) "percent" else "fraction"
  if (scale == "percent") v <- v / 100
  v[!is.na(v) & (v <= 0 | v > 1)] <- NA
  v
}

.to_fvs <- function(x, codes, map = NULL) {
  if (!is.null(map)) {
    f <- toupper(unname(map[trimws(as.character(x))]))
    f[!f %in% meforlab_species()$fvs] <- NA
    return(f)
  }
  num <- function(v) suppressWarnings(as.numeric(as.character(v)))
  switch(codes,
         tt     = species_lookup(num(x), "tt", "fvs"),
         spcd   = species_lookup(num(x), "spcd", "fvs"),
         common = { sp <- meforlab_species()
                    sp$fvs[match(tolower(trimws(x)), tolower(sp$common))] },
         fvs    = { f <- toupper(trimws(x)); f[!f %in% meforlab_species()$fvs] <- NA; f })
}

.find_col <- function(d, name) {
  if (is.null(name)) return(NULL)
  if (name %in% names(d)) return(name)
  hit <- names(d)[tolower(names(d)) == tolower(name)]
  if (!length(hit)) stop("column '", name, "' not found; available: ",
                         paste(names(d), collapse = ", "))
  hit[1]
}

.read_any <- function(x, sheet = 1) {
  if (is.data.frame(x)) return(as.data.frame(x))
  if (!is.character(x) || !file.exists(x)) stop("not a data.frame or existing file: ", x)
  if (grepl("\\.csv$", x, ignore.case = TRUE))
    return(utils::read.csv(x, stringsAsFactors = FALSE, check.names = FALSE))
  if (grepl("\\.xlsx?$", x, ignore.case = TRUE)) {
    if (!requireNamespace("readxl", quietly = TRUE))
      stop("install.packages('readxl') to read Excel files")
    return(suppressWarnings(as.data.frame(readxl::read_excel(x, sheet = sheet))))
  }
  stop("unsupported file type: ", x)
}

.top_height <- function(dbh, ht, tpa, n = 40) {
  o <- order(dbh, decreasing = TRUE)
  w <- pmin(tpa[o], pmax(n - c(0, cumsum(tpa[o]))[seq_along(o)], 0))
  if (sum(w) <= 0) return(NA_real_)
  sum(w * ht[o]) / sum(w)
}
