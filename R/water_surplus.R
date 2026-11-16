#' Water surplus for a set of points with per-point time windows
#'
#' Extracts monthly precipitation, terrain-corrected Hargreaves reference
#' evapotranspiration (ETo) and water surplus (WS = P - ETo) for each point on
#' the Northern Forest 20 m grid, 1991-2020, then summarises each point over its
#' own window and by temperature-derived growing season.
#'
#' @section Method:
#' ETo follows FAO-56 Hargreaves with extraterrestrial radiation integrated over
#' every day of the month (\eqn{0.0023 \cdot 0.408 R_a (T_{mean} + 17.8)
#' \sqrt{T_{max} - T_{min}}}, \eqn{T_{mean} = (T_{max} + T_{min})/2}), computed
#' on the PRISM 800 m monthly series and set to zero in months with mean
#' temperature below 0 degrees C. The 800 m ETo and precipitation are evaluated
#' bilinearly at the centre of the 20 m cell containing each point, and ETo is
#' multiplied by the 20 m terrain radiation ratio (TRR) for the calendar month.
#' This reproduces the stored `WS_YYYYMM.tif` rasters to within 1 mm (those
#' files truncate to integer mm).
#'
#' Terrain-corrected ETo is not numerically comparable to published Hargreaves
#' values, because the 0.0023 coefficient was fitted against flat-surface
#' radiation. `eto = "flat"` returns the uncorrected values.
#'
#' The growing season for each point-year runs between the spring and autumn
#' crossings of `threshold` by monthly mean temperature, each placed by linear
#' interpolation between mid-month values. Months at the season edges
#' contribute in proportion to the days they fall inside the season, assuming
#' P and ETo are uniform within a month. Season limits always come from the
#' full year's temperatures; WS accumulates only over the part of the season
#' inside the point's window, and a season cut by the window is flagged
#' `partial` and excluded from `gs_ws_mean_mm`.
#'
#' @section Windows:
#' `start` and `end` accept `Date`, `POSIXct`, a year (numeric or character),
#' `"YYYY-MM"`, `"YYYY-MM-DD"` or `"M/D/YYYY"`. Resolution is monthly and
#' inclusive: a bare year means January for `start` and December for `end`;
#' the day of month is ignored. Missing values mean 1991-01 / 2020-12. Windows
#' extending beyond 1991-2020 are clamped and noted.
#'
#' @param x A data frame of points, or an `sf` point object (any CRS; requires
#'   the sf package).
#' @param lat,lon Column names of latitude and longitude in decimal degrees
#'   (NAD83 / WGS84). Ignored when `x` is `sf`.
#' @param id Optional column name of a point identifier. Defaults to row number.
#' @param start,end Optional column names of window start and end. See Windows.
#' @param threshold Growing-season threshold on monthly mean temperature,
#'   degrees C.
#' @param eto `"terrain"` (default) or `"flat"`.
#' @param output Which result to return: `"all"` (a list), `"summary"`,
#'   `"growing_season"` or `"monthly"`.
#' @param data_dir Directory holding `TRR_monthly.tif`,
#'   `ETo_flat_monthly_800m.tif` and `800m/{ppt,tmax,tmin}_YYYY.tif`. Defaults
#'   to option `nfws.data_dir`, then environment variable `NFWS_DATA`.
#' @param chunk_size Points extracted per pass. Bounds memory; the monthly table
#'   holds 360 rows per point.
#' @param progress Print progress per chunk.
#'
#' @return For `output = "all"`, a list of class `nf_ws` with elements:
#' \describe{
#'   \item{summary}{One row per input row: window requested and used, snapped
#'     cell centre, `n_months`, `P_mm`, `ETo_mm`, `WS_mm` (cumulative over the
#'     window), `WS_mm_per_yr`, `n_gs`, `gs_ws_mean_mm`, `gs_ws_total_mm`,
#'     `n_gs_deficit`, `n_gs_partial`, and `note` describing any problem.}
#'   \item{growing_season}{One row per point-year with season days inside the
#'     window: dates, `gs_days`, `gs_days_in_window`, `p_gs`, `eto_gs`,
#'     `ws_gs`, `partial`.}
#'   \item{monthly}{In-window months: `ppt_mm`, `tmax_C`, `tmin_C`, `eto_mm`,
#'     `trr`, `ws_mm`, `ws_cum_mm`.}
#' }
#' Rows that cannot be processed are kept in `summary` with `NA` results and
#' a `note`; the function does not stop for bad rows.
#'
#' @examples
#' \dontrun{
#' options(nfws.data_dir = "/users/PUOM0008/premerm/ws_app_inputs")
#' plots <- data.frame(plot = c("A", "B"), lat = c(46.0, 44.9),
#'                     lon = c(-69.8, -71.6),
#'                     planted = c("1991", "1995-06"),
#'                     measured = as.Date(c("2020-08-15", "2003-09-30")))
#' res <- nf_water_surplus(plots, id = "plot", start = "planted", end = "measured")
#' res$summary
#' }
#' @export
nf_water_surplus <- function(x, lat = "lat", lon = "lon", id = NULL,
                             start = NULL, end = NULL, threshold = 5,
                             eto = c("terrain", "flat"),
                             output = c("all", "summary", "growing_season", "monthly"),
                             data_dir = NULL, chunk_size = 5000L, progress = interactive()) {
  eto <- match.arg(eto); output <- match.arg(output)
  if (!requireNamespace("terra", quietly = TRUE)) stop("nf_water_surplus() needs the terra package.", call. = FALSE)
  stopifnot(is.numeric(threshold), length(threshold) == 1, is.finite(threshold))
  P <- nf_ws_data_check(data_dir)
  pts <- .nfws_points(x, lat, lon, id, start, end)
  n <- nrow(pts)

  idx <- split(seq_len(n), ceiling(seq_len(n) / chunk_size))
  S <- vector("list", length(idx)); G <- S; M <- S
  for (j in seq_along(idx)) {
    i <- idx[[j]]; p <- pts[i, , drop = FALSE]
    if (progress) message(sprintf("chunk %d/%d: %d points", j, length(idx), length(i)))
    X <- .nfws_extract(ifelse(p$use, p$lon, NA), ifelse(p$use, p$lat, NA), P)
    X$mon <- .nfws_swap(X$mon, eto)
    r <- .nfws_summarise(p, X, threshold)
    S[[j]] <- r$summary; G[[j]] <- r$gs
    if (output %in% c("all", "monthly")) M[[j]] <- .nfws_monthly(p, X)
  }
  summary <- do.call(rbind, S); rownames(summary) <- NULL
  summary$gs_threshold_C <- threshold; summary$eto <- eto
  gs <- do.call(rbind, G); if (!is.null(gs)) rownames(gs) <- NULL
  mon <- do.call(rbind, M); if (!is.null(mon)) rownames(mon) <- NULL

  switch(output,
         summary = summary, growing_season = gs, monthly = mon,
         structure(list(summary = summary, growing_season = gs, monthly = mon),
                   class = "nf_ws", threshold = threshold, eto = eto))
}

#' Check that the water-surplus input rasters are present
#'
#' @inheritParams nf_water_surplus
#' @return Invisibly, a list of the resolved paths. Stops with a message naming
#'   any missing file.
#' @export
nf_ws_data_check <- function(data_dir = NULL) {
  if (is.null(data_dir)) data_dir <- getOption("nfws.data_dir", Sys.getenv("NFWS_DATA"))
  if (!nzchar(data_dir))
    stop("Set data_dir, options(nfws.data_dir = ...), or the NFWS_DATA environment variable.",
         call. = FALSE)
  P <- list(trr = file.path(data_dir, "TRR_monthly.tif"),
            eto = file.path(data_dir, "ETo_flat_monthly_800m.tif"),
            d800 = file.path(data_dir, "800m"))
  need <- c(P$trr, P$eto,
            file.path(P$d800, sprintf("%s_%d.tif", rep(c("ppt", "tmax", "tmin"), each = 30),
                                      rep(1991:2020, 3))))
  miss <- need[!file.exists(need)]
  if (length(miss))
    stop(sprintf("%d input file(s) missing under %s, e.g. %s", length(miss), data_dir,
                 basename(miss[1])), call. = FALSE)
  invisible(P)
}

#' @export
print.nf_ws <- function(x, ...) {
  s <- x$summary
  cat(sprintf("<nf_ws> %d points, %d with results; threshold %g C, ETo %s\n",
              nrow(s), sum(!is.na(s$WS_mm)), attr(x, "threshold"), attr(x, "eto")))
  if (any(nzchar(s$note))) cat(sprintf("%d rows have notes; see $summary$note\n", sum(nzchar(s$note))))
  print(utils::head(s[, c("id", "start_used", "end_used", "n_months", "WS_mm",
                          "WS_mm_per_yr", "gs_ws_mean_mm", "note")]), row.names = FALSE)
  invisible(x)
}

# ---- internals ---------------------------------------------------------------

.NFWS_Y0 <- 1991L
.NFWS_Y1 <- 2020L
.nfws_k0 <- function() .NFWS_Y0 * 12L
.nfws_k1 <- function() .NFWS_Y1 * 12L + 11L
.nfws_ym <- function(k) ifelse(is.na(k), NA_character_, sprintf("%04d-%02d", k %/% 12L, k %% 12L + 1L))

.nfws_dim <- function(y) {
  leap <- (y %% 4 == 0 & y %% 100 != 0) | y %% 400 == 0
  c(31, if (leap) 29 else 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
}

# Month index (year*12 + month-1) from Date / POSIXct / year / text.
.nfws_parse_ym <- function(x, end = FALSE) {
  if (inherits(x, c("Date", "POSIXt"))) x <- format(x, "%Y-%m")
  x <- trimws(as.character(x)); x[is.na(x)] <- ""
  x <- sub("\\.0+$", "", x)                                   # 1995.0 from numeric columns
  us <- grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$", x)
  if (any(us)) x[us] <- vapply(strsplit(x[us], "/"),
                               function(v) sprintf("%s-%02d", v[3], as.integer(v[1])), "")
  y <- suppressWarnings(as.integer(substr(x, 1, 4)))
  m <- suppressWarnings(as.integer(substr(x, 6, 7)))
  m[nchar(x) < 7 | is.na(m)] <- if (end) 12L else 1L
  k <- y * 12L + m - 1L
  k[is.na(y) | nchar(x) < 4 | m < 1 | m > 12] <- NA
  k
}

.nfws_points <- function(x, lat, lon, id, start, end) {
  if (inherits(x, "sf")) {
    if (!requireNamespace("sf", quietly = TRUE)) stop("sf object supplied but sf is not installed.")
    xy <- sf::st_coordinates(sf::st_transform(sf::st_geometry(x), 4269))
    df <- sf::st_drop_geometry(x); la <- xy[, 2]; lo <- xy[, 1]
  } else {
    if (!is.data.frame(x)) stop("x must be a data frame or an sf object.")
    for (v in c(lat, lon)) if (!v %in% names(x)) stop(sprintf("column '%s' not found in x", v))
    df <- x; la <- x[[lat]]; lo <- x[[lon]]
  }
  col <- function(v) {
    if (is.null(v)) return(NULL)
    if (!v %in% names(df)) stop(sprintf("column '%s' not found in x", v))
    df[[v]]
  }
  n <- nrow(df)
  idv <- col(id); if (is.null(idv)) idv <- seq_len(n)
  sv <- col(start); ev <- col(end)
  si <- if (is.null(sv)) rep(NA_integer_, n) else .nfws_parse_ym(sv)
  ei <- if (is.null(ev)) rep(NA_integer_, n) else .nfws_parse_ym(ev, TRUE)
  sr <- if (is.null(sv)) rep("", n) else as.character(sv)
  er <- if (is.null(ev)) rep("", n) else as.character(ev)
  sr[is.na(sr)] <- ""; er[is.na(er)] <- ""

  note <- rep("", n)
  add <- function(b, msg) note[b] <<- paste(note[b], msg)
  add(nzchar(sr) & is.na(si), "start unparsed, used 1991-01;")
  add(nzchar(er) & is.na(ei), "end unparsed, used 2020-12;")
  si[is.na(si)] <- .nfws_k0(); ei[is.na(ei)] <- .nfws_k1()
  add(si < .nfws_k0() | ei > .nfws_k1(), "window clamped to 1991-01..2020-12;")

  p <- data.frame(id = as.character(idv),
                  lat = suppressWarnings(as.numeric(la)), lon = suppressWarnings(as.numeric(lo)),
                  start_req = sr, end_req = er,
                  s = pmax(si, .nfws_k0()), e = pmin(ei, .nfws_k1()), stringsAsFactors = FALSE)
  inv <- p$e < p$s
  add(inv, "window empty (end before start, or wholly outside 1991-2020);")
  oor <- !is.finite(p$lat) | !is.finite(p$lon) | p$lat < 40 | p$lat > 49 | p$lon < -81 | p$lon > -66
  add(oor, "lat/lon missing or outside region (lon must be negative);")
  if (anyDuplicated(p$id)) add(duplicated(p$id) | duplicated(p$id, fromLast = TRUE), "duplicate id;")
  p$note <- trimws(note); p$use <- !inv & !oor
  p
}

.nfws_exm <- function(r, xy, ...) {
  v <- terra::extract(r, xy, ...)
  as.matrix(v[, setdiff(names(v), "ID"), drop = FALSE])
}

# One read per raster for all points. The 20 m grid is EPSG:6350; the 800 m
# series is NAD83 geographic. Inputs are evaluated at the 20 m cell centre.
.nfws_extract <- function(lon, lat, P, yrs = .NFWS_Y0:.NFWS_Y1) {
  trr <- terra::rast(P$trr); n <- length(lon); ny <- length(yrs)
  cell <- rep(NA_real_, n); g <- is.finite(lon) & is.finite(lat)
  if (any(g)) {
    v <- terra::project(terra::vect(cbind(lon[g], lat[g]), crs = "EPSG:4269"), "EPSG:6350")
    cell[g] <- terra::cellFromXY(trr, terra::crds(v))
  }
  g <- !is.na(cell)
  xy <- matrix(NA_real_, n, 2); ll <- xy; tr <- matrix(NA_real_, n, 12)
  if (any(g)) {
    xy[g, ] <- terra::xyFromCell(trr, cell[g])
    ll[g, ] <- terra::crds(terra::project(terra::vect(xy[g, , drop = FALSE], crs = "EPSG:6350"), "EPSG:4269"))
    tr[g, ] <- .nfws_exm(trr, xy[g, , drop = FALSE])
  }
  g <- g & rowSums(is.na(tr)) == 0
  site <- data.frame(pt = seq_len(n), lon_c = ll[, 1], lat_c = ll[, 2], ok = g)
  if (!any(g)) return(list(site = site, mon = NULL))

  L <- ll[g, , drop = FALSE]
  b8 <- function(v) do.call(cbind, lapply(yrs, function(y)
    .nfws_exm(terra::rast(file.path(P$d800, sprintf("%s_%d.tif", v, y))), L, method = "bilinear")))
  bi <- rep((yrs - .NFWS_Y0) * 12L, each = 12) + rep(1:12, ny)
  e8 <- .nfws_exm(terra::rast(P$eto)[[bi]], L, method = "bilinear")
  pp <- b8("ppt"); tx <- b8("tmax"); tn <- b8("tmin")
  ng <- sum(g); mo <- rep(1:12, ny)
  mon <- data.frame(pt = rep(which(g), each = 12L * ny),
                    year = rep(rep(yrs, each = 12), ng), month = rep(mo, ng),
                    ppt = as.vector(t(pp)), tmax = as.vector(t(tx)), tmin = as.vector(t(tn)),
                    eto_flat = as.vector(t(e8)), trr = as.vector(t(tr[g, mo, drop = FALSE])))
  list(site = site, mon = .nfws_derive(mon))
}

.nfws_derive <- function(mon) {
  mon$eto_terr <- mon$eto_flat * mon$trr
  mon$ws <- mon$ppt - mon$eto_terr
  mon$ws_flat <- mon$ppt - mon$eto_flat
  mon$tmid <- (mon$tmax + mon$tmin) / 2
  mon$k <- mon$year * 12L + mon$month - 1L
  mon
}

# After this, ws / eto_terr carry whichever ETo the caller asked for.
.nfws_swap <- function(m, eto) {
  if (!is.null(m) && eto == "flat") { m$ws <- m$ws_flat; m$eto_terr <- m$eto_flat }
  m
}

.nfws_gs1 <- function(d, thr) {
  dm <- .nfws_dim(d$year[1]); me <- cumsum(dm); ms <- me - dm; mid <- me - dm / 2
  tt <- d$tmid; a <- which(tt >= thr); w <- d$inw
  z <- data.frame(year = d$year[1], gs_start = NA_real_, gs_end = NA_real_, gs_days = 0,
                  gs_days_in_window = 0, p_gs = 0, eto_gs = 0, ws_gs = 0, partial = FALSE)
  if (!length(a) || anyNA(tt)) return(z)
  i1 <- a[1]; i2 <- a[length(a)]
  on  <- if (i1 == 1)  0      else mid[i1 - 1] + (thr - tt[i1 - 1]) / (tt[i1] - tt[i1 - 1]) * (mid[i1] - mid[i1 - 1])
  off <- if (i2 == 12) me[12] else mid[i2] + (tt[i2] - thr) / (tt[i2] - tt[i2 + 1]) * (mid[i2 + 1] - mid[i2])
  f0 <- pmax(0, pmin(me, off) - pmax(ms, on)) / dm
  f <- f0 * w
  z$gs_start <- on; z$gs_end <- off; z$gs_days <- off - on
  z$gs_days_in_window <- sum(f * dm)
  z$p_gs <- sum(f * d$ppt); z$eto_gs <- sum(f * d$eto_terr); z$ws_gs <- sum(f * d$ws)
  z$partial <- any(f0 > 0 & !w)
  z
}

.nfws_summarise <- function(pts, X, thr) {
  m <- X$mon; site <- X$site
  out <- data.frame(pts[, c("id", "lat", "lon", "start_req", "end_req")],
                    start_used = .nfws_ym(pts$s), end_used = .nfws_ym(pts$e),
                    lon_cell = site$lon_c, lat_cell = site$lat_c,
                    n_months = NA_integer_, P_mm = NA_real_, ETo_mm = NA_real_,
                    WS_mm = NA_real_, WS_mm_per_yr = NA_real_,
                    n_gs = NA_integer_, gs_ws_mean_mm = NA_real_, gs_ws_total_mm = NA_real_,
                    n_gs_deficit = NA_integer_, n_gs_partial = NA_integer_,
                    stringsAsFactors = FALSE)
  out$start_used[!pts$use] <- NA; out$end_used[!pts$use] <- NA
  nt <- pts$note
  b <- pts$use & !site$ok
  nt[b] <- paste(nt[b], "no 20 m data at point (water or outside grid);")
  gsl <- list(); ok <- which(pts$use & site$ok)
  if (length(ok) && !is.null(m)) {
    ms <- split(m, m$pt)
    for (i in ok) {
      d <- ms[[as.character(i)]]
      d$inw <- d$k >= pts$s[i] & d$k <= pts$e[i]
      w <- d[d$inw, ]
      if (anyNA(w$ws)) nt[i] <- paste(nt[i], "missing climate in window;")
      out$n_months[i] <- nrow(w)
      out$P_mm[i] <- sum(w$ppt); out$ETo_mm[i] <- sum(w$eto_terr); out$WS_mm[i] <- sum(w$ws)
      out$WS_mm_per_yr[i] <- 12 * sum(w$ws) / nrow(w)
      dd <- d[d$year %in% unique(w$year), ]
      g <- do.call(rbind, lapply(split(dd, dd$year), .nfws_gs1, thr = thr))
      inn <- g$gs_days_in_window > 0; full <- inn & !g$partial
      out$n_gs[i] <- sum(inn)
      out$gs_ws_total_mm[i] <- sum(g$ws_gs)
      out$gs_ws_mean_mm[i] <- if (any(full)) mean(g$ws_gs[full]) else NA_real_
      out$n_gs_deficit[i] <- sum(g$ws_gs[full] < 0)
      out$n_gs_partial[i] <- sum(inn & g$partial)
      if (any(inn)) gsl[[length(gsl) + 1]] <- data.frame(id = pts$id[i], g[inn, , drop = FALSE])
    }
  }
  out$note <- trimws(nt)
  gs <- if (length(gsl)) do.call(rbind, gsl) else NULL
  if (!is.null(gs)) {
    j1 <- as.Date(sprintf("%d-01-01", gs$year))
    gs$gs_start_date <- j1 + floor(gs$gs_start)
    gs$gs_end_date <- j1 + floor(gs$gs_end)
    gs <- gs[, c("id", "year", "gs_start_date", "gs_end_date", "gs_days", "gs_days_in_window",
                 "p_gs", "eto_gs", "ws_gs", "partial")]
  }
  list(summary = out, gs = gs)
}

.nfws_monthly <- function(pts, X) {
  m <- X$mon; if (is.null(m)) return(NULL)
  m <- m[pts$use[m$pt] & m$k >= pts$s[m$pt] & m$k <= pts$e[m$pt], ]
  if (!nrow(m)) return(NULL)
  data.frame(id = pts$id[m$pt], year = m$year, month = m$month,
             ppt_mm = m$ppt, tmax_C = m$tmax, tmin_C = m$tmin,
             eto_mm = m$eto_terr, trr = m$trr, ws_mm = m$ws,
             ws_cum_mm = stats::ave(m$ws, m$pt, FUN = cumsum))
}
