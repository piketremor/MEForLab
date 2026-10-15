## Stem taper, bark, and volume: the core used by every merchandising function
##
## Regions
##   "NE"  Northeast / Acadian parameters (Li & Weiskittel; MacFarlane &
##         Weiskittel 2016; FIA refits) from GreenTimbMerch (Premer,
##         adapted from Weiskittel and Smith).
##   "GL"  Great Lakes / Lake States. No Lake States-specific Kozak fits are
##         bundled yet, so GL uses the NE parameter set. The region is kept
##         separate so GL-fitted parameters can be dropped into
##         inst/extdata/kozak_taper.csv without changing any code.
##   "PNW" Pacific Northwest. Coastal Douglas-fir (Kozak 2004, BC) as used by
##         the original Kozak_merchR(); other species fall back to DF.

#' Kozak taper parameters
#'
#' @return data.frame with one row per region x species x bark ("ob"/"ib")
#'   and the Kozak (2004) coefficients a0-a2, b1-b7
#' @examples
#' head(kozak_params())
#' table(kozak_params()$region)
#' @export
kozak_params <- function() {
  if (is.null(.mfl$kozak)) {
    p <- utils::read.csv(system.file("extdata", "kozak_taper.csv", package = "MEForLab"),
                         stringsAsFactors = FALSE)
    gl <- p[p$region == "NE", ]; gl$region <- "GL"
    gl$source <- paste0(gl$source, "; NE parameters used for GL")
    .mfl$kozak <- rbind(p, gl)
  }
  .mfl$kozak
}

# species code aliases between FVS-NE / MEForLab codes and the taper table
.taper_alias <- c(RN = "RP", NC = "WC", TL = "TA", BE = "AB", HE = "EH", HM = "SM",
                  PO = "BT", WB = "PB", AS = "WA", OHW = "OH")

# resolve the parameter row for each tree; returns list(row index, used code, bark used)
.taper_rows <- function(spp, region, bark) {
  p <- kozak_params()
  spp <- toupper(trimws(as.character(spp)))
  a <- .taper_alias[spp]; spp[!is.na(a)] <- a[!is.na(a)]
  key <- paste(region, spp, bark)
  i <- match(key, paste(p$region, p$spp, p$bark))
  if (any(is.na(i))) {
    if (region == "PNW") {
      fb <- rep("DF", length(spp))
    } else {
      soft <- species_lookup(spp, "fvs", "softwood")
      gen <- .species_values()$wood_type[match(spp, .species_values()$spp)]
      soft[is.na(soft)] <- !is.na(gen[is.na(soft)]) & gen[is.na(soft)] == "SW"
      fb <- ifelse(soft, "OS", "OH")
    }
    i[is.na(i)] <- match(paste(region, fb[is.na(i)], bark), paste(p$region, p$spp, p$bark))
  }
  i
}

#' Kozak (2004) stem taper: diameter at any height
#'
#' Vectorised over trees and heights. Outside-bark (`"ob"`) diameters use the
#' species' outside-bark fit. Inside-bark (`"ib"`) diameters are, by default,
#' the outside-bark taper converted with the species bark model
#' ([dob_to_dib()]), which is what GreenTimbMerch's volumes used.
#' `ib_method = "fit"` uses an inside-bark taper fit where one exists instead.
#' Some of those fits look doubtful: eastern hemlock's implies about 25%
#' bark at mid-stem. Species without parameters use the generic
#' "OH" (hardwood) or "OS" (softwood) curve in NE/GL and Douglas-fir in PNW.
#'
#' @param dbh diameter at breast height
#' @param ht total height
#' @param h height(s) on the stem where diameter is wanted
#' @param spp species code (FVS alpha, e.g. "RS", "SM", "DF")
#' @param region "NE" (Northeast/Acadian), "GL" (Great Lakes; currently the NE
#'   parameters) or "PNW" (coastal Douglas-fir)
#' @param bark "ob" (outside bark) or "ib" (inside bark)
#' @param planted logical, planted tree (only some NE conifer fits use it)
#' @param ib_method "convert" (outside-bark taper + bark model) or "fit"
#'   (inside-bark taper parameters where available)
#' @param units "imperial" (in., ft) or "metric" (cm, m) for inputs and output
#' @return diameter at `h`, in the same units as `dbh`; 0 at or above the tip
#' @references Kozak, A. (2004). My last words on taper equations. The
#'   Forestry Chronicle 80: 507-515.
#'
#'   Li, R., Weiskittel, A., Dick, A.R., Kershaw, J.A., Seymour, R.S. (2012).
#'   Regional stem taper equations for eleven conifer species in the Acadian
#'   region of North America. Northern Journal of Applied Forestry 29: 5-14.
#'
#'   MacFarlane, D.W., Weiskittel, A.R. (2016). A new method for capturing
#'   stem taper variation for trees of diverse morphological types. Canadian
#'   Journal of Forest Research 46: 804-815.
#' @examples
#' # diameter outside bark every 8 ft up a 14-in, 75-ft red spruce
#' kozak_taper(14, 75, seq(1, 65, 8), "RS")
#' kozak_taper(35.6, 22.9, c(0.3, 5, 10), "SM", units = "metric")
#' kozak_taper(20, 120, c(1, 40, 80), "DF", region = "PNW")
#' @export
kozak_taper <- function(dbh, ht, h, spp, region = c("NE", "GL", "PNW"),
                        bark = c("ob", "ib"), planted = FALSE,
                        units = c("imperial", "metric"), ib_method = c("convert", "fit")) {
  region <- match.arg(region); bark <- match.arg(bark); units <- match.arg(units)
  ib_method <- match.arg(ib_method)
  n <- max(length(dbh), length(ht), length(h), length(spp))
  dbh <- rep_len(dbh, n); ht <- rep_len(ht, n); h <- rep_len(h, n)
  spp <- rep_len(as.character(spp), n); planted <- rep_len(as.numeric(planted), n)
  if (region == "PNW") return(.taper_pnw(dbh, ht, h, spp, bark, units))

  # NE/GL equations are fit in cm and m
  D <- if (units == "imperial") dbh * 2.54 else dbh
  H <- if (units == "imperial") ht * 0.3048 else ht
  Z <- if (units == "imperial") h * 0.3048 else h
  p <- kozak_params()
  use_ib <- bark == "ib" & ib_method == "fit" & !is.na(match(paste(region, .alias(spp), "ib"),
                                        paste(p$region, p$spp, p$bark)))
  out <- numeric(n)
  for (b in unique(c("ob", if (any(use_ib)) "ib"))) {
    k <- if (b == "ib") use_ib else !use_ib
    if (!any(k)) next
    cf <- p[.taper_rows(spp[k], region, b), ]
    y <- .kozak2004(D[k], H[k], Z[k], cf, planted[k])
    if (bark == "ib" && b == "ob") y <- dob_to_dib(y, spp[k], units = "metric")
    out[k] <- y
  }
  out[Z >= H] <- 0
  if (bark == "ob") out[abs(Z - 1.37) < 1e-9] <- D[abs(Z - 1.37) < 1e-9]
  out[Z < 0] <- NA
  if (units == "imperial") out / 2.54 else out
}

.alias <- function(spp) {
  spp <- toupper(trimws(as.character(spp)))
  a <- .taper_alias[spp]; spp[!is.na(a)] <- a[!is.na(a)]; spp
}

.kozak2004 <- function(D, H, Z, cf, planted) {
  p <- 1.3 / H
  z <- pmin(Z / H, 1)
  Xi <- (1 - z^(1/3)) / (1 - p^(1/3))
  Qi <- 1 - z^(1/3)
  y <- (cf$a0 * D^cf$a1 * H^cf$a2) *
    Xi^(cf$b1 * z^4 + cf$b2 * exp(-D / H) + cf$b3 * Xi^0.1 + cf$b4 / D +
          cf$b5 * H^Qi + cf$b6 * Xi + cf$b7 * planted)
  y[!is.finite(y) | y < 0] <- 0
  y
}

# Coastal Douglas-fir form carried over from Kozak_merchR(): imperial inputs,
# breast height 4.5 ft, inside bark
.taper_pnw <- function(dbh, ht, h, spp, bark, units) {
  D <- if (units == "metric") dbh / 2.54 else dbh
  H <- if (units == "metric") ht / 0.3048 else ht
  Z <- if (units == "metric") h / 0.3048 else h
  cf <- kozak_params()[.taper_rows(spp, "PNW", "ib"), ]
  XI <- (1 - (Z / H)^(1/3)) / (1 - (4.5 / H)^(1/3))
  QI <- 1 - (Z / H)^(1/3)
  ZI <- Z / H
  y <- cf$a0 * D^cf$a1 * H^cf$a2 *
    XI^(cf$b1 * ZI^4 + cf$b2 / exp(D / H) + cf$b3 * ((1 - ZI) / (1 - 4.5 / H))^0.1 +
          cf$b4 / D + cf$b5 * H^QI + cf$b6 * XI)
  y[Z >= H | !is.finite(y)] <- 0
  if (bark == "ob" && !isTRUE(getOption("MEForLab.pnw_ib_quiet")))
    message("PNW Douglas-fir taper is an inside-bark fit; returning inside-bark diameters ",
            "(set options(MEForLab.pnw_ib_quiet = TRUE) to silence)")
  if (units == "metric") y * 2.54 else y
}

#' Convert diameter outside bark to inside bark
#'
#' Species bark models from GreenTimbMerch: either a power function
#' (DIB = b0 * DOB^b1, fit in cm) or a fixed bark percentage.
#'
#' @param dob diameter outside bark
#' @param spp species (FVS alpha)
#' @param units "imperial" (in.) or "metric" (cm)
#' @return diameter inside bark, same units as `dob`
#' @examples
#' dob_to_dib(c(10, 15), c("SM", "RS"))
#' @export
dob_to_dib <- function(dob, spp, units = c("imperial", "metric")) {
  units <- match.arg(units)
  if (is.null(.mfl$bark)) .mfl$bark <- utils::read.csv(
    system.file("extdata", "bark_dib.csv", package = "MEForLab"), stringsAsFactors = FALSE)
  b <- .mfl$bark
  spp <- rep_len(.alias(spp), length(dob))
  i <- match(spp, b$spp)
  i[is.na(i)] <- match("YB", b$spp)          # GreenTimbMerch default
  d <- if (units == "imperial") dob * 2.54 else dob
  dib <- ifelse(b$pct_bark[i] == 0, b$b0[i] * d^b$b1[i], d * (1 - b$pct_bark[i] / 100))
  dib[dob <= 0] <- 0
  if (units == "imperial") dib / 2.54 else dib
}

#' Stem volume between two points by integrating the taper
#'
#' Splits the stem between `stump` and the top (the lower of `top_ht` and the
#' height where diameter falls below `top_d`) into `n` sections and sums
#' Smalian volumes.
#'
#' @inheritParams kozak_taper
#' @param stump stump height
#' @param top_ht top height to stop at (default: tree tip)
#' @param top_d minimum top diameter (outside bark) to stop at (default 0)
#' @param n number of sections
#' @return volume per tree: ft3 (imperial) or m3 (metric)
#' @examples
#' kozak_volume(14, 75, "RS")                       # total stem, ft3
#' kozak_volume(14, 75, "RS", stump = 1, top_d = 4) # merchantable to 4-in top
#' @export
kozak_volume <- function(dbh, ht, spp, region = c("NE", "GL", "PNW"),
                         bark = c("ob", "ib"), planted = FALSE,
                         units = c("imperial", "metric"),
                         stump = 0, top_ht = NA, top_d = 0, n = 100,
                         ib_method = c("convert", "fit")) {
  region <- match.arg(region); bark <- match.arg(bark); units <- match.arg(units)
  ib_method <- match.arg(ib_method)
  m <- max(length(dbh), length(ht), length(spp))
  dbh <- rep_len(dbh, m); ht <- rep_len(ht, m); spp <- rep_len(as.character(spp), m)
  stump <- rep_len(stump, m); top_d <- rep_len(top_d, m); planted <- rep_len(planted, m)
  top_ht <- rep_len(top_ht, m); top_ht[is.na(top_ht) | top_ht > ht] <- ht[is.na(top_ht) | top_ht > ht]
  vapply(seq_len(m), function(j) {
    if (!is.finite(dbh[j]) || !is.finite(ht[j]) || top_ht[j] <= stump[j]) return(0)
    hs <- seq(stump[j], top_ht[j], length.out = n + 1)
    L <- hs[2] - hs[1]
    hs[hs <= 0] <- if (units == "metric") 0.001 else 0.001 / 0.3048  # taper is singular at 0 (as GreenTimbMerch)
    dob <- kozak_taper(dbh[j], ht[j], hs, spp[j], region, "ob", planted[j], units)
    d <- if (bark == "ib") kozak_taper(dbh[j], ht[j], hs, spp[j], region, "ib", planted[j],
                                       units, ib_method) else dob
    d[dob < top_d[j]] <- 0
    k <- if (units == "imperial") pi / 4 / 144 else pi / 4 / 1e4   # in2->ft2, cm2->m2
    sum(k * (d[-1]^2 + d[-length(d)]^2) / 2 * L)
  }, numeric(1))
}

#' Height on the stem where the taper reaches a given diameter
#'
#' @inheritParams kozak_taper
#' @param d target diameter (outside bark)
#' @return height, or 0 if the tree never reaches `d` above the stump
#' @examples
#' height_at_diameter(14, 75, 8, "RS")   # where a 14-in spruce tapers to 8 in.
#' @export
height_at_diameter <- function(dbh, ht, d, spp, region = c("NE", "GL", "PNW"),
                               planted = FALSE, units = c("imperial", "metric")) {
  region <- match.arg(region); units <- match.arg(units)
  m <- max(length(dbh), length(ht), length(d), length(spp))
  dbh <- rep_len(dbh, m); ht <- rep_len(ht, m); d <- rep_len(d, m); spp <- rep_len(spp, m)
  vapply(seq_len(m), function(j) {
    f <- function(x) kozak_taper(dbh[j], ht[j], x, spp[j], region, "ob", planted, units) - d[j]
    lo <- ht[j] * 0.001
    if (f(lo) < 0) return(0)
    stats::uniroot(f, c(lo, ht[j] * 0.9999), tol = 1e-6)$root
  }, numeric(1))
}

.species_values <- function() {
  if (is.null(.mfl$values)) .mfl$values <- utils::read.csv(
    system.file("extdata", "species_values.csv", package = "MEForLab"), stringsAsFactors = FALSE)
  .mfl$values
}
