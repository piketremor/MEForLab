## GreenTimbMerch-compatible interfaces (metric), built on kozak_taper() etc.

#' Kozak taper equation (GreenTimbMerch interface, metric)
#'
#' Same arguments as GreenTimbMerch::KozakTaper(), now vectorised and
#' region-aware. Use [kozak_taper()] for imperial units.
#'
#' Fixes relative to GreenTimbMerch: `SPP == "BA"`, `"TA"` and `"NC"` were
#' matched regardless of `Bark` because of operator precedence; inside-bark
#' white spruce used a copy of the white pine outside-bark parameters; species
#' without inside-bark parameters fell through to the "OS" inside-bark curve
#' (they now use the outside-bark taper converted with [dob_to_dib()]).
#' As in GreenTimbMerch, `Bark = "ib"` here uses the inside-bark taper fits;
#' [KozakTreeVol()] uses the bark conversion. See `ib_method` in
#' [kozak_taper()].
#'
#' @param Bark "ob" or "ib"
#' @param SPP FVS species code
#' @param DHT height (m) where diameter is wanted
#' @param DBH diameter at breast height (cm)
#' @param HT total height (m)
#' @param Planted planted tree (TRUE/FALSE)
#' @param region "NE", "GL" or "PNW"; defaults to `getOption("MEForLab.taper_region", "NE")`
#' @return diameter (cm) at DHT
#' @examples
#' KozakTaper("ob", "RO", 12, 40, 20, FALSE)
#' KozakTaper("ib", "RS", 8, 28, 16, TRUE)
#' @export
KozakTaper <- function(Bark, SPP, DHT, DBH, HT, Planted = FALSE,
                       region = getOption("MEForLab.taper_region", "NE")) {
  round(kozak_taper(DBH, HT, DHT, SPP, region = region, bark = Bark,
                    planted = as.logical(Planted), units = "metric", ib_method = "fit"), 4)
}

#' Kozak stem volume (GreenTimbMerch interface, metric)
#'
#' @inheritParams KozakTaper
#' @param stump stump height (m) excluded from the volume; NA = 0
#' @param topHT height (m) to stop at; NA = tree tip
#' @param topD outside-bark diameter (cm) to stop at; NA = no limit
#' @return volume (m3)
#' @examples
#' KozakTreeVol("ib", "RO", 40, 18, stump = .5, topD = 25.3)
#' KozakTreeVol("ob", "WP", 32.5, 21)
#' @export
KozakTreeVol <- function(Bark, SPP, DBH, HT, Planted = FALSE, stump = NA, topHT = NA,
                         topD = NA, region = getOption("MEForLab.taper_region", "NE")) {
  round(kozak_volume(DBH, HT, SPP, region = region, bark = Bark,
                     planted = as.logical(Planted), units = "metric",
                     stump = ifelse(is.na(stump), 0, stump), top_ht = topHT,
                     top_d = ifelse(is.na(topD), 0.001, topD)), 6)
}

#' Outside- to inside-bark diameter (GreenTimbMerch interface, metric)
#' @param SPP FVS species code
#' @param dob diameter outside bark (cm)
#' @return diameter inside bark (cm)
#' @examples
#' DOBtoDIB("SM", 25.4)
#' @export
DOBtoDIB <- function(SPP, dob) round(dob_to_dib(dob, SPP, units = "metric"), 4)

#' @rdname smalians
#' @param r1,r2 GreenTimbMerch interface: end diameters (cm)
#' @param len GreenTimbMerch interface: length (cm); returns cm3
#' @export
Smalians <- function(r1, r2, len) round(((r1 / 2)^2 * pi + (r2 / 2)^2 * pi) / 2 * len, 4)

#' Merchandising specifications by species
#'
#' Minimum small-end (outside bark) diameters, in cm, for sawlogs, pallet
#' logs and pulp. 1000 means "never this product". These are baseline Maine
#' values from GreenTimbMerch; markets vary, so override them with
#' `options(MEForLab.merch_specs = my_specs)` using the same columns.
#'
#' @return data.frame: `spp`, `saw_cm`, `pallet_cm`, `pulp_cm`
#' @examples
#' head(merch_specs())
#' # local market: spruce-fir sawlogs to a 5-in top
#' my <- merch_specs(); my$saw_cm[my$spp %in% c("RS", "BF")] <- 12.7
#' @export
merch_specs <- function() {
  s <- getOption("MEForLab.merch_specs")
  if (!is.null(s)) return(s)
  if (is.null(.mfl$merch)) .mfl$merch <- utils::read.csv(
    system.file("extdata", "merch_specs.csv", package = "MEForLab"), stringsAsFactors = FALSE)
  .mfl$merch
}

#' Minimum merchantable diameters for one species (GreenTimbMerch interface)
#' @param SPP FVS species code
#' @return named vector: saw, pallet, pulp (cm)
#' @examples
#' MerchDiam("RO")
#' @export
MerchDiam <- function(SPP) {
  s <- merch_specs()
  i <- match(.alias(SPP)[1], s$spp)
  if (is.na(i)) return(c(saw = 1000, pallet = 1000, pulp = 10.16))
  c(saw = s$saw_cm[i], pallet = s$pallet_cm[i], pulp = s$pulp_cm[i])
}
