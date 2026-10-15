#' Coordinate conversion between UTM and lat/long
#'
#' Convert UTM (WGS84) coordinates to longitude/latitude, or the reverse. Uses
#' the `sf` package (rgdal, used by earlier versions, has been retired from
#' CRAN).
#'
#' @param x x coordinate: UTM easting or longitude
#' @param y y coordinate: UTM northing or latitude
#' @param proj Projection of the *input*: "UTM" or "longlat"
#' @param zone UTM zone of the input (or of the requested output), 1-60.
#'   Northern hemisphere is assumed.
#' @return data.frame with ID, X and Y in the other coordinate system
#' @examples
#' \dontrun{
#' # Orono, ME (UTM zone 19N) to long/lat
#' coord.convert(x = 520000, y = 4970000, proj = "UTM", zone = 19)
#' coord.convert(x = -68.67, y = 44.88, proj = "longlat", zone = 19)
#' }
#' @author Premer, M.I. - originally developed by Stephanie Patton
#' @export
coord.convert <- function(x, y, proj = c("UTM", "longlat"), zone) {
  if (!requireNamespace("sf", quietly = TRUE))
    stop("coord.convert needs the 'sf' package: install.packages('sf')")
  proj <- match.arg(proj)
  utm <- 32600 + as.integer(zone)            # EPSG code, WGS84 / UTM north
  crs_in  <- if (proj == "UTM") utm else 4326
  crs_out <- if (proj == "UTM") 4326 else utm
  pts <- sf::st_as_sf(data.frame(ID = seq_along(x), X = x, Y = y),
                      coords = c("X", "Y"), crs = crs_in)
  xy <- sf::st_coordinates(sf::st_transform(pts, crs_out))
  data.frame(ID = seq_along(x), X = xy[, 1], Y = xy[, 2])
}
