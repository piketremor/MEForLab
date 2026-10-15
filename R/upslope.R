#' Calculate Upslope area a given DEM
#'
#'This function allows you to calculate the upslope area for a given raster
#' @param dem  digital elevation model (RasterLayer, projected)
#' @param log return log(upslope area)
#' @param atb also return the topographic wetness index layer
#' @param deg minimum slope (degrees) used by sink filling
#' @param fill.sinks fill sinks first (topmodel::sinkfill)
#' @return the total, relative upslope area of any given location in the raster matrix
#' @examples
#' \dontrun{
#' upslope(dem)
#' }
#' @author Premer, M.I. - Maine Forest Lab- originally a function of topmodel
#' @description
#' Customized forestry and GIS functions


#' @export
upslope <- function (dem, log = TRUE, atb = FALSE, deg = 0.12, fill.sinks = TRUE)
{
  if (!isTRUE(all.equal(raster::xres(dem), raster::yres(dem)))) {
    stop("Raster has differing x and y cell resolutions. Check that it is in a projected coordinate system (e.g. UTM) and use raster::projectRaster to reproject to one if not. Otherwise consider using raster::resample")
  }
  if (fill.sinks) {
    utils::capture.output(dem <- invisible(raster::setValues(dem, topmodel::sinkfill(raster::as.matrix(dem), res = raster::xres(dem), degree = deg))))
  }
  topidx <- topmodel::topidx(raster::as.matrix(dem), res = raster::xres(dem))
  a <- raster::setValues(dem, topidx$area)
  if (log) {
    a <- log(a)
  }
  if (atb) {
    atb <- raster::setValues(dem, topidx$atb)
    a <- raster::addLayer(a, atb)
    names(a) <- c("a", "atb")
  }
  return(a)
}
