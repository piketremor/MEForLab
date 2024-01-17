#' Generate estimates of Topograhic Wetness Index, Upslope Area, and filled sinks of a Digitial Elevation Model
#'
#' @param dem  digital elevation model in .tif format
#' @return Filled Sinks DEM, Upslope Area, TWI
#' @examples
#' create_layers(dem)
#' @author Premer, M.I. - Maine Forest Lab - originally a function of topmodel
#' @description
#' Customized forestry and GIS functions

create_layers <- function (dem, fill.sinks = TRUE, deg = 0.1)
{
  layers <- stack(dem)
  message("Building upslope areas...")
  a.atb <- upslope(dem, atb = TRUE, fill.sinks = fill.sinks, deg = deg)
  layers <- addLayer(layers, a.atb)
  names(layers) <- c("filled.elevations", "upslope.area", "twi")
  return(layers)
}
