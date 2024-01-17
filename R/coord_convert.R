#' Coordinate conversion between UTM and lat/long
#'
#' Convert between UTM coordinates to latitude and longitude or from latitude and longitude to UTM coordinates.
#' Coordinates will convert to UTM if "longlat" is supplied as input projection and latitude longitude if "UTM" is
#' supplied as the input projection.
#'
#' @param x x coordinate: UTM X or longitude
#' @param y y coodinate: UTM Y or latitude
#' @param proj projection of the input coordinate system: c("UTM", "longlat")
#' @param zone UTM zone of the input coordinate system or requested output: c(1:60)

#' @import sp
#' @import rgdal
#'
#' @return This function returns a coordinate pair in UTM of longlat projections, opposite of the input.
#'
#' @examples
#' # Input coordinates in UTM zone 10, converting to long and lat
#'      # Seattle, WA
#' coord.convert(x = 550202.40, y = 5272749.67, proj = "UTM", zone = 10)
#'
#' # Input coordinates in long and lat, converting to UTM zone 17
#'      # Orlando, FL
#' coord.convert(x = -81.379237, y =  28.538335, proj = "longlat", zone = 17)
#'
#' @export
#'
#' @author Premer, M.I. - originally developed by Stephanie Patton


coord.convert<-function(x, y, proj, zone){
  if (proj == "UTM"){
    ## Build dataframe
    xy = data.frame(ID = 1:length(x), X = x, Y = y)

    ## Make spatial points dataframe
    coordinates(xy) = c("X", "Y")

    ## Define current projection
    proj4string(xy) = CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep=''))

    ## Reproject in defined zone
    res = spTransform(xy, CRS("+proj=longlat +datum=WGS84"))
    out = data.frame(res)

  } else if (proj == "longlat"){
    ## Build dataframe
    xy = data.frame(ID = 1:length(x), X = x, Y = y)

    ## Make spatial points dataframe
    coordinates(xy) = c("X", "Y")

    ## Define current projection
    proj4string(xy) = CRS("+proj=longlat +datum=WGS84")

    ## Reproject in defined zone
    res = spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
    out = data.frame(res)
  }
  out$optional = NULL
  return(out)
}
