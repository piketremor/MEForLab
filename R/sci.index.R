#' Calculate Structural Complexity Index (SCI)
#'
#' SCI - Delaunay triangulation of the stem map w/ DBH as z.
#' SCI = sum of 3d triangle areas / sum of 2d projected areas (At)
#' Needs >= 3 non-collinear stems, returns NA otherwise. Requires the geometry package.
#' @param dist - distance from the subject tree (subject tree = 0)
#' @param az - azimuth (deg) from the subject tree (subject tree = 0)
#' @param dbh - DBH of all trees incl. the subject tree
#' @return SCI (1 = flat, increases w/ structural complexity). Value depends on dbh vs. distance units, keep consistent across plots.
#' @examples
#' SCI(c(0,14,6.2,9.8,11.3),c(0,259,45,130,310),c(52,16,22,30,18))
#' @author Premer, M.I. - Maine Forest Lab
#' @references Zenner, E.K., and Hibbs, D.E. 2000. A new method for modeling the heterogeneity of forest structure. For. Ecol. Manage. 129: 75-87.
#'
#' Premer, M.I., Chhin, S., and Zhang, J. 2021. Local testing and calibration of species-specific competition indices in Sierran mixed-conifer forests: application transfer to evolving objectives. Can. J. For. Res. 51: 524-532.

#' @export
SCI<-function(dist, #distance from subject tree
              az, #azimuth from subject tree
              dbh) #dbh of all trees
{
  if(!requireNamespace("geometry",quietly=TRUE)) stop("install.packages('geometry')")
  x<-dist*sin(az*pi/180)
  y<-dist*cos(az*pi/180)
  ok<-!is.na(x)&!is.na(y)&!is.na(dbh)
  x<-x[ok];y<-y[ok];z<-dbh[ok]
  if(length(x)<3) return(NA)
  tri<-tryCatch(geometry::delaunayn(cbind(x,y),options="Qbb Qc Qz Qt"),error=function(e) NULL)
  if(is.null(tri)||NROW(tri)==0) return(NA)
  a3<-0;a2<-0
  for(k in 1:nrow(tri)){
    p1<-c(x[tri[k,1]],y[tri[k,1]],z[tri[k,1]])
    p2<-c(x[tri[k,2]],y[tri[k,2]],z[tri[k,2]])
    p3<-c(x[tri[k,3]],y[tri[k,3]],z[tri[k,3]])
    a<-p2-p1;b<-p3-p1
    cx<-c(a[2]*b[3]-a[3]*b[2],a[3]*b[1]-a[1]*b[3],a[1]*b[2]-a[2]*b[1])
    a3<-a3+sqrt(sum(cx^2))/2   #3d surface area
    a2<-a2+abs(cx[3])/2        #2d projected area (At)
  }
  sci<-a3/a2
  return(sci)
}
