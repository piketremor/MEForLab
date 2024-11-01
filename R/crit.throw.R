#' Calculates the critical windspeed for windthrow of an individual tree or stand reported in miles per hour
#'
#' Critical Tree Root-throw Windspeed
#' @param SPP  Species Code (FVS Format)
#' @param dbh -Diameter at breast height (in)
#' @param ht - Total stem height (ft)
#' @param thin_ratio - Thinning ratio (ratio of growing space post/pre thinning)
#' @param soil_depth - Soil depth (1 for soils < 80 cm deep, 2 for soils > 80 cm deep)
#' @param Species - DF, Douglas fir; WH, western hemlock; LP, loblolly pine; SP, slash pine
#' @return critical speed of windthrow in mph
#' @examples
#' crit.throw(dbh=10,ht=85,thin_ratio=1,soil_depth=1,"DF")
#' @author Premer, M.I. - Maine Forest Lab
#' @description
#' Hale, S.E., Gardiner, B., Peace, A., Nicoll, B., Taylor, P., and S. Pizzirani. Comparison and validation of three versions of a forest wind risk model. Ecological Modeling & Software.68: 27-41


crit.throw <- function(dbh,ht,thin_ratio,soil_depth,Species){
  if (soil_depth=="1"&Species=="DF")
  {I=197.4} # < 80 cm depth Douglas fir
  if (soil_depth=="2"&Species=="DF")
  {I=165.6} # > 80 cm depth Douglas fir
  if (soil_depth=="1"&Species=="WH")
  {I=126.0} # < 80 cm depth western hemlock
  if (soil_depth=="2"&Species=="WH")
  {I=168.7} # > 80 cm depth western hemlock
  if (soil_depth=="1"&Species=="LP")
  {I=137} # < 80 cm depth loblolly pine, for now mapped as lodgepole pine until rooting coefficients found
  if (soil_depth=="2"&Species=="LP")
  {I=148} # > 80 cm depth loblolly pine
  if (soil_depth=="1"&Species=="SP")
  {I=105.1} # < 80 cm depth slash pine, for now mapped as corsican pine until rooting coefficients found
  if (soil_depth=="2"&Species=="SP")
  {I=131} # > 80 cm depth slash pine
  if (Species=="DF"){b0=-2.2304
  b1=2.4435}
  if (Species=="WH"){b0=-2.5384
  b1 = 2.4814}
  if (Species=="LP"){b0=-2.5356
  b1=2.4349}
  if (Species=="SP"){b0=-2.5356
  b1=2.4349}
  dbh.m = dbh*2.54
  ht.m = ht*0.3048
  SW = exp(b0+(b1*log(dbh.m)))
  Creg = 0+(SW*I)
  crit_throw = sqrt(((Creg*SW)/(111.7*(dbh.m^2*(ht.m/100))))*
                      (1/1.136)*
                      (1/(thin_ratio*0.99)))
  crit_throw_mph = (crit_throw*3.6)/1.6
  return(crit_throw_mph)
}
