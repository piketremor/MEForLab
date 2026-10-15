#' Critical Tree Breakage Windspeed (mph)
#'
#'A tree and stand level estimator of windspeed critical to breakage reported in miles per hour
#' @param dbh Diameter at breast height (inches)
#' @param ht Total tree height (ft)
#' @param thin_ratio Thinning ratio (ratio of growing space post/pre thinning; 1 = unthinned)
#' @param SPP Species code: DF - Douglas fir; WH - western hemlock; LP - loblolly pine; SP - slash pine
#' @return critical wind speed for stem breakage (mph)
#' @examples
#' crit.break(dbh = 10, ht = 85, thin_ratio = 1, SPP = "DF")
#' @author Premer, M.I. - Maine Forest Lab
#' @description A tree and stand level estimator of windspeed critical to breakage reported in miles per hour
#' @references Hale, S.E., Gardiner, B., Peace, A., Nicoll, B., Taylor, P., and S. Pizzirani. Comparison and validation of three versions of a forest wind risk model. Ecological Modeling & Software.68: 27-41

#' @export
crit.break <- function(dbh,ht,thin_ratio,SPP){
  if(SPP=="DF"){
    MOR=51000000
    d0 = dbh*exp(0.04302*(4.5-0.1))}
  if(SPP=="WH"){
    MOR=77900000
    d0 = dbh+((0.12667*dbh)*((4.5-0.1)/(0.1+1)))
  }
  if(SPP=="LP"){
    MOR=51700000
    d0 = dbh+((0.06834*dbh)*((4.5-0.1)/(0.1+1)))
  }
  if(SPP=="SP"){
    MOR=112400000
    d0 = dbh+((0.08*dbh)*((4.5-0.1)/(0.1+1)))
  }
  #d0 = dbh*exp(0.04302*(4.5-0.1))
  d0.metric = d0*2.54
  ht.m = ht*0.3048
  dbh.metric = dbh*2.54
  fknot = 0.9
  crit.break = ((3.14*MOR*d0.metric)/(32*111.7*(dbh.metric^2*(ht.m/100))))^(1/2)*
    (fknot/1.136)^(1/2)*
    (1/thin_ratio)^(1/2)
  return(crit.break)
}
