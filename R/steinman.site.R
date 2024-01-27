#' Estimate site index of spruce/fir using Steinman equations
#'
#' Site Index
#' @param ht - mean top height (H40)
#' @param age - estimated age of origin
#' @return Estimated site index at base age 50
#' @examples
#' steinman.site(65,42)
#' @author Premer, M.I. - Maine Forest Lab
#' @description
#'  VSteinman, J.R. 1992. A comprehensive evaluation of spruce-fir growth and yield in Maine as related to physical and chemical soil properties. Ph.D. thesis, University of Maine, Orono, Maine.



steinman.site<- function(ht,age){
  site = ((2.775*(25^-0.7403))*((1-exp(-0.00002151*15))^(-0.2795*25^0.2816)))
  return(site)
}
