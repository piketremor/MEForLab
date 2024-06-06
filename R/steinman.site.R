#' Estimate site index of spruce/fir using Steinman equations
#'
#' Site Index
#' @param SPP  Species Code (FVS Format)
#' @param ht - mean top height (H40)
#' @param age - estimated age of origin
#' @return Estimated site index at base age 50
#' @examples
#' steinman.site("RS",65,42)
#' @author Premer, M.I. - Maine Forest Lab
#' @description
#' Steinman, J.R. 1992. A comprehensive evaluation of spruce-fir growth and yield in Maine as related to physical and chemical soil properties. Ph.D. thesis, University of Maine, Orono, Maine.


steinman.site <- function(SPP, HT, AGE){
  if(SPP=="RS"){b0=7.8531;  b1=-0.62589; b2=-0.00010125; b3=-0.22073; b4=0.33689}
  if(SPP=="BF"){b0=1.19373; b1=-0.77673; b2=-0.00000137; b3=-0.29571; b4=0.22433}
  site = ((b0*(HT-4.5)^b1)*(1-exp(b2*AGE))^(b3*(HT-4.5)^b4))
  return(site)
}

