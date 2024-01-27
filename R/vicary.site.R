#' Estimate site index of spruce/fir using Vicary, Brann, and Griffin
#'
#' Site Index
#' @param SPP - Species, either "BF", or "RS"
#' @param ht - mean top height (H40/H100)
#' @param age - estimated age of origin
#' @return Estimated site index at base age 50
#' @examples
#' vicary.site("BF",65,42)
#' @author Premer, M.I. - Maine Forest Lab
#' @description
#'  Vicary, Bret P.; Brann, Thomas B.; and Griffin, Raph H.. 1984. B802: Base-Age Invariant Polymorphic Site Index Curves for Even-Aged Spruce-Fir Stands in Maine. Maine Agricultural and Forest Experiment Station Bulletins 802. https://digitalcommons.library.umaine.edu/aes_bulletin/130



vicary.site<- function(SPP,ht,age){
  if(SPP=="RS"){
    a=110.9866}
  if(SPP=="BF"){
    a=112.3188
  }
  site = a*(1-exp((50/age)*log((ht-a)/-a)))
  return(site)
}
