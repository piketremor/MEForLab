#' Calculate tree volume (ft3)
#'
#'This function allows you to calculate tree volume given species, dbh, and height
#' @param SPP  Species Code (FVS Format)
#' @param DBH Diameter at breast height (inches)
#' @param HT Total Tree Height
#' @return stem volume (ft3)
#' @examples
#' vol_calc(BF,14.1,68)
#' @author Premer, M.I. - Maine Forest Lab


vol_calc <- function(SPP, DBH, HT){
  if(SPP=="BF"){a=2.139;  b=301.634}
  if(SPP=="EH"){a=1.112; b=350.092}
  if(SPP=="RS"){a=1.226; b=315.832}
  if(SPP=="WP"){a=0.691; b=363.832}
  if(SPP=="WC"){a=0.691; b=363.832}
  if(SPP=="QA"){a=-0.312; b=436.683}
  if(SPP=="RM"){a=1.046; b=383.927}
  if(SPP=="OT"){a=0.312; b=436.683}
  if(SPP=="PB"){a=2.222; b=300.373}
  if(SPP=="BC"){a=-0.312; b=436.683}
  if(SPP=="WP"){a=0.691; b=363.832}
  if(SPP=="GB"){a=2.222; b=300.373}
  if(SPP=="YB"){a=2.222; b=300.373}
  if(SPP=="ST"){a=2.222; b=300.373}
  if(SPP=="WILLOW"){a=-0.312; b=436.683}
  if(SPP=="PC"){a=-0.312; b=436.683}
  if(SPP=="BA"){a=-0.312; b=436.683}
  if(SPP=="OA"){a=-0.312; b=436.683}
  if(SPP=="MA"){a=-0.312; b=436.683}
  if(SPP=="UNK"){a=-0.312; b=436.683}
  if(SPP=="WS"){a=1.226; b=315.832}
  if(SPP=="WA"){a=-0.312; b=436.683}
  if(SPP=="BS"){a=1.226; b=315.832}
  if(SPP=="SM"){a=1.046; b=383.927}
  if(SPP=="MM"){a=1.046; b=383.927}
  if(SPP=="RP"){a=0.691; b=363.832}
  if(SPP=="AB"){a=0.959; b=334.829}
  if(SPP=="WB"){a=2.222; b=300.373}
  #else{a=1.112; b=350.092} #just to test
  VOL = (DBH^2)/(a+(b/HT))
  return(VOL=VOL)
}
