#' Estimate stem volume using Kozak BC equation
#'
#' BCKozakVol - ft3 per stem
#' @param SPP - unique Species ("DF","RA","SS","WH","WC")
#' @param dbh - tree diameter at breast height (inches)
#' @param ht total tree height
#' @return tree volume using Smalian's
#' #' @examples
#' BCKozakVol(SPP=df$SPP,dbh=df$dbh,ht=df$ht)
#' @author Premer, M.I. - Maine Forest Lab - adapated from Brian Smith - UMaine
#' @description Computes estimated tree volume using Kozak Taper Equation - Kozak, A. 1988. A variable-exponent taper equation. Can. J. For. Res. 18: 1363–1368.


BCKozakVol=function(SPP,dbh,hgt){
  sgmts = 100
  mhgt=hgt*.3048
  mdbh=dbh*2.54
  L = (mhgt - 0.01)/sgmts
  i = 0
  treeVolume = 0
  while (i < sgmts) {
    H1 = L * i
    H2 = L * (i + 1)
    dib1=BCKozakTap(SPP=SPP,htd=H1,hgt=hgt,dbh=dbh)
    dib2=BCKozakTap(SPP=SPP,htd=H2,hgt=hgt,dbh=dbh)
    treeVolume = treeVolume + smalians(dib1, dib2, L * 12)
    i <- i + 1}
  treeVolume <- round(treeVolume/1e6, 6)
  return(treeVolume=treeVolume) }
