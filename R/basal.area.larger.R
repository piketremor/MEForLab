#' Calculate Basal Area Larger of any given stem
#'
#' BAL - a metric of tree demographic position
#' @param uid - unique identifier to the plot/stand
#' @param dbh - tree diameter at breast height
#' @param tpa trees per acre
#' @return the basal area per acre in trees larger than the subject tree
#' #' @examples
#' basal.area.larger(uid=df$uid,dbh=df$dbh,tpa=df$tpa)
#' @author Premer, M.I. - Maine Forest Lab
#' @description Computes basal area per acre in larger trees (BAL, ft2/acre). BAL is computed by aggregating the unique DBH values and computing the sum of BA within each class (not a percentile).  The basal area above each unique DBH class, from smallest to largest is calculated.  The largest tree by DBH in a given stand will have a BAL = 0 while the smallest tree has BAL equal to the stand basal area minus the given trees BA. BAL is a distance independent competition index.  BAL is widely used in individual tree growth and yield equations.  One big limitation of this index is that each tree is assumed to have the same competitive effect, without respective to species and there is no differentiation for stand structure (the same goes for many stand level models, which project basal area).  It should be noted that there are different ways of computing BAL.  Schroder and van Gadow (1999) suggested taking Stages (1973) percentile in BA dist' and dividing it by relative spacing (see relative.spacing function), to form an index of competition.

basal.area.larger <- function(uid, dbh, tpa){
  ba <- dbh^2*.0005454
  BAT <- ba * tpa
  Temp <- data.frame(uid, dbh, BAT)
  Temp <- Temp[order(-dbh),]
  Temp$csum <- ave(Temp$BAT, Temp$uid, FUN=cumsum)
  return(Temp$csum)
}
