#' Calculate Basal Area Larger of any given stem
#'
#' BAL - a metric of tree demograhic position
#' @param dbh - tree diameter at breask height
#' @param tpa trees per acre
#' @return the basal area per acre in trees larger than the subject tree
#' @examples
#' basal.area.larger(10.2,142,complete=FALSE)
#' @author Premer, M.I. - Maine Forest Lab - originally coded by N. Osborne
#' @description
#' Computes basal area per acre in larger trees (BAL, ft2/acre). BAL is computed by aggregating the unique DBH values and computing the sum of BA within each class (not a percentile).  The basal area above each unique DBH class, from smallest to largest is calculated.  The largest tree by DBH in a given stand will have a BAL = 0 while the smallest tree has BAL equal to the stand basal area minus the given trees BA. BAL is a distance independent competition index.  BAL is widely used in individual tree growth and yield equations.  One big limitation of this index is that each tree is assumed to have the same competitive effect, without respective to species and there is no differentiation for stand structure (the same goes for many stand level models, which project basal area).  It should be noted that there are different ways of computing BAL.  Schroder and van Gadow (1999) suggested taking Stages (1973) percentile in BA dist' and dividing it by relative spacing (see relative.spacing function), to form an index of competition.
#'

basal.area.larger <- function(
    dbh,
    tpa,
    complete = FALSE
){

  # combine input data and compute ba (ft2/ac)
  df = data.frame(dbh, tpa)
  df$ba = (pi/(4 * 144)) * dbh^2 * df$tpa

  # basal area in larger trees for each unique diameter
  # computing the basal area in each unique diameter as a class
  look = aggregate(cbind(bac = ba) ~ dbh, data = df, FUN = sum)
  look = look[order(look$dbh, decreasing = FALSE),]
  look$bal = sum(look$bac) - cumsum(look$bac)

  # Interpolation of the lookup table as a function
  # plot(look$dbh, look$bal)
  # return the bal values to treelist
  if(nrow(look) == 1){
    df$bal = 0
  } else {
    fn = approxfun(x = look$dbh, y = look$bal, rule = 2)
    df$bal = fn(df$dbh)
  }

  # complete or just bal as an output
  if(complete == TRUE){out = df} else {out = df$bal}
  return(out)

}
