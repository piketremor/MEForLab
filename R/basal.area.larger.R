#' Calculate Basal Area Larger of any given stem
#'
#' BAL - a metric of tree demographic position
#' @param uid Unique identifier of the plot/stand
#' @param dbh Tree diameter at breast height (in.)
#' @param tpa Trees per acre represented by each record
#' @return Basal area per acre (ft2/ac) in trees strictly larger than the
#'   subject tree, in the same order as the input.
#' @examples
#' df <- data.frame(uid = c(1, 1, 1, 2), dbh = c(6, 10, 14, 8), tpa = c(100, 50, 20, 80))
#' basal.area.larger(df$uid, df$dbh, df$tpa)
#' @author Premer, M.I. - Maine Forest Lab
#' @description Computes basal area per acre in larger trees (BAL, ft2/acre).
#'   The largest tree in a stand has BAL = 0, and the smallest has BAL equal to
#'   stand basal area minus the basal area of its own size class (trees of
#'   equal DBH are not counted as larger than each other). BAL is a
#'   distance-independent competition index widely used in individual-tree
#'   growth and yield equations. Its main limitation is that every tree is
#'   assumed to have the same competitive effect regardless of species or
#'   stand structure. Schroder and von Gadow (1999) suggested combining Stage's
#'   (1973) BA percentile with relative spacing (see [relative.spacing()]) to
#'   form a related index.
#' @export
basal.area.larger <- function(uid, dbh, tpa) {
  bat <- 0.005454154 * dbh^2 * tpa
  out <- numeric(length(dbh))
  for (g in split(seq_along(dbh), uid)) {
    d <- dbh[g]; b <- bat[g]
    out[g] <- vapply(d, function(x) sum(b[d > x]), numeric(1))
  }
  out
}
