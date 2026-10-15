#' Calculate tree volume (ft3)
#'
#' Total stem cubic-foot volume from a Honer (1967)-form equation,
#' VOL = DBH^2 / (a + b / HT).
#'
#' @param SPP Species code (FVS alpha; vectorised)
#' @param DBH Diameter at breast height (in.)
#' @param HT Total tree height (ft)
#' @return Stem volume (ft3) per tree. Species without coefficients return NA
#'   with a warning.
#' @examples
#' vol.calc("BF", 14.1, 68)
#' vol.calc(c("BF", "RS", "SM"), c(10, 12, 14), c(55, 60, 70))
#' @author Premer, M.I. - Maine Forest Lab
#' @export
vol.calc <- function(SPP, DBH, HT) {
  cf <- .vol_calc_coefs
  i <- match(as.character(SPP), cf$SPP)
  if (anyNA(i[!is.na(SPP)]))
    warning("vol.calc: no coefficients for species ",
            paste(unique(SPP[is.na(i)]), collapse = ", "), "; returning NA")
  (DBH^2) / (cf$a[i] + (cf$b[i] / HT))
}

.vol_calc_coefs <- data.frame(
  SPP = c("BF","EH","RS","TA","WP","WC","QA","RM","OT","PB","BC","GB","YB","ST",
          "WILLOW","PC","BA","OA","MA","UNK","WS","NS","WA","BS","SM","MM","RP",
          "AB","WB","OS","BT","RO","WO","HE","HH","BW"),
  a = c(2.139,1.112,1.226,1.226,0.691,0.691,-0.312,1.046,0.312,2.222,-0.312,2.222,
        2.222,2.222,-0.312,-0.312,-0.312,-0.312,-0.312,-0.312,1.440,1.226,-0.312,
        1.588,1.046,1.046,0.691,0.959,2.222,1.226,-0.312,1.512,1.512,1.112,1.046,
        1.046),
  b = c(301.634,350.092,315.832,315.832,363.832,363.832,436.683,383.927,436.683,
        300.373,436.683,300.373,300.373,300.373,436.683,436.683,436.683,436.683,
        436.683,436.683,342.175,315.832,436.683,333.364,383.927,383.927,363.832,
        334.829,300.373,315.832,436.683,336.509,336.509,350.092,383.927,383.927),
  stringsAsFactors = FALSE)
