#' Estimate tree height using the Wykoff equation
#'
#' HT = 4.5 + exp(a + b / (DBH + 1)), coefficients from the FVS Northeast
#' (NE) variant.
#'
#' @param SPP Species code, FVS NE variant alpha code (vectorised)
#' @param DBH Diameter at breast height (in.)
#' @return Estimated total height (ft). Species without coefficients return
#'   NA with a warning.
#' @examples
#' wykoff.ht("RS", 14)
#' wykoff.ht(c("RS", "BF", "SM"), c(8, 10, 12))
#' @author Premer, M.I. - Maine Forest Lab
#' @references Wykoff, W.R.; Crookston, N.L.; Stage, A.R. 1982. User's guide
#'   to the Stand Prognosis Model. Gen. Tech. Rep. INT-133. Ogden, UT: USDA
#'   Forest Service, Intermountain Forest and Range Experiment Station. 112p.
#' @export
wykoff.ht <- function(SPP, DBH) {
  cf <- .wykoff_coefs
  i <- match(as.character(SPP), cf$SPP)
  if (anyNA(i[!is.na(SPP)]))
    warning("wykoff.ht: no coefficients for species ",
            paste(unique(SPP[is.na(i)]), collapse = ", "), "; returning NA")
  4.5 + exp(cf$a[i] + cf$b[i] / (DBH + 1.0))
}

.wykoff_coefs <- data.frame(
  SPP = c("BF","TA","WS","RS","NS","BS","PI","RN","WP","LP","VP","WC","AW","RC",
          "JU","EH","HM","OP","JP","SP","TM","PP","PD","SC","OS","RM","SM","BM",
          "SV","YB","SB","RB","PB","GB","HI","PH","SL","SH","MH","AB","AS","WA",
          "BA","GA","PA","YP","SU","CT","QA","BP","EC","BT","PY","BC","WO","BR",
          "CK","PO","RO","BW","AE","HH","OH","OT","PC","ST","HE"),
  a = c(4.5084,4.5084,4.5084,4.5084,4.5084,4.5084,4.5084,4.5084,4.6090,4.6897,
        4.4718,4.5084,4.5084,4.4718,4.4718,4.5084,4.5084,4.3898,4.5084,4.6271,
        4.3898,4.3898,4.5457,4.5457,4.0374,4.3379,4.4834,4.4834,4.5991,4.4388,
        4.4522,4.4388,4.4388,4.4388,4.5128,4.5128,4.5128,4.5128,4.5128,4.4772,
        4.4819,4.5959,4.6155,4.6155,4.4819,4.6892,4.5920,4.6067,4.5128,4.5959,
        4.9396,4.5959,4.5959,4.3286,4.5463,4.5225,4.3420,4.2496,4.5202,4.5820,
        4.6008,4.0322,4.0322,4.0322,4.0322,4.0322,4.5084),
  b = c(-6.0016,-6.0016,-6.0016,-6.0016,-6.0016,-6.0016,-6.0016,-6.0016,-6.1896,
        -6.8801,-5.0078,-6.0116,-6.0116,-5.0078,-5.0078,-6.0116,-6.0116,-5.7183,
        -6.0116,-6.4095,-5.7183,-5.7183,-6.8000,-6.8000,-4.2964,-3.8214,-4.5431,
        -4.5431,-6.6706,-4.0872,-4.5758,-4.0872,-4.0872,-4.0872,-4.9918,-4.9918,
        -4.9918,-4.9918,-4.9918,-4.7206,-4.5314,-6.4497,-6.2945,-6.2945,-4.5314,
        -4.9605,-5.1719,-5.2030,-4.9918,-6.4497,-8.1838,-6.4497,-6.4497,-4.0922,
        -5.2287,-4.9401,-5.1193,-4.8061,-4.8896,-5.0903,-7.2732,-3.0833,-3.0833,
        -3.0833,-3.0833,-3.0833,-6.0116),
  stringsAsFactors = FALSE)
