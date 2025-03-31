#' Estimate tree height using the Wykoff equation
#'
#' Tree height
#' @param SPP - Species code from FVS NE Varaiant
#' @param DBH - Diameter at breast height (in)
#' @return Estimated total height
#' @examples
#' wykoff.ht("RS",14)
#' @author Premer, M.I. - Maine Forest Lab
#' @description
#'  Wykoff, W.R.; Crookston, N.L.; Stage, A.R. 1982. User’s guide to the Stand Prognosis Model. Gen. Tech. Rep. INT-133. Ogden, UT: U. S. Department of Agriculture, Forest Service, Intermountain Forest and Range Experiment Station. 112p.

wykoff.ht <- function (SPP, DBH) {
  if (SPP == "BF") {a = 4.5084; b = -6.0016}
  if (SPP == "TA") {a = 4.5084; b = -6.0016}
  if (SPP == "WS") {a = 4.5084; b = -6.0016}
  if (SPP == "RS") {a = 4.5084; b = -6.0016}
  if (SPP == "NS") {a = 4.5084; b = -6.0016}
  if (SPP == "BS") {a = 4.5084; b = -6.0016}
  if (SPP == "PI") {a = 4.5084; b = -6.0016}
  if (SPP == "RN") {a = 4.5084; b = -6.0016}
  if (SPP == "WP") {a = 4.6090; b = -6.1896}
  if (SPP == "LP") {a = 4.6897; b = -6.8801}
  if (SPP == "VP") {a = 4.4718; b = -5.0078}
  if (SPP == "WC") {a = 4.5084; b = -6.0116}
  if (SPP == "AW") {a = 4.5084; b = -6.0116}
  if (SPP == "RC") {a = 4.4718; b = -5.0078}
  if (SPP == "JU") {a = 4.4718; b = -5.0078}
  if (SPP == "EH") {a = 4.5084; b = -6.0116}
  if (SPP == "HM") {a = 4.5084; b = -6.0116}
  if (SPP == "OP") {a = 4.3898; b = -5.7183}
  if (SPP == "JP") {a = 4.5084; b = -6.0116}
  if (SPP == "SP") {a = 4.6271; b = -6.4095}
  if (SPP == "TM") {a = 4.3898; b = -5.7183}
  if (SPP == "PP") {a = 4.3898; b = -5.7183}
  if (SPP == "PD") {a = 4.5457; b = -6.8000}
  if (SPP == "SC") {a = 4.5457; b = -6.8000}
  if (SPP == "OS") {a = 4.0374; b = -4.2964}
  if (SPP == "RM") {a = 4.3379; b = -3.8214}
  if (SPP == "SM") {a = 4.4834; b = -4.5431}
  if (SPP == "BM") {a = 4.4834; b = -4.5431}
  if (SPP == "SV") {a = 4.5991; b = -6.6706}
  if (SPP == "YB") {a = 4.4388; b = -4.0872}
  if (SPP == "SB") {a = 4.4522; b = -4.5758}
  if (SPP == "RB") {a = 4.4388; b = -4.0872}
  if (SPP == "PB") {a = 4.4388; b = -4.0872}
  if (SPP == "GB") {a = 4.4388; b = -4.0872}
  if (SPP == "HI") {a = 4.5128; b = -4.9918}
  if (SPP == "PH") {a = 4.5128; b = -4.9918}
  if (SPP == "SL") {a = 4.5128; b = -4.9918}
  if (SPP == "SH") {a = 4.5128; b = -4.9918}
  if (SPP == "MH") {a = 4.5128; b = -4.9918}
  if (SPP == "AB") {a = 4.4772; b = -4.7206}
  if (SPP == "AS") {a = 4.4819; b = -4.5314}
  if (SPP == "WA") {a = 4.5959; b = -6.4497}
  if (SPP == "BA") {a = 4.6155; b = -6.2945}
  if (SPP == "GA") {a = 4.6155; b = -6.2945}
  if (SPP == "PA") {a = 4.4819; b = -4.5314}
  if (SPP == "YP") {a = 4.6892; b = -4.9605}
  if (SPP == "SU") {a = 4.5920; b = -5.1719}
  if (SPP == "CT") {a = 4.6067; b = -5.2030}
  if (SPP == "QA") {a = 4.5128; b = -4.9918}
  if (SPP == "BP") {a = 4.5959; b = -6.4497}
  if (SPP == "EC") {a = 4.9396; b = -8.1838}
  if (SPP == "BT") {a = 4.5959; b = -6.4497}
  if (SPP == "PY") {a = 4.5959; b = -6.4497}
  if (SPP == "BC") {a = 4.3286; b = -4.0922}
  if (SPP == "WO") {a = 4.5463; b = -5.2287}
  if (SPP == "BR") {a = 4.5225; b = -4.9401}
  if (SPP == "CK") {a = 4.3420; b = -5.1193}
  if (SPP == "PO") {a = 4.2496; b = -4.8061}
  if (SPP == "RO") {a = 4.5202; b = -4.8896}
  if (SPP == "BW") {a = 4.5820; b = -5.0903}
  if (SPP == "AE") {a = 4.6008; b = -7.2732}
  if (SPP == "HH") {a = 4.0322; b = -3.0833}
  if (SPP == "OH") {a = 4.0322; b = -3.0833}
  if (SPP == "OT") {a = 4.0322; b = -3.0833}
  if (SPP == "PC") {a = 4.0322; b = -3.0833}
  if (SPP == "ST") {a = 4.0322; b = -3.0833}
  if (SPP == "HE") {a = 4.5084; b = -6.0116}
  HT = 4.5+exp(a+b/(DBH+1.0))
  return(HT)
}
