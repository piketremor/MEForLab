## Ported from GreenTimbMerch (Premer; adapted from Weiskittel and Smith).
## Inputs/outputs are metric (cm, m, m3) except board feet, as in the original.

#' Form/Risk Merchandizing
#'
#' This function calculates the total tree volume, merchantable volume,
#' sawlog volume, pulp volume, cull volume, and saw board feet for trees
#' using Form/Risk analysis. Uses either binary or probabilistic approaches.
#'
#' For explanation of form/risk analysis see castle. et al 2017 and
#' the NHRI Silvicultural guide for New Brunswick.
#'
#' Volumes determined using Kozak Taper Equations and Smalians Volume Formula.
#' Merch diameters establish by the MerchDiam function.
#'
#' Sawlog board feet is estimated using the international 1/4 inch rule. The length of the stem that is sawlog
#' quality is calculated based on the predicted sawlog volume using the Kozak Taper Equation.
#' The The sawlog portion of the stem is then broken into 2.4384m sections of equal length and the
#' international 1/4 inch rule is applied to each section. If the final section is longer than
#' 2.4384m but smaller than 4.8768m then that entire length will be used as the final log for
#' calculating board feet. \cr
#' object <- as.data.frame(object)\cr
#' df <- df %>% rownames_to_column()
#' %>% gather(variable, value, -rowname) %>% spread(rowname, value)\cr
#' is a useful pipe for unnesting the lists into dataframe when used with mapply.
#'
#'@details Current SPP with SPP specific coef are
#'
#'Hardwoods: "AB", "PB", "QA", "RM", "RO", "SM", "WA", "YB".
#'American Beech, Paper Birch, Quaking Aspen, Red Maple, Red Oak, Sugar Maple,
#'White Ash, and Yellow Birch.
#'
#'Softwoods: "RS", "WS", "BF", "EH", "WP", "OC", "WC".
#'Red Spruce, White Spruce, Balsam Fir, Eastern Hemlock, White Pine, Northern White Cedar.
#'
#'All other species are classified as Other Hardwood or Other Softwood and use non-species specific
#'HT and DBH coefs.
#'
#'
#'@param Stand The Unique Stand Identification Number
#'@param Plot The Unique Plot Identification Number
#'@param Tree The Unique Tree Identification Number
#'@param SPP The species identification using FVS codes: ex 'RO' = Red Oak
#'@param DBH Diameter at breast height in cm
#'@param HT Height of tree in meters
#'@param Form Form classes 1-8, "Ideal" or "Acceptable", "IF" or "AF". Defaults to "AF".
#'@param Risk Risk class may be entered using 1-4 values. Risk defaults to 3.
#'@param CSI  Climate Site Index Value. Defaults to 22.
#'@param BAL Basal area of larger trees in cubic meters at the plot level. Defaults to 0.
#'@param Cull if tree is cull enter TRUE, defaults to FALSE
#'@param Binary defaults to using binary model, Binary = FALSE uses probabilistic model.
#'@family Merchandising Functions
#' @seealso [kozak_taper()], [kozak_volume()], [merch_specs()]
#'@return
#' Metric, with the exception of Board Feet which is returned with imperial values.
#' ###
#' data.frame(Stand, Plot, Tree, Method, SPP, Saw.BF.FR, Saw.Vol.FR,
#' Pulp.Vol.FR, Cull.Vol.FR, Total.Vol, Merch.Vol, Percent.Sawlog.FR, Sawlog.Present)
#' Provides Tree Level Values listed in df as well as if a Sawlog would be present using a Binary model.
#'
#' @examples
#' Merchandize.Form.Risk(1,1,1,"RO", DBH = 35, HT = 16, Form = 1, Risk = 1, CSI = 22.2, BAL = 14, Cull = FALSE, Binary = TRUE)
#' Merchandize.Form.Risk(1,1,1,"RO", DBH = 35, HT = 16, Form = 1, Risk = 1, CSI = 22.2, BAL = 14, Cull = FALSE, Binary = FALSE)
#' @param region taper region: "NE" or "GL" (see [kozak_taper()])
#' @export

Merchandize.Form.Risk <- function(Stand, Plot, Tree, SPP, DBH, HT, Form = "AF",
                                  Risk = 1, CSI = 22, BAL = 0, Cull = FALSE, Binary = TRUE, region = "NE") {
  op <- options(MEForLab.taper_region = region); on.exit(options(op), add = TRUE)
  as.factor(SPP)

  # Merchantable Diameters By Species ---------------------------------------

  aa <- sapply(SPP, MerchDiam)
  sd <- as.numeric(t(aa)[, 1]) # Saw Diameter
  pald <- as.numeric(t(aa)[, 2]) # Pallet Diameter
  pd <- as.numeric(t(aa)[, 3]) # Pulp Diameter

  bb <- sapply(SPP, Genus)
  woodtype <- as.character(t(bb)[, 4])
  Form <- Form



  # Sawlog Presence ---------------------------------------------------------

  if (woodtype == "HW"){

    b131 <- -37.4916
    b132 <- 12.6034
    b133 <- 2.3705
    b134 <- -0.0317

    if(Form == 1 | Form == "IF" | Form == "Ideal"){
      b135 <- 1.2739
    } else {
      b135 <- 0
    }

    if(Risk == 1){
      b136 <- 0
    } else if(Risk == 2){
      b136 <- -0.7455
    } else if(Risk == 3 | Risk == 4) {
      b136 <- -4.0545
    } else {
      b136 <- -4.0545
      print("No Valid Risk")
    }

    #SPP x DBH Coef
    if(SPP == "AB"){
      b137 <- -0.1166
    } else if(SPP == "PB") {
      b137 <- -0.1331
    } else if(SPP == "QA"){
      b137 <- -0.1451
    } else if(SPP == "RM"){
      b137 <- -0.1522
    } else if(SPP == "RO"){
      b137 <- -0.0400
    } else if(SPP == "SM"){
      b137 <- -0.1405
    } else if(SPP == "WA"){
      b137 <- -0.0960
    } else if(SPP == "YB"){
      b137 <- -0.1414
    } else {
      b137 <- -0.1436
    }

    sawlog_presence <- (exp(b131 + b132*log(DBH) + b133*(log(HT/DBH)) + b134*(BAL) + b135 + b136 + b137*DBH)) /
      (1 + exp(b131 + b132*log(DBH) + b133*(log(HT/DBH)) + b134*(BAL) + b135 + b136 + b137*DBH))

    if(sawlog_presence >= 0.3936710){
      has_sawlog <- TRUE
    } else {
      has_sawlog <- FALSE
    }


  } else if (woodtype == "SW" & SPP %in% c("RS", "WS", "BF")){

    b140 <- -55.4585
    b141 <- 23.3889
    b143 <- 3.2084

    if(Risk == 1){
      b142 <- 0
    } else if(Risk == 2){
      b142 <- -3.6702
    } else {
      b142 <- -9.5663
    }

    if(SPP == "RS"){
      b144 <- -0.8472
    } else if (SPP == "WS"){
      b144 <- -0.8472
    } else if (SPP == "BF"){
      b144 <- -0.8407
    } else {
      stop("Function is Broken")
    }


    sawlog_presence <- (exp(b140 + b141*log(DBH) + b142 + b143*log(HT) + b144*DBH)) /
      (1 + exp(b140 + b141*log(DBH) + b142 + b143*log(HT) + b144*DBH))

    if(sawlog_presence >= 0.83006035){
      has_sawlog <- TRUE
    } else {
      has_sawlog <- FALSE
    }


  } else if (woodtype == "SW") {

    b150 <- -79.3273
    b151 <- 24.8516
    b153 <-  0.3641

    if(Risk == 1){
      b152 <- 0
    } else if(Risk == 2){
      b152 <- -1.4568
    } else {
      b152 <- -4.6883
    }

    if(SPP == "EH"){
      b154 <- -0.6503
      b155 <- 4.1018
    } else if (SPP == "WP"){
      b154 <- -0.5534
      b155 <- 3.4551
    } else if (SPP == "OC" | SPP == "WC"){
      b154 <- -0.6121
      b155 <-  2.9770
    } else {
      b154 <- -0.8246
      b155 <-  6.2570
    }

    sawlog_presence <- (exp(b150 + b151*log(DBH) + b152 + b153*CSI + b154*DBH + b155*log(HT))) /
      (1 + exp(b150 + b151*log(DBH) + b152 + b153*CSI + b154*DBH + b155*log(HT)))

    if(sawlog_presence >= 0.8141204){
      has_sawlog <- TRUE
    } else {
      has_sawlog <- FALSE
    }

  } else {
    has_sawlog <- FALSE
  }



  # PercentSawlog -----------------------------------------------------------


  if (woodtype == "HW"){

    b161 <- -35.1440
    b162 <- 12.7029
    b163 <- -0.7946
    b134 <- -0.0317

    if(Form == 1 | Form == "IF" | Form == "Ideal"){
      b164 <- 0.1393
    } else {
      b164 <- 0
    }

    if(Risk == 1){
      b165 <- 0
    } else if(Risk == 2){
      b165 <- -0.1801
    } else if(Risk == 3 | Risk == 4) {
      b165 <- -0.2903
    } else {
      b165 <- -0.2903
      print("No Valid Risk")
    }

    #SPP x DBH Coef
    if(SPP == "PB") {
      b166 <- -0.2928
    } else if(SPP == "QA"){
      b166 <- -0.2823
    } else if(SPP == "RM"){
      b166 <- -0.2916
    } else if(SPP == "RO"){
      b166 <- -0.2911
    } else if(SPP == "SM"){
      b166 <- -0.2901
    } else if(SPP == "WA"){
      b166 <- -0.2791
    } else if(SPP == "YB"){
      b166 <- -0.2886
    } else {
      b166 <- -0.2864
    }

    sawlog_percent <- (exp(b161 + b162*log(DBH) + b163*(log(HT/DBH)) + b164 + b165 + b166*DBH)) /
      (1 + exp(b161 + b162*log(DBH) + b163*(log(HT/DBH)) + b164 + b165 + b166*DBH))


  } else if (woodtype == "SW" & SPP %in% c("RS", "WS", "BF")){

    b170 <- -27.5014
    b171 <- 12.2515

    if(Form == 1 | Form == "IF" | Form == "Ideal"){
      b172 <- 0.1393
    } else {
      b172 <- 0
    }


    if(SPP == "RS"){
      b173 <- -0.3651
    } else if (SPP == "WS"){
      b173 <- -0.3651
    } else if (SPP == "BF"){
      b173 <- -0.3671
    } else {
      stop("Function is Broken")
    }


    sawlog_percent <- (exp(b170 + b171*log(DBH) + b172 + b173*DBH)) /
      (1 + exp(b170 + b171*log(DBH) + b172 + b173*DBH))

  } else if (woodtype == "SW") {

    b180 <- -14.5208
    b181 <- 5.9699
    b183 <- -0.0984

    if(Risk == 1){
      b182 <- 0
    } else if(Risk == 2){
      b182 <- 0
    } else {
      b182 <- -0.5475
    }

    if(SPP == "EH"){
      b184 <- -0.0818
    } else if (SPP == "WP"){
      b184 <- -0.0934
    } else if (SPP == "OC" | SPP == "WC"){
      b184 <- -0.0929
    } else {
      b184 <- -0.0292
    }

    sawlog_percent <- (exp(b180 + b181*log(DBH) + b182 + b183*CSI + b184*DBH)) /
      (1 + exp(b180 + b181*log(DBH) + b182 + b183*CSI + b184*DBH))

  }
  # Binary vs Probabilistic Models ------------------------------------------

  if(Binary == TRUE & has_sawlog == TRUE){
    Percent.Sawlog <- sawlog_percent
  } else if(Binary == TRUE & has_sawlog == FALSE){
    Percent.Sawlog <- 0
  } else if(Binary == FALSE){
    Percent.Sawlog <- sawlog_percent*sawlog_presence
  }


  Total.Vol <- KozakTreeVol(Bark = "ob", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = .1, topHT = NA, topD = NA)
  Merch.Vol <- KozakTreeVol(Bark = "ob", SPP = SPP, DBH = DBH, HT = HT, Planted = 0, stump = .3, topHT = NA, topD = pd)

  # International 1/4in Rule for Board Feet ---------------------------------
  board.feet <- function(TopDiam, Log) {
    a <- 0.049621
    b <- 0.00622
    c <- 0.185476
    d <- 0.000259
    e <- 0.011592
    f <- 0.04222
    len <- (Log * 3.28084)
    inches <- 0.3937008

    (a * (len * (inches * TopDiam)^2)) + (b * (len^2 * (inches * TopDiam))) - (c * (len * TopDiam * inches)) +
      (d * len^3) - (e * len^2) + (f * len)
  }

  # Sawlog Recovery
  if (Cull == TRUE) {
    Saw.Vol.FR <- 0
  } else {
    Saw.Vol.FR <- (Merch.Vol * Percent.Sawlog)
  }

  # Merchandise Sawlogs BF ---------------------------------------------------
  # Find Diameter at Saw Vol
  if (Saw.Vol.FR > 0) {
    LogHT <- function(x) abs(Saw.Vol.FR - KozakTreeVol("ob", SPP = SPP, DBH = DBH, HT = HT,
                                                       Planted = FALSE, stump = .3, topHT = x))
    LH <- optimize(LogHT,
                   lower = (HT * .0001), upper = (HT+1),
                   maximum = FALSE, tol = .Machine$double.eps^0.25
    )
    Log.Length <- (LH$minimum)[[1]]
  } else {
    Log.Length <- 0
  }

  if(Log.Length > 0){
    LogHeight <- Log.Length + .3
  } else {
    LogHeight <- 0
  }

  # Find Length of Log and Height from ground using Diameter at Vol
  topD <- KozakTaper("ob", SPP = SPP, LogHeight, DBH = DBH, HT = HT, Planted = FALSE)


  # Reduce Log Length if Log Diameter is < min saw diameter
  if(topD <= sd && DBH >= sd){
    topD <- sd
    f <- function(x) abs(sd - KozakTaper("ob", SPP = SPP, DHT = x, DBH = DBH, HT = HT, Planted = 0))
    o <- optimize(f,
                  lower = (HT * .1), upper = (HT + 1),
                  maximum = FALSE, tol = .Machine$double.eps^0.25
    )
    Log.Length <- (o$minimum)[[1]] - .3
  } else {
    Log.Length <- Log.Length
  }

  # Create Logs
  if(Log.Length < 2.4384){
    Log.Length <- 0
  } else if(Log.Length == 2.4384) {
    Log.Length <- 2.43840001
  } else {
    Log.Length <- Log.Length
  }

  if(Log.Length > 0){
    Sections <- seq(from = 0, to = Log.Length, by = 2.4384)
    LastLog <- (Log.Length - Sections[length(Sections)]) + 2.4384 # Remainder + 8ft
    Sections[length(Sections)] <- Sections[length(Sections)-1] + LastLog #LastLog replaces last sequence value
    LogHeights <- Sections[2:length(Sections)] # Remove 0 value from vector
    LogHeights <- LogHeights + .3
    Logs <- Sections[2:length(Sections)]

    #LogText <- "Height of Top Of Log"
    #print(paste(LogText, LogHeights, sep = " - "))

    for(i in 1:length(Logs)){
      Logs[i] <- Logs[i] - Sections[i]
    }
    TopDiam <- Logs
    for(i in 1:length(Logs)){
      TopDiam[i] <- KozakTaper("ob", SPP = SPP, LogHeights[i], DBH = DBH, HT = HT, Planted = 0)
    }
  } else {
    emptyvalue <- 0
  }

  if(Log.Length == 0){
    LogCount <- 0
  } else {
    LogCount <- length(Logs)
  }
  if (Log.Length > 0) {

    LogBF <- mapply(board.feet, TopDiam, Logs)

    #BFtext <- "BF in Log"
    #print(paste(BFtext, LogBF, sep = " - "))

    saw.bf <- sum(LogBF)
  } else {
    saw.bf <- 0
  }
  Saw.BF <- saw.bf

  if (LogCount < 1) {
    Saw.Vol.FR <- 0
  } else {
    Saw.Vol.FR <- (Merch.Vol * Percent.Sawlog)
  }

  # Pulp Recovery
  if (Cull == TRUE) {
    Pulp.Vol.FR <- 0
  } else {
    Pulp.Vol.FR <- (Merch.Vol - Saw.Vol.FR)
  }

  # Cull Volume
  if (Cull == FALSE) {
    Cull.Vol.FR <- (Total.Vol - Merch.Vol)
  } else {
    Cull.Vol.FR <- Total.Vol
  }

  # Fix NaN problems in Values ----------------------------------------------
  fix_nan <- function(x) {
    x[is.nan(x)] <- 0
    x
  }

  Cull.Vol.FR <- fix_nan(Cull.Vol.FR)
  Saw.Vol.FR <- fix_nan(Saw.Vol.FR)
  Pulp.Vol.FR <- fix_nan(Pulp.Vol.FR)
  Saw.BF <- fix_nan(Saw.BF)


  # Return Values
  Saw.BF.FR <- round(Saw.BF, 4)
  Saw.Vol.FR <- round(Saw.Vol.FR, 4)
  Pulp.Vol.FR <- round(Pulp.Vol.FR, 4)
  Cull.Vol.FR <- round(Cull.Vol.FR, 4)
  Total.Vol <- round(Total.Vol, 4)
  Merch.Vol <- round(Merch.Vol, 4)
  Sawlog.Present <- has_sawlog
  if(Cull == TRUE){
    Percent.Sawlog.FR <- 0
  } else {
    Percent.Sawlog.FR <- round(Percent.Sawlog * 100, 3)
  }

  if(Binary == TRUE){
    Method <- "Binary_Form_Risk"
  } else if (Binary == FALSE) {
    Method <- "Prob_Form_Risk"
  }

  values <- data.frame(Stand, Plot, Tree, SPP, LogHeight, LogCount, Method, Saw.BF.FR, Saw.Vol.FR, Pulp.Vol.FR, Cull.Vol.FR, Total.Vol, Merch.Vol, Percent.Sawlog.FR, Sawlog.Present)

  return(values)
}
