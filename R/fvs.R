## Running a compiled FVS variant from R -----------------------------------------
##
## These functions write FVS input (a tree file and a keyword file), run the
## FVS executable, and read the results back as a data.frame. FVS itself is
## not part of MEForLab: install it from the USDA Forest Service
## (https://www.fs.usda.gov/fvs/software/) or build it from
## https://github.com/USDAForestService/ForestVegetationSimulator. Maine is
## covered by the Northeast (NE) variant; the Acadian (ACD) variant is the
## regionally calibrated alternative where it is installed.

#' Locate an FVS executable
#'
#' Looks in `dir`, then `getOption("MEForLab.fvs_dir")`, the `FVS_BIN`
#' environment variable, the usual Windows install folders, and the PATH.
#'
#' @param variant FVS variant code, e.g. "ne" or "acd"
#' @param dir folder holding the executables (optional)
#' @return full path to the executable, or NA if not found
#' @examples
#' fvs_find("ne")
#' @export
fvs_find <- function(variant = "ne", dir = NULL) {
  exe <- paste0("FVS", tolower(variant), if (.Platform$OS.type == "windows") ".exe" else "")
  dirs <- c(dir, getOption("MEForLab.fvs_dir"), Sys.getenv("FVS_BIN"),
            "C:/FVSbin", "C:/FVS/FVSbin", "C:/FVS/FVSSoftware/FVSbin",
            file.path(Sys.getenv("USERPROFILE"), "FVSbin"))
  dirs <- dirs[nzchar(dirs)]
  cand <- c(file.path(dirs, exe), unname(Sys.which(exe)))
  cand <- cand[nzchar(cand) & file.exists(cand)]
  if (length(cand)) normalizePath(cand[1], winslash = "/") else NA_character_
}

#' Write one stand's tree list in FVS format
#'
#' Records carry trees per acre directly, so the keyword file uses
#' `DESIGN 0 1 999 1` (one 1-acre plot, no expansion).
#'
#' @param trees one stand's tree list (`fvs`, `dbh`, `tpa`, optional `ht`,
#'   `cr`, `plot`, `ht_imputed`)
#' @param file output path
#' @param pass_ht "measured" writes only field-measured heights (FVS imputes
#'   and calibrates the rest with its own equations), "all" writes every
#'   height, "none" writes none
#' @return the file path, invisibly
#' @export
fvs_write_trees <- function(trees, file, pass_ht = c("measured", "all", "none")) {
  pass_ht <- match.arg(pass_ht)
  ht <- if (is.null(trees$ht) || pass_ht == "none") rep(0, nrow(trees)) else trees$ht
  if (pass_ht == "measured" && !is.null(trees$ht_imputed)) ht[trees$ht_imputed] <- 0
  ht[is.na(ht)] <- 0
  cr <- if (is.null(trees$cr)) rep(0, nrow(trees)) else round(100 * trees$cr)
  cr[is.na(cr)] <- 0
  plot <- if (is.null(trees$plot)) rep(1L, nrow(trees)) else as.integer(factor(trees$plot))
  if (any(trees$tpa >= 1e5)) stop("TPA too large for the FVS tree record")
  lines <- sprintf("%4d%5d%9.3f%1d%-3s%5.1f%3s%5.0f%5.0f%4s%3d",
                   plot, seq_len(nrow(trees)), trees$tpa, 0L,
                   substr(trees$fvs, 1, 3), trees$dbh, "", ht, 0, "", as.integer(cr))
  writeLines(lines, file)
  invisible(file)
}

.fvs_treefmt <- "(I4,I5,F9.3,I1,A3,F5.1,F3.1,F5.0,F5.0,F4.1,I3,6I2,2I1,5I2,F4.0)"

.kw <- function(name, ...) {
  f <- c(...)
  paste0(sprintf("%-10s", name),
         paste(sprintf("%10s", if (is.numeric(f)) format(f, nsmall = 1, trim = TRUE) else f),
               collapse = ""))
}

.fvs_treatment_kw <- function(a, inv_year) {
  yr <- inv_year + a$year
  if (length(a$species))
    stop("species-restricted treatments are not translated to FVS keywords; ",
         "add your own lines through `keywords =`")
  switch(a$type,
    thin_below = .kw("THINBBA", c(yr, a$residual_ba, 1, a$min_dbh, a$max_dbh, 0, 999)),
    thin_above = .kw("THINABA", c(yr, a$residual_ba, 1, a$min_dbh, a$max_dbh, 0, 999)),
    clearcut   = .kw("THINDBH", c(yr, a$min_dbh, a$max_dbh, 1, 0, 0, 0)),
    stop("treatment type '", a$type, "' has no FVS keyword translation here; ",
         "use `keywords =` to pass FVS keywords directly"))
}

#' Write an FVS keyword file for one or more stands
#'
#' @param stand_files named character vector: stand ID -> tree file name
#'   (relative to the run folder)
#' @param file keyword file path
#' @param inv_year inventory year
#' @param years projection length; must be a multiple of `cycle`
#' @param cycle FVS cycle length (years)
#' @param regime list of [treatment()]s, applied to every stand
#' @param keywords extra FVS keyword lines added to every stand (e.g.
#'   `"SITECODE  RS  55"` or `"STDINFO ..."`)
#' @param db output SQLite database name
#' @param location FVS location code for STDINFO; 922 (White Mountain NF) is
#'   the NE-variant code FVS uses for Maine. NULL omits STDINFO.
#' @return the keyword file path, invisibly
#' @export
fvs_write_keywords <- function(stand_files, file, inv_year = 2025, years = 50,
                               cycle = 5, regime = list(), keywords = character(),
                               db = "fvsout.db", location = 922) {
  if (years %% cycle) stop("years must be a multiple of cycle")
  for (a in regime) if (a$year %% cycle)
    warning("treatment in year ", a$year, " is not on a ", cycle,
            "-year cycle boundary; FVS will apply it in the cycle that contains it")
  out <- character()
  for (i in seq_along(stand_files)) {
    sid <- names(stand_files)[i]
    out <- c(out,
      "STDIDENT", substr(sid, 1, 26),
      .kw("INVYEAR", inv_year),
      if (!is.null(location)) .kw("STDINFO", location),
      .kw("DESIGN", c(0, 1, 999, 1)),
      .kw("NUMCYCLE", years / cycle),
      .kw("TIMEINT", c(0, cycle)),
      "DATABASE", "DSNOUT", db, "SUMMARY", "END",
      "ECHOSUM",
      keywords,
      vapply(regime, .fvs_treatment_kw, character(1), inv_year = inv_year),
      .kw("OPEN", 2), stand_files[[i]],
      "TREEFMT", .fvs_treefmt, "",
      .kw("TREEDATA", 2),
      .kw("CLOSE", 2),
      "PROCESS")
  }
  writeLines(c(out, "STOP"), file)
  invisible(file)
}

#' Project tree lists with a compiled FVS variant
#'
#' Writes one tree file per stand and a single keyword file, runs FVS once,
#' and returns the FVS summary table for every stand.
#'
#' @param trees tree list (all stands); see [read_demeritt_inventory()]
#' @param years,cycle projection length and FVS cycle length
#' @param regime list of [treatment()]s
#' @param variant FVS variant ("ne" for Maine)
#' @param fvs_bin path to the executable; found with [fvs_find()] if NULL
#' @param inv_year inventory year
#' @param keywords extra keyword lines for every stand
#' @param pass_ht see [fvs_write_trees()]. The default, "all", hands FVS the
#'   same (imputed) heights the CONUS engine starts from, so differences
#'   between engines come from growth, not from height imputation. Use
#'   "measured" to let FVS impute missing heights with its own equations.
#' @param location see [fvs_write_keywords()]
#' @param workdir folder for FVS input/output (kept, so you can open the
#'   `.out` file to see exactly what FVS did)
#' @return list with `summary` (stand x year, same core columns as
#'   [conus_project()]), `files` and `log`
#' @examples
#' \dontrun{
#' res <- fvs_project(trees, years = 50, regime = list(treatment(0, "thin_below", 80)))
#' head(res$summary)
#' }
#' @export
fvs_project <- function(trees, years = 50, cycle = 5, regime = list(), variant = "ne",
                        fvs_bin = NULL, inv_year = 2025, keywords = character(),
                        pass_ht = "all", location = 922,
                        workdir = tempfile("fvs_")) {
  bin <- if (is.null(fvs_bin)) fvs_find(variant) else fvs_bin
  if (is.na(bin) || !file.exists(bin))
    stop("FVS", variant, " executable not found. Install FVS (https://www.fs.usda.gov/fvs/software/) ",
         "and point to it with options(MEForLab.fvs_dir = 'C:/FVSbin') or fvs_bin =.")
  dir.create(workdir, recursive = TRUE, showWarnings = FALSE)
  old_out <- list.files(workdir, pattern = "\\.(db|sum|out|key|tre)$", full.names = TRUE)
  if (length(old_out)) unlink(old_out)            # never mix results from earlier runs
  stands <- split(trees, trees$stand)
  ids <- make.unique(gsub("[^A-Za-z0-9_]", "_", names(stands)))
  tf <- stats::setNames(paste0("s", seq_along(stands), ".tre"), ids)
  for (i in seq_along(stands)) fvs_write_trees(stands[[i]], file.path(workdir, tf[i]), pass_ht)
  key <- file.path(workdir, "run.key")
  fvs_write_keywords(tf, key, inv_year, years, cycle, regime, keywords,
                     location = location)
  old <- setwd(workdir); on.exit(setwd(old), add = TRUE)
  log <- suppressWarnings(system2(bin, "--keywordfile=run.key", stdout = TRUE, stderr = TRUE))
  setwd(old)
  summ <- fvs_read_summary(workdir)
  if (is.null(summ) || !nrow(summ))
    stop("FVS produced no summary output; see ", file.path(workdir, "run.out"))
  summ$stand <- names(stands)[match(summ$stand, ids)]
  summ$year <- summ$year - inv_year
  errs <- grep("FVS[0-9]+ ERROR", readLines(file.path(workdir, "run.out"), warn = FALSE), value = TRUE)
  if (length(errs)) warning("FVS reported errors; check ", file.path(workdir, "run.out"),
                            ":\n", paste(utils::head(errs, 5), collapse = "\n"))
  out <- list(summary = summ, files = list(dir = workdir, key = key, out = file.path(workdir, "run.out")),
              log = log)
  attr(out, "engine") <- paste0("fvs-", variant)
  out
}

#' Read FVS summary output
#'
#' Reads `FVS_Summary` from the run database (needs the RSQLite package);
#' falls back to the ECHOSUM text file.
#'
#' @param workdir FVS run folder
#' @param db database file name
#' @return data.frame with `stand`, `year` (calendar), `tpa`, `ba`, `sdi`,
#'   `ccf`, `top_ht`, `qmd`, `vol_cuft` (total cubic), `mcuft`, `bdft`,
#'   `removed_tpa`, `removed_vol`, `removed_ba`
#' @export
fvs_read_summary <- function(workdir, db = "fvsout.db") {
  dbf <- file.path(workdir, db)
  if (file.exists(dbf) && requireNamespace("RSQLite", quietly = TRUE)) {
    con <- DBI::dbConnect(RSQLite::SQLite(), dbf)
    on.exit(DBI::dbDisconnect(con))
    if (DBI::dbExistsTable(con, "FVS_Summary")) {
      s <- DBI::dbGetQuery(con, "SELECT * FROM FVS_Summary ORDER BY StandID, Year")
      return(data.frame(stand = s$StandID, year = s$Year, tpa = s$Tpa, ba = s$BA,
                        sdi = s$SDI, ccf = s$CCF, top_ht = s$TopHt, qmd = s$QMD,
                        vol_cuft = s$TCuFt, mcuft = s$MCuFt, bdft = s$BdFt,
                        removed_tpa = s$RTpa, removed_vol = s$RTCuFt,
                        removed_ba = pmax(s$BA - s$ATBA, 0), ba_after = s$ATBA,
                        stringsAsFactors = FALSE))
    }
  }
  sf <- list.files(workdir, pattern = "\\.sum$", full.names = TRUE)
  if (!length(sf)) return(NULL)
  ln <- readLines(sf[1], warn = FALSE)
  rows <- list(); sid <- NA
  for (l in ln) {
    tk <- strsplit(trimws(l), "\\s+")[[1]]
    if (identical(tk[1], "-999")) { sid <- tk[3]; next }
    if (length(tk) >= 25 && grepl("^\\d{4}$", tk[1])) {
      v <- as.numeric(tk[1:25])
      rows[[length(rows) + 1]] <- data.frame(stand = sid, year = v[1], tpa = v[3], ba = v[4],
        sdi = v[5], ccf = v[6], top_ht = v[7], qmd = v[8], vol_cuft = v[9], mcuft = v[10],
        bdft = v[12], removed_tpa = v[13], removed_vol = v[14],
        removed_ba = pmax(v[4] - v[18], 0), ba_after = v[18], stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, rows)
}
