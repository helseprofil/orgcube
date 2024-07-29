#' @title do_cube_params
#' @keywords internal
#' @family worker
#' @description
#' Collects all relevant parameters and files necessary to produce the finished
#' data file. Reads tables from access, reads and filters file groups.
#' @param con_db connection to KHELSA
#' @param con_logg connection to KHlogg
#' @param con_duck connection to in-memory duck db
#' @param cube.id name of cube
#' @param batch batch date
#' @param year production year
#'
#' @return list containing all relevant information for cube production
#' including formatted filegroups.
do_cube_params <- function(con_db,
                           con_logg,
                           con_duck,
                           cube.id = cube.id,
                           batch = batch,
                           year = year){

  params <- list(
    cubename = cube.id,
    dbcon = con_db,
    loggcon = con_logg,
    duckcon = con_duck,
    year = year,
    batch = batch
  )

  params <- get_access_cube(params)
  params <- get_design_cube(params)
  # params <- get_filegroups(params)

  return(params)

}

## ---- MAIN HELPERS ----

#' @title get_access_cube
#' @keywords internal
#' @family helper
#' @description
#' Generate a list of necessary information to generate the data file.
#' The function reads tables in KHELSA Access database and returns the relevant
#' rows, and a list of files to be used to create the finished cube.
#'
#' @param params list of params
#' @returns updated params list
get_access_cube <- function(params = params){

  if(any(c("cubename", "batch", "dbcon", "loggcon", "year") %notin% names(params))){
    stop("Important parameters missing")
  }

  params[["cubenamestr"]] <- paste0("'", params$cubename, "'")
  params[["validstr"]] <- paste0("VERSJONFRA <=", format(params$batch, "#%Y-%m-%d#"), " AND VERSJONTIL >", format(params$batch, "#%Y-%m-%d#"))

  params[["KUBER"]] <- as.list(DBI::dbGetQuery(params$dbcon,
                                              paste0("SELECT * FROM KUBER WHERE KUBE_NAVN=", params$cubenamestr, " AND ", params$validstr)))

  if(!params$cubename %in% params$KUBER$KUBE_NAVN) stop("kube.id: ", params$cubenamestr, "' not found in KHELSA:KUBER")

  # TNPdscr
  if(is.na(params$KUBER$TNP) || params$KUBER$TNP == ""){
    stop("Access column 'KHELSA:KUBER:TNP' is missing or empty!")
  }
  TNPstr <- paste0("'", params$KUBER$TNP, "'")
  params[["TNP_PROD"]] <- as.list(DBI::dbGetQuery(params$dbcon,
                                                 paste0("SELECT * FROM TNP_PROD WHERE TNP_NAVN=", TNPstr, " AND ", params$validstr)))

  # STNPdscr
  if(params$KUBER$REFVERDI_VP != "P"){
    params[["STNP_PROD"]] <- list()
  } else if (is.na(params$TNP_PROD$STANDARDTNFIL) || params$TNP_PROD$STANDARDTNFIL == "") {
    params[["STNP_PROD"]] <- params$TNP_PROD
  } else {
    STNPstr <- paste0("'", params$TNP_PROD$STANDARDTNFIL, "'")
    params[["STNP_PROD"]] <- as.list(DBI::dbGetQuery(params$dbcon,
                                                    paste0("SELECT * FROM TNP_PROD WHERE TNP_NAVN=", STNPstr, " AND ", params$validstr)))
  }

  params[["FILFILTRE"]] <- data.table::setDT(DBI::dbGetQuery(params$dbcon,
                                                            paste0("SELECT * FROM FILFILTRE WHERE ", params$validstr)))

  # FRISKVIK
  params[["FRISKVIK"]] <- as.list(DBI::dbGetQuery(params$dbcon,
                                               paste0("SELECT INDIKATOR, ID FROM FRISKVIK WHERE AARGANG=", params$year, " AND KUBE_NAVN=", params$cubenamestr)))

  params[["KH_KODER"]] <- data.table::setDT(DBI::dbGetQuery(params$dbcon,
                                                            "SELECT DEL, KODE, ILLEG, TOTAL FROM KH_KODER"))

  return(params)
}

#' @title get_design_cube
#' @keywords internal
#' @family helper
#' @description
#' Fetches and format design parameters from ACCESS
#' @param params a list of parameters
#' @returns updated params list including DESIGN parameters
get_design_cube <- function(params){

  parts <- data.table::setDT(DBI::dbGetQuery(params$dbcon, paste0("SELECT * FROM KH_deler")))
  parts[, allparts := DelKol][TYPE == "INT", allparts := paste0(allparts, "l,", allparts, "h")]
  parts[, allpartsFull := allparts][!is.na(DelKolE), allpartsF := paste(allparts, DelKolE, sep = ",")]

  design <- list()
  design$allpartsname <- setNames(parts$DelKol, parts$DEL)
  design$allparts <- setNames(parts$allparts, parts$DEL)
  design$allpartsFull <- setNames(parts$allpartsFull, parts$DEL)

  #design$DelType <- setNames(parts$TYPE, parts$DEL)
  #design$DelFormat <- setNames(parts$FORMAT, parts$DEL)
  #design$AggPri <- parts$DEL[order(parts$AGGREGERPRI)]
  #design$AggVedStand <- parts$DEL[parts$AGGREGERvedPRED == 1]
  #design$IntervallHull <- setNames(parts$INTERVALLHULL, parts$DEL)
  #IntervallHull <- IntervallHull[!(is.na(IntervallHull) | IntervallHull == "")]

  params[["DESIGN"]] <- design

  return(params)
}


#' @title get_filegroups
#' @keywords internal
#' @family helper
#' @description
#' Uses information from ACCESS to read and filter the original files
#' to be used to create the final data. Add files, information about filegroup
#' design, and filegroup parameters to the list.
#' @param params a list of parameters
#' @returns updated params list, including files, filedesign, and filegroup parameters
get_filegroups <- function(params = params){

  if(any(c("dbcon", "KUBER", "TNP_PROD", "FILFILTRE") %notin% names(params))){
    stop("Important parameters missing")
  }

  params[["FILENAMES"]] <- find_files(params$KUBER, params$TNP_PROD, params$STNP_PROD)
  files <- list()
  design <- list()
  fgparams <- list()

  for(file in seq_along(params$FILENAMES)){
    filetype <- names(params$FILENAMES[file])
    filename <- params$FILENAMES[[file]]

    # Read file
    filter <- is_filter(filename, params)
    rawfile <- ifelse(filter, params$FILFILTRE[FILVERSJON == filename, ORGFIL], filename)
    filepath <- get_filegroup_path(rawfile)
    rawfilestr <- paste0("'", rawfile, "'")
    FGP <- as.list(DBI::dbGetQuery(params$dbcon,
                                   paste0("SELECT * FROM FILGRUPPER WHERE FILGRUPPE=", rawfilestr, " AND ", params$validstr)))

    cat("Reading file:", basename(filepath), "\n")
    is_fileaccess(filepath)
    dt <- data.table::setDT(readRDS(filepath))

    # FILTER TAB-columns (ACCESS:KUBER:TABX)
    teller <- is_teller(filename, params)
    tabsubset <- ifelse(teller, get_tabsubset(filename, params), "TRUE")
    cat("Filtering using TAB-columns:\n-n rows before is:", dim(dt)[1])
    dt <- dt[eval(parse(text = tabsubset))]
    cat("\n-n rows after is", dim(dt)[1])

    if(filter){
      cat("Filtering file", filename, "according to ACCESS:FILFILTRE")
      filterspec <- as.list(params$FILFILTRE[FILVERSJON == filename])
      dt <- use_filegroup_filter(dt, filterspec, params)
    }

    files[[filetype]] <- dt
    fgparams[[filetype]] <- FGP

    # Find filedesign
    # TODO: WRITE CODE TO EXTRACT DESIGN
    design[[filetype]] <- NA
  }

  params[["FILES"]] <- files
  params[["FILEPARAMETERS"]] <- fgparams
  params[["FILEDESIGN"]] <- design

  return(params)
}

## ---- OTHER HELPERS ----

#' @title use_filegroup_filter
#' @keywords internal
#' @family helper
#' @description
#' Filters a data file according to KHELSA:FILFILTRE
#'
#' @param file a data.table
#' @param filerparams relevant params from KHELSA:FILFILTRE
#' @returns a filtered data file
use_filegroup_filter <- function(dt, filterspec, params){

  cat("\nFiltering according to ACCES:FILFILTRE")
  orgcols <- names(dt)
  coltypes <- is_coltype(dt)
  data.table::setkeyv(dt, coltypes$dims)

  if(grepl("\\S", filterspec$KOLLAPSdeler)){
    totals <- params$KH_KODER[TOTAL == 1]
    totals <- as.list(setNames(totals$KODE,totals$DEL))
    for(i in seq_along(totals)){if(grepl("^\\d*$", totals[[i]])) totals[[i]] <- as.integer(totals[[i]])}
    collapse <- params$DESIGN$allpartsFull[unlist(strsplit(filterspec$KOLLAPSdeler, ","))]
    collapse <- collapse[collapse %in% orgcols]
    cat("\nCollapsing on:", paste(collapse, collapse = ", "))
    dt[, (collapse) := totals[names(collapse)]]
    dt[, lapply(mget(coltypes$vals), sum), by = mget(coltypes$dims)]
    dt <- dt[, ..orgcols]
  }

  if(!(is.na(filterspec$NYEKOL_KOL_preRAD) | filterspec$NYEKOL_KOL_preRAD == "")) {
    # MUST DEVELOP WITH SYSVAK, ALL OTHERS ARE NA/""
    # FIL <- LeggTilNyeVerdiKolonner(FIL, FilterDscr$NYEKOL_KOL_preRAD, slettInf = TRUE)
  }

  # Filter <- SettFilterDesign(FilterDscr, bruk0 = FALSE, FGP = FGP, globs = globs)
  # if (length(Filter) > 0) {
  #   FIL <- OmkodFil(FIL, FinnRedesign(FinnDesign(FIL), list(Parts = Filter)), globs = globs, echo = 1)
  # }

  # if (FilterDscr$GEOHARM == 1) {
  #   rektiser <- ifelse(FilterDscr$REKTISER == 1, 1, 0)
  #   FIL <- GeoHarm(FIL, vals = FGP$vals, rektiser = rektiser, batchdate = batchdate, globs = globs)
  # }

  # if (!(is.na(FilterDscr$NYETAB) | FilterDscr$NYETAB == "")) {
  #   # NOT USED
  #   FIL <- AggregerRader(FIL, FilterDscr$NYETAB, FGP = FGP)
  # }

  # if (grepl("\\S", FilterDscr$NYEKOL_RAD)) {
  #   FIL <- LeggTilSumFraRader(FIL, FilterDscr$NYEKOL_RAD, FGP = FGP, globs = globs)
  # }

  # if (!(is.na(FilterDscr$NYEKOL_KOL) | FilterDscr$NYEKOL_KOL == "")) {
  #   # NOT USED
  #   FIL <- LeggTilNyeVerdiKolonner(FIL, FilterDscr$NYEKOL_KOL, slettInf = TRUE)
  # }

  # if (!(is.na(FilterDscr$NYKOLSmerge) | FilterDscr$NYKOLSmerge == "")) {
  #   NY <- eval(parse(text = FilterDscr$NYKOLSmerge))
  #   tabK <- intersect(FinnTabKols(names(NY)), FinnTabKols(names(FIL)))
  #   data.table::setkeyv(NY, tabK)
  #   data.table::setkeyv(FIL, tabK)
  #   FIL <- NY[FIL]
  # }

  # if (!(is.na(FilterDscr$FF_RSYNT1) | FilterDscr$FF_RSYNT1 == "")) {
  #   FilterDscr$FF_RSYNT1 <- gsub("\\\r", "\\\n", FilterDscr$FF_RSYNT1)
  #   rsynt1err <- try(eval(parse(text = FilterDscr$FF_RSYNT1)), silent = TRUE)
  #   print("***AD HOC MANIPULERING\n")
  #   if ("try-error" %in% class(rsynt1err)) {
  #     print(rsynt1err)
  #   }
  # }

  return(dt)

}

#' @title find_files
#' @keywords internal
#' @family helper
#' @param KUBER ACCESS table
#' @param TNP_PROD ACCESS table
#' @param STNP_PROD ACCESS table
#' @return a list containing the names of files needed to create the final data
find_files <- function(KUBER, TNP_PROD, STNP_PROD){

  files <- list()

  # TELLER
  if(is.na(TNP_PROD$TELLERFIL) || TNP_PROD$TELLERFIL == ""){
    stop("Access column 'KHELSA:TNP:TELLERFIL' is missing or empty!")
  }
  files$TELLER <- TNP_PROD$TELLERFIL

  # NEVNER
  if(!is.na(TNP_PROD$NEVNERFIL) && TNP_PROD$NEVNERFIL != ""){
    files$NEVNER <- TNP_PROD$NEVNERFIL
  }

  # Extra files for standardization
  if(KUBER$REFVERDI_VP == "P"){
    # PREDNEVNER
    if (!is.na(TNP_PROD$PREDNEVNERFIL) && TNP_PROD$PREDNEVNERFIL != "") {
      files$PREDNEVNER <- gsub("^(.*):(.*)", "\\1", TNP_PROD$PREDNEVNERFIL)
    } else if (!is.na(TNP_PROD$NEVNERFIL)) {
      files$PREDNEVNER <- TNP_PROD$NEVNERFIL
    } else {
      files$PREDNEVNER <- TNP_PROD$TELLERFIL
    }

    # ST
    if(is.na(STNP_PROD$TELLERFIL) || STNP_PROD$TELLERFIL == ""){
      stop("Access column 'KHELSA:TNP_PROD:TELLERFIL' is missing or empty")
    }
    files$ST <- STNP_PROD$TELLERFIL

    # SN
    if(!is.na(STNP_PROD$NEVNERFIL) && STNP_PROD$NEVNERFIL != ""){
      files$SN <- STNP_PROD$NEVNERFIL
    } else {
      files$SN <- STNP_PROD$TELLERFIL
    }
  }
  return(files)
}

#' @title is_filegroup_path
#' @keywords internal
#' @family helper
#' @description
#' Fetch the path to the most recent file corresponding to the filegroup name
#' from the filegroup folder.
#'
#' @param filename name of filegroup
#' @returns Path to filegroup
get_filegroup_path <- function(filename){

  path <- is_orgcube_path("filegroups", "nyeste")
  filepath <- list.files(path,
                      pattern = paste0("^", filename, ".rds$"),
                      full.names = T)
  if(length(filepath) == 0) stop("Filegroup ", filename, " not found in ", path,
                              ". \nMake sure you have created the filegroup!")
  if(length(filepath) > 1) stop("> 1 file found for filegroup ", filename, ":\n", filepath)
  return(filepath)
}

#' @title is_filter
#' @keywords internal
#' @family helper
#' @description Check if filters are set up in ACCESS::FILFILTRE
#' @param filegroup name of filegroup
#' @param params access params
is_filter <- function(filegroup, params){
  isTRUE(filegroup %in% params$FILFILTRE$FILVERSJON)
}

#' @title is_teller
#' @keywords internal
#' @family helper
#' @description Check if file is equal to the TELLER file
#' @param filegroup name of filegroup
#' @param params access params
is_teller <- function(filename, params){
  isTRUE(filename == params$FILENAMES$TELLER)
}

#' @title get_tabsubset
#' @keywords internal
#' @family helper
#' @description
#' Check for filters set up in ACCESS::KUBER::TAB-columns and set up a filter to
#' remove unneeded categories. Uses values in `TABx` unless `TABx_0` is non-empty,
#' then values in `TABx_0`is used. Only applies to `TELLER`-file.
#' @param filename name of filegroup
#' @param params List of access params generated with `ìs_access_cube`
#' @returns formatted filtering string for TAB-columns.
get_tabsubset <- function(filename, params = params){

  tabs <- grep("^TAB\\d+$", names(params$KUBER), value = T)
  tabfilter <- character()

  # all(is.na(params$KUBER[grep("^TAB.*\\d+$", names(params$KUBER))]))

  for(tab in tabs){
    val <- params$KUBER[[tab]]

    if(!is.null(val) && !is.na(val) && val != ""){
      val0 <- params$KUBER[[paste0(tab, "_0")]]
      if(!is.null(val0) && !is.na(val0) && val0 != ""){
        filter <- val0
      } else if(!is.null(val) && !is.na(val) && val != ""){
        filter <- val
      }

      remove <- grepl("^-\\[", filter)
      filter <- paste0("\"", gsub(",", "\",\"", filter), "\"")

      filter <- ifelse(remove,
                       paste0(tab, " %notin% c(", filter, ")"),
                       paste0(tab, " %in% c(", filter, ")"))

      tabfilter <- c(tabfilter, filter)
    }
  }

  out <- paste(tabfilter, collapse = " & ")
  if(out == ""){out <- "TRUE"}

  return(out)
}

#' @title do_filegroupdesign
#' @keywords internal
#' @family helper
#' @param files
#' @param params
#'
#' @return design of filegroup
get_filegroupdesign <- function(files, params){

}

do_controlparams <- function(params){

  return(params)
}
