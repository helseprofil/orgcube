## Utility functions used throughout the package
## ---------------------------------------------

#' @title Update package
#' @family utils
#' @description Update orgcube directly with Github repo. Default is `main` branch.
#' @param branch The branch in Github to install from, `main` or `dev`
#' @param force Use the laterst version(s) of all dependencies. Default is FALSE
#' \dontrun{
#' update_orgcube(branch = "main") #default
#' update_orgcube(branch = "dev") #to upgrade with dev branch
#' }
#' @export
update_orgcube <- function(branch = "main", force = FALSE){
  unloadNamespace("orgcube")
  repo <- paste("helseprofil/orgcube")
  if(branch != "main") repo <- paste(repo, branch, sep = "@")
  pak::pkg_install(repo, upgrade = force)
  attachNamespace("orgcube")
  invisible()
}

#' @title is_orgcube_path
#' @keywords internal
#' @family utils
#' @param folder one of c("kh", "nh", "data", "dumps", "filegroups")
#' @param type NULL or one of c("datert", "nyeste", "qc", "specs")
#' @param format NULL or c("R", "csv")
#' @keywords internal
#' @return desired path. Will throw error if path does not exist.
is_orgcube_path <- function(folder = c("kh", "nh", "data", "dumps", "filegroups"),
                            type = NULL,
                            format = NULL){

  folder = match.arg(folder)
  root <- getOption("orgcube.folder.root")
  path1 <- switch(folder,
                  kh = getOption("orgcube.folder.kommunehelsa"),
                  nh = getOption("orgcube.folder.norgeshelsa"),
                  data = getOption("orgcube.folder.data"),
                  dumps = getOption("orgcube.folder.dumps"),
                  filegroups = getOption("orgcube.folder.filegroups"))

  if(is.null(type)){
    path2 <- NULL
  } else {
    validtype <- c("datert", "nyeste", "qc", "specs")
    if(!type %in% validtype){
      stop("type must be either NULL or one of 'datert', 'nyeste', 'qc', or 'specs'")
    }
    path2 <- switch(type,
                    datert = getOption("orgcube.folder.sub.datert"),
                    nyeste = getOption("orgcube.folder.sub.nyeste"),
                    qc = getOption("orgcube.folder.sub.qc"),
                    specs = getOption("orgcube.folder.sub.specs"))
  }

  if(is.null(format)){
    path3 <- NULL
  } else {
    validformat <- c("R", "csv")
    if(!format %in% validformat){
      stop("format must be either NULL or one of 'R' or 'csv'")
    }
    path3 <- switch(format,
                    R = getOption("orgcube.folder.format.R"),
                    csv = getOption("orgcube.folder.format.csv"))
  }

  outpath <- file.path(root, path1)
  if(!is.null(path2)) outpath <- file.path(outpath, path2)
  if(!is.null(path3)) outpath <- file.path(outpath, path3)

  realpath <- dir.exists(outpath)
  if(!realpath){
    stop("Folder ", outpath, " does not exist")
  }

  return(outpath)
}


## ---- FILE OR COLUMN CHECKS ----

#' @title is_fileaccess
#' @keywords internal
#' @family utils
#' @description
#' Uses file.access() to check if file exists and can be read
#' @param file
is_fileaccess <- function(file){

  if(file.access(file, mode = 0) == -1){
    stop("FILE '", basename(file) , "' does not exist!")
  }

  if(file.access(file, mode = 4) == -1){
    stop("FILE '", basename(file), "' exists, but cannot be read")
  }

  invisible()
}

#' @title is_coltype
#' @keywords internal
#' @family utils
#' @param dt data table
#'
#' @returns list of values and dimensions
is_coltype <- function(dt){

  misc <- c("KOBLID", "ROW")

  potentialvals <- gsub("^(.*?)\\.f$", "\\1", names(dt)[grepl("^(.*?)\\.f$", names(dt))])
  vals <- character()
  for(val in potentialvals){
    vals <- c(vals,
              paste0(val, c("", ".f", ".a", ".n")))
  }
  vals <- names(dt)[names(dt) %in% vals]

  dims <- names(dt)[names(dt) %notin% c(misc, vals)]

  return(list(vals = vals,
              dims = dims))
}

