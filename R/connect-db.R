# Uses functions from orgdata::KHelse to connect, read and write to database.

#' @title Connect to Database
#' @description Produce an R6 object for connecting to the database
#' @param db Database file `kh` (KHELSA) or `logg` (KHlogg)
#' @param dbname Database filename with complete path
#' @param .test Use for testing only, returns db name
#' @param ... Other arguments
#' @keywords internal
connect_khelsa <- function(db = c("kh", "logg"), dbname = NULL, .test = FALSE, ...){

  db <- match.arg(db)
  dbfile <- switch(db,
                   kh = getOption("orgcube.db"),
                   logg = getOption("orgcube.logg"),
                   getOption("orgcube.db"))

  if (is.null(dbname)){
    dbname <- file.path(getOption("orgcube.folder.root"),
                        getOption("orgcube.folder.db"),
                        dbfile)
  }

  if (.test){
    return(dbname)
  }

  orgdata::KHelse$new(dbname = dbname, ...)

}

