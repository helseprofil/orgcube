#' @title Make Filegroup
#' @description
#' Reads and process original data and returns a filegroup which can be
#' further processed by [make_cube()]
#'
#' Performs all data processing starting from a filegroup and returns a
#' complete publication-ready data file.
#'
#' @param group.id name of filegroup, corresponding to an entry in KHELSA::FILGRUPPER::FILGRUPPE
#'
#' @return a complete filegroup
#' @export
make_filegroup <- function(group.id = NULL){

  batchdate <- format(Sys.time(), "%Y-%m-%d-%H-%M")

  # Connect to db
  kh <- connect_khelsa("kh")
  on.exit(kh$db_close(), add = TRUE)




}
