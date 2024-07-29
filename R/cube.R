#' @title Make Cube
#' @description
#' Performs all data processing starting from a filegroup generated with
#' [make_filegroup()] and returns a complete publication-ready data file.
#' @family main
#'
#' @param cube.id name of cube, corresponding to an entry in KHELSA::KUBER::KUBE_NAVN
#' @param year production year
#'
#' @return cube
#' @export
make_cube <- function(cube.id = NULL,
                      year = NULL){

  if(is.null(cube.id)) stop("cube.id not provided")

  batch <- Sys.time()
  datetag <- format(batch, "%Y-%m-%d-%H-%M")
  if(is.null(year)) year <- getOption("orgcube.year")

  # Connect to db
  kh <- connect_khelsa("kh")
  logg <- connect_khelsa("logg")
  duck <- duckdb::dbConnect(duckdb::duckdb())
  on.exit(kh$db_close(), add = TRUE)
  on.exit(logg$db_close(), add = TRUE)

  # Fetch relevant info from ACCESS, including connections
  params <- do_cube_params(con_db = kh$dbconn,
                           con_logg = logg$dbconn,
                           con_duck = duck, cube.id, batch, year)


  # Read and format files
  # KUBE <- read_files(access_specs)

}
