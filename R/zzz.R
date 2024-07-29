## Global options
opt.orgcube <- orgdata:::is_globs("orgcube")

.onLoad <- function(libname, pkgname) {
  optOrgcube <- orgdata:::is_globs("orgcube")
  orgDT <- !(names(optOrgcube) %in% names(options()))
  if (any(orgDT)) options(optOrgcube[orgDT])

  corrglobs <- orgdata:::is_correct_globs(optOrgcube)
  if(!isTRUE(corrglobs)){
    x <- utils::askYesNo("Options are not the same as in the config file, update options now?")
    if(isTRUE(x)){
      orgdata:::update_globs("orgcube")
    }
  }

  invisible()
}

# .onAttach <- function(libname, pkgname) {
#   packageStartupMessage(paste("orgcube version",
#                               utils::packageDescription("orgcube")[["Version"]], "\n"))
#
#   latest <- orgdata:::is_latest_version("orgcube")
#   if (latest){
#     x <- utils::askYesNo("Update orgcube now?")
#     if (isTRUE(x)){
#       orgdata::update_orgcube()
#     }
#   }
# }
