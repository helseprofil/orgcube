% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cube-params.R
\name{get_filegroups}
\alias{get_filegroups}
\title{get_filegroups}
\usage{
get_filegroups(params = params)
}
\arguments{
\item{params}{a list of parameters}
}
\value{
updated params list, including files, filedesign, and filegroup parameters
}
\description{
Uses information from ACCESS to read and filter the original files
to be used to create the final data. Add files, information about filegroup
design, and filegroup parameters to the list.
}
\seealso{
Other helper: 
\code{\link{find_files}()},
\code{\link{get_access_cube}()},
\code{\link{get_design_cube}()},
\code{\link{get_filegroup_path}()},
\code{\link{get_filegroupdesign}()},
\code{\link{get_tabsubset}()},
\code{\link{is_filter}()},
\code{\link{is_teller}()},
\code{\link{use_filegroup_filter}()}
}
\concept{helper}
\keyword{internal}
