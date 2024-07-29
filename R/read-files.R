## Functions used to read and format file groups

## ---- READ FILES ----

#' @title Read Files
#' @description
#' Uses information from ACCESS to read and filter the original files
#' to be used to create the final data using [make_cube()]. Returns a list
#' of formatted files.
#'
#' @param specs List of access specs generated with [is_access_cube()]
#'
#' @returns description
#' @export
read_files <- function(specs = specs){



  for(filename in filelist){
    teller <- is_teller(filename, specs)
    filter <- is_filter(filename, specs)

    tabsubset <- ifelse(teller, do_tabsubset(file, specs), "")

    path <- ifelse(filter,
                   is_filegroup_path(specs$FILFILTRE[FILVERSJON == filename, ORGFIL]),
                   is_filegroup_path(filename))

    file <- do_readfilegroup(path, filter)





    # if(!is_filter(file, specs)){
    #   do_readfilegroup(path, tabsubset)
    # } else {
    filefilter <- do_filefilter()
    #   do_readfilegroup_filter(path, tabsubset, specs)
    # }


    files[[filename]] <- ...
    design[[filename]] <- ...
    FILGRUPPE[[filename]] <- ...


  }

}


## ---- WORKERS ----



#' @title do_readfile
#' @keywords internal
#'
#' @param filepath path to file
#' @param tabfilter string for filtering TAB-columns
#' @param filter TRUE/FALSE based on is_filter()
#'
#' @return filtered data.table
do_readfilegroup <- function(filepath, tabfilter, filter){

}



## ---- HELPERS ----


