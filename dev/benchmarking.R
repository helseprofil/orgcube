library(microbenchmark)

alt1 = function(){
  date <- format(batch, "%Y-%m-%d")
  x <- data.table::setDT(DBI::dbReadTable(kh$dbconn, "KUBER", check.names = F))
  y <- data.table::setDT(DBI::dbReadTable(kh$dbconn, "TNP_PROD", check.names = F))

  x <- x[KUBE_NAVN == "HKR" & VERSJONFRA <= date & VERSJONTIL > date]
  y <- y[TNP_NAVN == x[, TNP]]
  return(list(x = x, y = y))
}

alt2 = function(){

  datestr <- format(batch, "#%Y-%m-%d#")
  validstr <- paste0("VERSJONFRA <=", datestr, " AND VERSJONTIL >", datestr)
  namestr <- paste0("'HKR'")
  x = data.table::setDT(DBI::dbGetQuery(kh$dbconn,
                      paste0("SELECT * FROM KUBER WHERE KUBE_NAVN=", namestr, " AND ", validstr)))
  TNPstr <- paste0("'", x$TNP, "'")
  y = data.table::setDT(DBI::dbGetQuery(kh$dbconn,
                      paste0("SELECT * FROM TNP_PROD WHERE TNP_NAVN=", TNPstr, " AND ", validstr)))

  return(list(x = x, y = y))
}

alt3 = function(x, y){
  out = ""
  if(!(is.na(x) || x == "")){out <- x}
  if(!(is.null(y) || is.na(y) || y == "")){out <- y}

  return(out)
}

# alt3 = function(){
#
#   if(file == teller){
#     tabsubset <- is_tabsubset()
#   } else {
#     tabsubset <- ""
#   }
# }

microbenchmark(alt1(),
               alt2(),
               # alt3(val, val0),
               times = 100, check = "equal")
