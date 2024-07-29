#' @noRd
KlargjorFil <- function(FilVers, TabFSub = "", rolle = "", KUBEid = "", versjonert = FALSE, FILbatch = NA, batchdate = SettKHBatchDate(), GeoHarmDefault = 1, globs = FinnGlobs()) {
  is_kh_debug()
  TilBuffer <- 0
  if (!exists("BUFFER")) {
    .GlobalEnv$BUFFER <- list()
  }
  datef <- format(strptime(batchdate, "%Y-%m-%d-%H-%M"), "#%Y-%m-%d#")

  FilterDscr <- as.list(sqlQuery(globs$dbh, paste("SELECT * FROM FILFILTRE WHERE FILVERSJON='", FilVers, "' AND VERSJONFRA<=", datef, " AND VERSJONTIL>", datef, sep = ""), as.is = TRUE))

  # Har oppsatt filter
  if (length(FilterDscr$FILVERSJON) > 0) {
    FGP <- FinnFilgruppeParametre(FilterDscr$ORGFIL, batchdate = batchdate, globs = globs)
    if (is.null(BUFFER[[FilVers]])) {
      if (!is.na(FilterDscr$SUBSET)) {
        if (FilterDscr$SUBSET != "") {
          if (TabFSub != "") {
            TabFSub <- paste(TabFSub, FilterDscr$SUBSET, sep = " & ")
          } else {
            TabFSub <- FilterDscr$SUBSET
          }
        }
      }

      FILn <- FinnFil(FilterDscr$ORGFIL, batch = FILbatch, versjonert = versjonert)
      FIL <- FILn$FT
      sqlQuery(globs$log, paste("INSERT INTO KUBEBATCH (KUBEBATCH,FILBATCH) SELECT '", KUBEid, "_", batchdate, "','", FilterDscr$ORGFIL, "_", FILn$batch, "'", sep = ""))
      if (TabFSub != "") {
        # print("ASKJDLKJASLDKJL  TabFSub")
        cat("Filtrer med tab-filter, f r er dim(FIL)", dim(FIL))
        FIL <- eval(parse(text = paste("subset(FIL,", TabFSub, ")", sep = "")))
        cat(" og etter", dim(FIL), "\n")
      }

      orgkols <- data.table::copy(names(FIL))
      if (grepl("\\S", FilterDscr$KOLLAPSdeler)) {
        cat("F r aggregering er dim(FIL)", dim(FIL))
        tabkols <- FinnTabKols(names(FIL))
        data.table::setkeyv(FIL, tabkols)
        kolldel <- unlist(stringr::str_split(FilterDscr$KOLLAPSdeler, ","))
        kolldelN <- unlist(globs$DefDesign$DelKolsF[kolldel])
        # FIL[,(kolldelN):=NULL]
        FIL[, (kolldelN) := KHglobs$TotalKoder[kolldel]]
        FIL <- FIL[, lapply(.SD, sum), by = tabkols]
        FIL[, (kolldelN) := KHglobs$TotalKoder[kolldel]]
        FIL <- FIL[, orgkols, with = FALSE]
        cat(" og etter", dim(FIL), "\n")
      }

      if (!(is.na(FilterDscr$NYEKOL_KOL_preRAD) | FilterDscr$NYEKOL_KOL_preRAD == "")) {
        FIL <- LeggTilNyeVerdiKolonner(FIL, FilterDscr$NYEKOL_KOL_preRAD, slettInf = TRUE)
      }
      Filter <- SettFilterDesign(FilterDscr, bruk0 = FALSE, FGP = FGP, globs = globs)
      if (length(Filter) > 0) {
        FIL <- OmkodFil(FIL, FinnRedesign(FinnDesign(FIL), list(Parts = Filter)), globs = globs, echo = 1)
      }

      if (FilterDscr$GEOHARM == 1) {
        rektiser <- ifelse(FilterDscr$REKTISER == 1, 1, 0)
        FIL <- GeoHarm(FIL, vals = FGP$vals, rektiser = rektiser, batchdate = batchdate, globs = globs)
      }
      if (!(is.na(FilterDscr$NYETAB) | FilterDscr$NYETAB == "")) {
        FIL <- AggregerRader(FIL, FilterDscr$NYETAB, FGP = FGP)
      }

      if (grepl("\\S", FilterDscr$NYEKOL_RAD)) {
        FIL <- LeggTilSumFraRader(FIL, FilterDscr$NYEKOL_RAD, FGP = FGP, globs = globs)
      }
      if (!(is.na(FilterDscr$NYEKOL_KOL) | FilterDscr$NYEKOL_KOL == "")) {
        FIL <- LeggTilNyeVerdiKolonner(FIL, FilterDscr$NYEKOL_KOL, slettInf = TRUE)
      }

      if (!(is.na(FilterDscr$NYKOLSmerge) | FilterDscr$NYKOLSmerge == "")) {
        NY <- eval(parse(text = FilterDscr$NYKOLSmerge))
        tabK <- intersect(FinnTabKols(names(NY)), FinnTabKols(names(FIL)))
        data.table::setkeyv(NY, tabK)
        data.table::setkeyv(FIL, tabK)
        FIL <- NY[FIL]
      }

      # FF_RSYNT1
      if (!(is.na(FilterDscr$FF_RSYNT1) | FilterDscr$FF_RSYNT1 == "")) {
        FilterDscr$FF_RSYNT1 <- gsub("\\\r", "\\\n", FilterDscr$FF_RSYNT1)
        rsynt1err <- try(eval(parse(text = FilterDscr$FF_RSYNT1)), silent = TRUE)
        print("***AD HOC MANIPULERING\n")
        if ("try-error" %in% class(rsynt1err)) {
          print(rsynt1err)
        }
      }

      .GlobalEnv$BUFFER[[FilVers]] <- FIL
      TilBuffer <- 1
    }
    # Bruk ferdig lagret versjon
    else {
      FIL <- data.table::copy(BUFFER[[FilVers]])
      print(FilVers)
      # print(BUFFERbatch)
      if (versjonert == TRUE) {
        # sqlQuery(globs$log,paste("INSERT INTO KUBEBATCH (KUBEBATCH,FILBATCH) SELECT '",KUBEid,"_",batchdate,"','",FilterDscr$ORGFIL,"_",BUFFERbatch[[FilVers]],"'",sep=""))
      }
    }
  }
  # Har ikke oppsatt filter, bruk r
  else {
    FILn <- FinnFil(FilVers, versjonert = versjonert, batch = FILbatch)
    FIL <- FILn$FT
    # sqlQuery(globs$log,paste("INSERT INTO KUBEBATCH (KUBEBATCH,FILBATCH) SELECT '",KUBEid,"_",batchdate,"','",FilVers,"_",FILn$batch,"'",sep=""))

    if (TabFSub != "") {
      cat("Filtrer med tab-filter, f r er dim(FIL)", dim(FIL))
      FIL <- eval(parse(text = paste("subset(FIL,", TabFSub, ")", sep = "")))
      cat(" og etter", dim(FIL), "\n")
    }
    FGP <- FinnFilgruppeParametre(FilVers, batchdate = batchdate, globs = globs)
    if (GeoHarmDefault == 1) {
      FIL <- GeoHarm(FIL, vals = FGP$vals, rektiser = FALSE, batchdate = batchdate, globs = globs)
    }
    .GlobalEnv$BUFFER[[FilVers]] <- FIL
    # .GlobalEnv$BUFFERbatch[[FilVers]]<-FILn$batch
    TilBuffer <- 1
  }


  FILd <- FinnDesign(FIL, FGP = FGP, globs = globs)
  gc()
  return(list(FIL = FIL, FGP = FGP, FILd = FILd, TilBuffer = TilBuffer))
}


#' @noRd
LeggTilNyeVerdiKolonner <- function(TNF, NYEdscr, slettInf = TRUE, postMA = FALSE) {
  is_kh_debug()

  TNF <- data.table::copy(TNF) # Får uønsket warning om self.reference under om ikke gjør slik
  data.table::setDT(TNF)
  valKols <- gsub("^(.+)\\.f$", "\\1", names(TNF)[grepl(".+\\.f$", names(TNF))])
  # FinnValKols(names(TNF))
  if (!(is.na(NYEdscr) | NYEdscr == "")) {
    for (nycolexpr in unlist(stringr::str_split(NYEdscr, ";"))) {
      nycol <- gsub("^(.*?)=(.*)$", "\\1", nycolexpr)
      expr <- gsub("^(.*?)=(.*)$", "\\2", nycolexpr)
      invKols <- valKols[sapply(valKols, FUN = function(x) {
        grepl(x, expr)
      })]
      eval(parse(text = paste(
        "TNF[,c(\"", paste(nycol, c("", ".f", ".a"), collapse = "\",\"", sep = ""), "\")
      :=list(", expr, ",pmax(", paste(invKols, ".f", collapse = ",", sep = ""), "),
                      pmax(", paste(invKols, ".a", collapse = ",", sep = ""), "))]",
        sep = ""
      )))
      if (postMA == TRUE) {
        eval(parse(text = paste(
          "TNF[,c(\"", paste(nycol, c(".n", ".fn1", ".fn3", ".fn9"), collapse = "\",\"", sep = ""), "\")
        :=list(1,0,0,0)]",
          sep = ""
        )))
      }
      if (slettInf == TRUE) {
        eval(parse(text = paste("suppressWarnings(",
                                "TNF[", nycol, "%in% c(Inf,NaN,NA),c(\"", paste(nycol, c("", ".f"), collapse = "\",\"", sep = ""), "\"):=list(NA,2)])",
                                sep = ""
        )))
      }
    }
  }
  return(TNF)
}



#' @noRd
SettFilterDesign <- function(KUBEdscr, OrgParts = list(), bruk0 = TRUE, FGP = list(amin = 0, amax = 120), globs = FinnGlobs()) {
  is_kh_debug()

  Deler <- list()
  for (del in names(globs$DefDesign$DelKolN)) {
    # for (del in names(unlist(globs$DefDesign$DelKolN[ORGd$OmkDeler]))){
    # if (del %in% names(ORGd$Part) | grepl("^T\\d$",del)){
    # Les liste
    koldel <- globs$DefDesign$DelKolN[del]
    koldel0 <- paste(koldel, "_0", sep = "")

    if (bruk0 == TRUE && !is.null(KUBEdscr[[koldel0]]) && !is.na(KUBEdscr[[koldel0]]) && KUBEdscr[[koldel0]] != "") {
      delListStr <- KUBEdscr[[koldel0]]
    } else {
      delListStr <- KUBEdscr[[koldel]]
    }
    if (!(is.null(delListStr) || is.na(delListStr) || delListStr == "")) {
      delListStr <- gsub("^ *| *$", "", delListStr)
      minus <- grepl("^-\\[", delListStr)
      delListStr <- gsub("^-\\[(.*)\\]$", "\\1", delListStr)
      delListA <- unlist(stringr::str_split(delListStr, ","))
      if (globs$DefDesign$DelType[del] == "INT") {
        if (del == "A") {
          delListA <- gsub("ALLE", paste(FGP$amin, "_", FGP$amax, sep = ""), delListA)
          delListA <- gsub("^_(\\d+)", paste(FGP$amin, "_\\1", sep = ""), delListA)
          delListA <- gsub("(\\d+)_$", paste("\\1_", FGP$amax, sep = ""), delListA)
        }
        delListA <- gsub("^(\\d+)$", "\\1_\\1", delListA)
        delListA <- data.table::as.data.table(matrix(as.integer(stringr::str_split_fixed(delListA, "_", 2)), ncol = 2))
      } else if (globs$DefDesign$DelFormat[del] == "integer") {
        delListA <- as.integer(delListA)
      } else if (globs$DefDesign$DelFormat[del] == "numeric") {
        delListA <- as.numeric(delListA)
      }
      listDT <- data.table::setnames(as.data.table(delListA), globs$DefDesign$DelKols[[del]])
      if (minus == TRUE) {
        if (!is.null(OrgParts[[del]])) {
          data.table::setkeyv(listDT, names(listDT))
          data.table::setkeyv(OrgParts[[del]], names(listDT))
          Deler[[del]] <- OrgParts[[del]][!listDT, ]
        } else {
          print("**********************KAN IKKE BRUKE -[liste] i SettFilterDesign når ikke OrgParts")
        }
      } else {
        Deler[[del]] <- listDT
      }
    } else if (globs$DefDesign$DelType[del] == "INT") {
      start <- KUBEdscr[[paste(koldel, "_START", sep = "")]]
      stopp <- KUBEdscr[[paste(koldel, "_STOP", sep = "")]]
      if (!(is.null(start) | is.null(stopp))) {
        if (!(is.na(start) | is.na(stopp))) {
          if (!(start == "" | stopp == "")) {
            if (stopp >= start) {
              if (!is.null(OrgParts[[del]])) {
                Deler[[del]] <- subset(OrgParts[[del]], eval(parse(text = paste(koldel, "l>=", start, " & ", koldel, "h<=", stopp, sep = ""))))
              } else {
                # Deler[[del]]<-setNames(as.data.frame(cbind(start:stopp,start:stopp)),paste(koldel,c("l","h"),sep=""))
              }
            } else {
              cat("FEIL!!!!!!!! kan ikke ha start ", start, "> stopp ", stopp, "\n")
            }
          }
        }
      }
    }
  }
  return(Deler)
}

#' @noRd
OmkodFil <- function(FIL, RD, globs = FinnGlobs(), echo = 0) {
  is_kh_debug()

  orgkols <- names(FIL)
  data.table::setDT(FIL)
  tabnames <- FinnTabKols(names(FIL))
  valkols <- FinnValKols(names(FIL))
  lp <- paste(valkols, "=sum(", valkols, "),",
              valkols, ".f=max(", valkols, ".f),",
              valkols, ".a=sum(", valkols, ".a*(!is.na(", valkols, ") & ", valkols, "!=0))",
              sep = "", collapse = ","
  )

  if (nrow(RD$FULL) > 0) {
    for (del in names(RD$Filters)) {
      data.table::setkeyv(FIL, names(RD$Filters[[del]]))
      data.table::setkeyv(RD$Filters[[del]], names(RD$Filters[[del]]))
      if (echo == 1) {
        cat("Filtrerer", del, "før dim(FIL)=", dim(FIL))
      }
      if (any(duplicated(RD$Filters[[del]]))) {
        print("CARTESIAN????")
        print(RD$Filters[[del]])
        print(RD$Filters[[del]][duplicated(RD$Filters[[del]]), ])
      }
      FIL <- FIL[RD$Filters[[del]], nomatch = 0]
      if (echo == 1) {
        cat(" og etter", dim(FIL), "\n")
      }
    }

    # NB! Rekkefølge er essensiell, dvs at ubeting kommer til slutt
    beting <- intersect(globs$DefDesign$AggPri[length(globs$DefDesign$AggPri):1], c(globs$DefDesign$BetingOmk, globs$DefDesign$BetingF))
    ubeting <- intersect(globs$DefDesign$AggPri[length(globs$DefDesign$AggPri):1], c(globs$DefDesign$UBeting))

    for (del in intersect(c(beting, ubeting), names(RD$KBs))) {
      orgtabs <- names(RD$KBs[[del]])[!grepl("_omk$", names(RD$KBs[[del]]))]
      omktabs <- names(RD$KBs[[del]])[grepl("_omk$", names(RD$KBs[[del]]))]
      bycols <- c(setdiff(tabnames, gsub("_omk", "", omktabs)), omktabs)

      # Sjekk type omkoding som trengs.
      # Dersom hver orgkode skal til mange omkkoder
      # er det uheldig å merge FIL[KB] om FIL er stor siden det lages mange kopier av orglinjer i FIL
      # i slike tilfeller kobles i stedet inn en loop over omkoding til hver omktab  (jfr laging av tiårssnitt i KREFT)

      data.table::setkeyv(RD$KBs[[del]], orgtabs)
      replikfaktor <- RD$KBs[[del]][, list(N = .N), by = orgtabs][, mean(N)]
      data.table::setkeyv(FIL, orgtabs)
      if (echo == 1) {
        cat("Omkoder", del, "dim(FIL) er ", dim(FIL), "originalt")
      }
      if (nrow(FIL) < 1000000 | replikfaktor < 4 | del == "Gn") {
        FIL <- FIL[RD$KBs[[del]], nomatch = 0, allow.cartesian = TRUE]
        # FIL<-FIL[RD$KBs[[del]],nomatch=0]
        if (echo == 1) {
          cat(" og", dim(FIL), "etter merge")
        }
        if (del == "Gn") {
          # Omkod geo
          FIL[GEOniv_omk == "K", GEO := substr(GEO, 0, 4)]
          FIL[GEOniv_omk == "F", GEO := FYLKE]
          FIL[GEOniv_omk == "L", c("GEO", "FYLKE") := list("0", "00")]
          # FIL[GEOniv_omk=="B" & GEOniv=="S" & grepl("^(0301|1103|1201|1601)",GEO),c("GEO","FYLKE"):=list(substr(GEO,0,6),substr(GEO,0,2))]
          # FIL[GEOniv_omk=="B" & GEOniv=="S" & !grepl("^(0301|1103|1201|1601)",GEO),c("GEO","FYLKE"):=list("999999","99")]
          FIL[GEOniv_omk == "B" & GEOniv == "S" & !grepl("^(0301|1103|1201|1601|4601|5001)", GEO), c("GEO", "FYLKE") := list("999999", "99")]
          # Dette er dårlig, bør endre til
          # FIL[GEOniv_omk=="B" & GEOniv=="S" & !GEO %in% globs$GeoKoder[GEOniv=="B"]$GEO,c("GEO","FYLKE"):=list("999999","99")]
          FIL[GEOniv_omk == "H" & GEOniv != "H", GEO := plyr::mapvalues(FYLKE, globs$HELSEREG$FYLKE, globs$HELSEREG$HELSEREG, warn_missing = FALSE)]
          # FIL[GEOniv_omk=="H" & GEOniv!="H",FYLKE:="00"]
          FIL[GEOniv_omk == "H", FYLKE := "00"]
        }
        data.table::setkeyv(FIL, bycols)
        lpl <- paste("list(", lp, ")", sep = "")
        FIL <- FIL[, eval(parse(text = lpl)), by = bycols]
        # Dette skulle vel vært bedre, men blir alt for tregt? når ikke bycols er key
        # FIL<-FIL[RD$KBs[[del]],nomatch=0,allow.cartesian=TRUE][, eval(parse(text=lp)), by=bycols]
      } else {
        KB <- data.table::copy(RD$KBs[[del]])
        data.table::setkeyv(KB, omktabs)
        OMKs <- unique(KB[, omktabs, with = FALSE])
        FILt <- FIL[0, ]
        for (i in 1:nrow(OMKs)) {
          OMK <- OMKs[i, ]
          print(OMK)
          KBt <- KB[OMK]
          data.table::setkeyv(KBt, orgtabs)
          FILd <- FIL[KBt, nomatch = 0, allow.cartesian = TRUE]
          data.table::setkeyv(FILd, bycols)
          lpt <- paste("list(", paste(gsub("_omk$", "", names(OMK)), OMK, sep = "=", collapse = ","), ",", lp, ")", sep = "")
          FILt <- rbind(FILt, FILd[, eval(parse(text = lpt)), by = bycols][, names(FILt), with = FALSE])
        }
        FIL <- FILt
      }
      if (echo == 1) {
        cat(" og til slutt", dim(FIL), "\n")
      }
      data.table::setnames(FIL, names(FIL), gsub("_omk$", "", names(FIL)))
    }
  }

  if (nrow(RD$Udekk) > 0) {
    UDekk <- data.table::copy(RD$Udekk)
    restkols <- setdiff(tabnames, names(UDekk))
    data.table::setkeyv(FIL, names(UDekk))
    data.table::setkeyv(UDekk, names(UDekk))
    FIL <- FIL[!UDekk, ]
    valkolsF <- unlist(lapply(valkols, function(x) {
      paste(x, c("", ".f", ".a"), sep = "")
    }))
    ## feil med recycling av := lest NEWS 1.12.2 data.table
    ## UDekk[,(valkolsF):=list(NA,9,0)]
    UDekk[, (valkols) := NA]
    valg_f <- grep(".f$", valkolsF, value = TRUE)
    UDekk[, (valg_f) := 9]
    valg_a <- grep(".a$", valkolsF, value = TRUE)
    UDekk[, (valg_a) := 0]
    if (length(restkols) > 0) {
      rest <- as.data.frame(unique(FIL[, restkols, with = FALSE]))
      UDekk <- data.table::data.table(expand.grid.df(rest, as.data.frame(UDekk)))
    }
    FIL <- rbind(FIL[, orgkols, with = FALSE], UDekk[, orgkols, with = FALSE])
    cat("UDEKKA:", nrow(RD$Udekk), "\n")
    # print(subset(RD$Udekk,GEOniv!="B"))
    print(RD$Udekk)
  }
  return(FIL)
}

#' @noRd
FinnRedesign <- function(DesFRA, DesTIL, SkalAggregeresOpp = character(), ReturnerFullFull = FALSE, globs = FinnGlobs(), prios = globs$DefDesign, KB = globs$KB, IntervallHull = globs$DefDesign$IntervallHull, AggPri = globs$DefDesign$AggPri, echo = 0) {
  is_kh_debug()

  # Merk assymtri mellom DesFRA og DesTIL.
  # For DesTIL brukes bare DesTil$Part og DesTIL$OmkDesign.
  # DesFRA må derfor komme fra FinnDesign med alle egenskaper satt der, mens DesTIL kan være enklere og satt andre steder

  #   #Deler i DesFra som ikke er i DesTil må legegs til i DesTil (full kryss mot Part[del])
  #   #Merk at deler i DesTil som ikke er i DesFra går greit (all omkoding er indirekte "betinget" på disse)
  #   kryssdeler<-names(DesFRA$Part)[!(names(DesFRA$Part) %in% names(DesTIL$Part))]
  #   if (length(kryssdeler)>0){
  #     DesTIL<-ModifiserDesign(DesFRA$Part[kryssdeler],DesTIL,globs=globs)
  #   }
  #   Redesign<-list()

  # Sett partiell omkoding
  # For intervaller kalles FinnKodebokINtervaller, elllers hentes fast kodebok som utgnagspunkt
  # Disse kodebøkene (KJONN etc) filtreres til de omkodingene som er aktuelle (bør gjøres her for å begrense kombinatorikk, selv om dette kunne vært utsatt)
  # Dvs omkodinger til en TIL som ikke har alle nødvendige deler i FRA filtreres bort
  # Merk at noen deler i FRA ikke er obligatoriske (slik som KJONN=9 for omkoding til KJONN=0)

  # Rydd DesTIL$Design (Kan variere litt mht HAR avhengig av hvor kallet på denne funksjonen er gjort fra. Skal ha 1 har felt)
  if (is.null(DesTIL$Design)) {
    komblist <- paste("as.data.frame(DesTIL$Part[[\"", names(DesTIL$Part), "\"]])", sep = "", collapse = ",")
    ## FULL <- data.table::data.table(eval(parse(text = paste("expand.grid.df(", komblist, ")", sep = ""))))
    FULL <- do.call(expand.grid.df, DesTIL$Part)
    data.table::setDT(FULL)

    harkols <- names(FULL)[grepl("_HAR$", names(FULL))]
    if (length(harkols) > 0) {
      FULL[, (harkols) := NULL]
    }
  } else {
    FULL <- DesTIL$Design[HAR == 1, ]
    harkols <- names(FULL)[grepl("_HAR$|^HAR$", names(FULL))]
    FULL[, (harkols) := NULL]
  }
  data.table::setnames(FULL, names(FULL), paste(names(FULL), "_omk", sep = ""))

  TempFile <- file.path(tempdir(), paste0("full", SettKHBatchDate(), ".RDS"))
  saveRDS(FULL, TempFile)

  # Need to get the colnames before manipulation
  namesFULL <- names(FULL)
  gc()

  betKols <- setdiff(names(DesFRA$SKombs$bet), "HAR")
  if (length(betKols) > 0) {
    FULL <- expand.grid.df(FULL, DesFRA$SKombs$bet[, ..betKols])
    data.table::setDT(FULL)
  }
  for (del in DesFRA$UBeting) {
    if (is.null(DesTIL$Part[[del]])) {
      DesTIL$Part[[del]] <- data.table::copy(DesFRA$Part[[del]])
    }
  }

  gc()
  Parts <- list()
  for (del in names(KB)) {
    # if (del %in% names(DesTIL$Part)){

    if (del %in% names(DesTIL$Part) & del %in% names(DesFRA$Part)) {
      DesTIL$Part[[del]] <- data.table::copy(data.table::as.data.table(DesTIL$Part[[del]])) # Får noen rare warnings uten copy, bør debugge dette
      delH <- paste(del, "_HAR", sep = "")
      if (!delH %in% names(DesTIL)) {
        DesTIL$Part[[del]][, (delH) := 1]
      }
      KBD <- KB[[del]]
      kol <- globs$DefDesign$DelKolN[del]
      kolomk <- paste(kol, "_omk", sep = "")
      kols <- globs$DefDesign$DelKols[[del]]
      kolsomk <- paste(kols, "_omk", sep = "")

      # Sett 1-1 koding for T1,T2,.. dersom ikke annet gitt
      if (grepl("^T\\d$", del) & nrow(KBD) == 0) {
        tabN <- globs$DefDesign$DelKolN[del]
        tilTabs <- DesTIL$Part[[del]][, tabN, with = FALSE]
        KBD <- setNames(data.frame(tilTabs, tilTabs, 0, 1), c(tabN, paste(tabN, "_omk", sep = ""), paste(del, c("_pri", "_obl"), sep = "")))
        Parts[[del]] <- KBD
      }
      # Behandling av enkle kolonner
      if (globs$DefDesign$DelType[del] == "COL") {
        if (nrow(KBD) > 0) {
          # Filtrer bort TIL-koder i global-KB som ikke er i desTIL

          KBD <- KBD[KBD[, kolomk] %in% DesTIL$Part[[del]][[kol]], ]
          omkcols <- c(kolomk, paste(del, "_pri", sep = ""))
          kolsomkpri <- c(kolsomk, paste(del, "_pri", sep = ""))
          KBD <- data.table::data.table(KBD, key = omkcols)
          # Sett HAR og Dekk
          eval(parse(text = paste(
            "KBD[,", del, "_HAR:=as.integer(", kol, " %in% DesFRA$Part[[del]][[kol]])]",
            sep = ""
          )))
          eval(parse(text = paste(
            "KBD[,", del, "_Dekk:=as.integer(!any(", del, "_HAR==0 & ", del, "_obl==1)),by=kolsomkpri]",
            sep = ""
          )))
          # Kast omkodinger uten noen deler i FRA, behold de som dekkes helt og delvis
          eval(parse(text = paste(
            "KBD[,Kast:=!any(", del, "_HAR==1),by=kolsomkpri]",
            sep = ""
          )))
          KBD <- subset(KBD, Kast == FALSE)
          KBD$Kast <- NULL
          Parts[[del]] <- KBD
        }

        # Behandling av intervaller (to kolonner etc)
      } else if (globs$DefDesign$DelType[del] == "INT") {
        # Global KB kan inneholde (fil)spesifikke koden "ALLE", må erstatte denne med "amin_amax" og lage intervall
        # Merk: dette gjelder typisk bare tilfellene der ukjent alder og evt tilsvarende skal settes inn under "ALLE"
        Imin <- eval(parse(text = paste("min(DesFRA$Part[[del]][,", globs$DefDesign$DelKolN[[del]], "l])", sep = "")))
        Imax <- eval(parse(text = paste("max(DesFRA$Part[[del]][,", globs$DefDesign$DelKolN[[del]], "h])", sep = "")))
        alle <- paste(Imin, "_", Imax, sep = "")
        if (nrow(KBD) > 0) {
          KBD[, kol] <- gsub("^(ALLE)$", alle, KBD[, kol])
          KBD[, kolomk] <- gsub("^(ALLE)$", alle, KBD[, kolomk])
          # KBD[,globs$DefDesign$DelKols[[del]]]<-as.integer(stringr::str_split_fixed(KBD[,kol],"_",2))
          KBD[, globs$DefDesign$DelKols[[del]]] <- matrix(as.integer(stringr::str_split_fixed(KBD[, kol], "_", 2)), ncol = 2)
          KBD[, paste(globs$DefDesign$DelKols[[del]], "_omk", sep = "")] <- matrix(as.integer(stringr::str_split_fixed(KBD[, kolomk], "_", 2)), ncol = 2)
          # Kodebok ferdig mod

          # Filtrer KBD mot TIL!!
          # KBD<-KBD[KBD[,kolomk] %in% paste(DesTIL$Part[[del]][,kols,with=FALSE],sep="_"),]
          KBD <- KBD[KBD[, kolomk] %in% apply(DesTIL$Part[[del]][, kols, with = FALSE], 1, paste, collapse = "_"), ]
        }
        # Må fjerne "del_HAR" inn i omkodintervall, fjerner dessuten del_HAR==0 i TIL

        delkols <- KHglobs$DefDesign$DelKols[[del]]
        IntFra <- DesFRA$Part[[del]][, delkols, with = FALSE]
        # IntTil<-DesTIL$Part[[del]][DesTIL$Part[[del]][[paste(del,"_HAR",sep="")]]==1,delkols,with=FALSE]
        # Merk: eneste som ikke har del_HAR er udekkede intervaller mellom amin og amax.
        # Videre er disse bare med når TilDes er satt fra FinnDesign(FG), ikke når TilDes er fra Parts
        # Usikker på om det alltid er best å slippe disse gjennom.
        IntTil <- DesTIL$Part[[del]][, delkols, with = FALSE]
        # Fjerner spesialkoder (dvs uoppgitt etc i KB) før intervallomregning
        IntFra <- IntFra[!apply(IntFra[, kols, with = FALSE], 1, paste, collapse = "_") %in% globs$LegKoder[[del]]$KODE]
        IntTil <- IntTil[!apply(IntTil[, kols, with = FALSE], 1, paste, collapse = "_") %in% globs$LegKoder[[del]]$KODE]
        # print("aksdløkaslødkøalsdkøkø")
        # print(IntFra)
        # print(IntTil)
        KBInt <- FinnKodebokIntervaller(as.data.frame(IntFra), as.data.frame(IntTil), deln = del)

        KBInt[, paste(del, "_obl", sep = "")] <- 1
        # DEVELOP:   DETTE ER TENMMELIG AD HOC!!!!!!!!!!
        if (del == "A") {
          KBInt[KBInt$ALDERl >= 90, paste(del, "_obl", sep = "")] <- 0
        }

        KBInt[, paste(del, "_ok", sep = "")] <- NULL # Denne brukes bare ved filtrering rett fra KBint
        # Legg til spesialkoder igjen
        if (nrow(KBD) > 0) {
          KBD <- rbind(KBInt, KBD[, c(kols, kolsomk, paste(del, c("_pri", "_obl"), sep = ""))])
        } else {
          KBD <- KBInt
        }

        # Koble på "del_HAR"
        omkcols <- c(kolomk, paste(del, "_pri", sep = ""))
        KBD <- data.table::data.table(KBD, key = kols)
        KBD <- data.table::data.table(DesFRA$Part[[del]], key = kols)[KBD]
        har <- paste(del, "_HAR", sep = "")
        eval(parse(text = paste(
          "KBD[is.na(KBD[,", har, "]),", har, ":=0]",
          sep = ""
        )))

        KBD <- SettPartDekk(KBD, del = del, IntervallHull = IntervallHull, globs = globs)
        # data.table::setnames(KBD,"DEKKok",paste(del,"_Dekk",sep=""))

        # Kast omkodinger uten noen deler i FRA, behold de som dekkes helt og delvis
        # USIKKER på om dette er optimalt. Det må ikke kastes for mye for riktig bruk fra FinnFellesTab
        # Egentlig er det jo unødvenig å kaste noe som helst. Dette er mest for rapport/lesing av KBD
        kolsomkpri <- c(kolsomk, paste(del, "_pri", sep = ""))
        eval(parse(text = paste(
          "KBD[,Kast:=!any(", del, "_HAR==1 | ", del, "_obl==0),by=kolsomkpri]",
          sep = ""
        )))
        #         eval(parse(text=paste(
        #           "KBD[,Kast:=!any(",del,"_HAR==1),by=kolsomkpri]",sep=""
        #         )))

        KBD <- KBD[Kast == FALSE, ]
        KBD[, Kast := NULL]
        Parts[[del]] <- KBD
      }
    }
  }

  if (echo >= 1) {
    cat("Parts:\n")
    print(Parts)
  }

  gc()
  SKombs <- list()
  KBs <- list()
  Filters <- list()
  DelStatus <- list()

  # Må passe på rekkefølge (Ubeting til slutt), ellers kan det gå galt i FULL
  beting <- intersect(globs$DefDesign$AggPri[length(globs$DefDesign$AggPri):1], c(globs$DefDesign$BetingOmk, globs$DefDesign$BetingF))
  ubeting <- intersect(globs$DefDesign$AggPri[length(globs$DefDesign$AggPri):1], c(globs$DefDesign$UBeting))

  for (del in intersect(c(beting, ubeting), names(Parts))) {
    delkols <- globs$DefDesign$DelKols[[del]]
    if (length(DesFRA[["UBeting"]]) > 0) {
      if (del %in% DesFRA[["UBeting"]]) {
        kombn <- "bet"
      } else {
        kombn <- paste("bet", del, sep = "")
      }
      # Koble med DeSFRA
      data.table::setkeyv(Parts[[del]], delkols)
      data.table::setkeyv(DesFRA$SKombs[[kombn]], delkols)
      betD <- DesFRA$SKombs[[kombn]][Parts[[del]], allow.cartesian = TRUE]
      betD[is.na(HAR), HAR := 0]
      # Må kaste de som ikke har del_Dekk==1 hvis ikke kan de feilaktig
      # få del_Dekk==1 under dersom del er i beting, da vil en annen del i beting få NA og by=betcols går galt!)
      betD <- subset(betD, eval(parse(text = paste(del, "_Dekk==1", sep = ""))))
      # Sett (betinget) dekning
      betcols <- unlist(globs$DefDesign$DelKols[setdiff(DesFRA[["UBeting"]], del)])
      betD <- SettPartDekk(betD, del = del, har = "HAR", IntervallHull = IntervallHull, betcols = betcols, globs = globs)
    } else {
      betcols <- character()
      betD <- data.table::data.table(Parts[[del]])
    }
    if (echo >= 1) {
      cat("betD 1:\n", kombn, "\n")
      print(betD)
      print(komblist)
    }

    # Finn beste alternativ
    OmkCols <- names(betD)[grepl("_(omk)$", names(betD))]
    bycols <- c(OmkCols, betcols)
    if (del %in% SkalAggregeresOpp) {
      eval(parse(text = paste("betD[", del, "_Dekk==1,Bruk:=max(", del, "_pri),by=bycols]", sep = "")))
    } else {
      eval(parse(text = paste("betD[", del, "_Dekk==1,Bruk:=min(", del, "_pri),by=bycols]", sep = "")))
    }

    prid <- paste(del, "_pri", sep = "")
    KB <- betD[eval(parse(text = paste("Bruk==", prid, " & ", del, "_HAR==1", sep = "")))]
    SKombs[[del]] <- betD

    # Sjekk om del kan omkodes helt partielt (fra Part) eller om må betinge (dvs KB)

    # Finner om en omk_kode bruker flere versjoner av partiell omkoding (hver versjon fra Part har ulik prid)
    # Om en slik finnes beholdes KB, ellers fjernes overlødig betinging
    maxBet <- KB[, eval(parse(text = paste("list(NOPri=length(unique(", prid, ")))", sep = ""))), by = OmkCols][, max(NOPri)]
    # Utgått se KB<- over
    # KB<-KB[[del]][eval(parse(text=paste(del,"_HAR==1",sep=""))),]
    if (maxBet == 1) {
      # brukcols<-setdiff(names(KB),betcols)
      brukcols <- c(gsub("_omk$", "", OmkCols), OmkCols)
      data.table::setkeyv(KB, brukcols)
      KBs[[del]] <- unique(KB[, brukcols, with = FALSE])
      DelStatus[[del]] <- "P"
    } else {
      KB[, (names(KB)[grepl("(_obl|_{0,1}HAR|_Dekk|_pri|Bruk)$", names(KB))]) := NULL]
      KBs[[del]] <- KB
      DelStatus[[del]] <- "B"
    }

    if (del == "Y" & DelStatus[[del]] == "B") {
      KHerr("Har DelStatus[[Y]]==B, dette takles per nå ikke i FilOmkod og vil gi feil der!!!")
    }

    # Sett dekning i FULL
    # common<-intersect(names(FULL),names(KBs[[del]]))
    common <- intersect(names(FULL), names(KB))
    data.table::setkeyv(KB, common)
    data.table::setkeyv(FULL, common)
    FULL <- FULL[KB[, common, with = FALSE], nomatch = 0, allow.cartesian = TRUE]

    # if (D_develop_predtype=="DIR"){
    #   delkols<-KHglobs$DefDesign$DelKols[[del]]
    # }

    # Ignorer KB der det ikke foregår reell omkoding
    if (all(KBs[[del]][, delkols, with = FALSE] == KBs[[del]][, paste(delkols, "_omk", sep = ""), with = FALSE])) {
      Filters[[del]] <- KBs[[del]][, names(KBs[[del]])[!grepl("_omk$", names(KBs[[del]]))], with = FALSE]
      KBs[del] <- NULL
      DelStatus[[del]] <- "F"
    }
  }
  omkkols <- names(FULL)[grepl("_omk$", names(FULL))]
  data.table::setkeyv(FULL, omkkols)
  Dekk <- unique(FULL[, omkkols, with = FALSE])
  data.table::setnames(Dekk, names(Dekk), gsub("_omk$", "", names(Dekk)))

  data.table::setkeyv(FULL, namesFULL)
  Udekk <- handle_udekk(FULL, namesFULL, TempFile)

  gc()
  return(list(Parts = Parts, SKombs = SKombs, KBs = KBs, Filters = Filters, FULL = FULL, Dekk = Dekk, Udekk = Udekk, DelStatus = DelStatus))
}

#' @noRd
FinnDesign <- function(FIL, FGP = list(amin = 0, amax = 120), globs = FinnGlobs()) {
  is_kh_debug()

  if (identical(class(FIL), "data.frame")) {
    FIL <- data.table::data.table(FIL)
  }
  keyorg <- data.table::key(FIL)
  # Sett defdesign
  DelKols <- globs$DefDesign$DelKols
  UBeting <- globs$DefDesign$UBeting
  BetingOmk <- globs$DefDesign$BetingOmk
  BetingF <- globs$DefDesign$BetingF

  DesignKols <- globs$DefDesign$DesignKolsF[globs$DefDesign$DesignKolsF %in% names(FIL)]

  DesignKols <- globs$DefDesign$DesignKolsF[globs$DefDesign$DesignKolsF %in% names(FIL)]
  OmkKols <- globs$DefDesign$DesignKols[globs$DefDesign$DesignKols %in% names(FIL)]

  # Initier tomt resultat
  Design <- list()
  Design[["KolNavn"]] <- names(FIL)
  # Finn faktisk design
  setkeym(FIL, c(DesignKols))
  ObsDesign <- unique(FIL[, DesignKols, with = FALSE])
  # print(unique(FIL[,c("ALDERl","ALDERh"),with=FALSE]))
  # print(subset(FIL,GEOniv==1 & AARl==2009 & TAB1=="Total"))

  # Finn deler inneholdt i tabell
  Deler <- character()
  for (del in names(DelKols)) {
    if (all(DelKols[[del]] %in% DesignKols)) {
      Deler <- c(Deler, del)
    }
  }
  # Sjekk for evt ugyldig med bare ALDERl etc?

  # Sett omkodingskombinasjoner
  Design[["UBeting"]] <- UBeting[UBeting %in% Deler]
  Design[["BetingOmk"]] <- BetingOmk[BetingOmk %in% Deler]
  Design[["BetingF"]] <- BetingF[BetingF %in% Deler]
  Alle <- c(Design[["UBeting"]], Design[["BetingOmk"]], Design[["BetingF"]])
  Design[["OmkDeler"]] <- c(Design[["UBeting"]], Design[["BetingOmk"]])

  # Sett alle partielle tabuleringer (Gn,Y,K,A,T1,T2,T3),
  for (del in Deler) {
    kols <- DelKols[[del]]
    data.table::setkeyv(ObsDesign, kols)
    # SETT HAR
    Design[["Part"]][[del]] <- data.table::data.table(setNames(cbind(unique(ObsDesign[, kols, with = FALSE]), 1), c(kols, paste(del, "_HAR", sep = ""))), key = kols)
  }

  # Fyll evt hull i aldersintervaller
  # Bør generaliserer til INT !!!
  if (globs$DefDesign$AMissAllow == TRUE) {
    if ("A" %in% names(Design$Part)) {
      mangler <- intervals::interval_difference(Intervals(c(FGP$amin, FGP$amax), type = "Z"), Intervals(Design$Part$A[, DelKols$A, with = FALSE], type = "Z"))
      if (nrow(mangler) > 0) {
        mangler <- setNames(cbind(as.data.frame(mangler), 0), c("ALDERl", "ALDERh", "A_HAR"))
        #         if (max(mangler$ALDERl)>=95){
        #           mangler[ALDERl==max(mangler$ALDERl),A_HAR]<-1
        #         }
        Design[["Part"]][["A"]] <- rbind(Design[["Part"]][["A"]], mangler)
      }
    }
  }

  # Finn fullt design, dvs kryssing av alle partielle.
  delerlist <- paste("as.data.frame(Design[[\"Part\"]][[\"", Alle, "\"]])", sep = "", collapse = ",")
  FullDesign <- data.table::data.table(eval(parse(text = paste("expand.grid.df(", delerlist, ")", sep = ""))))
  setkeym(ObsDesign, names(ObsDesign))
  setkeym(FullDesign, names(ObsDesign))
  # Sett HAR=1 om denne finnes i fakttisk design
  FullDesign[, HAR := 0]
  FullDesign[ObsDesign, HAR := 1]
  Design[["Design"]] <- FullDesign

  # Utgått
  # Filtrer til bare den delen av designet som er aktuell for omkoding (dsv uten TAB1 etc)
  # setkeym(FullDesign,OmkKols)
  # Design[["OmkDesign"]]<-FullDesign[,list(HAR=max(HAR)),by=OmkKols]

  # Sett omkodingskombinasjone
  # Noen dimensjoner får variere fritt (UBeting). Andre må være fast for alle versjoner av UBeting
  # Def er at Gn og Y er frie, mens K og A må være fast for hver Gn,Y kombinasjon
  Beting <- c("", Design[["BetingOmk"]], Design[["BetingF"]])
  komb <- Design[["UBeting"]]
  for (del in Beting) {
    if (del != "") {
      komb <- c(Design[["UBeting"]], del)
    }
    if (length(komb) > 0) {
      kols <- character(0)
      for (k in komb) {
        kols <- c(kols, DelKols[[k]])
      }
      data.table::setkeyv(ObsDesign, kols)
      data.table::setkeyv(FullDesign, kols)
      kombFull <- data.table::data.table(unique(FullDesign[, kols, with = FALSE]))
      kombObs <- data.table::data.table(unique(ObsDesign[, kols, with = FALSE]))
      kombFull[, HAR := 0]
      kombFull[kombObs, HAR := 1]
      kombn <- paste("bet", del, sep = "")
      Design[["SKombs"]][[kombn]] <- kombFull
    }
  }

  # Tilbakestill key
  setkeym(ObsDesign, names(ObsDesign))
  setkeym(FIL, keyorg)
  gc()
  return(Design)
}

#' @noRd
GeoHarm <- function(FIL, vals = list(), rektiser = TRUE, FDesign = list(), batchdate = SettKHBatchDate(), globs = FinnGlobs(), GEOstdAAR = globs$KHaargang) {
  is_kh_debug()

  if (identical(class(FIL), "data.frame")) {
    FIL <- data.table::data.table(FIL)
  }
  keyorg <- data.table::key(FIL)
  geoomk <- globs$KnrHarm
  FIL$GEO <- plyr::mapvalues(FIL$GEO, geoomk$GEO, geoomk$GEO_omk, warn_missing = FALSE)
  FIL[, FYLKE := NULL]
  FIL <- KHaggreger(FIL, vals = vals, globs = globs)
  # Rektangulariser
  if (rektiser == TRUE) {
    REKT <- data.table::data.table()
    if (length(FDesign) == 0) {
      FDesign <- FinnDesign(FIL)
    }
    FDes <- FDesign$Design
    # Switch for TYP=="O" ??
    for (Gn in FDesign$Part[["Gn"]][["GEOniv"]]) {
      GEOK <- subset(globs$GeoKoder, FRA <= GEOstdAAR & TIL > GEOstdAAR & GEOniv == Gn)$GEO
      FDesG <- FDes[HAR == 1 & GEOniv == Gn, intersect(names(FIL), names(FDes)), with = FALSE]
      REKT <- rbind(data.table::data.table(expand.grid.df(data.frame(FDesG), data.frame(GEO = GEOK))), REKT)
    }
    data.table::setkeyv(REKT, names(REKT))
    data.table::setkeyv(FIL, names(REKT))
    FIL <- FIL[REKT]
    FIL <- SettMergeNAs(FIL, vals = vals)
  }


  FIL[, FYLKE := ifelse(GEOniv %in% c("H", "L"), "00", substr(GEO, 1, 2))]
  return(FIL)
}

AggregerRader <- function(FG, nyeexpr, FGP) {
  is_kh_debug()

  if (!(is.na(nyeexpr) || nyeexpr == "")) {
    nytabs <- unlist(stringr::str_split(nyeexpr, ";"))
    for (nytab in nytabs) {
      # PARSING av syntaks
      nylab <- gsub("^\\[(.*?)\\]=.*", "\\1", nytab)
      subexp <- gsub(".*\\]=\\{(.*)\\}", "\\1", nytab)
      if (grepl("%in%|==", subexp)) {
        tab <- gsub("^ *(.*) *(%in%|==).*", "\\1", subexp)
      } else {
        tab <- gsub("^ *(.*) *$", "\\1", subexp)
        subexp <- TRUE
      }
      if (!tab %in% names(FG)) {
        tabE <- names(FGP)[which(FGP == tab)]
        subexp <- gsub("^ *tab(.*)", paste(tabE, "\\1", sep = ""), subexp)
        tab <- tabE
      }
      FG2 <- eval(parse(text = paste("subset(FG,", subexp, ")", sep = "")))
      FG2 <- KHaggreger(FG2[, setdiff(names(FG), tab), with = FALSE])
      FG2[, eval(parse(text = paste(tab, ":='", nylab, "'", sep = "")))]
      FG <- rbind(FG, FG2[, names(FG), with = FALSE])
    }
  }
  return(FG)
}

#' @noRd
LeggTilSumFraRader <- function(TNF, NYdscr, FGP = list(amin = 0, amax = 120), globs = FinnGlobs()) {
  is_kh_debug()

  if (!(is.na(NYdscr) | NYdscr == "")) {
    for (sumfra in unlist(stringr::str_split(NYdscr, ";"))) {
      # cat("SUMFRA: ",sumfra,"\n")
      if (grepl("^ *(.+?) *= *(.+?)\\{(.*)\\} *$", sumfra)) {
        nycol <- gsub("^ *(.+?) *= *(.+?)\\{(.*)\\} *$", "\\1", sumfra)
        gmlcol <- gsub("^ *(.+?) *= *(.+?)\\{(.*)\\} *$", "\\2", sumfra)
        expr <- gsub("^ *(.+?) *= *(.+?)\\{(.*)\\} *$", "\\3", sumfra)
        # cat("nycol:",nycol,"gmlcol:",gmlcol,"expr:",expr,"\n")
        NF <- EkstraherRadSummer(TNF, expr, FGP = FGP, globs = globs)
        gmlcols <- paste(gmlcol, c("", ".f", ".a"), sep = "")
        nycols <- paste(nycol, c("", ".f", ".a"), sep = "")
        data.table::setnames(NF, gmlcols, nycols)
        # print(NF)
        # Sy sammen
        commontabs <- globs$DefDesign$DesignKolsFA[globs$DefDesign$DesignKolsFA %in% names(NF)]

        # Er usikker på om hva som egentlig er best her.
        # Siden OmkodFraPart brukt i EkstraherRadSummer gir full rektulangusering kan man ha satt
        # deler i NF som er udekket i TNF. 1) Disse ønskes vel egentlig ikke med
        # men motsatt, 2) dersom TNF ha manglende GEO-koder som finnes i NF er det kanskje ønskelig å ha disse med
        # Jeg velger å sette venstre join TNF->NF slik at problem 1 faller bort
        # Så lenge herværende prosedyre bare kjøres etter at TNF er rektangularisert mht GEO faller også 2) bort
        # Dette gjelder i standard produskjonsløype (LagTnTabell, LagKUBE etc)

        setkeym(TNF, commontabs)
        setkeym(NF, commontabs)
        dimorg <- dim(TNF)
        TNF <- NF[, c(commontabs, nycols), with = FALSE][TNF]
        cat("LeggTilSumFraRader. Før er dim(TNF)", dimorg, "og dim(NF)", dim(NF), "etter er dim(TNF)", dim(TNF), "\n")
        # altså ikke
        # TNF<-merge(TNF,NF[,c(commontabs,nycols),with=FALSE],all=TRUE,by=commontabs)

        # TNF<-merge(TNF,NF[,c(commontabs,nycols),with=FALSE],all=TRUE,by=commontabs)
        TNF <- SettMergeNAs(TNF, list(gmlcol = FGP$vals, nycol = FGP$vals[gmlcol]))

        # print(TNF)
      } else {
        cat("FEIL!!!!!: NYEKOL_RAD har feil format:", NYdscr, "\n")
      }
    }
  }

  return(TNF)
}


