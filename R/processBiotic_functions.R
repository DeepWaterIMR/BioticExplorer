#' @title Read and process a NMD Biotic xml file for further use in the BioticExplorer
#' @description A wrapper for \code{\link[RstoxData]{readXmlFile}} to enable further use in the BioticExplorer
#' @param file character string specifying the file path to the xml file. Accepts only one file at the time.
#' @param removeEmpty logical indicating whether empty columns should be removed from the output. 
#' @param convertColumns logical indicating whether the column types should be converted. See \code{link{convertColumnTypes}}. Setting this to \code{FALSE} considerably speeds up the function, but leads to problems with non-unicode characters.
#' @param returnOriginal logical indicating whether the original data (\code{$mission} through \code{$agedetermination}) should be returned together with combined data.
#' @param missionidPrefix A prefix for the \code{missionid} identifier, which separates cruises. Used in \code{\link{processBioticFiles}} function when several xml files are put together. \code{NULL} (default) omits the prefix. Not needed in \code{processBioticFile} function.
#' @details This function should be identical to the BioticExplorerServer::bioticToDatabase function. Included here to make avoid adding a dependency. Copy that function here when it is changed.
#' @return Returns a list of Biotic data with \code{$mission}, \code{$stnall} and \code{$indall} data tables. The \code{$stnall} and \code{$indall} are merged from \code{$fishstation} and \code{$catchsample} (former) and  \code{$fishstation}, \code{$catchsample}, \code{$individual} and \code{$agedetermination} (latter). 
#' @author Mikko Vihtakari, Ibrahim Umar (Institute of Marine Research) 
#' @import RstoxData data.table
#' @export

# Debugging parameters
# removeEmpty = TRUE; convertColumns = TRUE; returnOriginal = FALSE; missionidPrefix = NULL
processBioticFile <- function(file, removeEmpty = TRUE, convertColumns = TRUE, returnOriginal = FALSE, missionidPrefix = NULL) {
  
  ## Checks
  
  if(!file.exists(file)) stop("file does not exist. Check your file path.")
  
  ## Read the Biotic file ----
  
  dt <- RstoxData::readXmlFile(file)
  
  ## Modification for Dr.F.Nansen xmls
  
  if(dt$mission$callsignal== "LDLG" & dt$mission$platformname=="Dr.Fridtjof Nansen"){
    if("fishingdepthstart" %in% colnames(dt$fishstation)){
      if(!anyNA(dt$fishstation$fishingdepthstart)){
        dt$fishstation$fishingdepthmin <- dt$fishstation$fishingdepthstart
      }
    }
    
    dt$individual$maturationstage <- ifelse(as.numeric(dt$individual$maturationstage)>=100,
                                            as.character(as.numeric(dt$individual$maturationstage)-100),
                                            dt$individual$maturationstage)
  }
  
  
  ## Mission data ---
  
  msn <- dt$mission
  
  if (convertColumns) {
    date.cols <- grep("date", names(msn), value = TRUE)
    msn[, eval(date.cols) := lapply(.SD, as.Date), .SDcols = eval(date.cols)]
  }
  
  if (is.null(missionidPrefix)) {
    msn$missionid <- rownames(msn)
  } else {
    msn$missionid <- paste(missionidPrefix, rownames(msn), sep = "_")
  }
  
  ## Station data ---
  
  stn <- dt$fishstation
  
  stn[is.na(stationstarttime), stationstarttime := "00:00:00.000Z"]
  stn[is.na(stationstoptime), stationstoptime := "00:00:00.000Z"]
  
  stn[, stationstartdate := as.POSIXct(paste(stn$stationstartdate, stn$stationstarttime), format = "%Y-%m-%dZ %H:%M:%S", tz = "GMT")]
  stn[, stationstopdate := as.POSIXct(paste(stn$stationstopdate, stn$stationstoptime), format = "%Y-%m-%dZ %H:%M:%S", tz = "GMT")]
  
  stn[, stationstarttime := NULL]
  stn[, stationstoptime := NULL]
  
  # if (convertColumns) { # This fixed the time issue. Left here in case there are unforseen consequences.
    # date.cols <- grep("date", names(stn), value = TRUE)
    # stn[, eval(date.cols) := lapply(.SD, as.Date), .SDcols = eval(date.cols)]
  # }
  
  ##________________
  ## Sample data ---
  
  cth <- dt$catchsample
  
  ##____________________
  ## Individual data ---
  
  ind <- dt$individual
  
  ## Age data ---
  
  age <- dt$agedetermination
  
  if (convertColumns) {
    date.cols <- grep("date", names(age), value = TRUE)
    age[, eval(date.cols) := lapply(.SD, as.Date), .SDcols = eval(date.cols)]
  }
  
  # if (nrow(age) == 0) {
  #   age <- rapply(age, as.integer, how = "replace")
  # }
  
  ## Compiled datasets ----
  
  coredat <- merge(msn[,!names(msn) %in% c("purpose"), with = FALSE], stn, by = names(msn)[names(msn) %in% names(stn)], all = TRUE)
  
  # Stndat
  
  stndat <- merge(coredat, cth, all.y = TRUE, by = c("missiontype", "missionnumber", "startyear", "platform", "serialnumber"))
  
  # Inddat
  
  inddat <- merge(stndat[,!names(stndat) %in% c("purpose", "stationcomment", "catchcomment"), with = FALSE], ind, all.y = TRUE, by = names(stndat)[names(stndat) %in% names(ind)])
  
  inddat[is.na(preferredagereading), preferredagereading := 1]
  inddat <- merge(inddat, age, by.x=c(intersect(names(inddat), names(age)), "preferredagereading"), by.y= c(intersect(names(inddat), names(age)), "agedeterminationid"), all.x = TRUE)
  
  if(sum(is.na(inddat$commonname)) > 0) stop(paste(sum(is.na(inddat$commonname)), "missing commonname records. This is likely due to merging error between individual and agedetermination data tables. File a bug report."))
  
  ## Return ----
  
  ### Format
  
  if (returnOriginal) {
    out <- list(mission = msn, fishstation = stn, catchsample = cth, individual = ind, agedetermination = age, stnall = stndat, indall = inddat)
  } else {
    out <- list(mission = msn, stnall = stndat, indall = inddat)
  }
  
  ### Remove empty columns to save space
  
  if (removeEmpty) {
    out <- lapply(out, function(k) {
      k[, which(unlist(lapply(k, function(x) !all(is.na(x))))), with = FALSE]
    })
  }
  
  ### Class
  
  class(out) <- "bioticProcData"
  
  ### Return
  
  out
  
}

#' @title Read and process NMD Biotic xml files for further use in the BioticExplorer
#' @description A wrapper for \code{\link{processBioticFile}} allowing processing multiple files simultaneously
#' @param files character string specifying the file path to the xml file. Accepts only one file at the time.
#' @param removeEmpty logical indicating whether empty columns should be removed from the output. 
#' @param convertColumns logical indicating whether the column types should be converted. See \code{link{convertColumnTypes}}. Setting this to \code{FALSE} considerably speeds up the function, but leads to problems with non-unicode characters.
#' @param returnOriginal logical indicating whether the original data (\code{$mission} through \code{$agedetermination}) should be returned together with combined data.
#' @return Returns a list of Biotic data with \code{$mission}, \code{$stnall} and \code{$indall} data tables. The \code{$stnall} and \code{$indall} are merged from \code{$fishstation} and \code{$catchsample} (former) and  \code{$fishstation}, \code{$catchsample}, \code{$individual} and \code{$agedetermination} (latter). 
#' @author Mikko Vihtakari (Institute of Marine Research) 
#' @import RstoxData data.table
#' @export

# Debugging parameters
# removeEmpty = TRUE; convertColumns = TRUE; returnOriginal = FALSE
processBioticFiles <- function(files, removeEmpty = TRUE, convertColumns = TRUE, returnOriginal = FALSE) {
  
  # Read xml files
  
  out <- lapply(seq_along(files), function(i, returnOriginal. = returnOriginal, convertColumns. = convertColumns) {
    print(paste("i =", i, "file = ", files[i]))
    print(paste(round(100*i/length(files), 0), "%"))
    processBioticFile(files[i], removeEmpty = FALSE, returnOriginal = returnOriginal., convertColumns = convertColumns., missionidPrefix = i)
  })
  
  
  # Combine
  
  out <- do.call(Map, c(f = rbind, out))
  
  ### Remove empty columns to save space
  
  if (removeEmpty) {
    out <- lapply(out, function(k) {
      k[, which(unlist(lapply(k, function(x) !all(is.na(x))))), with = FALSE]
    })
  }
  
  # Define class
  
  class(out) <- "bioticProcData"
  
  # return
  
  out
  
}

## Core data columns list ----

#' @title List of core data columns by data type in NMD Biotic data
#' @description List of core data types used in the \code{\link{processBioticFile}} function
#' @param type character string specifying the data type. Alternatives: "mission", "fishstation", "individual", "catchsample", or "agedetermination".
#' @return Returns a character vector of core data types for a given data type.
#' @author Mikko Vihtakari (Institute of Marine Research)
#' @keywords internal
#' @export

coreDataList <- function(type) {
  switch(type,
         mission = c(c("missiontype", "startyear", "platform", "missionnumber", "missiontypename", "callsignal", "platformname", "cruise", "missionstartdate", "missionstopdate", "purpose")),
         fishstation = c("missiontype", "startyear", "platform", "missionnumber", "serialnumber", "station", "stationstartdate", "stationstarttime", "longitudestart", "latitudestart", "bottomdepthstart", "fishingdepthmin", "gear", "distance"),
         individual = c("missiontype", "startyear", "platform", "missionnumber", "serialnumber", "catchsampleid", "specimenid", "sex", "maturationstage", "specialstage", "length", "individualweight"),
         catchsample = c("missiontype", "startyear", "platform", "missionnumber", "serialnumber", "catchsampleid", "commonname", "catchcategory", "catchpartnumber", "catchweight", "catchcount", "lengthsampleweight", "lengthsamplecount"),
         agedetermination = c("missiontype", "startyear", "platform", "missionnumber", "serialnumber", "catchsampleid", "specimenid", "age", "readability"),
         stop("Undefined type argument"))
}

## Convert column types ----

#' @title Converts column types in a data frame to (hopefully) correct types
#' @description Converts column types in a data frame to (hopefully) correct types
#' @param df a data.table
#' @return Returns a data.table with corrected column types. Also corrects misinterpreted Norwegian letters and dates.
#' @import data.table
#' @author Mikko Vihtakari (Institute of Marine Research)
#' @export

convertColumnTypes <- function(df) {
  
  ## Conversion function
  
  convertFun <- function(k) {
    
    if (all(is.na(k))) { # no conversion if all NA
      k
    } else if (any(grepl("POSIX", class(k)))) { # no conversion if k is already time class
      k
    } else if (all(!unlist(tryCatchWE(as.Date(k))))) { # k is a date
      as.Date(k)
    } else if (tryCatchWE(as.numeric(k))$warning) { # k is a character
      trimws(k)
    } else if (all(stats::na.omit(as.numeric(k) == as.integer(k)))) { # k is an integer
      as.integer(k)
    } else if (all(!unlist(tryCatchWE(as.numeric(k))))) { # k is numeric
      as.numeric(k)
    } else {
      stop("column type conversion failed.")
    }
  }
  
  ## Conversion
  
  df[, lapply(.SD, convertFun)]
  
}



## Warning/Error catcher ----

#' @title tryCatch both warnings (with value) and errors
#' @param expr an \R expression to evaluate
#' @return List of logicals indicating whether the \code{expr} produces a warning or error.
#' @author Martin Maechler; Copyright (C) 2010-2012 The R Core Team, Mikko Vihtakari (Institute of Marine Research)
#' @export

tryCatchWE <- function(expr) {
  W <- NULL
  
  w.handler <- function(w){ # warning handler
    W <<- w
    invokeRestart("muffleWarning")
  }
  
  er <- withCallingHandlers(tryCatch(expr, error = function(e) e), warning = w.handler)
  
  list(error = "error" %in% class(er), warning = !is.null(W))
  
}


# Print method for bioticProcData ----

#' @title Print processed NMD Biotic data (\code{bioticProcData}) objects
#' @description \code{\link{print}} function for \code{\link[=processBioticFile]{bioticProcData}} objects
#' @param x \code{bioticProcData} object to be printed.
#' @param ... further arguments passed to \code{\link{print}}.
#' @method print bioticProcData
#' @author Mikko Vihtakari
#' @seealso \code{\link{processBioticFile}} \code{\link{processBioticFiles}}
#' @export

print.bioticProcData <- function(x, ...) {
  
  cat("Processed Biotic Data object")
  cat(paste(" of class", class(x)), sep = "\n")
  cat(NULL, sep = "\n")
  cat("A list of data containing following elements:", sep = "\n")
  cat(NULL, sep = "\n")
  cat(paste0("$mission: ", nrow(x$mission), " rows and ", ncol(x$mission), " columns"), sep = "\n")
  cat(paste0("$fishstation: ", nrow(x$fishstation), " rows and ", ncol(x$fishstation), " columns"), sep = "\n")
  cat(paste0("$catchsample: ", nrow(x$catchsample), " rows and ", ncol(x$catchsample), " columns"), sep = "\n")
  cat(paste0("$individual: ", nrow(x$individual), " rows and ", ncol(x$individual), " columns"), sep = "\n")
  cat(paste0("$agedetermination: ", nrow(x$agedetermination), " rows and ", ncol(x$agedetermination), " columns"), sep = "\n")
  cat(paste0("$stnall: ", nrow(x$stnall), " rows and ", ncol(x$stnall), " columns"), sep = "\n")
  cat(paste0("$indall: ", nrow(x$indall), " rows and ", ncol(x$indall), " columns"), sep = "\n")
  cat(NULL, sep = "\n")
  cat("Object size: ", sep = "")
  print(utils::object.size(x), unit = "auto")
  cat("Years: ", sep = "")
  cat(unique(x$mission$startyear), sep = ", ")
  cat(NULL, sep = "\n")
  cat(paste0(length(unique(x$mission$cruise)), " cruises, ", length(unique(paste(x$stnall$startyear, x$stnall$serialnumber))), " separate stations and ", nrow(x$indall), " measured fish."), sep = "\n")
  cat(NULL, sep = "\n")
  cat(paste0("Geographic range: ", round(min(x$stnall$longitudestart, na.rm = TRUE), 1), "-", round(max(x$stnall$longitudestart, na.rm = TRUE), 1), " degrees longitude and ", round(min(x$stnall$latitudestart, na.rm = TRUE), 1), "-", round(max(x$stnall$latitudestart, na.rm = TRUE), 1), " latitude."), sep = "\n")
  cat("Number of missing station coordinates: ", sep = "")
  cat(sum(is.na(x$stnall$longitudestart) | is.na(x$stnall$latitudestart)))
  cat(NULL, sep = "\n")
  cat("Unique species: ", sep = "")
  cat(sort(unique(x$stnall$commonname)), sep = ", ")
  cat(NULL, sep = "\n")
  cat(NULL, sep = "\n")
  
}
