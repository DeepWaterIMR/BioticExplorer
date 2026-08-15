#' @title Read and process a NMD Biotic xml file for further use in the BioticExplorer
#' @description A wrapper for \code{\link[RstoxData]{readXmlFile}} to enable further use in the BioticExplorer
#' @param file character string specifying the file path to the xml file. Accepts only one file at the time.
#' @param removeEmpty logical indicating whether empty columns should be removed from the output.
#' @param convertColumns logical indicating whether the column types should be converted. See \code{link{convertColumnTypes}}. Setting this to \code{FALSE} considerably speeds up the function, but leads to problems with non-unicode characters.
#' @param returnOriginal logical indicating whether the original data (\code{$mission} through \code{$agedetermination}) should be returned together with combined data.
#' @param missionidPrefix A prefix for the \code{missionid} identifier, which separates cruises. Used in \code{\link{processBioticFiles}} function when several xml files are put together. \code{NULL} (default) omits the prefix. Not needed in \code{processBioticFile} function.
#' @param icesAreas optional \code{sf} polygon object with ICES areas. When provided, each station is spatially joined to the ICES area grid. Pass \code{BioticExplorerServer::icesAreas} if the package is installed. Requires the \pkg{sf} package.
#' @param gearCodes optional data.table of gear codes with columns \code{code} and \code{category} (and optionally \code{description}). When provided, a \code{gearcategory} column is added to \code{$stnall}. Pass \code{BioticExplorerServer::gearList} if the package is installed.
#' @return Returns a list of Biotic data with \code{$mission}, \code{$stnall} and \code{$indall} data tables. The \code{$stnall} and \code{$indall} are merged from \code{$fishstation} and \code{$catchsample} (former) and \code{$fishstation}, \code{$catchsample}, \code{$individual} and \code{$agedetermination} (latter). Stations with no catch have \code{NA} in all catch-related columns including \code{commonname}.
#' @author Mikko Vihtakari, Ibrahim Umar (Institute of Marine Research)
#' @import RstoxData data.table
#' @export

# Debugging parameters
# removeEmpty = TRUE; convertColumns = TRUE; returnOriginal = FALSE; missionidPrefix = NULL; icesAreas = NULL; gearCodes = NULL
processBioticFile <- function(file, removeEmpty = TRUE, convertColumns = TRUE, returnOriginal = FALSE, missionidPrefix = NULL, icesAreas = NULL, gearCodes = NULL) {

  ## Checks

  if(!file.exists(file)) stop("file does not exist. Check your file path.")

  ## Read the Biotic file ----

  dt <- RstoxData::readXmlFile(file)

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

  ### Fix FDIR area code

  stn[, area := as.integer(area)]

  ### Optional: add ICES area via spatial join

  if (!is.null(icesAreas)) {
    s2_mode <- sf::sf_use_s2()
    suppressMessages(sf::sf_use_s2(FALSE))
    on.exit({suppressMessages(sf::sf_use_s2(s2_mode))})

    points <- stn[, c("longitudestart", "latitudestart")]
    points[is.na(longitudestart) | is.na(latitudestart), `:=`(longitudestart = 0, latitudestart = 0)]

    if (nrow(points) > 0) {
      points <- sf::st_as_sf(points, coords = c(1, 2), crs = 4326)
      suppressWarnings(sf::st_crs(icesAreas) <- 4326)
      points <- sf::st_transform(points, sf::st_crs(icesAreas))
      stn[, icesarea := icesAreas[as.integer(suppressMessages(sf::st_intersects(points, icesAreas))),]$Area_Full]
    } else {
      stn[, icesarea := as.character(NA)]
    }
  }

  ### Optional: add gear category

  if (!is.null(gearCodes)) {
    stn <- merge(stn, gearCodes[, !names(gearCodes) %in% c("description"), with = FALSE], by.x = "gear", by.y = "code", all.x = TRUE, sort = FALSE)
  }

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

  ## Compiled datasets ----

  coredat <- merge(msn[, !names(msn) %in% c("purpose"), with = FALSE], stn, by = names(msn)[names(msn) %in% names(stn)], all = TRUE)

  # Stndat: left join so stations with no catch are retained (commonname = NA)

  stndat <- merge(coredat, cth, all.x = TRUE, by = c("missiontype", "missionnumber", "startyear", "platform", "serialnumber"))

  # Inddat: all.y keeps only rows with individual measurements; empty stations are not included

  inddat <- merge(stndat[, !names(stndat) %in% c("purpose", "stationcomment", "catchcomment"), with = FALSE], ind, all.y = TRUE, by = names(stndat)[names(stndat) %in% names(ind)])

  inddat[is.na(preferredagereading), preferredagereading := 1]
  inddat <- merge(inddat, age, by.x = c(intersect(names(inddat), names(age)), "preferredagereading"), by.y = c(intersect(names(inddat), names(age)), "agedeterminationid"), all.x = TRUE)

  if(sum(is.na(inddat$commonname)) > 0) stop(paste(sum(is.na(inddat$commonname)), "missing commonname records. This is likely due to merging error between individual and agedetermination data tables. File a bug report."))

  ## Return ----

  ### Format

  if (returnOriginal) {
    out <- list(mission = msn, fishstation = stn, catchsample = cth, individual = ind, agedetermination = age, stnall = stndat, indall = inddat)
  } else {
    out <- list(mission = msn, stnall = stndat, indall = inddat)
  }

  ### Preserve optional fields required by the explorer

  out$stnall <- ensureStationDepthColumns(out$stnall)
  out$indall <- ensureStationDepthColumns(out$indall)

  ### Remove empty columns to save space

  if (removeEmpty) {
    out <- lapply(out, function(k) {
      keep <- unlist(lapply(k, function(x) !all(is.na(x)))) |
        names(k) %in% c("bottomdepthstart", "fishingdepthmin")
      k[, which(keep), with = FALSE]
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
#' @param icesAreas optional \code{sf} polygon object passed through to \code{\link{processBioticFile}}. See that function for details.
#' @param gearCodes optional data.table of gear codes passed through to \code{\link{processBioticFile}}. See that function for details.
#' @return Returns a list of Biotic data with \code{$mission}, \code{$stnall} and \code{$indall} data tables. The \code{$stnall} and \code{$indall} are merged from \code{$fishstation} and \code{$catchsample} (former) and \code{$fishstation}, \code{$catchsample}, \code{$individual} and \code{$agedetermination} (latter). Stations with no catch have \code{NA} in all catch-related columns including \code{commonname}.
#' @author Mikko Vihtakari (Institute of Marine Research)
#' @import RstoxData data.table
#' @export

# Debugging parameters
# removeEmpty = TRUE; convertColumns = TRUE; returnOriginal = FALSE; icesAreas = NULL; gearCodes = NULL
processBioticFiles <- function(files, removeEmpty = TRUE, convertColumns = TRUE, returnOriginal = FALSE, icesAreas = NULL, gearCodes = NULL) {

  # Read xml files

  out <- lapply(seq_along(files), function(i, returnOriginal. = returnOriginal, convertColumns. = convertColumns) {
    print(paste("i =", i, "file = ", files[i]))
    print(paste(round(100*i/length(files), 0), "%"))
    processBioticFile(files[i], removeEmpty = FALSE, returnOriginal = returnOriginal., convertColumns = convertColumns., missionidPrefix = i, icesAreas = icesAreas, gearCodes = gearCodes)
  })

  # Combine

  out <- do.call(Map, c(f = rbind, out, fill = TRUE))
  
  ### Preserve optional fields required by the explorer

  out$stnall <- ensureStationDepthColumns(out$stnall)
  out$indall <- ensureStationDepthColumns(out$indall)

  ### Remove empty columns to save space
  
  if (removeEmpty) {
    out <- lapply(out, function(k) {
      keep <- unlist(lapply(k, function(x) !all(is.na(x)))) |
        names(k) %in% c("bottomdepthstart", "fishingdepthmin")
      k[, which(keep), with = FALSE]
    })
  }
  
  # Define class
  
  class(out) <- "bioticProcData"
  
  # return
  
  out
  
}

#' @title Ensure optional station-depth fields are available
#' @description Adds empty station-depth columns when they are absent from an NMD Biotic file.
#' @param data data.table containing station data.
#' @return The input data.table with \code{bottomdepthstart} and \code{fishingdepthmin} columns.
#' @keywords internal

ensureStationDepthColumns <- function(data) {

  depthColumns <- c("bottomdepthstart", "fishingdepthmin")
  missingColumns <- setdiff(depthColumns, names(data))

  if (length(missingColumns) > 0) {
    data[, (missingColumns) := NA_real_]
  }

  data
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
  if(!is.null(x$fishstation)) cat(paste0("$fishstation: ", nrow(x$fishstation), " rows and ", ncol(x$fishstation), " columns"), sep = "\n")
  if(!is.null(x$catchsample)) cat(paste0("$catchsample: ", nrow(x$catchsample), " rows and ", ncol(x$catchsample), " columns"), sep = "\n")
  if(!is.null(x$individual)) cat(paste0("$individual: ", nrow(x$individual), " rows and ", ncol(x$individual), " columns"), sep = "\n")
  if(!is.null(x$agedetermination)) cat(paste0("$agedetermination: ", nrow(x$agedetermination), " rows and ", ncol(x$agedetermination), " columns"), sep = "\n")
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
  cat(sort(unique(x$stnall$commonname[!is.na(x$stnall$commonname)])), sep = ", ")
  cat(NULL, sep = "\n")
  cat(NULL, sep = "\n")
  
}
