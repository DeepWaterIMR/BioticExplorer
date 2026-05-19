#' @title Process range text input

processRangeInput <- function(inp, idx) {
  # Sanitize input
  if(is.null(inp))
    return(NULL)
  spl <- trimws(unlist(strsplit(as.character(inp), "[,]")))
  toEval <- grep("^\\d+$|^\\d+:\\d+$", spl, perl = TRUE, value = TRUE)
  gearRange <- try(eval(parse(text = paste0("c(", paste(toEval, collapse = ","), ")"))))
  if(class(gearRange ) != "try-error")
    ret <- intersect(idx, gearRange)
  else
    ret <- NULL
  return(ret)
}

#' @title Update subset selectors

updateSelectors <- function(db = FALSE) {
  
  # Update selectors
  rv$all <- list()
  rv$all$startyear <- rv$mission %>% lazy_dt() %>% select(startyear) %>% distinct() %>% pull() %>% sort()
  rv$all$commonname <- rv$stnall %>% lazy_dt() %>% filter(!is.na(commonname)) %>% select(commonname) %>% distinct() %>% pull() %>% sort()
  rv$all$cruise <- rv$mission %>% lazy_dt() %>% select(cruise) %>% distinct() %>% pull() %>% sort()
  rv$all$platformname <- rv$stnall %>% lazy_dt() %>% select(platformname) %>% distinct() %>% pull() %>% sort()
  rv$all$serialnumber <- rv$stnall %>% lazy_dt() %>% select(serialnumber) %>% distinct() %>% pull() %>% sort()
  rv$all$gear <- rv$stnall %>% lazy_dt() %>% select(gear) %>% distinct() %>% pull() %>% sort()
  
  rv$all$indSpecies <- rv$indall %>% lazy_dt() %>% 
    filter(!is.na(length) & !is.na(individualweight)) %>%
    group_by(commonname) %>% summarize(n = n()) %>% filter(n > 5) %>%
    select(commonname) %>% distinct() %>% pull() %>% sort()
  
  lon <- rv$stnall %>% lazy_dt() %>% filter(!is.na(longitudestart)) %>% summarise(min = suppressWarnings(min(longitudestart)), max = suppressWarnings(max(longitudestart))) %>% collect()
  lat <- rv$stnall %>% lazy_dt() %>% filter(!is.na(latitudestart)) %>% summarise(min = suppressWarnings(min(latitudestart)), max = suppressWarnings(max(latitudestart))) %>% collect()
  
  if(nrow(lon) == 0) {
    rv$all$min.lon <- -180
    rv$all$max.lon <- 180
  } else {
    rv$all$min.lon <- floor(lon$min)
    rv$all$max.lon <- ceiling(lon$max)
  }
  
  if(nrow(lat) == 0) {
    rv$all$min.lat <- -90
    rv$all$max.lat <- 90
  } else {
    rv$all$min.lat <- round_any(lat$min, 0.1, floor)
    rv$all$max.lat <- round_any(lat$max, 0.1, ceiling)
  }
  
  rv$all$date <- rv$stnall %>% lazy_dt() %>% summarise(min = min(stationstartdate, na.rm = TRUE), max = max(stationstartdate, na.rm = TRUE)) %>% collect()
  
  if(db) {
    rv$all$missiontypename <- rv$mission %>% lazy_dt() %>% select(missiontypename) %>% distinct() %>% pull() %>% sort()
    rv$all$gearcategory <- rv$stnall %>% lazy_dt() %>% select(gearcategory) %>% distinct() %>% pull() %>% sort()

    tmpCS <- names(index$cruiseseries)
    names(tmpCS) <- index$cruiseseries
    rv$all$cruiseseries <- unique(unlist(strsplit(rv$mission %>% lazy_dt() %>% select(cruiseseriescode) %>% distinct() %>% pull(), "[,]")))
    rv$all$cruiseseries <- sort(as.integer(rv$all$cruiseseries[rv$all$cruiseseries != "NA"]))
    
    if(length(rv$all$cruiseseries) > 0) {
      names(rv$all$cruiseseries) <- tmpCS[rv$all$cruiseseries]
    }
       
    rv$all$icesarea <- rv$stnall %>% lazy_dt() %>% select(icesarea) %>% distinct() %>% pull() %>% sort()
    rv$all$area <- rv$stnall %>% lazy_dt() %>% select(area) %>% distinct() %>% pull() %>% sort()
  }
  
}

#' @title Update data selection filters
#' @param db Logical. If \code{TRUE}, the operation will be targeted for database data, otherwise file data
#' @param loadDb Logical. Set to \code{TRUE} when database selection is reset or loaded for the first time. Otherwise \code{FALSE}.

updateFilterform <- function(db = FALSE, loadDb = FALSE) {
  
  if(loadDb) {
    updateSelectizeInput(session, "selMissionTypeDb", choices = index$missiontypename, server = TRUE)
    updateSelectizeInput(session, "selCruiseSeriesDb", choices = index$cruiseseries, server = TRUE)
    updateSelectizeInput(session, "selCruiseDb", choices = index$cruise, server = TRUE)
    updateSelectizeInput(session, "selYearDb", choices = index$year, server = TRUE)
    updateSelectizeInput(session, "selSpeciesDb", choices = index$commonname, server = TRUE)
    updateSelectizeInput(session, "selPlatformDb", choices = index$platformname, server = TRUE)
    updateSelectizeInput(session, "selFDIRAreaDb", choices = index$fdirarea, server = TRUE)
    updateSelectizeInput(session, "selICESAreaDb", choices = index$icesarea, server = TRUE)
    updateSelectizeInput(session, "selSerialnumberDb", choices = index$serialnumber, server = TRUE)
    updateSelectizeInput(session, "selGearDb", choices = index$gear, server = TRUE)
    updateSelectizeInput(session, "selGearCategoryDb", choices = index$gearcategory, server = TRUE)
    updateDateRangeInput(session, "selDateDb", start = index$date[[1]], end = index$date[[2]])
    updateSliderInput(session, "selLonDb", min = -180,max = 180, value = c(-180, 180), step = 0.1)
    updateSliderInput(session, "selLatDb", min = -90, max = 90, value = c(-90, 90), step = 0.1)
  } else if(db) {
    updateSelectizeInput(session, "selMissionTypeDb", choices = rv$all$missiontypename, server = TRUE)
    updateSelectizeInput(session, "selCruiseSeriesDb", choices = rv$all$cruiseseries, server = TRUE)
    updateSelectizeInput(session, "selYearDb", choices = rv$all$startyear, server = TRUE)
    updateSelectizeInput(session, "selSpeciesDb", choices = rv$all$commonname, server = TRUE)
    updateSelectizeInput(session, "selCruiseDb", choices = rv$all$cruise, server = TRUE)
    updateSelectizeInput(session, "selPlatformDb", choices = rv$all$platformname, server = TRUE)
    updateSelectizeInput(session, "selSerialnumberDb", choices = rv$all$serialnumber, server = TRUE)
    updateSelectizeInput(session, "selFDIRAreaDb", choices = rv$all$area, server = TRUE)
    updateSelectizeInput(session, "selICESAreaDb", choices = rv$all$icesarea, server = TRUE)
    updateSelectizeInput(session, "selGearDb", choices = rv$all$gear, server = TRUE)
    updateSelectizeInput(session, "selGearCategoryDb", choices = rv$all$gearcategory, server = TRUE)
    updateDateRangeInput(session, "selDateDb", start = rv$all$date[[1]], end = rv$all$date[[2]])
    updateSelectInput(session, "catchMapSpecies", choices = c("All", rv$all$commonname))
    updateSelectInput(session, "catchMapExportSpecies", choices = c("All", rv$all$commonname))
    updateSelectInput(session, "indSpecies", choices = c("Select a species to generate the plots", rv$all$indSpecies))
    updateSliderInput(session, "selLonDb", min = rv$all$min.lon, max = rv$all$max.lon, value = rv$sub$lon, step = 0.1)
    updateSliderInput(session, "selLatDb", min = rv$all$min.lat, max = rv$all$max.lat, value = rv$sub$lat, step = 0.1) 
  } else {
    updateSelectizeInput(session, "subYear", choices = rv$all$startyear, server = TRUE)
    updateSelectizeInput(session, "subSpecies", choices = rv$all$commonname, server = TRUE)
    updateSelectizeInput(session, "subCruise", choices = rv$all$cruise, server = TRUE)
    updateSelectizeInput(session, "subPlatform", choices = rv$all$platformname, server = TRUE)
    updateSelectizeInput(session, "subSerialnumber", choices = rv$all$serialnumber, server = TRUE)
    updateSelectizeInput(session, "subGear", choices = rv$all$gear, server = TRUE)
    updateDateRangeInput(session, "subDate", start = rv$all$date[[1]], end = rv$all$date[[2]])
    updateSelectizeInput(session, "subMissionType", choices = rv$all$missiontypename, server = TRUE)
    updateSelectInput(session, "catchMapSpecies", choices = c("All", rv$all$commonname))
    updateSelectInput(session, "catchMapExportSpecies", choices = c("All", rv$all$commonname))
    updateSelectInput(session, "indSpecies", choices = c("Select a species to generate the plots", rv$all$indSpecies))
    updateSliderInput(session, "subLon", min = rv$all$min.lon, max = rv$all$max.lon, value = rv$sub$lon, step = 0.1)
    updateSliderInput(session, "subLat", min = rv$all$min.lat, max = rv$all$max.lat, value = rv$sub$lat, step = 0.1) 
  }
}

#' @title Generate the station map

updateMap <- function(db = FALSE) {
  
  # Data 
  
  x <- rv$stnall %>% lazy_dt() %>% select(missiontype, startyear, platform, platformname, missionnumber, missionid, serialnumber, latitudestart, longitudestart) %>% 
    filter(!is.na(longitudestart) & !is.na(latitudestart)) %>% distinct() %>% collect()
  
  ## Station map ##
  
  if(db) {
    output$stationMapDb <- renderLeaflet(stationMap(data = x) %>% 
                                           addRectangles(
                                             lng1 = input$selLonDb[1], lat1 = input$selLatDb[1], 
                                             lng2 = input$selLonDb[2], lat2 = input$selLatDb[2],
                                             fillColor = "transparent")
    )
  } else {
    output$stationMap <- renderLeaflet(stationMap(data = x) %>% 
                                         addRectangles(
                                           lng1 = input$subLon[1], lat1 = input$subLat[1], 
                                           lng2 = input$subLon[2], lat2 = input$subLat[2],
                                           fillColor = "transparent")
    )
  }
}

#' @title Populate the quick overview panel and station map
#' @param db Logical. If \code{TRUE}, the operation will be targeted for database data, otherwise file data

obsPopulatePanel <- function(db = FALSE) {
  
  # Functions
  
  nCruisesBox <- function() {
    renderValueBox({
      valueBox(
        rv$mission %>% lazy_dt() %>% count() %>% pull(),
        "Cruises"
      )
    })
  }
  
  nStationsBox <- function() {
    renderValueBox({
      
      tmp <- rv$stnall %>% lazy_dt() %>% select(missionid, startyear, serialnumber) %>% distinct() %>% count() %>% pull()
      
      valueBox(
        value = tags$p(tmp, style = "font-size: 80%;"),
        subtitle = "Stations"
      )
    })
  }
  
  nYearsBox <- function() {
    renderValueBox({
      valueBox(
        length(rv$all$startyear),
        "Unique years"
      )
    })
  }
  
  nSpeciesBox <- function() {
    renderValueBox({
      valueBox(
        length(rv$all$commonname),
        "Unique species"
      )
    })
  }
  
  DateStartBox <- function() {
    renderValueBox({
      valueBox(
        value = tags$p(rv$all$date$min,
                       style = "font-size: 80%;"),
        subtitle = "First date"
      )
    })
  }
  
  DateEndBox <- function() {
    renderValueBox({
      valueBox(
        value = tags$p(rv$all$date$max,
                       style = "font-size: 80%;"),
        subtitle = "Last date"
      )
    })
  }
  
  nMeasuredBox <- function() {
    renderValueBox({
      valueBox(
        value = tags$p(rv$indall %>% lazy_dt() %>% select(length) %>% count() %>% pull(), style = "font-size: 60%;"),
        subtitle = "Measured specimen"
      )
    })
  }
  
  nGearsBox <- function() {
    renderValueBox({
      valueBox(
        length(unique(rv$all$gear)),
        "Gear types"
      )
    })
  }
  
  # Reset all subset
  rv$sub <- list()
  
  # Inform that a subset has never been performed
  # rv$substart <- FALSE
  
  # Update selectors
  updateSelectors(db = db)
  
  # Reset some specific subsets as this is the first run
  rv$sub$lon <- c(rv$all$min.lon, rv$all$max.lon)
  rv$sub$lat <- c(rv$all$min.lat, rv$all$max.lat)
  
  # Reset form too
  updateFilterform(db = db)
  
  # Update stats
  
  if(db) {
    output$nCruisesBoxDb <- nCruisesBox()
  } else {
    output$nCruisesBox <- nCruisesBox()
  }
  
  if(db) {
    output$nStationsBoxDb <- nStationsBox()
  } else {
    output$nStationsBox <- nStationsBox() 
  }
  
  if(db) {
    output$nYearsBoxDb <- nYearsBox()
  } else {
    output$nYearsBox <- nYearsBox()
  }
  
  if(db) {
    output$nSpeciesBoxDb <- nSpeciesBox()
  } else {
    output$nSpeciesBox <- nSpeciesBox()
  }
  
  if(db) {
    output$DateStartBoxDb <- DateStartBox()
  } else {
    output$DateStartBox <- DateStartBox() 
  }
  
  if(db) {
    output$DateEndBoxDb <- DateEndBox()
  } else {
    output$DateEndBox <- DateEndBox()
  }
  
  if(db) {
    output$nMeasuredBoxDb <- nMeasuredBox()
  } else {
    output$nMeasuredBox <- nMeasuredBox()  
  }
  
  if(db) {
    output$nGearsBoxDb <- nGearsBox()
  } else {
    output$nGearsBox <- nGearsBox()
  }
  
  updateMap(db = db)
}

#' @title Make a filter chain for file-based- and database-data loaded into the memory

makeFilterChain <- function(db = FALSE) {
  
  sub <- list()
  filterChain <- list()
  
  ## Year
  
  if(db) {
    sub$year <- processRangeInput(input$selYearDb, index$year)
  } else {
    sub$year <- processRangeInput(input$subYear, rv$all$startyear)
  }
  
  if (!is.null(sub$year)) { 
    filterChain <- append(filterChain, paste0("startyear %in% c('", paste0(sub$year, collapse = "', '"), "')"))
  }
  
  ## Species
  
  if(db) {
    sub$species <- input$selSpeciesDb
  } else {
    sub$species <- input$subSpecies    
  }
  
  if (!is.null(sub$species)) {
    filterChain <- append(filterChain, paste0("commonname %in% c('", paste0(sub$species, collapse = "', '"), "')"))
  }
  
  ## Cruise 
  
  if(db) {
    sub$cruise <- input$selCruiseDb
  } else {
    sub$cruise <- input$subCruise
  }
  
  if (!is.null(sub$cruise)) {
    filterChain <- append(filterChain, paste0("cruise %in% c('", paste0(sub$cruise, collapse = "', '"), "')"))
  }

  ## Cruise Series
  if(db) {
    sub$cruiseseries <- input$selCruiseSeriesDb
  } else {
    sub$cruiseseries <- NULL
  }

  for(xx in seq_len(length(sub$cruiseseries))) {
    yy <- sub$cruiseseries[[xx]]
    filterChain <- append(filterChain, paste0("cruiseseriescode %like% '", yy, ",%' | cruiseseriescode %like% '%,", yy,"' | cruiseseriescode %in% c('", yy,"')"))
  }

  ## Platform
  
  if(db) {
    sub$platform <- input$selPlatformDb
  } else {
    sub$platform <- input$subPlatform 
  }
  
  if (!is.null(sub$platform)) {
    filterChain <- append(filterChain, 
                          paste0("platformname %in% c('", paste0(sub$platform, collapse = "', '"), "')"))
  }
  
  ## Serial number
  
  if(db) {
    sub$serialnumber <- processRangeInput(input$selSerialnumberDb, index$serialnumber)
  } else {
    sub$serialnumber <- processRangeInput(input$subSerialnumber, rv$all$serialnumber)
  }
  
  if (!is.null(sub$serialnumber)) {
    filterChain <- append(filterChain, 
                          paste0("serialnumber %in% c('", paste0(sub$serialnumber, collapse = "', '"), "')"))
  }
  
  ## Gear
  
  if(db) {
    sub$gear <- processRangeInput(input$selGearDb, index$gear)
  } else {
    sub$gear <- processRangeInput(input$subGear, rv$all$gear)
  }
  
  if (!is.null(sub$gear)) {
    filterChain <- append(filterChain, paste0("gear %in% c('", paste0(sub$gear, collapse = "', '"), "')"))
  }
  
  ## Mission type name
  
  if(db) {
    sub$missiontypename <- input$selMissionTypeDb
  } else {
    sub$missiontypename <- NULL
  }
  
  if (!is.null(sub$missiontypename)) {
    filterChain <- append(filterChain, paste0("missiontypename %in% c('", paste0(sub$missiontypename, collapse = "', '"), "')"))
  }
  
  ## Gear category
  
  if(db) {
    sub$gearcategory <- input$selGearCategoryDb
  } else {
    sub$gearcategory <- NULL
  }
  
  if (!is.null(sub$gearcategory)) {
    filterChain <- append(filterChain, paste0("gearcategory %in% c('", paste0(sub$gearcategory, collapse = "', '"), "')"))
  }
  
  ## FDIR area
  
  if(db) {
    sub$area <- input$selFDIRAreaDb
  } else {
    sub$area <- NULL
  }
  
  if (!is.null(sub$area)) {
    filterChain <- append(filterChain, paste0("area %in% c('", paste0(sub$area, collapse = "', '"), "')"))
  }
  
  ## ICES area
  
  if(db) {
    sub$icesarea <- input$selICESAreaDb
  } else {
    sub$icesarea <- NULL
  }
  
  if (!is.null(sub$icesarea)) {
    filterChain <- append(filterChain, paste0("icesarea %in% c('", paste0(sub$icesarea, collapse = "', '"), "')"))
  }
  
  ## Longitude
  
  if(db) {
    sub$lon <- as.numeric(input$selLonDb)
  } else {
    sub$lon <- as.numeric(input$subLon)
  }
 
  if(all(sub$lon == c(-180, 180))) sub$lon <- NULL
  
  if (!is.null(sub$lon)) {
    if(sub$lon[1] > rv$all$min.lon) {
      filterChain <- append(filterChain, paste0("longitudestart > ",sub$lon[1]))
    }
    if(sub$lon[2] < rv$all$max.lon) {
      filterChain <- append(filterChain, paste0("longitudestart < ",sub$lon[2]))
    }
  }
  
  ## Latitude
  
  if(db) {
    sub$lat <- as.numeric(input$selLatDb)
  } else {
    sub$lat <- as.numeric(input$subLat)
  }
  
  if(all(sub$lat == c(-90, 90))) sub$lat <- NULL
  
  if (!is.null(sub$lat)) {
    if(sub$lat[1] > rv$all$min.lat) {
      filterChain <- append(filterChain, paste0("latitudestart > ",sub$lat[1]))
    }
    if(sub$lat[2] < rv$all$max.lat) {
      filterChain <- append(filterChain, paste0("latitudestart < ",sub$lat[2]))
    }
  }
  
  # if (!identical(as.numeric(input$subLon), c(rv$all$min.lon, rv$all$max.lon))) {
  #   sub$lon <- as.numeric(input$subLon)
  #   filterChain <- append(filterChain, paste0("longitudestart >= '", input$subLon[1], "' & longitudestart <= '", input$subLon[2], "'"))
  # } else {
  #   sub$lon <- NULL
  # }
  
  # if (!identical(as.numeric(input$subLat), c(rv$all$min.lat, rv$all$max.lat))) {
  #   sub$lat <- as.numeric(input$subLat)
  #   filterChain <- append(filterChain, paste0("latitudestart >= '", input$subLat[1], "' & latitudestart <= '", input$subLat[2], "'"))
  # } else {
  #   sub$lat <- NULL
  # }

  # print("We have filter:") 
  # print(filterChain)
 
  list(filterChain = filterChain, sub = sub)
}

