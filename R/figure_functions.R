#' @title Station overview map using leaflet
#' @param data data required by the map. See \code{updateMap}

stationMap <- function(data) {
  
  if(nrow(data) == 0) {
    leaflet::leaflet() %>% 
      addTiles(urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/Ocean/World_Ocean_Base/MapServer/tile/{z}/{y}/{x}",
               attribution = "Tiles &copy; Esri &mdash; Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri") %>% 
      addMarkers(lng = 20, lat = 70, label = "No position information")
  } else {
    leaflet::leaflet(data) %>% 
      addTiles(urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/Ocean/World_Ocean_Base/MapServer/tile/{z}/{y}/{x}",
               attribution = "Tiles &copy; Esri &mdash; Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri") %>% 
      addCircleMarkers(lat = ~ latitudestart, lng = ~ longitudestart, 
                       weight = 1, radius = 2, 
                       popup = ~as.character(platformname), 
                       label = ~as.character(serialnumber), 
                       color = "red", fillOpacity = 0.5,
                       clusterOptions = markerClusterOptions(disableClusteringAtZoom = 10, spiderfyOnMaxZoom = FALSE)
      )
  }
}

#' @title Generate data for species overview plots
#' @description Generates data required by species overview plots in Biotic Explorer
#' @param data stnall data.table from \link[=processBioticFile]{bioticProcData} class. Typically \code{rv$stnall}.
#' @return Returns a list of tibbles containing data required by station-based various plots.
#' @import dplyr data.table

speciesOverviewData <- function(data) {
  
  # Number of stations
  
  nStn <- data %>% lazy_dt() %>% dplyr::group_by(commonname) %>% dplyr::summarise(n = length(unique(paste(startyear, serialnumber)))) %>% arrange(-n) %>% collect()
  nStn$commonname <- factor(nStn$commonname, nStn$commonname)
  
  # Catch weight
  
  catchW <- data[!is.na(data$catchweight),]
  
  if(nrow(catchW) > 0) {
    catchS <- catchW %>% lazy_dt() %>% dplyr::group_by(commonname) %>% 
      dplyr::summarise(mean = mean(catchweight, na.rm = TRUE), se = se(catchweight), max = max(catchweight, na.rm = TRUE), min = min(catchweight, na.rm = TRUE), sum = sum(catchweight, na.rm = TRUE)) %>% 
      arrange(-sum) %>% collect()
    catchS$commonname <- factor(catchS$commonname, catchS$commonname)
    catchS$se[is.na(catchS$se)] <- 0
    catchW$commonname <- factor(catchW$commonname, catchS$commonname)
  } else {
    catchS <- tibble(commonname = "chr", mean = 1, se = 1, max = 1, min = 1, sum = 1, .rows = 0)
  }
  
  # Mean weight
  
  meanW <- data %>% lazy_dt() %>% filter(!is.na(catchweight) & catchweight > 0 & !is.na(catchcount) & catchcount > 0) %>% 
    group_by(commonname, cruise, startyear, serialnumber) %>% summarise(weight = sum(catchweight), n = sum(catchcount)) %>% 
    mutate(indw = weight/n) %>% collect()
  
  meanW <- droplevels(meanW)
  meanW <- meanW %>% group_by(commonname) %>% summarise(mean = mean(indw), min = min(indw), max = max(indw), sd = sd(indw), se = se(indw)) %>% arrange(-mean)
  meanW$commonname <- factor(meanW$commonname, meanW$commonname)
  
  # Numbers in catch
  
  catchN <- data[!is.na(data$catchcount) & data$catchcount > 0,]
  
  meanN <- catchN %>% lazy_dt() %>% group_by(commonname) %>% 
    summarise(mean = mean(catchcount), se = se(catchcount), max = max(catchcount), min = min(catchcount), Nstn = length(unique(paste(cruise, startyear, serialnumber)))) %>% 
    arrange(-mean) %>% collect()
  meanN$se[is.na(meanN$se)] <- 0
  meanN$commonname <- factor(meanN$commonname, meanN$commonname)
  catchN$commonname <- factor(catchN$commonname, meanN$commonname)
  
  # Catch by gear
  
  catchGBase <- data[!is.na(data$catchweight),]
  
  if("gearcategory" %in% colnames(catchGBase)) {
    catchG <- catchGBase %>% lazy_dt() %>% group_by(gearcategory, commonname) %>% 
      summarise(sum = sum(catchweight)) %>% collect() %>% rename(gear = gearcategory)
  } else {
    catchG <- catchGBase %>% lazy_dt() %>% group_by(gear, commonname) %>% 
      summarise(sum = sum(catchweight)) %>% collect()
  }
  
  catchG$commonname <- factor(catchG$commonname, catchS$commonname)
  
  # Bottom depth and fishing depth by station
  
  stnD <- data %>% lazy_dt() %>% group_by(cruise, startyear, serialnumber) %>% 
    summarise(bdepth = unique(bottomdepthstart), fdepth = unique(fishingdepthmin)) %>%
    collect()
  
  stnD <- data.table::melt(data.table::as.data.table(stnD), id.vars = 1:3)
  stnD$variable <- dplyr::recode_factor(stnD$variable, "bdepth" = "Bottom depth (start)", "fdepth" = "Minimum fishing depth")
  
  # catch composition data
  
  compDat <- data %>% lazy_dt() %>% filter(!is.na(catchweight)) %>% 
    group_by(cruise, startyear, serialnumber, longitudestart, latitudestart, fishingdepthmin, commonname) %>%
    summarise(catchweight = sum(catchweight)) %>% collect()
  
  sumCompDat <- compDat %>% group_by(commonname) %>% summarise(sum = sum(catchweight)) %>% arrange(-sum)
  
  compDat$commonname <- factor(compDat$commonname, sumCompDat$commonname)
  
  if (length(sumCompDat$commonname) > 6) {
    levels(compDat$commonname)[!levels(compDat$commonname) %in% sumCompDat$commonname[1:6]] <- "Andre arter"
  }
  
  levels(compDat$commonname) <- gsub("(^[[:alpha:]])", "\\U\\1", levels(compDat$commonname), perl = TRUE)    
  
  compDat <- compDat %>% 
    group_by(cruise, startyear, serialnumber, longitudestart, latitudestart, fishingdepthmin, commonname, .drop = FALSE) %>% 
    summarise(catchweight = sum(catchweight)) %>% 
    arrange(cruise, startyear, serialnumber, commonname)
  
  compDatW <- tidyr::spread(compDat, commonname, catchweight)
  
  compDatW$total <- rowSums(compDatW[,levels(compDat$commonname)]) 
  
  # Return
  
  list(nStn = nStn, catchW = catchW, catchS = catchS, meanW = meanW, catchN = catchN, meanN = meanN, catchG = catchG, stnD = stnD, compDat = compDat, compDatW = compDatW)
}

#' @title Plot Number of stations containing a species
#' @description Plots a species composition in an \link[=processBioticFile]{bioticProcData} object. 
#' @param data data object from \link{speciesOverviewData}. Requires the nStn data frame.
#' @param base_size base size parameter for ggplot. See \link[ggplot2]{theme_bw}.
#' @return Returns a ggplot object
#' @import ggplot2

speciesCompositionPlot <- function(data, base_size = 14) {
  
  x <- data$nStn
  
  ggplot(x, aes(y = n, x = commonname)) + 
    geom_col() +
    ylab("Number of stations containing the species") +
    xlab("Species database name") +
    coord_cartesian(expand = FALSE, ylim = range(pretty(x$n))) + 
    theme_bw(base_size = base_size) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  
}

#' @title Plot summed catch weight
#' @description Plots summed catch weights in an \link[=processBioticFile]{bioticProcData} object. 
#' @param data data object from \link{speciesOverviewData}. Requires the catchS data frame.
#' @param base_size base size parameter for ggplot. See \link[ggplot2]{theme_bw}.
#' @return Returns a ggplot object
#' @import ggplot2

catchweightSumPlot  <- function(data, base_size = 12) {
  
  x <- data$catchS
  
  if(nrow(x) == 0) {
    ggplot(x, aes(x = commonname, y = sum)) +
      annotate("text", x = 1, y = 1, label = "No weight data") +
      ylab("Summed catch weight [log10(kg)]") +
      xlab("Species database name") +
      theme_bw(base_size = base_size) +
      theme(axis.text = element_blank())
  } else {
    ggplot(x, aes(x = commonname, y = sum)) +
      geom_col() +
      scale_y_log10("Summed catch weight [log10(kg)]") +
      xlab("Species database name") +
      coord_cartesian() +
      theme_bw(base_size = base_size) +
      annotate("text", x = Inf, y = Inf, label = paste("Total catch\n all species\n", round(sum(x$sum), 0), "kg"), vjust = 1, hjust = 1, size = 5) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  }
}

## 

#' @title Plot mean catch weight
#' @description Plots mean catch weights in an \link[=processBioticFile]{bioticProcData} object. Error bars are standard error of the mean.
#' @param data data object from \link{speciesOverviewData}. Requires the catchS data frame.
#' @param base_size base size parameter for ggplot. See \link[ggplot2]{theme_bw}.
#' @return Returns a ggplot object
#' @import ggplot2

catchweightMeanPlot  <- function(data, base_size = 14) {
  
  x <- data$catchS
  
  if(nrow(x) == 0) {
    ggplot(x, aes(x = commonname, y = mean)) +
      annotate("text", x = 1, y = 1, label = "No weight data") +
      ylab("Mean catch weight (kg; +/- SE)") +
      xlab("Species database name") +
      theme_bw(base_size = base_size) +
      theme(axis.text = element_blank())
  } else {
    ggplot(x, aes(x = commonname, y = mean, ymax = mean + se, ymin = mean - se)) +
      geom_linerange() +
      geom_point() +
      ylab("Mean catch weight (kg; +/- SE)") +
      xlab("Species database name") +
      coord_cartesian() +
      theme_bw(base_size = base_size) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  }
}

#' @title Plot range of catch weights
#' @description Plots the range of catch weights in an \link[=processBioticFile]{bioticProcData} object.
#' @param data data object from \link{speciesOverviewData}. Requires the catchS and catchW data frames.
#' @param base_size base size parameter for ggplot. See \link[ggplot2]{theme_bw}.
#' @return Returns a ggplot object
#' @import ggplot2

catchweightRangePlot  <- function(data, base_size = 14) {
  
  x <- data$catchS
  y <- data$catchW
  
  if(nrow(x) == 0 | nrow(y) == 0) {
    ggplot(x, aes(x = commonname, y = mean)) +
      annotate("text", x = 1, y = 1, label = "No weight data") +
      scale_y_log10("Catch weight range [log10(kg)]") +
      xlab("Species database name") +
      theme_bw(base_size = base_size) +
      theme(axis.text = element_blank())
  } else {
    ggplot() +
      geom_linerange(data = x, aes(x = commonname, ymax = max, ymin = min), color = "red") +
      geom_point(data = y, aes(x = commonname, y = catchweight), size = 1, shape = 21) +
      scale_y_log10("Catch weight range [log10(kg)]") +
      xlab("Species database name") +
      coord_cartesian() +
      theme_bw(base_size = base_size) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
  }
}

#' @title Plot mean weight of fish in catch
#' @description Plots mean weight of fish in catch in an \link[=processBioticFile]{bioticProcData} object.
#' @param data data object from \link{speciesOverviewData}. Requires the meanW data frame.
#' @param base_size base size parameter for ggplot. See \link[ggplot2]{theme_bw}.
#' @return Returns a ggplot object
#' @import ggplot2

catchIndMeanWeightPlot <- function(data, base_size = 14) {
  
  x <- data$meanW
  
  if(nrow(x) == 0) {
    ggplot(x, aes(x = commonname, y = mean)) +
      annotate("text", x = 1, y = 1, label = "No weight data") +
      scale_y_log10("Mean specimen weight [log10(kg) +/- range]") +
      xlab("Species database name") +
      theme_bw(base_size = base_size) +
      theme(axis.text = element_blank())
  } else {
    ggplot(x, aes(x = commonname, y = mean, ymin = min, ymax = max)) +
      geom_pointrange() +
      scale_y_log10("Mean specimen weight [log10(kg) +/- range]", labels = scales::number_format(accuracy = 0.001)) +
      xlab("Species database name") +
      theme_bw(base_size = base_size) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
  }
}

#' @title Plot mean number in catches
#' @description Plots mean number of fish in catches in an \link[=processBioticFile]{bioticProcData} object.
#' @param data data object from \link{speciesOverviewData}. Requires the meanN data frame.
#' @param base_size Base size parameter for ggplot. See \link[ggplot2]{theme_bw}.
#' @return Returns a ggplot object
#' @import ggplot2

catchcountMeanPlot <- function(data, base_size = 14) {
  
  x <- data$meanN
  
  if(nrow(x) == 0) {
    ggplot(x, aes(x = commonname, y = mean)) +
      annotate("text", x = 1, y = 1, label = "No catch data") +
      ylab("Mean number in catch (+/- SE)") +
      xlab("Species database name") +
      theme_bw(base_size = base_size) +
      theme(axis.text = element_blank())
  } else {
    ggplot(x, aes(x = commonname, y = mean, ymax = mean + se, ymin = mean - se)) +
      geom_linerange() +
      geom_point() +
      ylab("Mean number in catch (+/- SE)") +
      xlab("Species database name") +
      theme_bw(base_size = base_size) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  }
}

#' @title Plot range of number in catches
#' @description Plots range of number of fish in catches in an \link[=processBioticFile]{bioticProcData} object.
#' @param data data object from \link{speciesOverviewData}. Requires the meanN and catchN data frames.
#' @param base_size Base size parameter for ggplot. See \link[ggplot2]{theme_bw}.
#' @return Returns a ggplot object
#' @import ggplot2

catchcountRangePlot <- function(data, base_size = 14) {
  
  x <- data$meanN
  y <- data$catchN
  
  if(nrow(x) == 0 | nrow(y) == 0) {
    ggplot(x, aes(x = commonname, y = mean)) +
      annotate("text", x = 1, y = 1, label = "No catch data") +
      scale_y_log10("Range for number in catch (log10)") +
      xlab("Species database name") +
      theme_bw(base_size = base_size) +
      theme(axis.text = element_blank())
  } else {
    ggplot() +
      geom_linerange(data = x,
                     aes(x = commonname, ymax = max, ymin = min), color = "red") +
      geom_point(data = y,
                 aes(x = commonname, y = catchcount), size = 1, shape = 21) +
      scale_y_log10("Range for number in catch (log10)") +
      xlab("Species database name") +
      coord_cartesian() +
      theme_bw(base_size = base_size) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
  }
}

#' @title Plot catch by species and gear code
#' @description Plots total catch by fish and gear type in an \link[=processBioticFile]{bioticProcData} object.
#' @param data data object from \link{speciesOverviewData}. Requires the catchG data frame.
#' @param base_size Base size parameter for ggplot. See \link[ggplot2]{theme_bw}.
#' @return Returns a ggplot object
#' @import ggplot2

gearCatchPlot <- function(data, base_size = 14) {
  
  x <- data$catchG
  
  if(nrow(x) == 0) {
    ggplot(x, aes(x = commonname, y = gear)) +
      annotate("text", x = 1, y = 1, label = "No gear information") +
      ylab("Gear") +
      xlab("Species database name") +
      theme_bw(base_size = base_size) +
      theme(axis.text = element_blank())
  } else {
    ggplot(x, aes(x = commonname, y = as.factor(gear),
                  size = sum, color = sum)) +
      geom_point() +
      scale_color_distiller(name = "Total catch [log10(kg)]",
                            palette = "Spectral", trans = "log10",
                            breaks = c(1 %o% 10^(-4:6))
      ) +
      scale_size(name = "Total catch [log10(kg)]", trans = "log10"#,
                 #breaks = c(1 %o% 10^(-4:4), range = c(1,8))
      ) +
      ylab("Gear") +
      xlab("Species database name") +
      theme_bw(base_size = base_size) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 
  }
}

#' @title Plot bottom depth and minimum fishing depth distribution for stations
#' @description Plots bottom depth and minimum fishing depth distribution for stations in an \link[=processBioticFile]{bioticProcData} object.
#' @param data data object from \link{speciesOverviewData}. Requires the stnD data frame.
#' @param base_size Base size parameter for ggplot. See \link[ggplot2]{theme_bw}.
#' @return Returns a ggplot object
#' @import ggplot2

stationDepthPlot <- function(data, base_size = 14) {
  
  x <- data$stnD
  
  if(all(is.na(x$value))) {
    ggplot(x, aes(x = value)) +
      annotate("text", x = 1, y = 1, label = "No depth data") +
      ylab("Count") +
      xlab("Depth (m)") +
      theme_bw(base_size = base_size) +
      theme(axis.text = element_blank())
  } else {
    ggplot(x, aes(x = value)) +
      geom_histogram(binwidth = 100, color = "black", fill = "grey") +
      facet_wrap(~variable) +
      scale_y_continuous("Count", expand = c(0, 0)) +
      scale_x_continuous("Depth (m)", expand = c(0,0.05)) +
      expand_limits(x = 0) +
      theme_classic(base_size = base_size) +
      theme(strip.background = element_blank())
  }
}

#' @title Plot minimum fishing depth by catch for six most dominant species
#' @description Plots minimum fishing depth by catch for six most dominant species in an \link[=processBioticFile]{bioticProcData} object.
#' @param data data object from \link{speciesOverviewData}. Requires the compDat data frame.
#' @param base_size Base size parameter for ggplot. See \link[ggplot2]{theme_bw}.
#' @return Returns a ggplot object
#' @import ggplot2

catchSpeciesWeightPlot <- function(data, base_size = 14) {
  
  x <- data$compDat
  
  if(nrow(x) == 0) {
    ggplot(x, aes(x = fishingdepthmin, y = catchweight)) +
      annotate("text", x = 1, y = 1, label = "No depth data") +
      ylab("Catch weight (kg)") + 
      xlab("Minimum fishing depth (m)") +
      theme_bw(base_size = base_size) +
      theme(axis.text = element_blank())
  } else {
    ggplot(x[x$commonname != "Andre arter",], aes(x = fishingdepthmin, y = catchweight, group = commonname)) +
      geom_smooth(se = FALSE) +
      geom_point() +
      ylab("Catch weight (kg)") + 
      xlab("Minimum fishing depth (m)") +
      facet_wrap(~commonname, scales = "free_y") + 
      theme_bw(base_size = base_size)
  }
  
  
}

#' @title Plot catch composition on a map
#' @description Plots catch composition in an \link[=processBioticFile]{bioticProcData} object on a \link[leaflet]{leaflet} map.
#' @param data data object from \link{speciesOverviewData}. Requires the compDat data frame.
#' @return Returns a \link[leaflet]{leaflet} object
#' @import leaflet

catchCompMap <- function(data) {
  
  x <- data$compDatW
  y <- data$compDat
  
  if(nrow(x) == 0 | nrow(y) == 0) {
    leaflet::leaflet() %>% 
      addTiles(urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/Ocean/World_Ocean_Base/MapServer/tile/{z}/{y}/{x}",
               attribution = "Tiles &copy; Esri &mdash; Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri") %>% 
      addMarkers(lng = 20, lat = 70, label = "No catch data")
  } else {
    leaflet::leaflet() %>% 
      addTiles(urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/Ocean/World_Ocean_Base/MapServer/tile/{z}/{y}/{x}",
               attribution = "Tiles &copy; Esri &mdash; Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri") %>% 
      addMinicharts(
        x$longitudestart, x$latitudestart,
        type = "pie", chartdata = x[,levels(y$commonname)],
        colorPalette = ColorPalette,
        width = 40 * log(x$total) / log(max(x$total)), 
        transitionTime = 0
      )
  }
}


#' @title Plot total catch of a specific species on a mpa
#' @description Plots total catch of a species in an \link[=processBioticFile]{bioticProcData} object on a \link[leaflet]{leaflet} map.
#' @param data stnall data.table from \link[=processBioticFile]{bioticProcData} class. Typically \code{rv$stnall}.
#' @param species NMD commonname for a species. The names are in Norwegian. Use "All" to plot a sum of total catch including all species.
#' @return Returns a \link[leaflet]{leaflet} object
#' @import leaflet

catchMap <- function(data, species) {
  
  ## Definitions
  #species='All'
  if (species == "All") {
    sps <- unique(data$commonname)
  } else {
    sps <- species
  }
  
  ## Prepare data
  
  tmp2 <- data %>% lazy_dt() %>% filter(!is.na(longitudestart) & !is.na(latitudestart)) %>% collect()
  
  tmp <- tmp2 %>% lazy_dt() %>% 
    filter(commonname %in% sps) %>% 
    group_by(startyear, serialnumber, longitudestart, latitudestart, gear, bottomdepthstart, stationstartdate) %>% 
    summarize(catchsum = round(sum(catchweight, na.rm = TRUE), 2)) %>% collect()
  
  tmp2 <- tmp2[!paste(tmp2$startyear, tmp2$serialnumber, sep = "_") %in% paste(tmp$startyear, tmp$serialnumber, sep = "_"), !names(tmp2) %in% c("catchsampleid", "commonname", "catchcategory", "catchpartnumber", "catchweight", "catchcount", "lengthsampleweight", "lengthsamplecount")]
  
  if (nrow(tmp2) > 0) tmp2$catchsum <- 0
  
  ## Legend breaks
  library(purrr)
  library(data.table)
  library(rlang)

  breaks=c(-Inf, 1, 10, 10^2, 0.5*10^3, 10^3, 0.5*10^4, +Inf)
  sizes = c(2, 3, 6, 9, 12, 16, 20, 25)
  conditions = mapply(c, breaks, shift(breaks,1,type = 'lead'), shift(sizes,1,type='lead'), SIMPLIFY = FALSE)
  names(conditions) = map2_chr(breaks, shift(breaks,1,type = 'lead'), ~ paste0(.x, "-", .y))
  conditions <- purrr::map(.x = conditions[-length(conditions)], ~quo(between(catchsum, !!.x[1], !!.x[2]) ~ !!.x[3]))
  conditions[length(conditions)] <- NULL
  labels = c("Not caught", 
             paste0("<", as.character(breaks[2])),
             map2_chr(shift(breaks,1,type = 'lead'), shift(breaks,2,type = 'lead'), ~ paste0(.x, "-", .y))[1:5],
             paste0(">", as.character(breaks[length(breaks)-1])))
  
  tmp <- tmp %>% mutate(WeightClass = case_when(!!!conditions) %>% 
         factor(levels=sizes))  
  
  ## Plot
  
  p <- leaflet::leaflet(tmp, options = leafletOptions(zoomControl = FALSE)) %>% 
    addTiles(urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/Ocean/World_Ocean_Base/MapServer/tile/{z}/{y}/{x}",
             attribution = "Tiles &copy; Esri &mdash; Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri") %>%
    addCircleMarkers(lat = ~ latitudestart, lng = ~ longitudestart, 
                     weight = 2, stroke = T, 
                     radius = 0.5*as.numeric(as.character(tmp$WeightClass)), #radius is in px, 0.5 needed as the border width is set to 50% in the addLegendCustom function
                     label = paste0(tmp$serialnumber, "; ", tmp$catchsum, " kg"), 
                     popup = paste("Serial number:", tmp$serialnumber, "<br>",
                                   "Date:", tmp$stationstartdate, "<br>",
                                   "Gear code:", tmp$gear, "<br>",
                                   "Bottom depth:", round(tmp$bottomdepthstart, 0), "m",
                                   "<br>", species, "catch:", tmp$catchsum, 
                                   "kg"), 
                     color = "red", fill = NA) %>% 
    addLegendCustom(title = paste0(species, "<br>", "Weight (Kg)"),
                    colors = c("black", rep("red",length(sizes)-1)),
                    labels = labels,
                    sizes = sizes  
    )
    
  
  if (nrow(tmp2) > 0) {
    p %>% 
      addCircles(lat = tmp2$latitudestart, lng = tmp2$longitudestart, 
                 weight = 2, radius = 1, 
                 label = paste0(tmp2$serialnumber, "; ", tmp2$catchsum, " kg"), 
                 popup = paste("Serial number:", tmp2$serialnumber, "<br>",
                               "Date:", tmp2$stationstartdate, "<br>",
                               "Gear code:", tmp2$gear, "<br>",
                               "Bottom depth:", round(tmp2$bottomdepthstart, 0), "m",
                               "<br>", species, "catch:", tmp2$catchsum, 
                               "kg"), 
                 color = "black"
      )
  } else {
    p
  }
  
}

#' @title Plot overview of fish length by species
#' @description Plots overview of fish length by species in an \link[=processBioticFile]{bioticProcData} object.
#' @param indall indall data.table from \link[=processBioticFile]{bioticProcData} class. Typically \code{rv$indall}.
#' @param nLimit Integer for the minimum number of length measurements / species to be included in the plot.
#' @param unit character giving the measurement unit.
#' @param base_size base size parameter for ggplot. See \link[ggplot2]{theme_bw}.
#' @return Returns a ggplot object
#' @import ggplot2


indLengthPlot <- function(indall, nLimit = 10, unit = "m", base_size = 14) {
  
  sps <- indall %>% lazy_dt() %>% filter(!is.na(length)) %>% group_by(commonname) %>% count() %>% filter(n > nLimit) %>% pull(commonname) 
  
  if(length(sps) > 0) {
    
    indLen <- indall %>% lazy_dt() %>%
      filter(!is.na(length), commonname %in% sps) %>% collect() %>% as.data.table()
    
    meanIndLen <- indLen %>% lazy_dt() %>%  
      group_by(commonname) %>%
      summarise(medLength = median(length)) %>% 
      arrange(-medLength) %>% collect()
    
    indLen[, commonname := factor(commonname, levels = meanIndLen$commonname)]
    
    ggplot() +
      geom_violin(data = indLen, aes(y = length, x = commonname)) +
      geom_point(data = meanIndLen, aes(y = medLength, x = commonname), shape = 95, size = 5) + 
      ylab(paste0("Length (", unit, ")")) +
      xlab("Species database name") +
      #coord_cartesian(expand = FALSE, ylim = range(pretty(x$n))) +
      theme_bw(base_size = base_size) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    
  } else {
    
    ggplot() +
      annotate("text", x = 1, y = 1, label = paste0("None of the species has\n >", nLimit, " length measured individuals")) +
      ylab(paste0("Length (", unit, ")")) +
      xlab("Species database name") +
      theme_bw(base_size = base_size) +
      theme(axis.text = element_blank())
  }
}

#' @title Plot overview of fish weight by species
#' @description Plots overview of fish weight by species in an \link[=processBioticFile]{bioticProcData} object on a \link[leaflet]{leaflet} map.
#' @param indall indall data.table from \link[=processBioticFile]{bioticProcData} class. Typically \code{rv$indall}.
#' @param nLimit Integer for the minimum number of length measurements / species to be included in the plot.
#' @param unit character giving measurement unit.
#' @param base_size base size parameter for ggplot. See \link[ggplot2]{theme_bw}.
#' @return Returns a ggplot object
#' @import ggplot2

indWeightPlot <- function(indall, nLimit = 10, unit = "kg", base_size = 14) {
  
  sps <- indall %>% lazy_dt() %>% filter(!is.na(individualweight)) %>% group_by(commonname) %>% count() %>% filter(n > nLimit) %>% pull(commonname)
  
  if(length(sps) > 0) {
    
    indWei <- indall %>% lazy_dt() %>%
      filter(!is.na(individualweight), commonname %in% sps) %>% collect() %>% as.data.table()
    
    meanIndWei <- indWei %>% lazy_dt() %>%  
      group_by(commonname) %>%
      summarise(medWeight = median(individualweight)) %>% 
      arrange(-medWeight) %>% collect()
    
    indWei[, commonname := factor(commonname, levels = meanIndWei$commonname)]
    
    ggplot() +
      geom_violin(data = indWei, aes(y = individualweight, x = commonname)) +
      geom_point(data = meanIndWei, aes(y = medWeight, x = commonname), shape = 95, size = 5) + 
      ylab(paste0("Weight (", unit, ")")) +
      xlab("Species database name") +
      theme_bw(base_size = base_size) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    
  } else {
    
    ggplot() +
      annotate("text", x = 1, y = 1, label = paste0("None of the species has\n >", nLimit, " length measured individuals")) +
      ylab(paste0("Weight (", unit, ")")) +
      xlab("Species database name") +
      theme_bw(base_size = base_size) +
      theme(axis.text = element_blank())
  }
}

## individualFigureData ####

#' @title Generate data for individual plots
#' @description Generates data required by individual plots in Biotic Explorer
#' @param indall data.table from \link[=processBioticFile]{bioticProcData} class. Typically \code{rv$indall}.
#' @param indSpecies character defining the species in \code{indall$commonname). Typically input$indSpecies.
#' @param lengthUnit character defining the unit of length measurements for output. Options: "mm", "cm" or "m". The NMD standard is "m". 
#' @param weightUnit character defining the unit of weight measurements for output. Options: "g", "kg". The NMD standard is "kg".
#' @param useEggaSystem logical indicating whether "delnummer" (catchpartnumber) defines the sex of individuals. This has systematically been used for Greenland halibut collected during "EggaNord" and "EggaSør" surveys.  
#' @return Returns a list of tibbles containing data required by various individual-based plots.
#' @import dplyr data.table

# indall = rv$indall; indSpecies = "snabeluer"; lengthUnit = "m"; weightUnit = "kg"; useEggaSystem = FALSE
# indall = rv$indall; indSpecies = input$indSpecies; lengthUnit = input$lengthUnit; weightUnit = input$weightUnit; useEggaSystem = FALSE
individualFigureData <- function(indall, indSpecies = input$indSpecies, lengthUnit = "m", weightUnit = "kg", useEggaSystem = FALSE) {
  
  ## Base data
  
  tmpBase <- indall[commonname == indSpecies, ] 
  
  if (indSpecies == "blåkveite" & useEggaSystem) {
    
    tmpTab <- data.table::dcast(tmpBase, cruise + startyear + serialnumber + longitudestart + latitudestart ~ catchpartnumber, fun.aggregate = length, value.var = "length")
    
    if(all(c(1, 2) %in% names(tmpTab))) {
      
      tmpTab$EggaSystem <- tmpTab$`1` > 0 & tmpTab$`2` > 0
      
      tmpBase <- dplyr::left_join(tmpBase, tmpTab[, !names(tmpTab) %in% 1:10, with = FALSE], by = c("startyear", "serialnumber", "cruise", "longitudestart", "latitudestart"))  
      
      tmpBase$sex <- ifelse(!is.na(tmpBase$sex), tmpBase$sex, ifelse(is.na(tmpBase$sex) & tmpBase$EggaSystem & tmpBase$catchpartnumber == 1, 1, ifelse(is.na(tmpBase$sex) & tmpBase$EggaSystem & tmpBase$catchpartnumber == 2, 2, NA)))
      
      tmpBase <- as.data.table(tmpBase[, names(tmpBase) != "EggaSystem"])
    }
  }
  
  tmpBase$sex[is.na(tmpBase$sex)] <- "Unidentified"
  tmpBase$sex <- factor(tmpBase$sex)
  tmpBase$sex <- dplyr::recode_factor(tmpBase$sex, "1" = "Female", "2" = "Male", "3" = "Unidentified", "4" = "Unidentified")
  
  ## Length-weight data
  
  if(nrow(na.omit(tmpBase[, .(length, individualweight)])) > 10) {
    
    lwDat <- tmpBase[!is.na(length) & !is.na(individualweight),]
    lwDat$weightMod <- log(lwDat$individualweight*1000)
    lwDat$lengthMod <- log(lwDat$length*100)
    
    lwMod <- lm(weightMod ~ lengthMod, 
                data = lwDat[!is.infinite(lengthMod) & !is.infinite(weightMod)]
    )
    lwModA <- unname(exp(coef(lwMod)[1]))
    lwModB <- unname(coef(lwMod)[2])
    
    if(lengthUnit == "cm") lwDat$length <- lwDat$length*100
    if(lengthUnit == "mm") lwDat$length <- lwDat$length*1000
    if(weightUnit == "g") lwDat$individualweight <- lwDat$individualweight*1000
    
    lwDat$weightModTrans <- log(lwDat$individualweight)
    lwDat$lengthModTrans <- log(lwDat$length)
    
    lwModTrans <- lm(weightModTrans ~ lengthModTrans, 
                     data = lwDat[!is.infinite(lengthModTrans) & !is.infinite(weightModTrans)]
    )
    lwModTransA <- unname(exp(coef(lwModTrans)[1]))
    
  } else {
    
    lwDat <- NULL
    lwModA <- NULL
    lwModB <- NULL
    lwModTransA <- NULL
  }
  
  ## Transform tmpBase units (untransformed needed above)
  
  if(lengthUnit == "cm") tmpBase$length <- tmpBase$length*100
  if(lengthUnit == "mm") tmpBase$length <- tmpBase$length*1000
  if(weightUnit == "g") tmpBase$individualweight <- tmpBase$individualweight*1000
  
  ## Length-age data
  
  if (all(c("length", "age") %in% names(tmpBase))) {
    if (nrow(na.omit(tmpBase[, c("length", "age"), with = FALSE])) > 10) {
      
      laDat <- tmpBase[!is.na(tmpBase$age) & !is.na(tmpBase$length), ]
      
    } else {laDat <- NULL}} else {laDat <- NULL}
  
  ## L50 maturity data
  
  if (nrow(na.omit(tmpBase[, .(length, sex, maturationstage)])) > 20) { 
    
    l50Dat <- tmpBase[!is.na(tmpBase$length) & !is.na(tmpBase$sex) & !is.na(tmpBase$maturationstage) & (tmpBase$sex == "Female" | tmpBase$sex == "Male"), ]
    
    tmp <- table(l50Dat$sex)
    
    if(tmp[names(tmp) == "Female"] < 10 | tmp[names(tmp) == "Male"] < 10) {
      
      l50Dat <- NULL
      
    } else {
      
      l50Dat$maturity <- ifelse(l50Dat$maturationstage < 2, 0, ifelse(l50Dat$maturationstage >= 2, 1, NA))
      
    } 
  } else {
    
    l50Dat <- NULL
    
  }
  
  ## Sex ratio data
  
  if(nrow(na.omit(tmpBase[, .(sex)])) > 5) {
    srDat <- tmpBase %>% lazy_dt() %>% 
      dplyr::filter(!is.na(sex)) %>% 
      dplyr::group_by(cruise, startyear, serialnumber, longitudestart, latitudestart) %>% 
      dplyr::summarise(
        Female = sum(sex == "Female"), 
                       Male = sum(sex == "Male"),
        .groups = "drop") %>% 
      dplyr::mutate(Total = Female + Male) %>% 
      dplyr::filter(Total > 0) %>% 
      dplyr::collect()
  } else {
    srDat <- NULL
  }
  
  ## Geographic size distribution data
  
  if(nrow(na.omit(tmpBase[, .(length)])) > 20) {
    sdDat <- tmpBase %>% lazy_dt() %>% 
      dplyr::filter(!is.na(length)) %>% 
      dplyr::select(cruise, startyear, serialnumber, longitudestart, latitudestart, length) %>% 
      dplyr::mutate(interval = ggplot2::cut_interval(length, n = 5)) %>% 
      dplyr::group_by(cruise, startyear, serialnumber, longitudestart, latitudestart, interval, .drop = FALSE) %>% 
      dplyr::summarise(count = n(), .groups = "drop") %>% 
      dplyr::collect()
  } else {
    sdDat <- NULL
  }
  
  ## Length distribution data
  
  if(nrow(na.omit(tmpBase[, .(length, sex)])) > 10) {
    ldDat <- tmpBase %>% as_tibble() %>% 
      filter(!is.na(length)) %>% 
      replace_na(list(sex = "Unidentified")) %>% 
      select(sex, length, maturationstage, specialstage) 
  } else {
    ldDat <- NULL
  }
  
  ## Return
  
  list(units = list(length = lengthUnit, weight = weightUnit), tmpBase = tmpBase, lwDat = lwDat, lwMod = list(a = lwModA, b = lwModB, aTrans = lwModTransA), laDat = laDat, l50Dat = l50Dat, srDat = srDat, sdDat = sdDat, ldDat = ldDat)
  
}

## lwPlot ####

lwPlot <- function(data, lwPlotLogSwitch = input$lwPlotLogSwitch) {
  
  p <- suppressWarnings({
    ggplot(data = data$lwDat) +
      geom_point(aes(x = length, y = individualweight, text = paste0(  "cruise: ", cruise, "\nserialnumber: ", serialnumber, "\ncatchpartnumber: ", catchpartnumber, "\nspecimenid: ", specimenid))) + 
      theme_classic(base_size = 12)
  })
  
  if (lwPlotLogSwitch) {
    p <- suppressMessages({
      p + 
        scale_x_log10(paste0("Length [log10(", data$units$length, ")]")) +
        scale_y_log10(paste0("Weight [log10(", data$units$weight, ")]")) + 
        geom_smooth(aes(x = length, y = individualweight), method = "lm", formula = y ~ x, se = TRUE) 
    })
    
  } else {
    tmp <- data.frame(x = seq(min(data$lwDat$length), max(data$lwDat$length), length.out = 50))
    tmp$y <- data$lwMod$aTrans*tmp$x^data$lwMod$b
    
    p <- suppressWarnings({
      p + 
        scale_x_continuous(paste0("Length (", data$units$length, ")")) +
        scale_y_continuous(paste0("Weight (", data$units$weight, ")")) +
        geom_path(data = tmp, aes(x = x, y = y), color = "blue", size = 1)
        
        # stat_function(data = data.frame(x = range(data$lwDat$length)), 
        #               aes(x = .data$x),
        #               fun = function(a, b, x) {a*x^b},
        #               args = list(a = data$lwMod$aTrans, b = data$lwMod$b),
        #               color = "blue", size = 1)
    })
  }
  
  suppressMessages(p)
}

## laPlot ####

# data = indOverviewDat = individualFigureData(indall = rv$indall, indSpecies = input$selSpeciesDb); laPlotSexSwitch = FALSE; growthModelSwitch = "vout"; forceZeroGroupLength = NA; forceZeroGroupStrength = 10
laPlot <- function(data, laPlotSexSwitch, growthModelSwitch, forceZeroGroupLength = NA, forceZeroGroupStrength = 10) {
  
  modName <- c("von Bertalanffy" = "vout", "Gompertz" = "gout", "Logistic" = "lout")
  modName <- names(modName[modName == growthModelSwitch])
  
  if (laPlotSexSwitch) {
    
    laDat <- data$laDat %>% lazy_dt() %>% filter(!is.na(sex) & (sex == "Female" | sex == "Male")) %>% select(cruise, serialnumber, catchpartnumber, specimenid, sex, age, length) %>% collect()
    
    laDatF <- laDat %>% filter(sex == "Female") %>% select(age, length)
    laDatM <- laDat %>% filter(sex == "Male") %>% select(age, length)
    
    if(nrow(laDatM) < 10 | nrow(laDatF) < 10) {
      
      Plot <- ggplot() +
        geom_blank() +
        annotate("text", x = 1, y = 1, label = "Not enough age data for\nsex separated growth models", size = 6) +
        ylab(paste0("Total length (", data$units$length, ")")) +
        xlab("Age (years)") +
        coord_cartesian(expand = FALSE, clip = "off") +
        theme_classic(base_size = 14)
      
      Text <- paste0(
        "Not enough age data:",
        "\n Number of included specimens = ", nrow(laDatF), " and ", nrow(laDatM)
      )
      
    } else {
      
      if(!is.na(forceZeroGroupLength)) {
        laDatF <- rbind(laDatF, tibble(age = rep(0, ceiling(nrow(laDatF)*(forceZeroGroupStrength/100))), length = rep(forceZeroGroupLength, ceiling(nrow(laDatF)*(forceZeroGroupStrength/100)))))
        laDatM <- rbind(laDatM, tibble(age = rep(0, ceiling(nrow(laDatM)*(forceZeroGroupStrength/100))), length = rep(forceZeroGroupLength, ceiling(nrow(laDatM)*(forceZeroGroupStrength/100)))))
      } 
      
      laModF <- fishmethods::growth(age = laDatF$age, size = laDatF$length, Sinf = max(laDatF$length), K = 0.1, t0 = 0, graph = FALSE)
      laModM <- fishmethods::growth(age = laDatM$age, size = laDatM$length, Sinf = max(laDatM$length), K = 0.1, t0 = 0, graph = FALSE)
      
      laModFpred <- data.frame(age = 0:max(laDat$age), length = predict(eval(parse(text = paste0("laModF$", growthModelSwitch))), newdata = data.frame(age = 0:max(laDat$age))))
      laModMpred <- data.frame(age = 0:max(laDat$age), length = predict(eval(parse(text = paste0("laModM$", growthModelSwitch))), newdata = data.frame(age = 0:max(laDat$age))))
      
      laModFpars <- coef(eval(parse(text = paste0("laModF$", growthModelSwitch))))
      laModMpars <- coef(eval(parse(text = paste0("laModM$", growthModelSwitch))))
      
      ## Plot 
      
      Plot <- suppressWarnings({
        ggplot() +
          geom_point(data = laDat, aes(x = age, y = length, color = as.factor(sex), text = paste0("cruise: ", cruise, "\nserialnumber: ", serialnumber, "\ncatchpartnumber: ", catchpartnumber, "\nspecimenid: ", specimenid))) +
          expand_limits(x = c(0, round_any(max(laDat$age), 10, ceiling)), y = c(0, max(pretty(c(0, max(laDat$length)))))) +
          scale_color_manual("Sex", values = c(ColorPalette[4], ColorPalette[1])) + 
          geom_hline(yintercept = laModFpars[1], linetype = 2, color = ColorPalette[4], alpha = 0.5) +
          geom_hline(yintercept = laModMpars[1], linetype = 2, color = ColorPalette[1], alpha = 0.5) +
          geom_path(data = laModFpred, aes(x = age, y = length), color = ColorPalette[4]) + 
          geom_path(data = laModMpred, aes(x = age, y = length), color = ColorPalette[1]) + 
          ylab(paste0("Total length (", data$units$length, ")")) +
          xlab("Age (years)") +
          coord_cartesian(expand = FALSE, clip = "off") +
          theme_classic(base_size = 14)
      })
      
      ## Text
      
      Text <- paste0(
        modName, " growth function coefficients\n for females and males, respectively: \n Linf (asymptotic average length) = ", round(laModFpars[1], 3), " and ", round(laModMpars[1], 3), " ", data$units$length, 
        "\n K (growth rate coefficient) = ", round(laModFpars[2], 3), " and ", round(laModMpars[2], 3), 
        "\n t0 = ", round(laModFpars[3], 3), " and ", round(laModMpars[3], 3), " ", data$units$length, 
        "\n tmax (life span; t0 + 3/K) = ", round(laModFpars[3] + 3 / laModFpars[2], 1), " and ", round(laModMpars[3] + 3 / laModMpars[2], 1), " years",
        "\n Number of included specimens = ", nrow(laDatF), " and ", nrow(laDatM),
        "\n Total number of measured = ", nrow(data$tmpBase), 
        "\n Excluded (length, age or sex missing): \n Length = ", sum(is.na(data$tmpBase$length)), "; age = ", sum(is.na(data$tmpBase$age)), "; sex = ", sum(is.na(data$tmpBase$sex))
      )
    }
  } else {
    
    laDat <- data$laDat %>% lazy_dt() %>% select(cruise, serialnumber, catchpartnumber, specimenid, sex, age, length) %>% collect()
    
    if(!is.na(forceZeroGroupLength)) {
      laDat <- bind_rows(laDat, tibble(age = rep(0, ceiling(nrow(laDat)*(forceZeroGroupStrength/100))), length = rep(forceZeroGroupLength, ceiling(nrow(laDat)*(forceZeroGroupStrength/100)))))
    } 
    
    if(length(laDat$age) < 30) {
    
    #if(eval(parse(text = paste0("laMod$", growthModelSwitch))) == "Fit failed") {
      
      Plot <- ggplot() +
        geom_blank() +
        annotate("text", x = 1, y = 1, label = "Not enough age data to\ncalculate a growth model", size = 6) +
        ylab(paste0("Total length (", data$units$length, ")")) +
        xlab("Age (years)") +
        coord_cartesian(expand = FALSE, clip = "off") +
        theme_classic(base_size = 14)
      
      Text <- paste0(
        "Not enough age data:",
        "\n Number of included specimens = ", nrow(laDat)
      )
      
    } else {
      
      laMod <- fishmethods::growth(age = laDat$age, size = laDat$length, Sinf = max(laDat$length), K = 0.1, t0 = 0, graph = FALSE)
      
      laModpred <- data.frame(age = 0:max(laDat$age), length = predict(eval(parse(text = paste0("laMod$", growthModelSwitch))), newdata = data.frame(age = 0:max(laDat$age))))
      
      laModpars <- coef(eval(parse(text = paste0("laMod$", growthModelSwitch))))
      
      ## Plot
      
      Plot <- suppressWarnings({
        ggplot() +
          geom_point(data = laDat, aes(x = age, y = length, text = paste0("cruise: ", cruise, "\nserialnumber: ", serialnumber, "\ncatchpartnumber: ", catchpartnumber, "\nspecimenid: ", specimenid))) +
          expand_limits(x = c(0, round_any(max(laDat$age), 10, ceiling)), y = c(0, max(pretty(c(0, max(laDat$length)))))) +
          geom_hline(yintercept = laModpars[1], linetype = 2, color = "blue", alpha = 0.5) +
          geom_path(data = laModpred, aes(x = age, y = length), color = "blue") + 
          ylab(paste0("Total length (", data$units$length, ")")) +
          xlab("Age (years)") +
          coord_cartesian(expand = FALSE, clip = "off") +
          theme_classic(base_size = 14)
      })
      
      ## Text
      
      Text <- paste0(
        modName, " growth function coefficients: \n Linf (asymptotic average length) = ", round(laModpars[1], 3), " ", data$units$length, 
        "\n K (growth rate coefficient) = ", round(laModpars[2], 3), 
        "\n t0 (length at age 0) = ", round(laModpars[3], 3), " ", data$units$length, 
        "\n tmax (life span; t0 + 3/K) = ", round(laModpars[3] + 3 / laModpars[2], 1), " years", 
        "\n Number of included specimens = ", nrow(data$laDat), 
        "\n Total number of measured = ", nrow(data$tmpBase), 
        "\n Excluded (length or age missing): \n Length = ", sum(is.na(data$tmpBase$length)), "; age = ", sum(is.na(data$tmpBase$age))
      )
    }
  }
  
  ## Return
  
  return(list(laPlot = Plot, laText = Text))
}

## l50Plot ####

l50Plot <- function(data) {
  
  modF <- glm(maturity ~ length, data = data$l50Dat[data$l50Dat$sex == "Female",], family = binomial(link = "logit"))
  modM <- glm(maturity ~ length, data = data$l50Dat[data$l50Dat$sex == "Male",], family = binomial(link = "logit"))
  
  Fdat <- unlogit(0.5, modF)
  Fdat$sex <- "Female"
  Mdat <- unlogit(0.5, modM)
  Mdat$sex <- "Male"
  modDat <- rbind(Fdat, Mdat)
  
  ### Plot
  
  Plot <- suppressMessages({
    
    ggplot(data$l50Dat, aes(x = length, y = maturity, shape = sex)) + 
      geom_point() + 
      geom_segment(data = modDat, 
                   aes(x = mean, xend = mean, y = 0, yend = 0.5, color = sex),
                   linetype = 2) +
      geom_segment(data = modDat, 
                   aes(x = -Inf, xend = mean, y = 0.5, yend = 0.5, color = sex),
                   linetype = 2) +
      geom_text(data = modDat, 
                aes(x = mean, y = -0.03, label = paste(round(mean, 2), data$units$length),
                    color = sex), size = 3) +
      stat_smooth(aes(color = sex), method = "glm", formula = y ~ x,
                  method.args = list(family = "binomial")) +
      xlab(paste0("Total length (", data$units$length, ")")) +
      ylab("Maturity") + 
      scale_color_manual("Sex", values = c(ColorPalette[4], ColorPalette[1])) +
      scale_shape("Sex", solid = FALSE) + 
      theme_bw(base_size = 14) + 
      guides(color=guide_legend(override.aes=list(fill=NA))) + 
      theme(legend.position = c(0.9, 0.25), 
            legend.background = element_blank(), legend.key = element_blank())
  })
  
  ### Text
  
  Text <- paste0(
    "50% maturity at length (L50) based on logit regressions and assuming maturitystage >= 2 as mature:",
    "\n\n Females: ", round(modDat[modDat$sex == "Female", "mean"], 3), " ", data$units$length, ". 95% confidence intervals: ", round(modDat[modDat$sex == "Female", "ci.min"], 3), " - ", round(modDat[modDat$sex == "Female", "ci.max"], 3),
    "\n  Number of specimens: ", nrow(data$l50Dat[data$l50Dat$sex == "Female",]),
    "\n\n Males: ", round(modDat[modDat$sex == "Male", "mean"], 3), " ", data$units$length, ". 95% confidence intervals: ", round(modDat[modDat$sex == "Male", "ci.min"], 3), " - ", round(modDat[modDat$sex == "Male", "ci.max"], 3),
    "\n  Number of specimens: ", nrow(data$l50Dat[data$l50Dat$sex == "Male",])
  )
  
  ### Return
  
  return(list(Plot = Plot, Text = Text))
  
}

## Sex ratio map ####

sexRatioMap <- function(data) {
  x <- data$srDat %>% 
    filter(!is.na(longitudestart), !is.na(latitudestart))
  
  leaflet::leaflet() %>% 
    addTiles(urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/Ocean/World_Ocean_Base/MapServer/tile/{z}/{y}/{x}",
             attribution = "Tiles &copy; Esri &mdash; Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri") %>% 
    addMinicharts(
      x$longitudestart, x$latitudestart,
      type = "pie", chartdata = x[,c("Female", "Male")],
      colorPalette = c(ColorPalette[4], ColorPalette[1]),
      width = 40 * log10(x$Total) / log10(max(x$Total)), 
      transitionTime = 0
    )
}

## Size distribution map ####

sizeDistributionMap <- function(data) {
  sdDatW <- data$sdDat %>% 
    filter(!is.na(longitudestart), !is.na(latitudestart)) %>% 
    tidyr::spread(., interval, count, fill = 0)
  sdDatW$total <- rowSums(sdDatW[,levels(data$sdDat$interval)])
  
  leaflet::leaflet() %>% 
    addTiles(urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/Ocean/World_Ocean_Base/MapServer/tile/{z}/{y}/{x}",
             attribution = "Tiles &copy; Esri &mdash; Sources: GEBCO, NOAA, CHS, OSU, UNH, CSUMB, National Geographic, DeLorme, NAVTEQ, and Esri") %>% 
    addMinicharts(
      sdDatW$longitudestart, sdDatW$latitudestart,
      type = "pie", chartdata = sdDatW[,levels(data$sdDat$interval)],
      colorPalette = viridis::viridis(5),
      width = 40 * log10(sdDatW$total) / log10(max(sdDatW$total)), 
      transitionTime = 0
    ) 
}

## Length distribution plot ####

lengthDistributionPlot <- function(data) {
  ggplot(data$ldDat, aes(x = length, after_stat(count), color = sex)) +
    geom_density(adjust = 0.5) +
    xlab(paste0("Total length (", data$units$length, ")")) +
    ylab("Count density") +
    scale_color_manual("Sex", values = c("Female" = ColorPalette[4], "Male" = ColorPalette[1], "Unidentified" = ColorPalette[2])) +
    coord_cartesian(expand = FALSE) +
    theme_classic(base_size = 14)
}

## Stage distribution plot ####

stageDistributionPlot <- function(data, selectedStage) {
  
  stageName <- c("maturationstage" = "Maturation stage", "specialstage" = "Special stage")
  stageName <- unname(stageName[names(stageName) == selectedStage])
  tmp <- data$ldDat %>% rename(stage = all_of(selectedStage)) %>% filter(sex != "Unidentified" & !is.na(stage))
  
  
  ggplot(tmp, aes(x = length, fill = as.factor(stage))) +
    geom_histogram(bins = 30) +
    xlab(paste0("Total length (", data$units$length, ")")) +
    ylab("Count") +
    facet_wrap(~sex, ncol = 2, scales = "free_y") +
    scale_fill_discrete(stageName) +
    coord_cartesian(expand = FALSE) +
    theme_classic(base_size = 14)
}
