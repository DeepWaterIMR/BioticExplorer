### Define operating system

if (Sys.info()["sysname"] == "Windows") {
  os <- "Win"
} else if (Sys.info()["sysname"] == "Darwin") {
  os <- "Mac"
} else if (Sys.info()["sysname"] == "Linux") {
  os <- "Linux"
}

## Set the database alternative ("local" for locally run MonetDB, "server" for the MonetDB on Eucleia, "desktop" for no database connection)

if(Sys.getenv(c("SERVER_MODE"))=="") {
  dbHost <- "localhost"
  dbIndexPath <- "~/IMR_biotic_BES_database/dbIndex.rda"
} else {
  dbHost <- "dbserver"
  dbIndexPath <- "/Documents/IMR_biotic_BES_database/dbIndex.rda"
}

dbFound <- FALSE

## Libraries required to run the app ####

require(shiny)

### OS specific exceptions

if(os == "Linux") {
  if(!capabilities()["X11"] && capabilities()["cairo"]) {
    options(bitmapType = 'cairo')
  }
}

## Install pre-requisites
source("R/install_requirements.R")

## Source functions used by the app

source("R/other_functions.R", encoding = "utf-8")
source("R/processBiotic_functions.R", encoding = "utf-8")
source("R/figure_functions.R", encoding = "utf-8")
if(!interactive()) pdf(NULL) # To fix a crash with fishmethods::growth() on the server

##____________________
## User interface ####

##..............
## Settings  and definitions ####

tagList(
  tags$head(
    tags$title("BioticExplorer"),
    tags$style(
      HTML(
        ".checkbox-inline { 
                    margin-left: 0px;
                    margin-right: 10px;
          }
         .checkbox-inline+.checkbox-inline {
                    margin-left: 0px;
                    margin-right: 10px;
          }
        "
      )
    ) 
  )
)

stationOverviewFigureList <- list("Species composition" = "speciesCompositionPlot",
                                  "Total catch weight" = "catchweightSumPlot",
                                  "Mean catch weight" = "catchweightMeanPlot",
                                  "Catch weight range" = "catchweightRangePlot",
                                  "Mean weight of specimen" = "catchIndMeanWeightPlot",
                                  "Mean number in catch" = "catchcountMeanPlot", 
                                  "Range of number in catch" = "catchcountRangePlot",
                                  "Total catch by gear type" = "gearCatchPlot", 
                                  "Station depth" = "stationDepthPlot", 
                                  "Fishing depth of the six most dominant species" = "catchSpeciesWeightPlot"
)

stationMapList <- list("Total catch map" = "catchMap", "Catch composition map" = "catchCompMap")

individualOverviewFigureList <- list("Length distribution of species" = "indLengthPlot", "Weight distribution of species" = "indWeightPlot")

speciesFigureList <- list("Length-weight" = "lwPlot", "Growth" = "laPlot", "Maturity" = "l50Plot", "Sex ratio map" = "sexRatioMap", "Length distribution map" = "sizeDistributionMap", "Length/sex disrtibution" = "lengthDistributionPlot", "Length/stage distribution" = "stageDistributionPlot")

## Find the database

if("~/IMR_biotic_BES_database/bioticexplorer.duckdb" %>% 
   normalizePath() %>% 
   duckdb::duckdb(read_only = TRUE) %>% 
   DBI::dbCanConnect()) {
  con_db <-"~/IMR_biotic_BES_database/bioticexplorer.duckdb" %>% 
    normalizePath() %>% 
    duckdb::duckdb(read_only = TRUE) %>% 
    DBI::dbConnect()
  dbFound <- TRUE
}

if(dbFound) {
  message("Database found. Enabling server version.")
  if(file.exists(dbIndexPath)) {
    load(dbIndexPath, envir = .GlobalEnv)
    message("dbIndexPath found. Loading the database index.")
  }
} else {
  message("Database not found. Enabling desktop version.")
}

##............
## Header ####

header <- dashboardHeader(title = div(
  fluidRow(
    column(width = 1, 
           loadingLogo("https://www.hi.no", "logo.png", "logo_bw.png", 
                       height = "40px")), 
    column(width = 10, p("Biotic Explorer", align = "center"))
  ), use_bs_tooltip()
),
dropdownMenu(type = "notifications", headerText = scan("VERSION", what = "character", quiet = TRUE),
             icon = icon("cog"), badgeStatus = NULL,
             notificationItem("Usage of Biotic Explorer", icon = icon("book"), status = "info", href = "https://github.com/MikkoVihtakari/BioticExplorer"),
             notificationItem("Data fields", icon = icon("question-circle"), status = "info", href = "http://www.imr.no/formats/nmdbiotic/v3/nmdbioticv3_en.html"),
             notificationItem("Data types and codes", icon = icon("question-circle"), status = "info", href = "https://hinnsiden.no/tema/forskning/PublishingImages/Sider/SPD-gruppen/H%C3%A5ndbok%205.0%20juli%202019.pdf#search=h%C3%A5ndbok%20pr%C3%B8vetaking"),
             notificationItem("Download NMD data", icon = icon("download"), status = "info", href = "https://datasetexplorer.hi.no/"),
             notificationItem("Data policy", icon = icon("creative-commons"), status = "info", href = "https://www.hi.no/resources/Data-policy-HI.pdf")
)
)

##_____________
## Sidebar ####

sidebar <- dashboardSidebar(sidebarMenu(
  # Setting id makes input$tabs give the tabName of currently-selected tab
  id = "tabs",
  
  menuItem("Information", tabName = "info", icon = icon("info-circle")),
  
  menuItem("Load data & filter", tabName = "info", icon = icon("arrow-circle-up"),
           menuSubItem("From the database", tabName = "uploadDb", icon = icon("database")),
           menuSubItem("From files", tabName = "upload", icon = icon("file-code"))
           
  ),
  
  menuItem("Cruise overview", icon = icon("bar-chart-o"), tabName = "missionExamine"
  ),
  
  menuItem("Stations & catches", icon = icon("ship"),
           menuSubItem("Overview", tabName = "stnallOverview"),
           menuSubItem("Map of catches", tabName = "stnallMap"),
           menuSubItem("Examine data", tabName = "stnallExamine")
  ),
  
  menuItem("Individuals & ages", icon = icon("fish"),
           menuSubItem("Overview", tabName = "indallOverview"),
           menuSubItem("Species plots", tabName = "indallSpecies"),
           menuSubItem("Examine data", tabName = "indallExamine")
  ),
  
  menuItem("Hierarchical data tables", tabName = "bioticTables", icon = icon("sort-by-attributes", lib = "glyphicon"),
           menuSubItem("Station data", icon = icon("bar-chart-o"), tabName = "fishstationExamine"),
           menuSubItem("Catch data", icon = icon("bar-chart-o"), tabName = "catchsampleExamine"),
           menuSubItem("Individual data", icon = icon("bar-chart-o"), tabName = "individualExamine"),
           menuSubItem("Age data", icon = icon("bar-chart-o"), tabName = "agedeterminationExamine")    
  ),
  
  menuItem("Download", icon = icon("arrow-circle-down"),
           menuSubItem("Data", tabName = "downloadDatasets", icon = icon("table")),
           menuSubItem("Figures", tabName = "exportFigures", icon = icon("file-image"))
  )
  
)
)

##..........
## Body ####

body <- 
  dashboardBody(
    tabItems(
      
      ## Info tab ####   
      
      tabItem("info", 
              fluidRow(
                column(width = 12,
                       h1("Welcome to the Biotic Explorer", align = "center"),
                       br(),
                       p("This is a", a("Shiny app", href = "http://shiny.rstudio.com"), "allowing data exploration of the Institute of Marine Research's database (IMR). The app can also be used to examine Norwegian Maritime Data-center (NMD) Biotic v3 .xml files. Click the", strong("'Load data & filter'"), "tab on the side panel to get started. Note that the IMR logo turns to a 'BUSY' symbol when the app is processing information. Avoid clicking anything while the app is busy."),
                       h3("Usage"),
                       h4("Read"),
                       p(strong("Database:"), "Click 'Load data & filter -> From the database'. Select the desired data and click 'Send inquiry'. The 'BUSY' symbol on the left top corner will disappear when the operation is done. This may take time depending on the size of the selected dataset. You will get an overview of selected data and positions on the right. You can now further limit the dataset using the 'Subset' button and reset the data selection using the 'Reset' button."),
                       p(strong("File:"), "Click 'Load data & filter -> From files -> Browse..' and select one or multiple .xml files from your computer. An overview of data and sampling station locations will be shown below. Use the 'Filter data by' options to select data you want to keep. Click the 'Subset' button once you are ready and see how the overview will change based on the information you selected. The 'Reset' button will reset the selection."),
                       p(strong("Resume a previous session:"), "Click 'Load data & filter -> From files -> Browse..' and open an .rds file saved using the app (see 'Download'). You can now continue working on data from an earlier Biotic Explorer session."),
                       h4("Examine"),
                       p(strong("Cruise overview:"), "Click the 'Cruise overview' tab to see all cruises in the dataset. These data comprise of the '$mission' element in NMD Biotic files."),
                       p(strong("Stations & catches:"), "The 'Overview' tab lists selected plots to establish an overview of the '$fishstation' and '$catchsample' elements in NMD Biotic files. 'Map of catches' tab shows a location overview of catches and the 'Examine data' the data in a tabular form."),
                       p(strong("Individuals & ages:"), "The 'Overview' tab shows a general overview of the '$individual' and '$agedetermination' elements in NMD Biotic files. The 'Species plots' tab can be used to generate plots describing basic life-history parameters of a species with sufficient data and the 'Examine data' tab shows the data in a tabular form."),
                       p(strong("Hierarchical data tables"), "tab shows the data in NMD Biotic hierarchical format."),
                       h4("Download"),
                       p(strong("Data:"), "Data from a Biotic Explorer session can be downloaded using the 'Download -> Data' tab. If you want to reopen the data in Biotic Explorer or open the data in R, use the 'R' option without changing 'Data to download' options. This will save the data as an .rds file, which can be opened using the", a("'readRDS'", href = "https://stat.ethz.ch/R-manual/R-devel/library/base/html/readRDS.html"), "function in R and reopened using Biotic Explorer. Data can also be downloaded as .zip compressed .csv files or as an Excel file. The data are automatically placed to tabs in Excel files."),
                       p(strong("Figures:"), "You can select which Biotic Explorer figures to download and in which format using the 'Download -> Figures' tab. If you want to modify the figures beyond the options given in the app, you may", a("download Biotic Explorer", href = "https://github.com/MikkoVihtakari/BioticExplorer"), "and modify the figure functions listed under 'R/figure_functions.R'."),
                       br(),
                       br(),
                       h5("Authors: The StoX project team (Mikko Vihtakari, Ibrahim Umar)", align = "left"),
                       h5("Contact person: Mikko Vihtakari (mikko.vihtakari@hi.no)", align = "left"),
                       h5("(c) Institute of Marine Research, Norway, acknowledging the", a("RStudio team, Shiny developers", href = "https://www.rstudio.com/about/"), "and the", a("community", hfer = "https://stackoverflow.com/questions/tagged/shiny"), align = "left")
                )
              )
      ),
      
      ## Upload file tab ####
      
      tabItem("upload",
              
              fluidRow(
                
                ## Column 1 ###
                column(width = 6,
                       
                       box(
                         title = "1. Upload NMD Biotic files", status = "primary", solidHeader = TRUE,
                         width = NULL, collapsible = TRUE, 
                         
                         p("Here you can open a local file from your computer. BioticExplorer accepts the standard NMD Biotic V3 xml files and R native rds files. The R files can be used to return to a previous data manipulation instance using this app. Use the 'R' download file type in the Download data tab to enable this feature."),
                         
                         fileInput("file1",
                                   label = "Choose xml or rds input file",
                                   multiple = TRUE,
                                   accept = c(".xml", ".rds")
                         ),
                         "Note that processing takes some time for large files even after 'Upload complete' shows up. Be patient.",
                         hr(),
                         
                         strong("Drop excess data:"),
                         checkboxInput("removeEmpty", "Remove empty columns", TRUE)
                       ), 
                       
                       box(
                         title = "Selected data", width = NULL, status = "primary",
                         valueBoxOutput("nCruisesBox"),
                         valueBoxOutput("nStationsBox"),
                         valueBoxOutput("nYearsBox"),
                         valueBoxOutput("nGearsBox"),
                         valueBoxOutput("nSpeciesBox"),
                         valueBoxOutput("nMeasuredBox"),
                         valueBoxOutput("DateStartBox", width = 6),
                         valueBoxOutput("DateEndBox", width = 6)
                       )
                ),
                
                ## Column 2 ###
                
                column(width = 6,
                       
                       box(
                         title = "2. Filter data by", status = "primary", solidHeader = TRUE,
                         collapsible = TRUE, width = NULL, 
                         
                         fluidRow(
                           column(6, 
                                  selectizeInput(inputId = "subYear", label = "Year:",
                                                 choices = NULL, multiple = TRUE, options = list(create = TRUE, createFilter = "^\\d+$|^\\d+:\\d+$"))
                                  %>% bs_embed_tooltip(title = "Range is supported (e.g., 1990:1995). Write manually and click \"Add ...\".", trigger="focus"),
                                  selectizeInput(inputId = "subCruise", label = "Cruise number:",
                                                 choices = NULL, multiple = TRUE),
                                  selectizeInput(inputId = "subPlatform", label = "Platform name:",
                                                 choices = NULL, multiple = TRUE),
                                  dateRangeInput(inputId = "subDate", label = "Date:",
                                                 start = "1900-01-01", startview = "decade", weekstart = 1)
                           ),
                           
                           column(6,
                                  selectizeInput(inputId = "subSpecies", label = "Species:", 
                                                 choices = NULL, multiple = TRUE),
                                  selectizeInput(inputId = "subSerialnumber", 
                                                 label = "Serial number:",
                                                 choices = NULL, multiple = TRUE, options = list(create = TRUE, createFilter = "^\\d+$|^\\d+:\\d+$"))
                                  %>% bs_embed_tooltip(title = "Range is supported (e.g., 10000:10010). Write manually and click \"Add ...\".", trigger="focus"),
                                  selectizeInput(inputId = "subGear", label = "Gear code:",
                                                 choices = NULL, multiple = TRUE, options = list(create = TRUE, createFilter = "^\\d+$|^\\d+:\\d+$"))
                                  %>% bs_embed_tooltip(title = "Range is supported (e.g., 3700:3710). Write manually and click \"Add ...\".", trigger="focus"),
                                  selectizeInput(inputId = "subMissionType", label = "Mission type:",
                                                 choices = NULL, multiple = TRUE)
                           )),
                         
                         
                         sliderInput(inputId = "subLon", label = "Longitude:", min = -180, 
                                     max = 180, value = c(-180, 180)),
                         sliderInput(inputId = "subLat", label = "Latitude:", min = -90, 
                                     max = 90, value = c(-90, 90)),
                         
                         actionButton(inputId = "Subset", label = "Subset"),
                         actionButton(inputId = "Reset", label = "Reset"),
                         bs_button("Show active filter", button_type = "info") %>% bs_attach_collapse("ex_collapse2"),
                         br(),
                         bs_collapse(
                           id = "ex_collapse2",
                           content = tags$div(textOutput("activeFilter2"))
                         )
                         
                         
                         #, verbatimTextOutput("test") # For debugging
                         
                       ),
                       
                       box(title = "Station locations", status = "primary", width = NULL,
                           leafletOutput(outputId = "stationMap")
                       )
                )
                
                ## End columns ###
                
              )
      ),
      
      ### Database tab ####
      
      tabItem("uploadDb",
              
              fluidRow(
                
                ## Column 1 ###
                column(width = 6,
                       
                       box(
                         title = "1. Select data from the IMR database", 
                         status = "primary", solidHeader = TRUE,
                         width = NULL, collapsible = TRUE, 
                         
                         conditionalPanel(
                           condition = "output.serverVersion == true",
                           
                           conditionalPanel(condition = "output.fetchedDb == false",
                                            p("The IMR database contains tens of gigabytes of data. It is therefore important to select only the data you need before sending an inquiry. The estimated size of the database can be seen on the right. Once you have selected the needed data using this box, press the 'Send inquiry' button. Note that the data processing will take some time. Please, be patient. The server or your browser have probably not crashed even though you see nothing happening.")
                                            
                           ),
                           
                           conditionalPanel(condition = "output.fetchedDb == true",
                                            p("You have now selected data from the database and can see the overview on the right. You can use the 'Subset' button to further limit the data selection and the 'Reset' button to reload the entire database. Once you are happy with your dataset, you may proceed to other tabs in this application.")
                                            
                           ),
                           
                           strong("Drop excess data:"),
                           fluidRow(
                             column(4, checkboxInput("removeEmpty", "Remove empty columns", TRUE))
                           ),
                           
                           fluidRow(
                             column(6, 
                                    selectizeInput(inputId = "selMissionTypeDb", 
                                                   label = "Mission type:",
                                                   choices = NULL, multiple = TRUE),
                                    
                                    selectizeInput(inputId = "selCruiseSeriesDb", 
                                                   label = "Cruise series:",
                                                   choices = NULL, multiple = TRUE),
                                    
                                    selectizeInput(inputId = "selCruiseDb", 
                                                   label = "Cruise number:",
                                                   choices = NULL, multiple = TRUE),
                                    
                                    selectizeInput(inputId = "selFDIRAreaDb", 
                                                   label = "FDIR Statistical area:",
                                                   choices = NULL, multiple = TRUE),
                                    
                                    selectizeInput(inputId = "selICESAreaDb", 
                                                   label = "FAO area:",
                                                   choices = NULL, multiple = TRUE),
                                    
                                    dateRangeInput(inputId = "selDateDb", label = "Date:",
                                                   startview = "decade", weekstart = 1)
                             ),
                             
                             column(6,
                                    selectizeInput(inputId = "selYearDb", 
                                                   label = "Year:",
                                                   choices = NULL, multiple = TRUE, options = list(create = TRUE, createFilter = "^\\d+$|^\\d+:\\d+$"))
                                    %>% bs_embed_tooltip(title = "Range is supported (e.g., 1990:1995). Write manually and click \"Add ...\".", trigger="focus"),
                                    selectizeInput(inputId = "selSpeciesDb", 
                                                   label = "Species:", 
                                                   choices = NULL, multiple = TRUE),
                                    
                                    selectizeInput(inputId = "selPlatformDb", 
                                                   label = "Platform name:",
                                                   choices = NULL, multiple = TRUE),
                                    
                                    selectizeInput(inputId = "selSerialnumberDb", 
                                                   label = "Serial number:",
                                                   choices = NULL, multiple = TRUE, options = list(create = TRUE, createFilter = "^\\d+$|^\\d+:\\d+$"))
                                    %>% bs_embed_tooltip(title = "Range is supported (e.g., 10000:10010). Write manually and click \"Add ...\".", trigger="focus"),
                                    
                                    selectizeInput(inputId = "selGearDb", 
                                                   label = "Gear code:",
                                                   choices = NULL, multiple = TRUE, options = list(create = TRUE, createFilter = "^\\d+$|^\\d+:\\d+$"))
                                    %>% bs_embed_tooltip(title = "Range is supported (e.g., 3700:3710). Write manually and click \"Add ...\".", trigger="focus"),
                                    
                                    selectizeInput(inputId = "selGearCategoryDb", 
                                                   label = "Gear category:",
                                                   choices = "Not implemented yet", multiple = TRUE)
                                    
                                    
                                    
                             )),
                           
                           
                           sliderInput(inputId = "selLonDb", label = "Longitude:", min = -180, 
                                       max = 180, value = c(-180, 180)),
                           sliderInput(inputId = "selLatDb", label = "Latitude:", min = -90, 
                                       max = 90, value = c(-90, 90)),
                           
                           conditionalPanel(condition = "output.fetchedDb == false",
                                            actionButton(inputId = "doFetchDB", label = "Send inquiry") 
                           ),
                           
                           conditionalPanel(condition = "output.fetchedDb == true",
                                            actionButton(inputId = "SubsetDB", label = "Subset"),
                                            actionButton(inputId = "ResetDB", label = "Reset"),
                                            bs_button("Show active filter", button_type = "info") %>% bs_attach_collapse("ex_collapse"),
                                            br(),
                                            bs_collapse(
                                              id = "ex_collapse",
                                              content = tags$div(textOutput("activeFilter"))
                                            )
                           ),
                         ),
                         
                         conditionalPanel(
                           condition = "output.serverVersion == false",
                           h4("Desktop version. The database is not available", align = "center"),
                           p("If you are trying to run the app as a server version, make sure that the database is defined correctly.", align = "center")
                         )
                         
                       )
                ),
                ## Column 2 ###
                column(width = 6,
                       conditionalPanel(
                         condition = "output.fetchedDb == false",
                         box(
                           title = "All data in the database", width = NULL, status = "primary",
                           textOutput("snapshotTime"),
                           br(),
                           valueBoxOutput("EstYearsBox", width = 6),
                           valueBoxOutput("EstSpeciesBox", width = 6),
                           valueBoxOutput("EstGearsBox", width = 6),
                           valueBoxOutput("EstVesselsBox", width = 6),
                           valueBoxOutput("EstStationsBox", width = 6),
                           valueBoxOutput("EstMeasuredBox", width = 6),
                           valueBoxOutput("EstDateStartBox", width = 6),
                           valueBoxOutput("EstDateEndBox", width = 6)
                           # ,verbatimTextOutput("test") # For debugging
                         )
                       ),
                       conditionalPanel(
                         condition = "output.fetchedDb == true",
                         box(
                           title = "Selected data", width = NULL, status = "primary",
                           valueBoxOutput("nCruisesBoxDb"),
                           valueBoxOutput("nStationsBoxDb"),
                           valueBoxOutput("nYearsBoxDb"),
                           valueBoxOutput("nGearsBoxDb"),
                           valueBoxOutput("nSpeciesBoxDb"),
                           valueBoxOutput("nMeasuredBoxDb"),
                           valueBoxOutput("DateStartBoxDb", width = 6),
                           valueBoxOutput("DateEndBoxDb", width = 6)
                           # ,verbatimTextOutput("test") # For debugging
                         ),
                         box(title = "Station locations", status = "primary", width = NULL,
                             leafletOutput(outputId = "stationMapDb")
                         )
                       )
                )
              )
              
      ),
      
      ### Stnall tab ####      
      tabItem("stnallOverview",
              fluidRow(
                box(title = "Species composition", width = 12, status = "info", 
                    solidHeader = TRUE, plotOutput("speciesCompositionPlot")
                ),
                
                box(title = "Total catch weight", width = 12, status = "info", 
                    solidHeader = TRUE, plotlyOutput("catchweightSumPlot")
                ),
                
                box(title = "Catch weight mean and standard error (stations containing the species are replicates)", width = 12, status = "info", solidHeader = TRUE, plotOutput("catchweightMeanPlot")),
                
                box(title = "Catch weight range", width = 12, status = "info", solidHeader = TRUE, plotOutput("catchweightRangePlot")),
                
                box(title = "Mean weight of specimen", width = 12, status = "info", solidHeader = TRUE, plotOutput("catchIndMeanWeightPlot")),
                
                box(title = "Mean number in catch and standard error", width = 12, status = "info", solidHeader = TRUE, plotOutput("catchcountMeanPlot")),
                
                box(title = "Range of number in catch", width = 12, status = "info", solidHeader = TRUE, plotOutput("catchcountRangePlot")),
                
                box(title = "Total (summed) catch by gear type", width = 12, status = "info", solidHeader = TRUE, plotOutput("gearcatchPlot", height = "600px")),
                
                box(title = "Station depth", width = 12, status = "info", solidHeader = TRUE, plotOutput("stationDepthPlot")),
                box(title = "Fishing depth of six most dominant species", width = 12, status = "info", solidHeader = TRUE, plotOutput("catchSpeciesWeightPlot"))
              )
      ),
      
      tabItem("stnallMap", 
              fluidRow(
                box(title = "Map of catches (in kg)", width = 12, status = "info", 
                    solidHeader = TRUE, height = 840,
                    selectInput("catchMapSpecies", "Select species:", 
                                choices = NULL),
                    leafletOutput(outputId = "catchMap", height = 700)
                ),
                
                box(title = "Catch composition", width = 12, status = "info", 
                    solidHeader = TRUE, height = 760,
                    leafletOutput(outputId = "catchCompMap", height = 700)
                )
              )
      ),
      
      tabItem("stnallExamine", DT::dataTableOutput("stnall")),
      
      ### Indall tab ####
      
      tabItem("indallOverview", 
              fluidRow(
                box(title = "Individual sample overview", width = 12, status = "info", 
                    solidHeader = TRUE,
                    DT::dataTableOutput("individualSummaryTable"),
                    br(),
                    box(title = "Median length and distribution of species with > 10 length measurements", width = 12, status = "info", solidHeader = TRUE, plotOutput("indLengthPlot")),
                    box(title = "Median weight and distribution of species with > 10 weight measurements", width = 12, status = "info", solidHeader = TRUE, plotOutput("indWeightPlot"))
                )
              )
      ),
      
      tabItem("indallSpecies", 
              fluidRow(
                box(title = "Select species", width = 12, status = "info", 
                    solidHeader = TRUE, 
                    selectInput("indSpecies", "Select species:", choices = NULL),
                    radioButtons("lengthUnit", "Fish length unit:",
                                 c("Millimeter" = "mm",
                                   "Centimeter" = "cm",
                                   "Meter" = "m"),
                                 selected = "m",
                                 inline = TRUE),
                    
                    radioButtons("weightUnit", "Fish weight unit:",
                                 c("Grams" = "g",
                                   "Kilograms" = "kg"),
                                 selected = "kg",
                                 inline = TRUE)
                ),
                
                ### weightData
                
                box(title = "Length - weight relationship", width = 12, status = "info", 
                    solidHeader = TRUE, 
                    
                    conditionalPanel(
                      condition = "output.weightData == true",
                      
                      plotlyOutput("lwPlot"),
                      
                      br(),
                      column(4,
                             checkboxInput("lwPlotLogSwitch", "Logarithmic axes", FALSE)),
                      # column(8,
                      #        actionButton("lwPlotExcludeSwitch", "Exclude points"),
                      #        actionButton("lwPlotResetSwitch", "Reset")),
                      column(12,
                             verbatimTextOutput("lwPlotText"))
                    ),
                    
                    conditionalPanel(
                      condition = "output.weightData == false",
                      h4("Weight data not available for the species.", align = "center")
                    )
                ),
                
                ### ageData
                
                box(title = "Age - length relationship", width = 12, status = "info", 
                    solidHeader = TRUE, 
                    
                    conditionalPanel(
                      condition = "output.ageData == true",
                      
                      p("Here you can estimate a growth model for a species. The growth models are fitted using the fishmethods::growth function. Select the desired growth model from the drop-down menu. If you have enough data, you can separate these models by sex. If there are not enough data for small individuals, you can try to force the model to a certain 0-group length. The strength of the forcing is defined using the 'Force 0-group strength' slider, which produces a number of age-0 fish of given length relative to the total number of all age-determined fish in the dataset."),
                      p("Please note that running this function with too little data or using non-sense 0-group lengths will make the app to crash."),
                      br(),
                      column(12,
                             splitLayout(
                               
                               selectInput("growthModelSwitch", "Growth model:", choices = list("von Bertalanffy" = "vout", "Gompertz" = "gout", "Logistic" = "lout"), selected = "vout"),
                               checkboxInput("laPlotSexSwitch", "Separate by sex", FALSE),
                               numericInput("forceZeroGroupLength", "Force 0-group length", value = NA, min = 0, step = 0.01),
                               sliderInput("forceZeroGroupStrength", "Force 0-group strength (%)", min = 1, max = 100, value = 10),
                               
                               cellWidths = c("25%", "15%", "30%", "30%"), cellArgs = list(style = "padding: 6px")
                               
                             )
                      ),
                      br(),
                      column(12,
                             plotlyOutput("laPlot"),
                      ),
                      # column(12,
                      # actionButton("laPlotExcludeSwitch", "Exclude points"),
                      # actionButton("laPlotResetSwitch", "Reset"),
                      # ),
                      
                      column(12,
                             verbatimTextOutput("laPlotText")
                      )
                    ),
                    
                    conditionalPanel(
                      condition = "output.ageData == false",
                      h4("Not enough age data available for the species.", align = "center")
                    )
                    
                ),
                
                ### maturityData
                
                box(title = "50% maturity based on length (L50)", width = 12, 
                    status = "info", solidHeader = TRUE,
                    
                    conditionalPanel(
                      condition = "output.maturityData == true",
                      plotOutput("l50Plot"),
                      verbatimTextOutput("l50PlotText")
                    ),
                    
                    conditionalPanel(
                      condition = "output.maturityData == false",
                      h4("Not enough maturity data available for the species.", align = "center")
                    )
                    
                ),
                
                ### sexData
                
                box(title = "Sex ratio", width = 12, status = "info", 
                    solidHeader = TRUE, height = 760,
                    
                    conditionalPanel(
                      condition = "output.sexData == true",
                      leafletOutput(outputId = "sexRatioMap", height = 700)
                    ),
                    
                    conditionalPanel(
                      condition = "output.sexData == false",
                      h4("Sex data not available for the species.", align = "center")
                    )
                    
                ),
                
                ### lengthData
                
                box(title = "Geographic length distribution", width = 12, status = "info", 
                    solidHeader = TRUE, height = 760,
                    
                    conditionalPanel(
                      condition = "output.lengthData == true",
                      leafletOutput(outputId = "sizeDistributionMap", height = 700)
                    ),
                    
                    conditionalPanel(
                      condition = "output.lengthData == false",
                      h4("Length data not available for the species.", align = "center")
                    )
                ),
                
                ### lengthDistributionData
                
                box(title = "Sex specific length distribution", width = 12, status = "info", 
                    solidHeader = TRUE, 
                    
                    conditionalPanel(
                      condition = "output.lengthDistributionData == true",
                      plotOutput(outputId = "lengthDistributionPlot")
                    ),
                    
                    conditionalPanel(
                      condition = "output.lengthDistributionData == false",
                      h4("Not enough sex specific length data available for the species.", align = "center")
                    )
                ),
                
                ### stageDistributionData
                
                box(title = "Stage specific length distribution", width = 12, status = "info", 
                    solidHeader = TRUE, 
                    
                    conditionalPanel(
                      condition = "output.stageDistributionData == true",
                      column(12,
                             radioButtons("stageSelectionSwitch", "Select stage", choices = list("Maturation stage" = "maturationstage", "Special stage" = "specialstage"),
                                          selected = "maturationstage", inline = TRUE)
                      ),
                      column(12,
                             plotOutput(outputId = "stageDistributionPlot")
                      )
                    ),
                    
                    conditionalPanel(
                      condition = "output.stageDistributionData == false",
                      h4("Not enough stage specific length data available for the species.", align = "center")
                    )
                )
              )
      ),
      
      tabItem("indallExamine", DT::dataTableOutput("indall")),
      
      ##..................
      ## NMD data tab ####
      
      tabItem("missionExamine", DT::dataTableOutput("missionTable")),
      tabItem("fishstationExamine", DT::dataTableOutput("fishstation")),
      tabItem("catchsampleExamine", DT::dataTableOutput("catchsample")),
      tabItem("individualExamine", DT::dataTableOutput("individualTable")),
      tabItem("agedeterminationExamine", DT::dataTableOutput("agedeterminationTable")),
      
      ##...................
      ## Download tab ####
      
      tabItem("downloadDatasets", 
              fluidRow(
                box(
                  title = "Download data", width = 12, status = "primary", solidHeader = TRUE,
                  
                  p("You can download data from your Biotic Explrer session here. If you want to reopen the data in Biotic Explorer or open the data in R, use the 'R' option without changing 'Data to download' options. This will save the data as an .rds file, which can be opened using the", a("'readRDS'", href = "https://stat.ethz.ch/R-manual/R-devel/library/base/html/readRDS.html"), "function in R and reopened using Biotic Explorer. Data can also be downloaded as .zip compressed .csv files or as an Excel file. The data are automatically placed to tabs in Excel files."),
                  
                  br(),
                  
                  splitLayout(
                    
                    radioButtons("downloadFileType", "Download file type:",
                                 c("R" = ".rda",
                                   "csv" = ".csv",
                                   "Excel" = ".xlsx"),
                                 selected = ".rda"
                    ),
                    
                    checkboxGroupInput("downloadDataType", "Data to download:",
                                       c("Cruise overview" = "mission",
                                         "Stations & catches" = "stnall",
                                         "Individuals & ages" = "indall"
                                         #"Original format" = "original"
                                       ),
                                       selected = c("mission", "stnall", "indall")
                    ), 
                    cellArgs = list(style = "padding: 6px")
                  ),
                  
                  downloadButton(outputId = "downloadData")
                  # verbatimTextOutput("test")
                ) 
              )
      ),
      
      ##........................
      ## Export figures tab ####
      
      tabItem("exportFigures",
              fluidRow(
                box(title = "1. Select the figures to export", width = 12, status = "info", 
                    solidHeader = TRUE,
                    
                    p("This tab is still under development and does not work as intended yet."),
                    
                    checkboxGroupInput("cruiseMapExport", label = h4("Station map"), 
                                       choices = list("Station map" = 1),
                                       selected = NULL, inline = TRUE),
                    
                    conditionalPanel(condition = "output.weightData == true", 
                                     radioButtons("plotCruiseTrack", "Cruise track:", 
                                                  choices = list("Do not plot" = "No", "From station sequence" = "Stations", "From file" = "File"),
                                                  selected = "No",
                                                  inline = TRUE
                                     )
                    ),
                    
                    conditionalPanel(condition = "input.plotCruiseTrack == 'File'",
                                     fileInput("cruiseTrackFile",
                                               label = "Upload cruise track",
                                               multiple = TRUE,
                                               accept = NA)
                    ),
                    
                    checkboxGroupInput("stationOverviewExport", 
                                       label = h4("Station overview figures"), 
                                       choices = stationOverviewFigureList,
                                       selected = NA, inline = FALSE),
                    
                    actionLink("selectAllStationOverviewExport", "Select/deselect all"),
                    
                    h4("Station based maps"),
                    
                    splitLayout(cellWidths = c("25%", "40%"),
                                checkboxGroupInput("stationCatchMapExport", label = NULL, choices = stationMapList[1]),
                                selectInput("catchMapExportSpecies", NULL, choices = NULL)
                    ),
                    
                    checkboxGroupInput("stationMapExport", label = NULL, 
                                       choices = stationMapList[-1],
                                       selected = NA, inline = TRUE),
                    
                    
                    actionLink("selectAllStationMapExport", "Select/deselect all"),
                    
                    checkboxGroupInput("individualOverviewExport", 
                                       label = h4("Individuals overview"), 
                                       choices = individualOverviewFigureList,
                                       selected = NA, inline = TRUE),
                    
                    h4("Species plots"),
                    
                    checkboxGroupInput("speciesFigureExport", 
                                       label = NULL, 
                                       choices = speciesFigureList,
                                       selected = NA, inline = TRUE)
                    
                ),
                
                box(title = "2. Download selected figures", width = 12, status = "info", 
                    solidHeader = TRUE,
                    
                    numericInput("figureWidth", "Figure width in cm (the height will be scaled automatically)", value = 18), 
                    
                    radioButtons("downloadFiguresAs", "Download as:", 
                                 choices = list("Figure files" = "File", "Cruise report template (not implemented)" = "Report"),
                                 selected = "File",
                                 inline = TRUE
                    ),
                    
                    
                    conditionalPanel(condition = "input.downloadFiguresAs == 'File'",
                                     radioButtons("downloadFigureFormat", "File format:",
                                                  choices = list("Png" = ".png", "Jpeg" = ".jpeg", "Pdf" = ".pdf"),
                                                  selected = ".png",
                                                  inline = TRUE)
                    ),
                    
                    conditionalPanel(condition = "input.downloadFiguresAs == 'Report'",
                                     radioButtons("downloadReportFormat", "File format:",
                                                  choices = list("Rmarkdown" = "Rmd", "Word" = "Doc", "Pdf" = "Pdf"),
                                                  selected = "Rmd",
                                                  inline = TRUE)
                    ),
                    
                    downloadButton(outputId = "downloadFigures")
                )
              )
      )
    )
  )

##.................
## Dashboard page ####

ui <- dashboardPage(header, sidebar, body)

##............
## Server ####

server <- shinyServer(function(input, output, session) {
  
  ## Options ####
  
  options(shiny.maxRequestSize = 1000*1024^2) ## This sets the maximum file size for upload. 1000 = 1 Gb. 
  
  output$serverVersion <- reactive(dbFound) 
  outputOptions(output, "serverVersion", suspendWhenHidden = FALSE)
  
  output$singleCruise <- reactive(FALSE) 
  outputOptions(output, "singleCruise", suspendWhenHidden = FALSE)
  
  source("R/filtering_functions.R", local = TRUE) ## Source functions for the session. See https://shiny.rstudio.com/articles/scoping.html
  
  rv <- reactiveValues(stnall = NULL, indall = NULL, mission = NULL, uploadDbclicked = FALSE) ## Create reactive values
  
  ## Read data file ####
  
  observeEvent(req(input$file1), {
    
    tryCatch({
      
      if (length(input$file1[[1]]) > 1) {
        rv$inputData <- processBioticFiles(files = input$file1$datapath, removeEmpty = input$removeEmpty, convertColumns = TRUE, returnOriginal = FALSE)
      } else {
        if(tolower(gsub("^.+\\.", "", input$file1$name)) == "rds") {
          rv$inputData <- readRDS(file = input$file1$datapath)
        } else {
          rv$inputData <- processBioticFile(file = input$file1$datapath, removeEmpty = input$removeEmpty, convertColumns = TRUE, returnOriginal = FALSE)
        }
      }
    },
    error = function(e) {
      stop(safeError(e))
    }
    )
    
    rv$stnall <- rv$inputData$stnall
    rv$indall <- rv$inputData$indall
    rv$mission <- rv$inputData$mission
    
    obsPopulatePanel()
    
  })
  
  ## Read database ####
  
  observeEvent(input$tabs, {
    if(input$tabs == "uploadDb") {
      if(dbFound) {
        
        if(!rv$uploadDbclicked) {
          output$fetchedDb <- reactive(FALSE)
          outputOptions(output, "fetchedDb", suspendWhenHidden = FALSE)
        }
        
        rv$uploadDbclicked <- TRUE
        
        rv$inputData$stnall <- dplyr::tbl(con_db, "stnall")
        rv$inputData$indall <- dplyr::tbl(con_db, "indall")
        rv$inputData$mission <- dplyr::tbl(con_db, "mission")
        
        if(DBI::dbExistsTable(con_db, "meta")) {
          rv$inputData$meta <- dplyr::tbl(con_db, "meta")
        } else {
          rv$inputData$meta <- NULL
        }
        
        # Copy the indexing script from BioticExplorerServer::indexDatabase. 
        if(!exists("index")) {
          index <- list()
          index$missiontypename <- rv$inputData$mission %>% select(missiontypename) %>% distinct() %>% pull() %>% sort()
          index$cruise <- rv$inputData$mission %>% select(cruise) %>% distinct() %>% pull() %>% sort()
          index$year <- rv$inputData$mission %>% select(startyear) %>% distinct() %>% pull() %>% sort()
          index$nstations <- rv$inputData$stnall %>% select(missionid, startyear, serialnumber) %>% distinct() %>% count() %>% pull()
          index$commonname <- rv$inputData$stnall %>% select(commonname) %>% distinct() %>% pull() %>% sort()
          index$platformname <- rv$inputData$stnall %>% select(platformname) %>% distinct() %>% pull() %>% sort()
          index$serialnumber <- rv$inputData$stnall %>% select(serialnumber) %>% distinct() %>% pull() %>% sort()
          index$gear <- rv$inputData$stnall %>% select(gear) %>% distinct() %>% pull() %>% sort()
          index$date <- rv$inputData$stnall %>% summarise(min = min(stationstartdate, na.rm = TRUE), max = max(stationstartdate, na.rm = TRUE)) %>% collect()
          index$nmeasured <- rv$inputData$indall %>% select(length) %>% count() %>% pull()
          if(!is.null(rv$inputData$meta)) {
            index$downloadstart <- rv$inputData$meta %>% select(timestart) %>% pull()
            index$downloadend <- rv$inputData$meta %>% select(timeend) %>% pull()
          } else {
            index$downloadstart <- NA
            index$downloadend <- NA
          }
          
          # Make index as global
          index <<- index
        }
        
        updateFilterform(loadDb = TRUE)
        
        output$EstStationsBox <- renderValueBox({
          
          valueBox(
            value = tags$p(paste(round(index$nstations/1000, 1), "k"), style = "font-size: 80%;"),
            subtitle = "Stations"
          )
        })
        
        output$EstMeasuredBox <- renderValueBox({
          
          valueBox(
            value = tags$p(paste(round(index$nmeasured/1e6, 1), "M"), style = "font-size: 80%;"),
            subtitle = "Measured specimen"
          )
        })
        
        output$EstYearsBox <- renderValueBox({
          valueBox(
            length(index$year),
            "Unique years"
          )
        })
        
        output$EstGearsBox <- renderValueBox({
          valueBox(
            length(index$gear),
            "Gear types"
          )
        })
        
        output$EstVesselsBox <- renderValueBox({
          valueBox(
            length(index$platformname),
            "Platforms"
          )
        })
        
        output$EstSpeciesBox <- renderValueBox({
          valueBox(
            length(index$commonname),
            "Unique species"
          )
        })
        
        output$EstDateStartBox <- renderValueBox({
          valueBox(
            value = tags$p(index$year %>% min(),
                           style = "font-size: 80%;"),
            subtitle = "First year"
          )
        })
        
        output$EstDateEndBox <- renderValueBox({
          valueBox(
            value = tags$p(index$year %>% max(),
                           style = "font-size: 80%;"),
            subtitle = "Last year"
          )
        })
        
        output$snapshotTime <- renderText({
          paste0("Database snapshot from ", index$downloadstart, ". Updates to the database are visible with one day delay.")
        })
        
      }
    }
    
  })
  
  ### Filter the database ####
  
  observeEvent(input$doFetchDB, {
    
    tmp <- makeFilterChain(db = TRUE)
    
    if(length(tmp$filterChain) > 0) {
      output$fetchedDb <- reactive(TRUE)
      rv$filterChain <- paste(tmp$filterChain, collapse = "; ")
      output$activeFilter <- renderText({rv$filterChain})
      
      rv$sub <- tmp$sub
      
      rv$stnall <- rv$inputData$stnall %>% filter(!!!rlang::parse_exprs(rv$filterChain)) %>% collect() %>% as.data.table()
      rv$indall <- rv$inputData$indall %>% filter(!!!rlang::parse_exprs(rv$filterChain)) %>% collect() %>% as.data.table()
      rv$mission <- rv$inputData$mission %>% filter(missionid %in% !!unique(rv$stnall$missionid)) %>% collect() %>% as.data.table()
      
      obsPopulatePanel(db = TRUE)
    }
  })
  
  #.................
  ## Test output ####
  
  # output$test <- renderText({
  #   #   # #   #
  #   #   # #   #   # length(input$file1[[1]])
  #   # paste(input$file1, collapse = "; ")
  #   paste(rv$fileext, collapse = "; ")
  #   #   # #   #   paste(rv$filterChain, collapse = "; ")
  #   #  paste(input$tabs, collapse = "; ")
  # })
  # 
  
  ##................... 
  ## Export figures tab
  
  observeEvent(c(input$selectAllStationOverviewExport), {
    
    if(input$selectAllStationOverviewExport > 0) {
      if(input$selectAllStationOverviewExport %% 2 == 1) {
        updateSelectInput(session, "stationOverviewExport", selected = stationOverviewFigureList)
      } else {
        updateSelectInput(session, "stationOverviewExport", selected = NA)
      }
    } 
    
  })
  
  observeEvent(c(input$selectAllStationMapExport), {
    
    if(input$selectAllStationMapExport > 0) {
      if(input$selectAllStationMapExport %% 2 == 1) {
        updateSelectInput(session, "stationMapExport", selected = stationMapList)
      } else {
        updateSelectInput(session, "stationMapExport", selected = NA)
      }
    } 
    
  })
  
  
  ##................
  ## Subsetting ####
  
  observeEvent(input$Subset, {
    
    tmp <- makeFilterChain()
    rv$filterChain <- paste(tmp$filterChain, collapse = "; ")
    output$activeFilter2 <- renderText({rv$filterChain})
    rv$sub <- tmp$sub
    
    rv$stnall <- rv$stnall %>% lazy_dt() %>% filter(!!!rlang::parse_exprs(rv$filterChain)) %>% collect() %>% as.data.table()
    rv$indall <- rv$indall %>% lazy_dt() %>% filter(!!!rlang::parse_exprs(rv$filterChain)) %>% collect() %>% as.data.table()
    rv$mission <- rv$mission %>% lazy_dt() %>% filter(missionid %in% !!unique(rv$stnall$missionid)) %>% collect() %>% as.data.table()
    
    obsPopulatePanel()
    
  })
  
  observeEvent(input$SubsetDB, {
    
    tmp <- makeFilterChain(db = TRUE)
    
    if(length(tmp$filterChain) > 0) {
      rv$filterChain <- paste(tmp$filterChain, collapse = "; ")
      output$activeFilter <- renderText({rv$filterChain})
      rv$sub <- tmp$sub
      
      rv$stnall <- rv$stnall %>% lazy_dt() %>% filter(!!!rlang::parse_exprs(rv$filterChain)) %>% collect() %>% as.data.table()
      rv$indall <- rv$indall %>% lazy_dt() %>% filter(!!!rlang::parse_exprs(rv$filterChain)) %>% collect() %>% as.data.table()
      rv$mission <- rv$mission %>% lazy_dt() %>% filter(missionid %in% !!unique(rv$stnall$missionid)) %>% collect() %>% as.data.table()
      
      obsPopulatePanel(db = TRUE)
    }
  })
  
  ##...............
  ## Resetting ####
  
  observeEvent(input$Reset, {
    
    output$activeFilter2 <- renderText({""})
    rv$stnall <- rv$inputData$stnall
    rv$indall <- rv$inputData$indall
    rv$mission <- rv$inputData$mission
    
    obsPopulatePanel()
    
  })
  
  observeEvent(input$ResetDB, {
    
    output$fetchedDb <- reactive(FALSE)
    output$activeFilter <- renderText({""})
    
    rv$stnall <- NULL
    rv$indall <- NULL
    rv$mission <- NULL
    
    updateFilterform(loadDb = TRUE)
    
  })
  
  ##.................
  ## Data tables ####
  
  output$stnall <- DT::renderDataTable({
    DT::datatable(rv$stnall, 
                  options = list(scrollX = TRUE, 
                                 pageLength = 20
                  ) 
    ) %>% formatRound(columns = c(FALSE, grepl("latitude|longitude|distance|weight|speed|soaktime", names(rv$stnall))), mark = " ")
  })
  
  output$indall <- DT::renderDataTable({
    DT::datatable(rv$indall, 
                  options = list(scrollX = TRUE, 
                                 pageLength = 20
                  ) 
    ) %>% formatRound(c("longitudestart", "latitudestart", "length", "individualweight"))
  })
  
  
  output$missionTable <- DT::renderDataTable({
    DT::datatable(rv$mission,
                  options = list(scrollX = TRUE, 
                                 pageLength = 20
                  ) 
    )
  })
  
  ### NMD data tables ####
  
  observeEvent(input$tabs, {
    
    ## Fish station ####
    if(input$tabs == "fishstationExamine") {
      
      cols <- colnames(rv$stnall)[colnames(rv$stnall) %in% RstoxData::xsdObjects$nmdbioticv3.xsd$tableHeaders$fishstation]
      
      rv$fishstation <- rv$stnall %>% lazy_dt() %>% 
        select(cols) %>% 
        distinct() %>% collect()
      
      output$fishstation <- DT::renderDataTable({
        
        DT::datatable(rv$fishstation,
                      options = list(scrollX = TRUE,
                                     pageLength = 20
                      )
        ) %>% formatRound(c("longitudestart", "latitudestart", "distance"))
      })
    }
    
    ## Catch sample ####
    if(input$tabs == "catchsampleExamine") {
      
      cols <- colnames(rv$stnall)[colnames(rv$stnall) %in% RstoxData::xsdObjects$nmdbioticv3.xsd$tableHeaders$catchsample]
      
      rv$catchsample <- rv$stnall %>% lazy_dt() %>% 
        select(cols) %>% 
        distinct() %>% collect()
      
      output$catchsample <- DT::renderDataTable({
        
        DT::datatable(rv$catchsample,
                      options = list(scrollX = TRUE,
                                     pageLength = 20
                      )
        ) %>% formatRound(c("catchweight", "lengthsampleweight"))
      })
    }
    
    ## Individual ####
    if(input$tabs == "individualExamine") {
      
      cols <- colnames(rv$indall)[colnames(rv$indall) %in% RstoxData::xsdObjects$nmdbioticv3.xsd$tableHeaders$individual]
      
      rv$individual <- rv$indall %>% lazy_dt() %>% 
        select(cols) %>% 
        distinct() %>% collect()
      
      output$individualTable <- DT::renderDataTable({
        DT::datatable(rv$individual,
                      options = list(scrollX = TRUE,
                                     pageLength = 20
                      )
        )
      })
    }
    
    ## Age ####
    if(input$tabs == "agedeterminationExamine") {
      
      cols <- colnames(rv$indall)[colnames(rv$indall) %in% RstoxData::xsdObjects$nmdbioticv3.xsd$tableHeaders$agedetermination]
      
      if("age" %in% cols) {
        rv$agedetermination <- rv$indall %>% lazy_dt() %>% 
          select(cols) %>% 
          distinct() %>% collect()
      } else {
        rv$agedetermination <- NULL
      }
      
      output$agedeterminationTable <- DT::renderDataTable({
        DT::datatable(rv$agedetermination,
                      options = list(scrollX = TRUE,
                                     pageLength = 20
                      )
        )
      })
    }
  })
  
  ##......................
  ## Stn data figures ####
  
  observeEvent(input$tabs, {
    if(input$tabs == "stnallOverview") {
      
      spOverviewDat <- speciesOverviewData(rv$stnall)
      
      ## Number of stations containing the species plot
      
      output$speciesCompositionPlot <- renderPlot(speciesCompositionPlot(spOverviewDat))
      
      ## Summed catch weight plot
      
      output$catchweightSumPlot <- renderPlotly({
        
        p <- catchweightSumPlot(spOverviewDat)
        
        ggplotly(p) %>% plotly::layout(annotations = list(x = 1, y= 1, xref = "paper", yref = "paper", text = paste("Total catch\n all species\n", round(sum(spOverviewDat$catchS$sum), 0), "kg"), showarrow = FALSE, font = list(size = 12)))
        
      })
      
      ## Mean catch weight plot
      
      output$catchweightMeanPlot <- renderPlot(catchweightMeanPlot(spOverviewDat))
      
      ## Catch weight range plot
      
      output$catchweightRangePlot <- renderPlot(catchweightRangePlot(spOverviewDat))
      
      ## Mean weight of fish in catch
      
      output$catchIndMeanWeightPlot <- renderPlot(catchIndMeanWeightPlot(spOverviewDat))
      
      ## Mean n in catch plot
      
      output$catchcountMeanPlot <- renderPlot(catchcountMeanPlot(spOverviewDat))
      
      ## N range in catch plot
      
      output$catchcountRangePlot <- renderPlot(catchcountRangePlot(spOverviewDat))
      
      ## Catch in gear plot
      
      output$gearcatchPlot <- renderPlot(gearCatchPlot(spOverviewDat))
      
      ## Station depth plot
      
      output$stationDepthPlot <- renderPlot(stationDepthPlot(spOverviewDat))
      
      ## Fishing depth plot 
      
      output$catchSpeciesWeightPlot <- renderPlot(catchSpeciesWeightPlot(spOverviewDat))
      
    } 
  })
  
  
  observeEvent(c(input$tabs,  input$catchMapSpecies), {
    if(input$tabs == "stnallMap") {
      
      ## Catch map ###
      
      output$catchMap <- renderLeaflet(catchMap(rv$stnall, species = input$catchMapSpecies))
      
    }
  }) 
  
  observeEvent(c(input$tabs,  input$catchMapSpecies), {
    if(input$tabs == "stnallMap") {
      
      spOverviewDat <- speciesOverviewData(rv$stnall)
      
      ## Catch composition map
      
      #if(nrow(spOverviewDat$compDatW) != 0) {
      output$catchCompMap <- renderLeaflet(catchCompMap(spOverviewDat))
      #}
      
    }
  }) 
  
  ##..........................................
  ## Ind data overview figures and tables ####
  
  observeEvent(input$tabs, {
    if(input$tabs == "indallOverview") {
      
      indSumTab <- rv$indall %>% lazy_dt() %>% 
        dplyr::group_by(commonname) %>% 
        dplyr::summarise(Total = n(), 
                         Length = {if("length" %in% names(.)) sum(!is.na(length)) else 0},
                         Weight = {if("individualweight" %in% names(.)) sum(!is.na(individualweight)) else 0}, 
                         Sex = {if("sex" %in% names(.)) sum(!is.na(sex)) else 0}, 
                         Maturationstage = {if("maturationstage" %in% names(.)) sum(!is.na(maturationstage)) else 0}, 
                         Specialstage = {if("specialstage" %in% names(.)) sum(!is.na(specialstage)) else 0}, 
                         Age = {if("age" %in% names(.)) sum(!is.na(age)) else 0},
                         Read = {if("readability" %in% names(.)) sum(!is.na(readability)) else 0}
        ) %>% arrange(-Total) %>% collect() 
      
      output$individualSummaryTable <- DT::renderDataTable({
        DT::datatable(indSumTab, options = list(searching = FALSE))
      })
      
      output$indLengthPlot <- renderPlot(indLengthPlot(indall = rv$indall))
      output$indWeightPlot <- renderPlot(indWeightPlot(indall = rv$indall))
    } 
  })
  
  ## Ind plots, selected species ####
  
  observe({
    
    ##..................
    ## Set switches ####
    
    output$ageData <- reactive(FALSE)
    output$weightData <- reactive(FALSE)
    output$maturityData <- reactive(FALSE)
    output$sexData <- reactive(FALSE)
    output$lengthData <- reactive(FALSE)
    output$lengthDistributionData <- reactive(FALSE)
    output$stageDistributionData <- reactive(FALSE)
    
    outputOptions(output, "ageData", suspendWhenHidden = FALSE)
    outputOptions(output, "weightData", suspendWhenHidden = FALSE)
    outputOptions(output, "maturityData", suspendWhenHidden = FALSE)
    outputOptions(output, "sexData", suspendWhenHidden = FALSE)
    outputOptions(output, "lengthData", suspendWhenHidden = FALSE)
    outputOptions(output, "lengthDistributionData", suspendWhenHidden = FALSE)
    outputOptions(output, "stageDistributionData", suspendWhenHidden = FALSE)
    
    if (input$tabs == "indallSpecies" & input$indSpecies != "" & !input$indSpecies %in% c("Select a species to generate the plots", "No species with sufficient data")) {
      
      ### Base individual data ####
      
      indOverviewDat <- individualFigureData(rv$indall, indSpecies = input$indSpecies, lengthUnit = input$lengthUnit, weightUnit = input$weightUnit, useEggaSystem = FALSE)
      
      ### Length-weight plot ####
      
      if (!is.null(indOverviewDat$lwDat)) {
        
        output$weightData <- reactive(TRUE)
        
        output$lwPlot <- renderPlotly({
          
          plotly::ggplotly(lwPlot(data = indOverviewDat, lwPlotLogSwitch = input$lwPlotLogSwitch))
          
        })
        
        output$lwPlotText <- renderText(paste0("Coefficients (calculated using cm and g): \n a = ", round(indOverviewDat$lwMod$a, 3), "; b = ", round(indOverviewDat$lwMod$b, 3), "\n Number of included specimens = ", nrow(indOverviewDat$lwDat), "\n Total number of measured = ", nrow(indOverviewDat$tmpBase), "\n Excluded (length or weight missing): \n Length = ", sum(is.na(indOverviewDat$tmpBase$length)), "; weight = ", sum(is.na(indOverviewDat$tmpBase$individualweight))))
        
      } 
      
      ### Age - length plot ####
      
      if (!is.null(indOverviewDat$laDat)) {
        
        output$ageData <- reactive(TRUE)
        
        LAPlot <- laPlot(data = indOverviewDat, laPlotSexSwitch = input$laPlotSexSwitch, 
                         growthModelSwitch = input$growthModelSwitch, forceZeroGroupLength = input$forceZeroGroupLength, 
                         forceZeroGroupStrength = input$forceZeroGroupStrength
        )
        
        output$laPlot <- renderPlotly(ggplotly(LAPlot$laPlot))
        output$laPlotText <- renderText(LAPlot$laText)
        
      }
      
      ### L50 maturity plot  ####
      
      if (!is.null(indOverviewDat$l50Dat)) {  
        
        output$maturityData <- reactive(TRUE)
        
        L50Plot <- l50Plot(data = indOverviewDat)
        
        output$l50Plot <- renderPlot(L50Plot$Plot)
        output$l50PlotText <- renderText(L50Plot$Text)
        
      } 
      
      ## Sex ratio map ####
      
      if (!is.null(indOverviewDat$srDat)) {
        output$sexData <- reactive(TRUE)
        output$sexRatioMap <- renderLeaflet(sexRatioMap(data = indOverviewDat))
      } 
      
      ## Size distribution map ####
      
      if (!is.null(indOverviewDat$sdDat)) { 
        output$lengthData <- reactive(TRUE)
        output$sizeDistributionMap <- renderLeaflet(sizeDistributionMap(data = indOverviewDat))
      }
      
      ## Length distribution plot ####
      
      if (!is.null(indOverviewDat$ldDat)) { 
        output$lengthDistributionData <- reactive(TRUE)
        output$lengthDistributionPlot <- renderPlot(lengthDistributionPlot(data = indOverviewDat))
      }
      
      ## Stage distribution plot ####
      
      if (!is.null(indOverviewDat$ldDat)) { 
        if(nrow(na.omit(indOverviewDat$ldDat[input$stageSelectionSwitch])) > 10) {
          output$stageDistributionData <- reactive(TRUE)
          output$stageDistributionPlot <- renderPlot(stageDistributionPlot(data = indOverviewDat, selectedStage = input$stageSelectionSwitch))
        }
      }
    } 
    
  }) 
  
  ##..............
  ## Download ####
  
  ### Download figures ####
  
  output$downloadFigures <- downloadHandler(
    
    filename = function() "BioticExplorer_figures.zip",
    
    content = function(file) {
      
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL
      
      spOverviewDat <- speciesOverviewData(rv$stnall)
      
      ## Station overview figures
      
      if(!is.null(input$stationOverviewExport)) {
        
        for (i in 1:length(input$stationOverviewExport)) {
          
          fileName <- paste0(input$stationOverviewExport[i], input$downloadFigureFormat)
          
          ggplot2::ggsave(fileName, plot = get(input$stationOverviewExport[i])(spOverviewDat, base_size = 8), width = input$figureWidth, height = 0.7*input$figureWidth, units = "cm")
          
          files <- c(fileName,files)
        }
      }
      
      ## Station maps
      
      if(!is.null(input$stationMapExport)) {
        
        for(i in 1:length(input$stationMapExport)) {
          
          fileName <- paste0(input$stationMapExport[i], input$downloadFigureFormat)
          
          mapview::mapshot(get(input$stationMapExport[i])(spOverviewDat), file = fileName, vwidth = 1323)
          
          files <- c(fileName,files)
          
        }
        
      }
      
      if(is.null(files)) {
        stop("Select figures to download")
      } else {
        zip(file,files)
      }
    }
  )
  
  ### Download data ####
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      
      if (length(input$downloadDataType) == 1 & !"original" %in% input$downloadDataType) {
        paste0(input$downloadDataType, input$downloadFileType)
      } else if (input$downloadFileType == ".rda") {
        "BioticExplorer_data.rds"
      } else if (input$downloadFileType == ".xlsx" & !"original" %in% input$downloadDataType) {
        "BioticExplorer_data.xlsx"
      } else {
        "BioticExplorer_data.zip"
      }
    },
    
    content = function(file) {
      
      if (sapply(strsplit(file, "\\."), "[", 2) == "zip") {
        
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        files <- NULL
        
        #loop through the sheets
        for (i in 1:length(input$downloadDataType)) {
          
          fileName <- paste0(input$downloadDataType[i], ".csv")
          write.csv(
            prettyDec(eval(parse(text = paste("rv", input$downloadDataType[i], sep = "$")))),
            fileName, row.names = FALSE, na = "") 
          files <- c(fileName,files)
        }
        #create the zip file
        zip(file,files)
        
      } else if (sapply(strsplit(file, "\\."), "[", 2) == "rds") {
        
        biotic <- lapply(input$downloadDataType, function(k) {
          eval(parse(text = paste("rv", k, sep = "$")))
        })
        
        names(biotic) <- input$downloadDataType
        
        saveRDS(biotic, file = file)
        
      } else if (sapply(strsplit(file, "\\."), "[", 2) == "xlsx") {
        wb <- openxlsx::createWorkbook()
        
        for (i in 1:length(input$downloadDataType)) {
          openxlsx::addWorksheet(wb, paste(input$downloadDataType[i]))
          openxlsx::writeData(wb, paste(input$downloadDataType[i]), 
                              eval(parse(text = paste("rv", input$downloadDataType[i], sep = "$"))))
        }
        
        openxlsx::saveWorkbook(wb, file)
        
      } else {
        tmp <- switch(input$downloadDataType,
                      "mission" = prettyDec(rv$mission),
                      "stnall" = prettyDec(rv$stnall),
                      "indall" = prettyDec(rv$indall))
        
        write.csv(tmp, file, row.names = FALSE, na = "") 
      }
    }
  )
  
  
})

##........................
## Compile to the app ####

shinyApp(ui, server)
