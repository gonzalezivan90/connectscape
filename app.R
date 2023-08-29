### step by step version
## workign version meant to scale by steps

# system('cd /home/shiny/connectscape/; git add . ; git commit -m "some edits"; git push')
# git pull connectscape

{
  library(bit) #
  library(digest)
  library(dplyr)
  library(ggplot2)
  library(highcharter)
  library(htmlwidgets)
  library(htmltools)
  library(leaflet)
  library(leaflet.extras)
  library(knitr)
  library(rgl) #
  library(magrittr)
  #library(mongolite)#
  
  library(raster)
  library(RColorBrewer)
  library(rgdal)
  library(rgeos)
  library(rmarkdown)
  library(shiny)
  library(shinydashboard)
  library(shinydashboardPlus)
  library(shinyjs)
  library(shinyWidgets)
  library(dashboardthemes)
  library(shinycssloaders)
  
  
  library(tidyverse)
  library(shiny)
  library(reshape2)
  library(DT)
  library(tibble)
}


source('/home/shiny/connectscape/cola_tools.R')
tempPath <- '/data/temp/'; #dir.create(tempPath)
# debug insall order: htmltools >> shiny >> shinyWidgets

### time stamp -----
## Create temporal tif from ID Raster
sessionIDgen <- function(letter = TRUE, sep = ''){
  tempID <- basename(tempfile())
  timeMark <- gsub('[[:punct:]]| ', '', format(as.POSIXct(Sys.time(), tz="CET"), tz="America/Bogota",usetz=TRUE))
  sessionID <- paste0( timeMark, sep, tempID )
  if (letter){
    sessionID <- paste0(sample(LETTERS, 1), sep, sessionID)
  }
  sessionID
}

(sessionID <- sessionIDgen())
tempFolder <- paste0(tempPath, '/', sessionID, '/')
dir.create(tempFolder)
print(paste('\n >>>> Tempfile: ', tempFolder))

#tempFolder <- paste0(tempPath, '/2023082821124805_file5762732c645/')

#tempPath <- sort(Sys.getenv(c("TEMP", 'TMP')), decreasing=T)[1]

py <- '/home/shiny/anaconda3/envs/cdujlab/bin/python'
# reactShp <- list(shp = FALSE,
#                  leaf0 = leaflet() %>% addTiles() %>% 
#                    addLayersControl(baseGroups = c("OpenStreetMap", "Esri.WorldImagery"),
#                                     options = layersControlOptions(collapsed = FALSE)) %>%
#                    addProviderTiles( "Esri.WorldImagery", group = "Esri.WorldImagery" ) %>%
#                    setView(lng = -74, lat = 4.6, zoom = 10)
# )

devug <<- TRUE


runCDPOP <- function(py, datapath = tempFolder){
  # outfiles: 
  #CDPOP/data/out11684988478/batchrun0mcrun0/grid0.csv [0, 1, ..]
  #CDPOP/data/out11684988478/batchrun0mcrun0/output.csv
  #CDPOP/data/out11684988478/batchrun1mcrun0/XY0.csv
  #CDPOP/data/out21684988758/cdpop.log
  
  #xyfilename  requires NO .csv
  #agefilename requires .csv
  #matecdmat	cdmats/EDcdmatrix16
  #dispcdmat	cdmats/EDcdmatrix16
  
  # python CDPOP.py %userprofile%\dockerdata\CDPOP\data inputvars.csv outAnac1
  src <- '/home/shiny/connecting-landscapes/lib/CDPOP/src/CDPOP.py'
  datapath <- tempFolder # datapath = tempFolder
  vars <- paste0('invars.csv') # Only file name
  timeMarkCDPOP <- gsub('[[:punct:]]| ', '', format(as.POSIXct(Sys.time(), tz="CET"), tz="America/Bogota",usetz=TRUE))
  cdpopPath <- paste0('cdpopout_', timeMarkCDPOP, '__')
  cdpopPath <- 'cdpopout'
  (cmd <- paste0(py, ' ', src, ' ', datapath, ' ', vars, ' ', cdpopPath))
  
  # setwd(tempFolder)
  #file.copy('inputvars.csv', 'in.csv')
  #file.copy('xyA.csv', 'xy.csv')
  #intCMD <- tryCatch(system(cmd, intern = TRUE, ignore.stdout = TRUE), error = function(e) NULL)
  
  newFiles <- list.files(path = tempFolder, recursive = TRUE)
  newFiles <- grep(pattern = cdpopPath, newFiles, value = TRUE)
  return(list(newFiles = newFiles, cdpopPath = cdpopPath))
}


runS2RES <- function(py, intif, outtif, 
                     param3, param4, param5, param6, param7){
  # intif = newtifPath
  # outtif = paste0(dirname(newtifPath), '/out_s2r.tif')
  
  # param3 = 0
  # param4 =  100
  # param5 = 100
  # param6 = 1
  # param7 = -9999
  
  src <- '/home/shiny/connecting-landscapes/src/s2res.py'
  datapath <- tempFolder # datapath = tempFolder
  
  timeMarkCDPOP <- gsub('[[:punct:]]| ', '', format(as.POSIXct(Sys.time(), tz="CET"), tz="America/Bogota",usetz=TRUE))
  
  (cmd_s2res <- paste0(py, ' ', src, ' ', intif, ' ', outtif, ' ', 
                       param3, ' ', param4, ' ', param5, 
                       ' ', param6, ' ', param7))
  
  intCMD <- tryCatch(system(cmd_s2res, intern = TRUE, ignore.stdout = TRUE), error = function(e) NULL)
  
  
  return(file = ifelse(file.exists(outtif), outtif, NA))
}


points_shp <- function(py, intif, outshp, 
                       param3, param4, param5){
  
  # param3 = 2
  # param4 =  95
  # param5 = 50
  
  src <- '/home/shiny/connecting-landscapes/src/create_source_points.py'
  datapath <- tempFolder # datapath = tempFolder
  
  (cmd_pts <- paste0(py, ' ', src, ' ', intif, ' ', outshp, ' ', 
                     param3, ' ', param4, ' ', param5))
  
  intCMD <- tryCatch(system(cmd_pts, intern = TRUE, ignore.stdout = TRUE), error = function(e) NULL)
  
  
  return(file = ifelse(file.exists(outshp), outshp, NA))
}


loadshp <- function(inFiles, tempFolder, sessID){ # inFiles <- input$shapefile
  # rv$inDistSessID
  # sessID <- 
  if ( class(inFiles) != "NULL" ){
    vtext <<- ''
    if ( nrow(inFiles) == 1){
      
      if(grepl('*\\.zip', inFiles$name)){ ## zip files
        outZip <- paste0(tempFolder); dir.create(outZip)
        unzip(zipfile = inFiles$datapath, exdir = outZip)
        uZ <- list.files(outZip)
        #x0 <- uZ; print(x0); print(class(x0)); print(str(x0))
        shp <<- tryCatch(readOGR(outZip, layer = tools::file_path_sans_ext(uZ[1])), error = function (e) NULL)
      } else if (grepl('\\.SQLite|\\.gpkg|\\.GeoJSON', inFiles$name)){ ## single
        #save(inFiles, file = 'inFileSingle.RData'); 
        shp <<- tryCatch(readOGR(inFiles$datapath[1]), error = function (e) NULL)
      }
    } else if ( nrow(inFiles) >= 3  & all(sapply(c('\\.shp', '\\.shx', '\\.dbf'), grep, inFiles$name))){ ## shp several
      #save(inFiles, file = 'inFileSeveral.RData');
      inFiles$datapath2 <- gsub('\\/[0-9]\\.', '/1.', inFiles$datapath)
      sapply(inFiles$datapath, USE.NAMES = F, function(x){
        file.rename(x,  gsub('\\/[0-9]\\.', '/1.', x) ) })
      
      shp <<- tryCatch(readOGR(dirname(inFiles$datapath2[1]),
                               basename(tools::file_path_sans_ext(inFiles$datapath2[1]))), error = function (e) e)
    }
    #tryCatch(sapply(inFiles$datapath, file.remove ))
    
    if(class(shp) == 'SpatialPolygonsDataFrame'){
      if(nrow(shp) > 1 ){
        shp <- shp[1, ]
        vtext <<- 'Más de un polígono. Se usa sólo el primero.\n'
      }
      vtext <<- paste0(vtext, '\nProyección:', shp@proj4string@projargs)
      if(shp@proj4string@projargs != prj_wgs84){ 
        #print(paste0(' Proj old: ', shp@proj4string@projargs))
        shp <<- spTransform(shp, CRSobj = CRS(prj_wgs84)) 
        #print(paste0(' Proj new: ', shp@proj4string@projargs))
        vtext <<- paste0(vtext, '\nProyección no es geográfica WGS84. Intentando reproyectar.')
      }
      
      if( min(shp@bbox['x', ])>-180 & max(shp@bbox['x', ])<180 &
          min(shp@bbox['y', ])>-90 & max(shp@bbox['y', ])<90 )  #><
      {
        polArea <- raster::area(shp)/1000000
        if ( polArea <= 5000){ # Smaller than 5k km
          isShpLoad <<- TRUE
          reactShp$shp <- TRUE
          vtext <<- paste0(vtext, '\nPolígono cargado!')
        } else {
          isShpLoad <<- FALSE
          reactShp$shp <- FALSE
          vtext <<- paste0(vtext, '\nError: Polígono mayor a 5.000 Km. Subir un archivo con menor área')
        }
      } else{
        vtext <<- paste0(vtext, '\nError: Polígono fuera del límite global')
      }
    } else {
      vtext <<- "Error: Intente con otros archivos. No se pudo cargar el polígono"
    }
    
    if (isShpLoad){
      #print("   shp: "); print(shp)
      reactShp$shp <- TRUE
      reactShp$leaf0 <<- leaflet() %>% addTiles() %>% addPolygons(data = shp) %>%
        addLayersControl(baseGroups = c("OpenStreetMap", "Esri.WorldImagery"),
                         options = layersControlOptions(collapsed = FALSE)) %>%
        addProviderTiles( "Esri.WorldImagery", group = "Esri.WorldImagery" )
      #output$loadMapLL <- renderLeaflet({reactShp$leaf0})
      # print("   reactShp: "); print(reactShp); print(str(reactShp))
      # save(reactShp, file = paste0('read.RData'))
      
      updateSelectInput(session, 'aoi_forest', choices = c('Dibujar', 'Capa'), selected = 'Capa')
      updateSelectInput(session, 'aoi_clc', choices = c('Dibujar', 'Capa'), selected = 'Capa')
      updateSelectInput(session, 'aoi_red', choices = c('Dibujar', 'Capa'), selected = 'Capa')
      updateSelectInput(session, 'aoi_biot', choices = c('Dibujar', 'Capa'), selected = 'Capa')
      updateSelectInput(session, 'aoi_biom', choices = c('Dibujar', 'Capa'), selected = 'Capa')
      updateSelectInput(session, 'aoi_param', choices = c('Dibujar', 'Capa'), selected = 'Capa')
      updateSelectInput(session, 'aoi_dry', choices = c('Dibujar', 'Capa'), selected = 'Capa')
      updateSelectInput(session, 'aoi_wet', choices = c('Dibujar', 'Capa'), selected = 'Capa')
      updateSelectInput(session, 'aoi_ap', choices = c('Dibujar', 'Capa'), selected = 'Capa')
      updateSelectInput(session, 'aoi_cole', choices = c('Dibujar', 'Capa'), selected = 'Capa')
      updateSelectInput(session, 'aoi_sma', choices = c('Dibujar', 'Capa'), selected = 'Capa')
      updateSelectInput(session, 'aoi_comp', choices = c('Dibujar', 'Capa'), selected = 'Capa')
      updateSelectInput(session, 'aoi_uicn', choices = c('Dibujar', 'Capa'), selected = 'Capa')
      updateSelectInput(session, 'aoi_rec', choices = c('Dibujar', 'Capa'), selected = 'Capa')
      updateSelectInput(session, 'aoi_biod', choices = c('Dibujar', 'Capa'), selected = 'Capa')
      updateSelectInput(session, 'aoi_sur', choices = c('Dibujar', 'Capa'), selected = 'Capa')
      
      
      
      
      ## Validate nchar
      gwkt_orig <<- gsub( ' ', '%20', paste0('POLYGON ((', 
                                             paste(apply(round(shp@polygons[[1]]@Polygons[[1]]@coords, 4), 1, 
                                                         paste, collapse = ' '), collapse = ', '), '))'))
      ## Simplify
      if(nchar(gwkt_orig) > 7795 ){
        vtext <<- paste0(vtext, '\nIntentando simplificar geometría del polígono')
        rng <- seq(0, 10)
        for(i in rng){ # i = 1
          tol.i <- as.numeric(i/1000)
          simpMun <- sp::SpatialPolygonsDataFrame(rgeos::gSimplify(shp, 
                                                                   tol = tol.i, 
                                                                   topologyPreserve = TRUE), 
                                                  data = data.frame(id = 1),match.ID = F)
          gwkt_simp <<- gsub( ' ', '%20', paste0('POLYGON((', 
                                                 paste(apply(round(simpMun@polygons[[1]]@Polygons[[1]]@coords, 4),
                                                             1, paste, collapse = ' '), collapse = ','), '))'))
          if (nchar(gwkt_simp) < 7795){
            gwkt_orig <<- gwkt_simp
            vtext <<- paste0(vtext, '\nPolígono simplificado')
            reactShp$leaf0 <<- leaflet() %>% addTiles() %>% addPolygons(data = simpMun) %>%
              addLayersControl(baseGroups = c("OpenStreetMap", "Esri.WorldImagery"),
                               options = layersControlOptions(collapsed = FALSE)) %>%
              addProviderTiles( "Esri.WorldImagery", group = "Esri.WorldImagery" )
            
            break
          }
        }
      }
      
      vtext <<- paste0(vtext, '\nNúmero de vértices del polígono: ', 
                       nrow(shp@polygons[[1]]@Polygons[[1]]@coords),
                       '\nNúmero de caracteres del polígono: ', 
                       nchar(gwkt_orig),
                       '\nÁrea en km2: ', round(polArea, 2))
    }
  }
}
  

  
# install.packages('shinyWidgets')
# install.packages('shinydashboardPlus')
# install.packages('dashboardthemes')

# if ( identical ( unname(Sys.info()[c("sysname", 'nodename')]), c("Windows", 'HP-Z400')) ){
#   setwd('N:/Mi unidad/IG/server_IG/gedivis')
#   #setwd('N:/Mi unidad/IG/server_IG/gedivis/')
# }



##function for removing the colum

# removecolumn <- function(df, nameofthecolumn){
#   df[ , -which(names(df) %in% nameofthecolumn)]
# }

removecolumn <- function(df, nameofthecolumn = NULL){
  id <- ifelse(is.null(nameofthecolumn), 
               ncol(df), 
               which(names(df) %in% nameofthecolumn))
  if(ncol(df) == 2){
    df[ , -id, drop = FALSE]
  } else if (ncol(df) == 1){
    df 
  } else if (ncol(df) > 2){
    df[ , -id]
  }
}

addcolumn <- function(df, nameofthecolumn = NULL){
  id <- ifelse(is.null(nameofthecolumn), ncol(df), nameofthecolumn)
  cbind(df, df[ , id])
}



# ui ---------------------------------------------------------------------------

ui <- dashboardPage(
  #useShinyjs(),
  header = shinydashboard::dashboardHeader(
    title = "ConnectingLandscapes"
    #,enable_rightsidebar = TRUE, rightSidebarIcon = "info-circle"
  ),
  
  # title ----
  
  # sidebar ----
  sidebar = 
    dashboardSidebar(
      sidebarMenu(id = "sidebarid",
                  menuItem("Home", tabName = "tab_home", icon = icon("house-user")),
                  #HTML(paste("Habitat suitability <>", "resistance surface", sep="<br/>"))
                  menuItem(HTML(paste("Habitat suitability <>", "  resistance surface", sep="<br/>")), 
                           tabName = "tab_surface", icon = icon("map-pin")),
                  conditionalPanel( 'input.sidebarid == "tab_surface"',
                                    shiny::fileInput('in_sur_tif', 'Load TIF', 
                                                     buttonLabel = 'Search', placeholder = 'No file',
                                                     accept=c('.tif'),
                                                     #accept= '.zip',
                                                     multiple=FALSE),
                  ),
                  
                  
                  menuItem("Create source points", tabName = "tab_points", icon = icon("map-pin")),
                  conditionalPanel( 'input.sidebarid == "tab_points"',
                                    shiny::fileInput('in_points_tif', 'Load TIF', 
                                                     buttonLabel = 'Search', placeholder = 'No file',
                                                     accept=c('.tif'),
                                                     #accept= '.zip',
                                                     multiple=FALSE),
                  ),
                  menuItem("Cost distance matrix", tabName = "tab_distance", icon = icon("border-all")),
                  conditionalPanel( 'input.sidebarid == "tab_distance"',
                                    shiny::fileInput('in_dist_tif', 'Load TIF', 
                                                     buttonLabel = 'Search', placeholder = 'No file',
                                                     accept=c('.tif'), multiple=FALSE),
                                    shiny::fileInput('indistshp', 'Load points', buttonLabel = 'Search', 
                                                     placeholder = 'No file(s) ',
                                                     accept=c('.shp','.dbf','.sbn','.sbx','.shx',".prj", '.zip', '.gpkg', '.SQLite', '.GeoJSON', '.csv'),
                                                     multiple=TRUE),
                                    #actionButton("dist_shp", "Load points!"),
                                    
                  ),
                  
                  menuItem("CDPOP", tabName = "tab_cdpop", icon = icon("hippo")),
                  menuItem("Connectivity - corridors", tabName = "tab_corridors", icon = icon("route")),
                  
                  # conditionalPanel(
                  #   'input.sidebarid %in% c("tab_kernels", "tab_plotting")',
                  #   shiny::fileInput('in_corrpoints', 'Load point file Co', buttonLabel = 'Search', placeholder = 'No choose',
                  #                    accept=c('.csv','.txt'), multiple=FALSE),
                  #   shiny::fileInput('in_corrsurface', 'Load point file Co', buttonLabel = 'Search', placeholder = 'No choose',
                  #                    accept=c('.csv','.txt'), multiple=FALSE)
                  # ),
                  menuItem(HTML(paste("Connectivity", "dispersal kernels", sep="<br/>")),
                           tabName = "tab_kernels", icon = icon("bezier-curve")),
                  menuItem("Plotting", tabName = "tab_plotting", icon = icon("image")),
                  menuItem("Mapping", tabName = "tab_Mapping", icon = icon("map")),
                  menuItem("Connectivity - prioritization", 
                           tabName = "tab_priori", icon = icon("trophy")),
                  menuItem(HTML(paste("Landscape genetics", "mapping tools", sep="<br/>")),
                           tabName = "tab_genetics", icon = icon("route")),
                  menuItem("Run locally", tabName = "tablocal", icon = icon("code-fork")),
                  
                  menuItem("Page 1", tabName = "page1"),
                  conditionalPanel(
                    'input.sidebarid == "page1"',
                    sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30),
                    selectInput("title", "Select plot title:", choices = c("Hist of x", "Histogram of x"))
                  ),
                  menuItem("Page 2", tabName = "page2")
                  # tabhome tabsurface tab_points tab_distance tab_cdpop 
                  # tab_corridors tab_kernels tab_plotting tab_Mapping tab_priori tab_genetics tablocal           
      )
    ),
  
  # body ----
  body = 
    dashboardBody(
      shinyDashboardThemes(
        theme = "grey_dark"
      ),
      tabItems(
        
        # tab_home tab_surface tab_points tab_distance tab_cdpop 
        # tab_corridors tab_kernels tab_plotting tab_Mapping tab_priori tab_genetics tablocal           
        
        
        tabItem('tab_home', 
                fluidPage(
                  #includeMarkdown("md_intro.md")
                  tabsetPanel(type = "pills",
                              tabPanel("Home", plotOutput("plot")),
                              tabPanel("Performance", verbatimTextOutput("summary")),
                              tabPanel("Showcase", tableOutput("table"))
                  )
                )),
        
        
        tabItem('tab_cdpop', 
                fluidPage(
                  #includeMarkdown("md_intro.md")
                  tabsetPanel(type = "pills",
                              tabPanel("Parameters", 
                                       fluidPage(
                                         
                                         fixedRow(
                                           column(width = 5,
                                                  fileInput("in_cdpop_par", "Choose CSV File", accept = ".csv")
                                                  
                                           ),
                                           column(width = 7,
                                                  #actionButton("Splitcolumn", "SplitColumn"),
                                                  checkboxInput("header", "Header", TRUE),
                                                  uiOutput("selectUI"),
                                                  #actionButton("deleteRows", "Delete Rows"),
                                                  #textInput("textbox", label="Input the value to replace:"),
                                                  #actionButton("replacevalues", label = 'Replace values'),
                                                  actionButton("addcolumn", "Add Column"),
                                                  actionButton("removecolumn", "Remove last column"),
                                                  actionButton("Undo", 'Undo')
                                                  
                                           )
                                         ),
                                         fluidRow(
                                           column(width = 12,
                                                  DT::dataTableOutput(outputId =  "table1"),
                                                  actionButton("cdpop_check1", "Check files"),
                                                  valueBoxOutput("cdpop_box1"),
                                                  plotOutput("cdpop_params"))
                                         )
                                       )
                              ),
                              tabPanel("Files", 
                                       fileInput("in_cdpop_xy", "XY CSV File", accept = ".csv"),
                                       fileInput("in_cdpop_age", "Ages CSV File", accept = ".csv"),
                                       fileInput("in_cdpop_cd", "CDmatrix CSV File", accept = ".csv"),
                                       tableOutput("cdpop_files")),
                              
                              tabPanel("Run", 
                                       valueBoxOutput("cdpop_box2"),
                                       actionButton("cdpop_check2", "Check files"),
                                       uiOutput("out_cdpop_files"),
                                       actionButton("cdpop_check3", "Load ouptut"),
                                       DT::dataTableOutput(outputId =  "out_cdpop_filestable"))
                  )
                )),
        
        
        tabItem('tab_surface',
                verbatimTextOutput("voutext") %>% withSpinner(color="#0dc5c1"),
                leafletOutput("loadMapLL", height = "600px") %>% withSpinner(color="#0dc5c1"),
                actionButton("surface_hs2rs", "Get resistance surface"),
                
                textInput("in_surf_3", "Min-grid:", '0'),
                textInput("in_surf_4", "Max-grid:", '100'),
                textInput("in_surf_5", "Max-resistance:", '100'),
                textInput("in_surf_6", "Shape:", '1'),
                textInput("in_surf_7", "No Data:", '-9999')
        ),
        
        
        # UI Tab points ----
        
        tabItem('tab_points',
                h1(' Create points'),
                verbatimTextOutput("voutext_points") %>% withSpinner(color="#0dc5c1"),
                leafletOutput("ll_map_points", height = "600px") %>% withSpinner(color="#0dc5c1"),
                actionButton("points_py", "Get points"),
                
                textInput("in_points_3", "Min-grid:", '2'),
                textInput("in_points_4", "Max-grid:", '95'),
                textInput("in_points_5", "Number of points:", '50')
        ),
        
        ##> voutext_points; ll_map_points; points_py; in_points_3 -- 5
        
        
        # UI Tab points ----
        
        tabItem('tab_distance',
                h1(' Create Distance'),
                verbatimTextOutput("voutext_dist") %>% withSpinner(color="#0dc5c1"),
                leafletOutput("ll_map_dist", height = "600px") %>% withSpinner(color="#0dc5c1"),
                actionButton("dist_py", "Get matrix"),
                textInput("in_dist_3", "Distance threshold (in cost distance units):", '25000')
        ),
        
        ##> voutext_dist; ll_map_dist; distance_py; in_distance_3, in_distance_shp in_dist_tif
        
        
        # tabItem('tab_example', 
        #         fluidPage( )
        # ),
        # tab_home tab_surface tab_points tab_distance tab_cdpop 
        # tab_corridors tab_kernels tab_plotting tab_Mapping tab_priori tab_genetics tablocal           
        
        
        
        # page 1 ----
        tabItem(tabName = "page1",
                "Page 1 content. This page doesn't have any sidebar menu items."),
        # page 2 ----
        tabItem(tabName = "page2", 
                "Page 2 content. This page has sidebar meny items that are used in the plot below.",
                br(), br(),
                plotOutput("distPlot"))
      )
    )
)


server <- function(input, output, session) {
  
  output$distPlot <- renderPlot({
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = "darkgray", border = "white", main = input$title)
  })
  
  
  ####### SRV DEFAULT  ------------------
  rv <- reactiveValues(data = NULL, orig = NULL,
                       cdpopRun = NULL, out_cdpop_files = c(''), 
                       
                       surfmap = NULL, pointsmap = NULL, 
                       distmap = NULL, distrast = NULL, distshp = NULL,
                       
                       newtifPath = NULL, 
                       newtifPath_pts = NULL,
                       newtifPath_dist = NULL,
                       newshpPath_dist = NULL,
                       
                       inSurSessID = NULL,
                       inPointsSessID = NULL,
                       inDistSessID = NULL,
                       )
  

  
  
  reactShp <- reactiveValues(layer = FALSE,
                             leaf0 = leaflet() %>% addTiles() %>% 
                               addLayersControl(baseGroups = c("OpenStreetMap", "Esri.WorldImagery"),
                                                options = layersControlOptions(collapsed = FALSE)) %>%
                               addProviderTiles( "Esri.WorldImagery", group = "Esri.WorldImagery" ) %>%
                               setView(lng = 25, lat = -21, zoom = 6)
  )
  # reactShp <- list(layer = FALSE,
  #                            leaf1 = leaflet() %>% addTiles() %>%
  #                              addLayersControl(baseGroups = c("OpenStreetMap", "Esri.WorldImagery"),
  #                                               options = layersControlOptions(collapsed = FALSE)) %>%
  #                              addProviderTiles( "Esri.WorldImagery", group = "Esri.WorldImagery" ) %>%
  #                              setView(lng = 25, lat = -21, zoom = 6),
  #                  leaf0 = leaflet() %>% addTiles()
  # )
  
  reactShp$leaf0 <<- leaflet() %>% addTiles() 
  
  output$ll_map_dist <- output$ll_map_points <- output$loadMapLL <- renderLeaflet({
    
    reactShp$leaf0 %>%
      leaflet.extras::addDrawToolbar(targetGroup='draw', polylineOptions = FALSE,
                                     rectangleOptions = FALSE, circleOptions = FALSE,
                                     markerOptions = FALSE, circleMarkerOptions = FALSE,
                                     editOptions = leaflet.extras::editToolbarOptions())
    
  })
  
  vtext <- "Waiting for the habitat sutiability TIF"
  output$voutext <- renderText({isolate(vtext)})
  
  voutext_points <- "Waiting for the surface resistance TIF"
  output$voutext_points <- renderText({isolate(voutext_points)})
  
  voutext_dist <- "Waiting for the surface resistance TIF and points"
  output$voutext_dist <- renderText({isolate(voutext_dist)})
  
  
  
  ####### SRV CDPOP  ------------------
  # in_cdpop_cd 
  # in_cdpop_age
  # in_cdpop_xy
  
  #   name size type datapath
  # input <- list(in_cdpop_par$datapath = "/data/temp/2023082820202105_file3c454758300e/",
  #               in_cdpop_age$datapath = "/tmp/Rtmp083gNo/9489e1ac4d4f142bb1718552/0.csv",
  #               in_cdpop_cd$datapath = "",
  #               in_cdpop_par$datapath = "")
  
  if(devug & FALSE){
    tempFolder <- '/data/temp/2023082821124805_file5762732c645'
    #setwd(tempFolder)
    
    cdpop_path_invars <- paste0(tempFolder, '/invars.csv')
    cdpop_path_xy <- paste0(tempFolder, '/xy.csv')
    cdpop_path_age <- paste0(tempFolder, '/age.csv')
    cdpop_path_cdmat <- paste0(tempFolder, '/cdmat.csv')
    
    writeParamCDPOP <- read.csv(file = cdpop_path_invars)
    #invarsorig <- writeParamCDPOP
    xyfile <- read.csv(file = cdpop_path_xy)
    age <- read.csv(file = cdpop_path_age)
    cdmat <- read.csv(file = cdpop_path_cdmat, header = FALSE)
  }
  
  
  
  observeEvent(input$in_cdpop_par, {
    if(devug){ print('cdpop_par: '); print(input$in_cdpop_par)}
    file <- input$in_cdpop_par
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    invarsorig <- (read.csv(file$datapath, header = input$header))
    rv$orig <- t(invarsorig)
    colnames(rv$orig) <- paste0('Scen', 1:ncol(rv$orig))
    rv$data <- (rv$orig)
    cdpop_path_invars <<- paste0(tempFolder, '/invars.csv')
    #cdpop_path_invars0 <<- paste0(tempFolder, '/invars0.csv')
    write.csv(invarsorig, file = cdpop_path_invars, row.names = FALSE, quote = FALSE)
    #write.csv(invarsorig, file = cdpop_path_invars0, row.names = FALSE)
  })
  
  
  observeEvent(input$in_cdpop_xy, {
    if(devug){ print('in_cdpop_xy: '); print(input$in_cdpop_xy)}
    xyfile <- input$in_cdpop_xy
    ext <- tools::file_ext(xyfile$datapath)
    req(xyfile)
    validate(need(ext == "csv", "Please upload a csv file"))
    if(devug){ print(' reading in_cdpop_xy: '); print(xyfile$datapath)}
    xyfile <<- xyfile <- (read.csv(xyfile$datapath, header = input$header))
    print(dim(xyfile))
    #colnames(rv$orig) <- paste0('Scen', 1:ncol(rv$orig))
    #rv$data <- (rv$orig)
    cdpop_path_xy <<- paste0(tempFolder, '/xy.csv')
    file.copy(xyfile$datapath, cdpop_path_xy) 
    #write.csv(xyfile, file = cdpop_path_xy, row.names = FALSE, quote = FALSE)
    if(!file.exists(cdpop_path_xy)){ 
      print('Error copy cdpop xy')
      write.csv(xyfile, file = cdpop_path_xy, row.names = FALSE, quote = FALSE)
      if(!file.exists(cdpop_path_xy)){ 
        print('Error copy cdpop xy 2')
        file.copy(xyfile$datapath, cdpop_path_xy) 
      }
    }
    
  })
  
  observeEvent(input$in_cdpop_age, {
    if(devug){ print('in_cdpop_age: '); print(input$in_cdpop_age)}
    
    agefile <- input$in_cdpop_age
    ext <- tools::file_ext(agefile$datapath)
    req(agefile)
    validate(need(ext == "csv", "Please upload a csv file"))
    age <<- (read.csv(agefile$datapath, header = input$header))
    #colnames(rv$orig) <- paste0('Scen', 1:ncol(rv$orig))
    cdpop_path_age <<- paste0(tempFolder, '/age.csv')
    write.csv(age, file = cdpop_path_age, row.names = FALSE, quote = FALSE)
  })
  
  
  #input <- list(in_cdpop_cd$)
  #cdmatfile <- list(datapath = '/tmp/RtmpxR2z6w/4f50c5b3fc19eb4be395c7f1/0.csv')
  observeEvent(input$in_cdpop_cd, {
    if(devug){ print('in_cdpop_cd: '); print(input$in_cdpop_cd)}
    #print(input$in_cdpop_cd)
    
    cdmatfile <- input$in_cdpop_cd
    ext <- tools::file_ext(cdmatfile$datapath)
    req(cdmatfile)
    validate(need(ext == "csv", "Please upload a csv file"))
    cdmat <<- (read.csv(cdmatfile$datapath, header = FALSE))
    #colnames(rv$orig) <- paste0('Scen', 1:ncol(rv$orig))
    cdpop_path_cdmat <<- paste0(tempFolder, '/cdmat.csv')
    write.table(cdmat, file = cdpop_path_cdmat, row.names = FALSE, col.names = F, sep = ',')
    file.copy(cdmatfile$datapath, cdpop_path_cdmat) 
  })
  
  
  
  
  
  
  # output$selectUI<-renderUI({
  #   req(rv$data)
  #   selectInput(inputId='selectcolumn', label='select column', choices = names(rv$data))
  # })
  
  #splitcolumn
  observeEvent(input$Splitcolumn, {
    rv$data <- splitColumn(rv$data, input$selectcolumn)
  })
  
  #delterows
  observeEvent(input$deleteRows,{
    if (!is.null(input$table1_rows_selected)) {
      rv$data <- rv$data[-as.numeric(input$table1_rows_selected),]
    }
  })
  
  
  ####### renderDT  ------------------
  
  
  # output$table1 <- renderDT({
  #   datatable(rv$data, editable = TRUE)
  # })
  
  output$table1 <- DT::renderDataTable(
    dat <- datatable(rv$data,
                     options = list(
                       paging =TRUE,
                       pageLength =  nrow(rv$data) 
                     )
    )
  )
  
  observeEvent(input$table1_cell_edit, {
    row  <- input$table1_cell_edit$row
    clmn <- input$table1_cell_edit$col
    rv$data[row, clmn] <- input$table1_cell_edit$value
  })
  
  observeEvent(input$replacevalues, {
    rv$data <- fillvalues(rv$data, input$textbox, input$selectcolumn)
  })
  observeEvent(input$removecolumn, {
    #rv$data <- removecolumn(rv$data,input$selectcolumn)
    rv$data <- removecolumn(rv$data)
  })
  observeEvent(input$addcolumn, {
    rv$data <- addcolumn(rv$data)
  })
  
  observeEvent(input$Undo, {
    rv$data <- rv$orig
  })
  
  observeEvent(input$cdpop_run, {
    #xyfilename no requires .csv
    #agefilename requires .csv
    
    
    rv$data <- rv$orig
  })
  
  
  output$cdpop_box1 <- renderValueBox({
    valueBox(
      "Not yet", "Ready", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "red"
    )
  })
  
  
  observeEvent(input$cdpop_check1, {
    writeParamCDPOP <- t(rv$data)[1, ]
    #writeParamCDPOP <- t(writeParamCDPOP)
    writeParamCDPOP$xyfilename <- 'xy'
    writeParamCDPOP$agefilename <- 'age.csv'
    writeParamCDPOP$matecdmat <- writeParamCDPOP$dispcdmat <- 'cdmat'
    write.csv(writeParamCDPOP, file = cdpop_path_invars, row.names = FALSE, quote = FALSE)
    cond <- TRUE
    if( cond){
      output$cdpop_box1 <- renderValueBox({
        valueBox(
          "YES", "Ready", icon = icon("thumbs-up", lib = "glyphicon"),
          color = "green"
        )
      })
    }
  })
  
  
  output$cdpop_box2 <- renderValueBox({
    valueBox(
      "Not yet", "Ready to run CDPOP", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "red"
    )
  })
  
  observeEvent(input$cdpop_check2, {
    cdmat2 <- cdmat[, apply(!is.na(cdmat), 2, sum) != 0]
    (cond <- (ncol(cdmat2) == nrow(cdmat2)) &
        ncol(cdmat2) > 2 &
        (nrow(cdmat2) == nrow(xyfile)))
    
    if( cond){
      output$cdpop_box2 <- renderValueBox({
        valueBox(
          "YES", "Ready", icon = icon("thumbs-up", lib = "glyphicon"),
          color = "green"
        )
      })
      
      cdpopRun <- runCDPOP(py, datapath = tempFolder)
      rv$cdpopRun <- cdpopRun
      output$out_cdpop_files<-renderUI({
        req(rv$cdpopRun)
        #cdpop_outfiles <- gsub(pattern = cdpopRun$cdpopPath, replacement = '', x = cdpopRun$newFiles)
        selectInput(inputId='out_cdpop_files', label='select column', choices = basename(cdpopRun$newFiles))
      })
      
    }
  })
  
  observeEvent(input$cdpop_check3, {
    if(input$out_cdpop_files != ''){
      selfile <- cdpopRun$newFiles[basename(cdpopRun$newFiles) %in% input$out_cdpop_files]
      out_cdpop_file_X <- as.data.frame(readLines(
        paste0(tempFolder,'/', selfile)))
      
      if(devug){ 
        print(' ---- input$out_cdpop_files: ')
        print(input$out_cdpop_files); 
        print(' ---- rv$cdpopRun: ')
        print(rv$cdpopRun)
        print(dim(out_cdpop_file_X))
        print(head(out_cdpop_file_X))
      }
      
      output$out_cdpop_filestable <- DT::renderDataTable(
        dat <- datatable(out_cdpop_file_X,
                         options = list( paging =TRUE,
                                         pageLength =  nrow(out_cdpop_file_X) 
                         )
        ))
    }
  })
  
  
  ####### LOAD MAPS  ------------------
  
  ####### > SURFACE  ------------------
  
  observeEvent(input$in_sur_tif, {
    
    #try(file.remove(c(tifpath, newtifPath)))
    invisible(suppressWarnings(tryCatch(file.remove(c(tifpath, newtifPath)), 
                                        error = function(e) NULL)))
    
    output$loadMapLL <- renderLeaflet({
      # tempFolder <- '/data/temp//T2023082911164705_file3112795957d7/'
      #tifpath <- '/data/temp//T2023082911164705_file3112795957d7//in_surface_C2023082911165605_file31126374e76.tif'
      #inSurSessID <- 'C2023082911165605_file31126374e76'
      (inSurSessID <- sessionIDgen())
      rv$inSurSessID <- inSurSessID
      tifpath <- paste0(tempFolder, '/in_surface_', inSurSessID, '.tif')
      file.copy(input$in_sur_tif$datapath, tifpath)
      
      #if(devug){ print(' ----- input$in_sur_tif'); print(input$in_sur_tif); print(tifpath); file.exists(tifpath)}
      
      vtext <<- paste0(vtext, '\nUpdating raster: square pixels and -9999 no data')
      isolate(output$voutext <- renderText({isolate(vtext)}))
      
      tifpathfixed <- paste0(tempFolder, '/in_surface_fixed_', inSurSessID, '.tif')
      newtifPath <- fitRaster2cola(inrasterpath = tifpath, outrasterpath = tifpathfixed)
      newtifPath <- ifelse(is.na(newtifPath), yes = tifpath, no = newtifPath)
      rv$newtifPath <- newtifPath
      vtext <<- paste0(vtext, ' --- DONE')
      (output$voutext <- renderText({(vtext)}))
      
      #newtifPath <- "/data/temp/2023082821124805_file5762732c645/in_surfaceoutfile2c243f6174c8.tif"
      # if(devug){ print(' ----- newtifPath'); print(tifpath); print(newtifPath)}
      newtif <- raster(newtifPath)
      
      #rng_newtif <- c(newtif@data@min, newtif@data@max)
      rng_newtif <- cellStats(newtif, stat = range)
      hsPal <<-  colorNumeric(palette = "viridis", reverse = TRUE,
                              domain = rng_newtif, na.color = "transparent")
      
      leafsurface <<- leaflet() %>% addTiles() %>% 
        addRasterImage(newtif, colors = hsPal, opacity = .7, group = "Habitat suitability") %>%
        addLegend(pal =  hsPal, values = newtif[],
                  position = 'topleft',
                  title= "Unknow units"#, opacity = .3
                  #, labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
        )
      
      leafsurface
      
    })
    rv$surfmap <- 1
    
  })
  
  observeEvent(input$surface_hs2rs, {
    if(devug){ print(' ----- newtifPath s2r'); print(rv$newtifPath)}
    if(!is.null(rv$surfmap)){
      # rv <- list(newtifPath = '/data/temp//E-2023082911285005_file3112135d2b4c//in_surface_V-2023082911285705_file3112303ea820.tif',
      #            inSurSessID = 'V-2023082911285705_file3112303ea820')
      # input <- list(in_surf_3 = 0, in_surf_4 =100, in_surf_5 = 100, in_surf_6 = 1, in_surf_7 = -9999)
      output$loadMapLL <- renderLeaflet({
        
        newtif <- raster(rv$newtifPath)
        outs2r <- paste0(tempFolder, '/out_surface_', rv$inSurSessID, '.tif')
        
        vtext <<- paste0(vtext, '\nCreating resistance surface')
        (output$voutext <- renderText({(vtext)}))
        
        hs2rs_file <- runS2RES(py = py, intif = rv$newtifPath, outtif = outs2r, 
                               as.numeric(input$in_surf_3), as.numeric(input$in_surf_4), 
                               as.numeric(input$in_surf_5), 
                               as.numeric(input$in_surf_6), as.numeric(input$in_surf_7))
        
        if(!is.na(hs2rs_file)){
          
          rv$newtifPath_pts <- hs2rs_file  ## For new points
          newtifPath_pts <<- hs2rs_file    ## For new points
          rv$pointsmap <- 1 ## For new points
          
          rv$newtifPath_dist <- hs2rs_file  ## For distance
          newtifPath_dist <<- hs2rs_file    ## For distance
          rv$distmap <- 1                   ## For distance 
          
          hs2rs_tif <- raster(hs2rs_file)
          rng_rstif <- cellStats(hs2rs_tif, stat = range)
          rsPal <<-  colorNumeric(palette = "magma", reverse = TRUE,
                                  domain = rng_rstif, na.color = "transparent")
          
          leafsurface <<- leafsurface %>%
            addRasterImage(newtif, colors = rsPal, opacity = .7, group = "Surface resistance") %>%
            addLegend(pal =  rsPal, values = hs2rs_tif[],
                      position = 'bottomleft',
                      title= "Unknow units"#, opacity = .3
                      #, labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
            )  %>% addLayersControl(
              baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
              overlayGroups = c("Habitat suitability", "Surface resistance"),
              options = layersControlOptions(collapsed = FALSE)
            )
          
          pointssurface <<- leafsurface
          output$ll_map_points <- renderLeaflet({pointssurface})
          
          distancesurface <<- leafsurface
          output$ll_map_dist <- renderLeaflet({distancesurface})
          
          
          leafsurface
        } else {
          vtext <<- paste0(vtext, '\n -- ERROR')
          (output$voutext <- renderText({(vtext)}))
        }
      })
    }
  })
  
  
  ####### > POINTS  ------------------
  
  ##> voutext_points; ll_map_points; points_py; in_points_3 -- 5; in_points_tif
  
  
  observeEvent(input$in_points_tif, {
    
    invisible(suppressWarnings(tryCatch(file.remove(c(tifpath_pts, newtifPath_pts)), 
                                        error = function(e) NULL)))
    
    # tempFolder <- '/data/temp//T2023082911164705_file3112795957d7/'
    #tifpath <- '/data/temp//T2023082911164705_file3112795957d7//in_surface_C2023082911165605_file31126374e76.tif'
    #inSurSessID <- 'C2023082911165605_file31126374e76'
    
    if(is.null(rv$inSurSessID)){
      if(devug){
        print(paste(' ----- new rv$inSurSessID ',  rv$inSurSessID) )
      }
      (inSurSessID <- sessionIDgen())
      rv$inSurSessID <- inSurSessID
    }
    
    (inPointsSessID <- sessionIDgen())
    rv$inPointsSessID <- inPointsSessID
    
    if(devug){
      #print(paste(' ----- inSurSessID ', inSurSessID, ' || rv$ ', rv$inSurSessID ) )
      print(paste(' -----  rv$newtifPath_pts ', rv$newtifPath_pts, '\n' ) ); 
      print(paste(' ----- is.null(rv$newtifPath_pts) || ', is.null(rv$newtifPath_pts) ) )
      #print(paste(' -----     newtifPath_pts ', newtifPath_pts, '\n' ) ); 
      #print(paste(' ----- file.exists(rv$newtifPath_pts) || ', file.exists(rv$newtifPath_pts) ) )
      #print(paste(' ----- file.exists(   newtifPath_pts) || ', file.exists( newtifPath_pts) ) )
      # print(paste(' ----- newtifPath_pts $ || ', newtifPath_pts, (rv$newtifPath_pts) ) )
    }
    
    
    tifpath_pts0 <- paste0(tempFolder, '/in_points_', inPointsSessID, '.tif')
    if (!is.null(rv$newtifPath_pts) ){
      if (file.exists(rv$newtifPath_pts)) {
        newtifPath_pts <- tifpath_pts <- rv$newtifPath_pts
        rv$newtifPath_pts <- newtifPath_pts
        rv$pointsmap <- 1
      } else {
        tifpath_pts <- tifpath_pts0
        file.copy(input$in_points_tif$datapath, tifpath_pts)
        rv$pointsmap <- 1
        
      }
    } else {
      tifpath_pts <- tifpath_pts0
      file.copy(input$in_points_tif$datapath, tifpath_pts)
      
      voutext_points <<- paste0(voutext_points, '\nUpdating raster: square pixels and -9999 no data')
      isolate(output$voutext <- renderText({isolate(voutext_points)}))
      
      tifpathfixed_pts <- paste0(tempFolder, '/in_surface_fixed_', rv$inSurSessID, '.tif')
      newtifPath_pts <- fitRaster2cola(inrasterpath = tifpath_pts, outrasterpath = tifpathfixed_pts)
      newtifPath_pts <- ifelse(is.na(newtifPath_pts), yes = tifpath_pts, no = newtifPath_pts)
      rv$newtifPath_pts <- newtifPath_pts
      voutext_points <<- paste0(voutext_points, ' --- DONE')
      (output$voutext_points <- renderText({(voutext_points)}))
      rv$pointsmap <- 1
      
    }
    
    output$ll_map_points <- renderLeaflet({
      #newtifPath <- "/data/temp/2023082821124805_file5762732c645/in_surfaceoutfile2c243f6174c8.tif"
      if(devug){ print(' ----- HERE1')}
      newtif_pts <- raster(newtifPath_pts)
      if(devug){ print(' ----- HERE2')}
      
      #rng_newtif <- c(newtif@data@min, newtif@data@max)
      rng_newtif_pts <- cellStats(newtif_pts, stat = range)
      ptsPal <<-  colorNumeric(palette = "viridis", reverse = TRUE,
                               domain = rng_newtif_pts, na.color = "transparent")
      
      pointssurface <<- leaflet() %>% addTiles() %>% 
        addRasterImage(newtif_pts, colors = ptsPal, opacity = .7, group = "Surface resistance") %>%
        addLegend(pal =  ptsPal, values = newtif_pts[],
                  position = 'topleft',
                  title= "Resistance"#, opacity = .3
                  #, labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
        )
      if(devug){ print(' ----- HERE3')}
      pointssurface
    })
  })
  
  
  observeEvent(input$points_py, {
    if(devug){ print(paste(' ----- rv$pointsmap ', rv$pointsmap ) ); 
      print(paste(' ----- inSurSessID ', # inSurSessID, 
                  ' -||- rv$inSurSessID ', rv$inSurSessID ) )}
    if(!is.null(rv$pointsmap)){
      # rv <- list(newtifPath = '/data/temp//E-2023082911285005_file3112135d2b4c//in_surface_V-2023082911285705_file3112303ea820.tif',
      #            inSurSessID = 'V-2023082911285705_file3112303ea820')
      # input <- list(in_points_3 = 5, in_points_4 =95, in_points_5 = 100, in_surf_6 = 1, in_surf_7 = -9999)
      # rv <- list(newtifPath_pts = "/data/temp//E2023082913264105file31121089830//in_points_X2023082913264705file3112603b8bfe.tif")
      # out_pts <- "/data/temp//E2023082913264105file31121089830//out_surface_K2023082913264705file311229869b74.shp"
      
      newtif <- raster(rv$newtifPath_pts)
      out_pts <- paste0(tempFolder, '/out_surface_', rv$inSurSessID, '.shp')
      if(devug){ print(' ----- newtifPath points'); print(rv$newtifPath_pts);
        print(' ----- newShp points'); print(out_pts)}
      
      output$ll_map_points <- renderLeaflet({
        voutext_points <<- paste0(voutext_points, '\nCreating points')
        (output$voutext_points <- renderText({(voutext_points)}))
        
        points_file <- points_shp(py = py, rv$newtifPath_pts, out_pts, 
                                  as.numeric(input$in_points_3),
                                  as.numeric(input$in_points_4),
                                  as.numeric(input$in_points_5))
        
        points_file <- readOGR(out_pts)
        points_file_wgs <- spTransform(points_file, CRSobj = CRS("+proj=longlat +ellps=GRS80"))
        points_file_wgs$ID <- 1:nrow(points_file)
        
        
        pointssurface <<- pointssurface %>%
          addMarkers(data = points_file_wgs, label = ~ID, group = 'Points') %>% 
          addLayersControl(
            baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
            overlayGroups = c("Points", "Surface resistance"),
            options = layersControlOptions(collapsed = FALSE)
          )
        pointssurface
      })
      
    } else {
      
    }
  })
  
  ####### > DISTANCE  ------------------
  
  ##> voutext_dist; ll_map_dist; distance_py; in_distance_3, 
  ##> in_distance_shp in_dist_tif, inDistSessID distmap newtifPath_dist newshpPath_dist
  # distmap = NULL, distrast = NULL, distshp = NULL,
  
  observeEvent(input$in_dist_tif, {
    invisible(suppressWarnings(tryCatch(file.remove(c(tifPath_dist, newtifPath_dist)), 
                                        error = function(e) NULL)))
    if(is.null(rv$inSurSessID)){
      if(devug){
        print(paste(' ----- new rv$inSurSessID ',  rv$inSurSessID) )
      }
      (inSurSessID <- sessionIDgen())
      rv$inSurSessID <- inSurSessID
    }
    
    (inDistSessID <- sessionIDgen())
    rv$inDistSessID <- inDistSessID
    
    if(devug){
      print(paste(' -----  rv$newtifPath_dist ', rv$newtifPath_dist, '\n' ) ); 
      print(paste(' ----- is.null(rv$newtifPath_dist) || ', is.null(rv$newtifPath_dist) ) )
    }
    
    
    tifpath_dist0 <- paste0(tempFolder, '/in_dist_', inDistSessID, '.tif')
    if (!is.null(rv$newtifPath_dist) ){
      if (file.exists(rv$newtifPath_dist)) {
        newtifPath_dist <- tifpath_pts <- rv$newtifPath_dist
        rv$newtifPath_dist <- newtifPath_dist
        rv$distrast <- 1
      } else {
        tifpath_dist <- tifpath_dist0
        file.copy(input$in_dist_tif$datapath, tifpath_dist)
        rv$distrast <- 1
      }
    } else {
      tifpath_dist <- tifpath_dist0
      file.copy(input$in_dist_tif$datapath, tifpath_dist) #!
      
      voutext_dist <<- paste0(voutext_dist, '\nUpdating raster: square pixels and -9999 no data')
      isolate(output$voutext <- renderText({isolate(voutext_dist)}))
      if(devug){
        print(paste(' -----  tifpath_dist ', tifpath_dist, '\n' ) ); 
        print(paste(' ----- file.exists(tifpath_dist) || ', file.exists(tifpath_dist) ) )
      }
      
      tifpathfixed_dist <- paste0(tempFolder, '/in_surface_fixed_', rv$inSurSessID, '.tif')
      newtifPath_dist <- fitRaster2cola(inrasterpath = tifpath_dist, outrasterpath = tifpathfixed_dist)
      newtifPath_dist <- ifelse(is.na(newtifPath_dist), yes = tifpath_dist, no = newtifPath_dist)
      rv$newtifPath_dist <- newtifPath_dist
      
      voutext_dist <<- paste0(voutext_dist, ' --- DONE')
      (output$voutext_dist <- renderText({(voutext_dist)}))
      rv$distmap <- 1
    }
    
    
    output$ll_map_dist <- renderLeaflet({
      
      
      #newtifPath_dist <- '/data/temp//Q2023082914052705file3112732859f8//in_dist_W2023082914053305file311253e79e6a.tif'
      newtif_dist <- raster(newtifPath_dist)
      rng_newtif_dist <- cellStats(newtif_dist, stat = range)
      distPal <<-  colorNumeric(palette = "viridis", reverse = TRUE,
                                domain = rng_newtif_dist, na.color = "transparent")

      distsurface <<- leaflet() %>% addTiles() %>% 
        addRasterImage(newtif_dist, colors = distPal, opacity = .7, group = "Surface resistance") %>%
        addLegend(pal =  distPal, values = newtif_dist[],
                  position = 'topleft',
                  title= "Resistance"#, opacity = .3
                  #, labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
        )
      rv$distrast <- 1
      distsurface

    })
  })
  
  
  ##> voutext_dist; ll_map_dist; distance_py; in_distance_3, 
  ##> in_distance_shp in_dist_tif, inDistSessID distmap newtifPath_dist newshpPath_dist
  observeEvent(input$indistshp, {
    if(devug){
      cat(paste(' \n\n----- INPUT ') )
      cat(names(input),'\n')
      print(str(input$indistshp))
      listX <- input$indistshp
      save(listX, file = paste0(tempPath, '/test_input_shp.RData'))
      load('/data/temp/test_input_shp.RData')
    }
    
    invisible(suppressWarnings(tryCatch(file.remove(c(in_distance_shp, newin_distance_shp)), 
                                        error = function(e) NULL)))
    if(is.null(rv$inSurSessID)){
      if(devug){
        print(paste(' ----- new rv$inSurSessID ',  rv$inSurSessID) )
      }
      (inSurSessID <- sessionIDgen())
      rv$inSurSessID <- inSurSessID
    }
    
    if(devug){ print(paste(' - HERE A ' ) ) }
    
    if(is.null(rv$inDistSessID)){
      if(devug){
        print(paste(' ----- new rv$inDistSessID ',  rv$inDistSessID) )
      }
      (inDistSessID <- sessionIDgen())
      rv$inDistSessID <- inDistSessID
    }
    
    if(is.null(rv$distrast)){
      
      voutext_dist <<- paste0(voutext_dist, '\nSTOP: load a valid surface raster first')
      
    } else {
      
    }
    # distmap = NULL, distrast = NULL, distshp = NULL,
    
    
    shpFilesPath <- paste0(tempFolder, '/in_shp_files_', inDistSessID, '.RData')
    inFiles <- input$indistshp # inFiles <- listX
    rv$inDistSessID
    #save(input$in_dist_shp, file = shppath_dist0)
    
    if(devug){
      print(paste(' -----  shppath_dist0 ', shppath_dist0, '\n' ) ); 
      print(paste(' -----  rv$newtifPath_dist ', rv$newtifPath_dist, '\n' ) ); 
      print(paste(' ----- is.null(rv$newtifPath_dist) || ', is.null(rv$newtifPath_dist) ) )
    }
    
    
    
    if (!is.null(rv$newtifPath_dist) ){
      if (file.exists(rv$newtifPath_dist)) {
        newtifPath_dist <- tifpath_pts <- rv$newtifPath_dist
        rv$newtifPath_dist <- newtifPath_dist
        rv$pointsmap <- 1
      } else {
        tifpath_dist <- tifpath_dist0
        file.copy(input$in_dist_tif$datapath, tifpath_dist)
        rv$distmap <- 1
      }
    } else {
      tifpath_dist <- tifpath_dist0
      file.copy(input$in_dist_tif$datapath, tifpath_dist) #!
      
      voutext_dist <<- paste0(voutext_dist, '\nUpdating raster: square pixels and -9999 no data')
      isolate(output$voutext <- renderText({isolate(voutext_dist)}))
      if(devug){
        print(paste(' -----  tifpath_dist ', tifpath_dist, '\n' ) ); 
        print(paste(' ----- file.exists(tifpath_dist) || ', file.exists(tifpath_dist) ) )
      }
      
      tifpathfixed_dist <- paste0(tempFolder, '/in_surface_fixed_', rv$inSurSessID, '.tif')
      newtifPath_dist <- fitRaster2cola(inrasterpath = tifpath_dist, outrasterpath = tifpathfixed_dist)
      newtifPath_dist <- ifelse(is.na(newtifPath_dist), yes = tifpath_dist, no = newtifPath_dist)
      rv$newtifPath_dist <- newtifPath_dist
      
      voutext_dist <<- paste0(voutext_dist, ' --- DONE')
      (output$voutext_dist <- renderText({(voutext_dist)}))
      rv$distmap <- 1
    }
    
    if(devug){ print(paste(' - HERE A ' ) ) }
    
    output$ll_map_dist <- renderLeaflet({
      
      
      #newtifPath_dist <- '/data/temp//Q2023082914052705file3112732859f8//in_dist_W2023082914053305file311253e79e6a.tif'
      newtif_dist <- raster(newtifPath_dist)
      rng_newtif_dist <- cellStats(newtif_dist, stat = range)
      distPal <<-  colorNumeric(palette = "viridis", reverse = TRUE,
                                domain = rng_newtif_dist, na.color = "transparent")
      if(devug){ print(paste(' - HERE A ' ) ) }
      
      distsurface <<- leaflet() %>% addTiles() %>% 
        addRasterImage(newtif_dist, colors = distPal, opacity = .7, group = "Surface resistance") %>%
        addLegend(pal =  distPal, values = newtif_dist[],
                  position = 'topleft',
                  title= "Resistance"#, opacity = .3
                  #, labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
        )
      rv$indistrast <- 1
      
      distsurface
    })
  })
  
  
  
  
  observeEvent(input$distance_py, {
    if(devug){ print(paste(' ----- rv$pointsmap ', rv$pointsmap ) ); 
      print(paste(' ----- inSurSessID ', # inSurSessID, 
                  ' -||- rv$inSurSessID ', rv$inSurSessID ) )}
    if(!is.null(rv$pointsmap)){
      # rv <- list(newtifPath = '/data/temp//E-2023082911285005_file3112135d2b4c//in_surface_V-2023082911285705_file3112303ea820.tif',
      #            inSurSessID = 'V-2023082911285705_file3112303ea820')
      # input <- list(in_points_3 = 5, in_points_4 =95, in_points_5 = 100, in_surf_6 = 1, in_surf_7 = -9999)
      # rv <- list(newtifPath_pts = "/data/temp//E2023082913264105file31121089830//in_points_X2023082913264705file3112603b8bfe.tif")
      # out_pts <- "/data/temp//E2023082913264105file31121089830//out_surface_K2023082913264705file311229869b74.shp"
      
      newtif <- raster(rv$newtifPath_pts)
      out_pts <- paste0(tempFolder, '/out_surface_', rv$inSurSessID, '.shp')
      if(devug){ print(' ----- newtifPath points'); print(rv$newtifPath_pts);
        print(' ----- newShp points'); print(out_pts)}
      
      output$ll_map_points <- renderLeaflet({
        voutext_points <<- paste0(voutext_points, '\nCreating points')
        (output$voutext_points <- renderText({(voutext_points)}))
        
        points_file <- points_shp(py = py, rv$newtifPath_pts, out_pts, 
                                  as.numeric(input$in_points_3),
                                  as.numeric(input$in_points_4),
                                  as.numeric(input$in_points_5))
        
        points_file <- readOGR(out_pts)
        points_file_wgs <- spTransform(points_file, CRSobj = CRS("+proj=longlat +ellps=GRS80"))
        points_file_wgs$ID <- 1:nrow(points_file)
        
        
        pointssurface <<- pointssurface %>%
          addMarkers(data = points_file_wgs, label = ~ID, group = 'Points') %>% 
          addLayersControl(
            baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
            overlayGroups = c("Points", "Surface resistance"),
            options = layersControlOptions(collapsed = FALSE)
          )
        pointssurface
      })
      
    } else {
      
    }
  })
  
}

shinyApp(ui, server)



###### LINUX SERVER COPY -------------

# sudo cp /home/shiny/connectscape/ /srv/shiny-server/cola -R
# sudo cp /home/shiny/connectscape/app.R /srv/shiny-server/cola/app.R


# sudo rm /home/shiny/tmpR/leafSim.RDatasudo cp /home/vmuser/gedivis /srv/shiny-server/gedivis -R
# sudo rm /var/log/shiny-server/gedivis/*
# #sudo rm /srv/shiny-server/gedivis/*
#  sudo su - -c "R -e \"shinyParallel::installShinyParallel('/home/vmuser/gedivis/', max.sessions = 25)\"" # home/shinyusername/
# #sudo rm /srv/shiny-server/gedivis2 -R
# sudo cp /home/vmuser/gedivis /srv/shiny-server/gedivis2 -R
# sudo cat /var/log/shiny-server/gedivis_
