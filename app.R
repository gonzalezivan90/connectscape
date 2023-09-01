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

# debug insall order: htmltools >> shiny >> shinyWidgets
# install.packages('shinyWidgets')
# install.packages('shinydashboardPlus')
# install.packages('dashboardthemes')

source('/home/shiny/connectscape/cola_tools.R')
rootPath <- '/data/temp/'; #dir.create(rootPath)
devug <<- TRUE
logPath <<- '/data/tempR/logFoldersR.txt'


# if ( identical ( unname(Sys.info()[c("sysname", 'nodename')]), c("Windows", 'HP-Z400')) ){
#   setwd('N:/Mi unidad/IG/server_IG/gedivis')
#   #setwd('N:/Mi unidad/IG/server_IG/gedivis/')
# }


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

checkEnv <- function(){
  print('Names rv')
  print(names(rv))
  
  print(' rv:' )
  print(rv)
}


(sessionID <<- sessionIDgen())
tempFolder <<- paste0(rootPath, '/', sessionID, '/')
dir.create(tempFolder)
print(paste(' >>>> tempFolder: ', tempFolder))


## Clean files
cleanMemory <- function(logPath){
  dfm <- data.frame(Rtmp = tempdir(), tempFolder = tempFolder)
  
  if(file.exists(logPath)){
    logDF <- read.csv(logPath)
  } else {
    logDF <- NULL
  }
  
  logDF <- rbind(logDF, dfm)
  
  openFolders <- dir.exists(logDF$Rtmp)
  sapply(logDF$tempFolder[!openFolders], unlink, recursive = TRUE)
  logDF <- logDF[dir.exists(logDF$Rtmp), ] 
  write.csv(x = logDF, logPath, row.names = FALSE)
}
cleanMemory(logPath)

#tempFolder <- paste0(rootPath, '/2023082821124805_file5762732c645/')

#rootPath <- sort(Sys.getenv(c("TEMP", 'TMP')), decreasing=T)[1]

py <- '/home/shiny/anaconda3/envs/cdujlab/bin/python'

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


points_shp <- function(py, intif, outshp, param3, param4, param5){
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

cdmat_py <- function(py, inshp, intif, outcsv, param3, param4 = 1){
  # param3 = 25000
  # create_cdmat.py
  # [1] source points
  # [2] resistance surface
  # [3] output file name
  # [4] distance threshold (in cost distance units)
  
  src <- '/home/shiny/connecting-landscapes/src/create_cdmat.py'
  (cmd_cdmat <- paste0(py, ' ', src, ' ', inshp, ' ', intif, ' ', outcsv, ' ', param3, ' ', param4))
  
  intCMD <- tryCatch(system(cmd_cdmat, intern = TRUE, ignore.stdout = TRUE), error = function(e) NULL)
  return(file = ifelse(file.exists(outcsv), outcsv, NA))
}

lcc_py <- function(py, inshp, intif, outtif, param4, param5, param6, param7){
  # param3 = 25000
  # [1] source points: Spatial point layer (any ORG driver), CSV (X, Y files), or *.xy file
  # [2] resistance surface
  # [3] output file name
  # [4] distance threshold (should be in meters*)
  # [5] corridor smoothing factor (in number of cells)
  # [6] corridor tolerance (in cost distance units)
  
  src <- '/home/shiny/connecting-landscapes/src/lcc.py'
  (cmd_lcc <- paste0(py, ' ', src, ' ', inshp, ' ', intif, ' ', outtif, ' ', param4, ' ', param5, ' ', param6, " ", param7))
  
  intCMD <- tryCatch(system(cmd_lcc, intern = TRUE, ignore.stdout = TRUE), error = function(e) NULL)
  return(file = ifelse(file.exists(outtif), outtif, NA))
}

crk_py <- function(py, inshp, intif, outtif, param4, param5, param6 = 1){
  # [1] source points
  # [2] resistance surface
  # [3] output file name
  # [4] distance threshold (in cost distance units)
  # [5] kernel shape (linear, gaussian)
  
  src <- '/home/shiny/connecting-landscapes/src/crk.py'
  (cmd_crk <- paste0(py, ' ', src, ' ', inshp, ' ', intif, ' ', outtif, ' ', param4, ' ', param5))
  
  intCMD <- tryCatch(system(cmd_crk, intern = TRUE, ignore.stdout = TRUE), error = function(e) NULL)
  return(file = ifelse(file.exists(outtif), outtif, NA))
}


loadShp <- function(inFiles, tempFolder, sessID){ # inFiles <- input$shapefile
  # rv$inDistSessID
  # sessID <- (inDistSessID <- sessionIDgen())
  # tempFolder <- "/data/temp//S2023083119293205file84e74af43438/"
  # save(inFiles, file = paste0(rootPath, '/inFiles_input_shp.RData'))
  # sss <- load(paste0(tempFolder, '/shpfiles.RData')) # sss
  outshp <- list()
  if ( class(inFiles) == "NULL" ){
    outshp$mssg <- 'Error in loading shapefile'
  } else {
    if ( nrow(inFiles) == 1){
      if(grepl('*\\.zip', inFiles$name)){ ## zip files
        outZip <- paste0(tempFolder, '/shp_', sessID); 
        dir.create(outZip)
        unzip(zipfile = inFiles$newFile, exdir = outZip)
        uZ <- list.files(outZip)
        #x0 <- uZ; print(x0); print(class(x0)); print(str(x0))
        outshp$shp <- tryCatch(readOGR(outZip, layer = tools::file_path_sans_ext(uZ[1])), error = function (e) NULL)
        outshp$files <- uZ
        outshp$layer <- grep(pattern = '.shp', outshp$files, value = TRUE)
      } else if (grepl('\\.SQLite|\\.gpkg|\\.GeoJSON', inFiles$name)){ ## single
        #save(inFiles, file = 'inFileSingle.RData'); 
        outshp$shp <- tryCatch(readOGR(inFiles$newFile[1]), error = function (e) NULL)
        outshp$files <- inFiles$newFile
        outshp$layer <- inFiles$newFile[1]
      } else if (grepl('\\.csv', inFiles$name)){ ## single
        #save(inFiles, file = 'inFileSingle.RData'); load(paste0(rootPath, '/inFiles_input_shp.RData')) # sss
        # inFiles <- inFiles[5, ]
        outshp$shp <- tryCatch(readOGR(inFiles$newFile[1]), error = function (e) NULL)
        outshp$files <- inFiles$newFile
        outshp$layer <- inFiles$newFile[1]
      } else if (grepl('\\.xy', inFiles$name)){ ## single
        #save(inFiles, file = 'inFileSingle.RData'); load(paste0(rootPath, '/inFiles_input_shp.RData')) # sss
        # inFiles <- inFiles[5, ]
        outshp$shp <- tryCatch(read.csv(inFiles$newFile[1]), error = function (e) NULL)
        outshp$shp$x1 <- outshp$shp[, grep('X|x', colnames(outshp$shp), value = TRUE)[1]]
        outshp$shp$y1 <- outshp$shp[, grep('Y|y', colnames(outshp$shp), value = TRUE)[1]]
        coordinates(outshp$shp) =~ x1 + y1
        outshp$files <- inFiles$newFile
        outshp$layer <- inFiles$newFile[1]
      }
    } else if ( nrow(inFiles) >= 3  & all(sapply(c('\\.shp', '\\.shx', '\\.dbf'), grep, inFiles$name)) ){ ## shp several
      
      #inFiles$datapath2 <- gsub('\\/[0-9]\\.', '/1.', inFiles$newFile)
      #sapply(inFiles$newFile, USE.NAMES = F, function(x){file.rename(x,  gsub('\\/[0-9]\\.', '/1.', x) ) })
      
      outshp$shp <- tryCatch(readOGR(dirname(inFiles$newFile[1]),
                                     basename(tools::file_path_sans_ext(inFiles$newFile[1]))),
                             error = function (e) e)
      outshp$files <- inFiles$newFile
      outshp$layer <- grep(pattern = '.shp', outshp$files, value = TRUE)
    }
    
    if (is.na(outshp$shp@proj4string@projargs)){
      outshp$mssg <- 'No proyection in shapefile'
    }
    
    if (class(outshp$shp) == 'SpatialPointsDataFrame'){
      outshp$shp$ID <- 1:nrow(outshp$shp)
    }
      
    # updateSelectInput(session, 'aoi_sur', choices = c('Dibujar', 'Capa'), selected = 'Capa')
    # pdebug("is.null(py)", 'inSurSessID', sep = '\n', pre = ' -- ')
  }
  return(outshp)
}


pdebug <- function(devug, sep = ' || ', pre = ' -- ', ...){
  if (devug){
    x. = c(...)
    # x. = c('is.null(rv$newtifPath_dist)', 'rv$newtifPath_dist')
    # print(x.)
    cat('\n', pre) 
    invisible(sapply(x., function(x){
      # x = x.[2]
      y <- tryCatch(expr = eval(parse(text = x)), error = function(e) '-err-')
      y <- ifelse(!is.null(y), y, 'NULL')
      tryCatch(cat(x, ": ", y, sep), error = function(e) e)
    }))
  }
} # pdebug("is.null(py)", 'inSurSessID', sep = '\n', pre = ' -- ')

# pdebug(devug=devug,sep='\n',pre='--', 'is.null(rv$newtifPath_dist)', 'rv$newtifPath_dist') ######
# pdebug(devug=devug,sep='\n',pre='--','print(inFiles)') 




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




#  >> UI ---------------------------------------------------------------------------

ui <- dashboardPage(
  #useShinyjs(),
  header = shinydashboard::dashboardHeader(
    title = "ConnectingLandscapes v0"
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
                                    shiny::fileInput('in_dist_shp', 'Load points', buttonLabel = 'Search', 
                                                     placeholder = 'No file(s) ',
                                                     accept=c('.shp','.dbf','.sbn','.sbx','.shx',".prj", '.zip', '.gpkg', '.SQLite', '.GeoJSON', '.csv', '.xy'),
                                                     multiple=TRUE),
                                    #actionButton("dist_shp", "Load points!"),
                                    
                  ),
                  
                  menuItem("CDPOP", tabName = "tab_cdpop", icon = icon("hippo")),
                  
                  
                  menuItem("Connectivity - corridors", tabName = "tab_corridors", icon = icon("route")),
                  conditionalPanel( 'input.sidebarid == "tab_corridors"',
                                    shiny::fileInput('in_lcc_tif', 'Load TIF', 
                                                     buttonLabel = 'Search', placeholder = 'No file',
                                                     accept=c('.tif'), multiple=FALSE),
                                    shiny::fileInput('in_lcc_shp', 'Load points', buttonLabel = 'Search', 
                                                     placeholder = 'No file(s) ',
                                                     accept=c('.shp','.dbf','.sbn','.sbx','.shx',".prj", '.zip', '.gpkg', '.SQLite', '.GeoJSON', '.csv', '.xy'),
                                                     multiple=TRUE),
                                    #actionButton("dist_shp", "Load points!"),
                                    
                  ),
                  menuItem(HTML(paste("Connectivity", "dispersal kernels", sep="<br/>")),
                           tabName = "tab_kernels", icon = icon("bezier-curve")),
                  conditionalPanel( 'input.sidebarid == "tab_kernels"',
                                    shiny::fileInput('in_crk_tif', 'Load TIF', 
                                                     buttonLabel = 'Search', placeholder = 'No file',
                                                     accept=c('.tif'), multiple=FALSE),
                                    shiny::fileInput('in_crk_shp', 'Load points', buttonLabel = 'Search', 
                                                     placeholder = 'No file(s) ',
                                                     accept=c('.shp','.dbf','.sbn','.sbx','.shx',".prj", '.zip', '.gpkg', '.SQLite', '.GeoJSON', '.csv', '.xy'),
                                                     multiple=TRUE),
                                    #actionButton("dist_shp", "Load points!"),
                                    
                  ),
                  
                  
                  menuItem("Plotting", tabName = "tab_plotting", icon = icon("image")),
                  menuItem("Mapping", tabName = "tab_maping", icon = icon("map")),
                  menuItem("Connectivity - prioritization", 
                           tabName = "tab_priori", icon = icon("trophy")),
                  menuItem(HTML(paste("Landscape genetics", "mapping tools", sep="<br/>")),
                           tabName = "tab_genetics", icon = icon("route")),
                  menuItem("Run locally", tabName = "tab_local", icon = icon("code-fork")),
                  
                  menuItem("Page 1", tabName = "page1"),
                  conditionalPanel(
                    'input.sidebarid == "page1"',
                    sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30),
                    selectInput("title", "Select plot title:", choices = c("Hist of x", "Histogram of x"))
                  ),
                  
                  # conditionalPanel(
                  #   'input.sidebarid <> "pagex"',
                  #   verbatimTextOutput("outext")
                  # ),
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
                verbatimTextOutput("vout_h2r") %>% withSpinner(color="#0dc5c1"),
                leafletOutput("ll_map_h2r", height = "600px") %>% withSpinner(color="#0dc5c1"),
                actionButton("h2r", "Get resistance surface"),
                
                textInput("in_surf_3", "Min-grid:", '0'),
                textInput("in_surf_4", "Max-grid:", '100'),
                textInput("in_surf_5", "Max-resistance:", '100'),
                textInput("in_surf_6", "Shape:", '1'),
                textInput("in_surf_7", "No Data:", '-9999')
        ),
        
        
        # UI Tab points ----
        
        tabItem('tab_points',
                h1(' Create points'),
                verbatimTextOutput("vout_points") %>% withSpinner(color="#0dc5c1"),
                leafletOutput("ll_map_points", height = "600px") %>% withSpinner(color="#0dc5c1"),
                actionButton("points_py", "Get points"),
                
                textInput("in_points_3", "Min-grid:", '2'),
                textInput("in_points_4", "Max-grid:", '95'),
                textInput("in_points_5", "Number of points:", '50')
        ),
        
        ##> vout_points; ll_map_points; points_py; in_points_3 -- 5
        
        
        # UI Tab points ----
        
        tabItem('tab_distance',
                h1(' Create Distance'),
                verbatimTextOutput("vout_dist") %>% withSpinner(color="#0dc5c1"),
                leafletOutput("ll_map_dist", height = "600px") %>% withSpinner(color="#0dc5c1"),
                actionButton("dist_py", "Get matrix"),
                textInput("in_dist_3", "Distance threshold (in cost distance units):", '25000'),
                valueBoxOutput("dist_box1"),
        ),
        
        ##> vout_dist; ll_map_dist; dist_py; in_distance_3, in_distance_shp in_dist_tif
        
        tabItem('tab_corridors',
                h1(' Create corridors'),
                verbatimTextOutput("vout_lcc") %>% withSpinner(color="#0dc5c1"),
                leafletOutput("ll_map_lcc", height = "600px") %>% withSpinner(color="#0dc5c1"),
                actionButton("lcc", "Get corridors"),
                textInput("in_lcc_4", "Distance threshold (in distance units):", '25000'),
                textInput("in_lcc_5", "Corridor smoothing factor:", '5'),
                textInput("in_lcc_6", "Corridor tolerance (in cost distance units):", '5')
                # ll_map_corr lcc vout_corr in_lcc_3 4 5
        ),
        
        tabItem('tab_kernels',
                h1(' Create kernels'),
                verbatimTextOutput("vout_crk") %>% withSpinner(color="#0dc5c1"),
                leafletOutput("ll_map_crk", height = "600px") %>% withSpinner(color="#0dc5c1"),
                actionButton("crk", "Get kernels"),
                textInput("in_crk_4", "Distance threshold (in cost distance unit):", '25000'),
                selectInput(inputId = "in_crk_5", label = "Kernel shape:",
                            choices =  c( 'linear', 'gaussian'), # 'RH',
                            selected = 'linear')
                
                # ll_map_crk crk vout_crk in_crk_3 4
        ),
        
        tabItem('tab_plotting',
                h1(' Create plots'),
                leafletOutput("ll_map_plot", height = "600px") %>% withSpinner(color="#0dc5c1")
        ),
        
        tabItem('tab_mapping',
                h1(' Create maps'),
                leafletOutput("ll_map_map", height = "600px") %>% withSpinner(color="#0dc5c1")
        ),
        
        tabItem('tab_priori',
                h1(' Priorization')
        ),
        tabItem('tab_genetics',
                h1(' Landscape genetics')
        ),
        tabItem('tab_local',
                h1(' Running this locally')
        ),
        
        
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


#  >> SERVER ---------------------------------------------------------------------------
server <- function(input, output, session) {
  
  updateLL <- function(ll){
    output$ll_map_lcc <- output$ll_map_crk <- output$ll_map_map <- output$ll_map_plot <- 
      output$ll_map_dist <- output$ll_map_points <- output$ll_map_h2r <- renderLeaflet({
        ll
      })
  }
  
  updateVTEXT <- function(txt, devug = FALSE){
    if(devug){print(txt)}
    output$vout_h2r <- output$vout_points <-  
      output$vout_dist <- output$vout_crk <- output$vout_cdpop <- 
      output$vout_lcc <- renderText({isolate(txt)})
  }
  
  output$distPlot <- renderPlot({
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = "darkgray", border = "white", main = input$title)
  })
  
  
  # SRV DEFAULT  ------------------
  rv <<- reactiveValues(
    sessionID = sessionID,  tempFolder = tempFolder,
    
    data = NULL, orig = NULL,
    cdpopRun = NULL, out_cdpop_files = c(''), 
    
    log = 'Waiting for inputs ... ',
    
    llmap0 = leaflet() %>% addTiles(),
    
    surfmap = NULL, pointsmap = NULL, 
    distmap = NULL, distrast = NULL, distshp = NULL,
    
    hsready = FALSE,
    tifready = FALSE,
    ptsready = FALSE,
    
    hs = FALSE, # path
    tif = FALSE, # path
    pts = FALSE, # path
    shp = FALSE, # spatial object
    
    
    newtifPath = NULL, 
    newtifPath_pts = NULL,
    newtifPath_dist = NULL,
    newshpPath_dist = NULL,
    tifpathlcc = NULL,
    tifpathcrk = NULL,
    tifpathlccfix = NULL,
    tifpathcrkfix = NULL,
    
    inSurSessID = NULL,
    inPointsSessID = NULL,
    inDistSessID = NULL,
    
    last = NULL)
  
  # rv <- list()
  
  rv$sessionID <- sessionID
  rv$tempFolder <- tempFolder
  
  llmap <- leaflet() %>% addTiles() %>% 
    addLayersControl(baseGroups = c("OpenStreetMap", "Esri.WorldImagery"),
                     options = layersControlOptions(collapsed = FALSE)) %>%
    addProviderTiles( "Esri.WorldImagery", group = "Esri.WorldImagery" ) %>%
    setView(lng = 25, lat = -21, zoom = 5) %>%
    leaflet.extras::addDrawToolbar(targetGroup='draw', polylineOptions = FALSE,
                                   rectangleOptions = FALSE, circleOptions = FALSE,
                                   markerOptions = FALSE, circleMarkerOptions = FALSE,
                                   editOptions = leaflet.extras::editToolbarOptions())
  rv$llmap0 <- rv$llmap <- llmap
  
  # rv$llmap rv$hsready rv$tifready rv$ptsready
  # ll_map_corr lcc vout_corr in_lcc_3 4 5
  # ll_map_crk crk vout_crk in_crk_3 4
  # ll_map_plot ll_map_map
  # rv$hsready = FALSE
  # rv$tifready = FALSE
  # rv$ptsready = FALSE
  
  updateLL(rv$llmap)
  updateVTEXT('Waiting for inputs')
  
  # output$ll_map_lcc <- output$ll_map_crk <- output$ll_map_map <- output$ll_map_plot <- 
  #   output$ll_map_dist <- output$ll_map_points <- output$ll_map_h2r <- renderLeaflet({
  #   rv$llmap
  # })
  
  vtext <- "Waiting for the habitat sutiability TIF"
  output$vout <- renderText({isolate(vtext)})
  
  vout_points <- "Waiting for the surface resistance TIF"
  output$vout_points <- renderText({isolate(vout_points)})
  
  vout_dist <- "Waiting for the surface resistance TIF and points"
  output$vout_dist <- renderText({isolate(vout_dist)})
  
  vout_crk <- "Waiting for the surface resistance TIF and points"
  output$vout_crk <- renderText({isolate(vout_crk)})
  
  vout_lcc <- "Waiting for the surface resistance TIF and points"
  output$vout_lcc <- renderText({isolate(vout_lcc)})
  
  
  ####### SRV CDPOP  ------------------
  # in_cdpop_cd 
  # in_cdpop_age
  # in_cdpop_xy
  
  #   name size type datapath
  # input <- list(in_cdpop_par$datapath = "/data/temp/2023082820202105_file3c454758300e/",
  #               in_cdpop_age$datapath = "/tmp/Rtmp083gNo/9489e1ac4d4f142bb1718552/0.csv",
  #               in_cdpop_cd$datapath = "",
  #               in_cdpop_par$datapath = "")
  
  if(FALSE){
    #tempFolder <- '/data/temp/2023082821124805_file5762732c645'
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
  
  # checkEnv()
  
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
    dat <- datatable(rv$data, editable = TRUE,
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
  
  
  output$cdpop_box1 <- output$dist_box1 <- renderValueBox({
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
  #DEBUG
  observeEvent(input$in_sur_tif, {
    
    #try(file.remove(c(tifpath, newtifPath)))
    invisible(suppressWarnings(tryCatch(file.remove(c(tifpath, newtifPath)), 
                                        error = function(e) NULL)))
    
    output$ll_map_h2r <- renderLeaflet({
      # tempFolder <- '/data/temp//T2023082911164705_file3112795957d7/'
      #tifpath <- '/data/temp//T2023082911164705_file3112795957d7//in_surface_C2023082911165605_file31126374e76.tif'
      #inSurSessID <- 'C2023082911165605_file31126374e76'
      (inSurSessID <<- sessionIDgen())
      rv$inSurSessID <<- inSurSessID
      tifpath <<- paste0(tempFolder, '/in_surface_', inSurSessID, '.tif')
      tifpathfixed <- paste0(tempFolder, '/in_surface_fixed_', inSurSessID, '.tif')
      
      
      file.copy(input$in_sur_tif$datapath, tifpath)
      
      #if(devug){ print(' ----- input$in_sur_tif'); print(input$in_sur_tif); print(tifpath); file.exists(tifpath)}
      
      rv$log <- paste0(rv$log, '\nUpdating raster: making pixels squared and -9999 as no data')
      updateVTEXT(rv$log)
      
      newtifPath <- fitRaster2cola(inrasterpath = tifpath, outrasterpath = tifpathfixed)
      newtifPath <- ifelse(is.na(newtifPath), yes = tifpath, no = newtifPath)
      if(is.na(newtifPath)){
        rv$log <- paste0(rv$log, '\n -- Error uploading the "Habitat suitability" TIF file')
        updateVTEXT(rv$log)
        
      } else {
        
        rv$newtifPath <- newtifPath
        rv$hs <- newtifPath
        rv$hsready <- TRUE
        rvhsready <- rv$hsready <- TRUE
        pdebug(devug=devug,sep='|',pre='-',"tempFolder","inSurSessID", 
             "rv$inSurSessID", "rvhsready")
        
        rv$log <- paste0(rv$log, '--- DONE')
        updateVTEXT(rv$log)
        
        # newtifPath <- "/data/temp//W2023083103371005file7a6d551e95a8//in_surface_W2023083103371705file7a6d3505be33.tif"
        pdebug(devug=devug,sep='\n',pre='-',"tifpath", "newtifPath", "rv$newtifPath", "rv$hs", "rv$hsready")
        
        newtif <- raster(newtifPath)
        
        #rng_newtif <- c(newtif@data@min, newtif@data@max)
        rng_newtif <- cellStats(newtif, stat = range)
        
        hsPal <<-  colorNumeric(palette = "viridis", reverse = TRUE,
                                domain = rng_newtif, na.color = "transparent")
        
        # rv$llmap rv$hsready rv$tifready rv$ptsready #  rv$llmap
        #rv$llmap <<- rv$llmap %>% 
        #leafsurface <<- leaflet() %>% addTiles() %>% 
        leafsurface <<- rv$llmap %>% removeControl('legendHabitat') %>% removeImage('habitatSuitability')  %>%
          addRasterImage(newtif, colors = hsPal, opacity = .7, 
                         layerId = "habitatSuitability",
                           group = "Habitat suitability") %>%
          addLegend(pal =  hsPal, values = newtif[], layerId = 'legendHabitat',
                    position = 'topleft',
                    title= "Unknow units"#, opacity = .3
                    #, labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
          ) %>% clearBounds() %>% addLayersControl(
            baseGroups = c("OpenStreetMap", "Esri.WorldImagery"),
          overlayGroups = c("Habitat suitability"),
          options = layersControlOptions(collapsed = FALSE)
        )
        
        
        rv$llmap <<- llmap <<- leafsurface
        updateLL(leafsurface)
        #llmap
        rv$llmap
      }
    })
  })
  
  
  
  observeEvent(input$h2r, {
    
    if(rv$hsready){
      # rv <- list(newtifPath = '/data/temp//E-2023082911285005_file3112135d2b4c//in_surface_V-2023082911285705_file3112303ea820.tif',
      #            inSurSessID = 'V-2023082911285705_file3112303ea820')
      # input <- list(in_surf_3 = 0, in_surf_4 =100, in_surf_5 = 100, in_surf_6 = 1, in_surf_7 = -9999)
      output$ll_map_h2r <- renderLeaflet({
        
        outs2r <- paste0(tempFolder, '/out_surface_', rv$inSurSessID, '.tif')
        rv$log <- paste0(rv$log,  # _______
                         '\nCreating resistance surface');updateVTEXT(rv$log) # _______
        
        hs2rs_file <- runS2RES(py = py, intif = rv$newtifPath, outtif = outs2r, 
                               as.numeric(input$in_surf_3), as.numeric(input$in_surf_4), 
                               as.numeric(input$in_surf_5), 
                               as.numeric(input$in_surf_6), as.numeric(input$in_surf_7))
        
        if(!is.na(hs2rs_file)){
          
          rv$tifready <- TRUE
          rv$tif <- hs2rs_file
          
          hs2rs_tif <- raster(hs2rs_file)
          rng_rstif <- cellStats(hs2rs_tif, stat = range)
          rsPal <<-  colorNumeric(palette = "magma", reverse = TRUE,
                                  domain = rng_rstif, na.color = "transparent")
          
          
          # rv$llmap rv$hsready rv$tifready rv$ptsready #  rv$llmap
          #rv$llmap <<- rv$llmap %>% 
          #leafsurface <<- leaflet() %>% addTiles() %>% 
          
          pdebug(devug=devug,sep='\n',pre='---H2S\n'," hs2rs_tif[]") # = = = = = = =  = = =  = = =  = = =  = = = 
          
          leafsurface <<- rv$llmap %>% removeControl('legendSurface') %>% removeImage('SurfaceResistance')  %>%
            addRasterImage(hs2rs_tif, colors = rsPal, opacity = .7,
                           layerId = 'SurfaceResistance',
                           group = "Surface resistance") %>%
            addLegend(pal =  rsPal, values = hs2rs_tif[], layerId = 'legendSurface',
                      position = 'bottomleft',
                      title= "Unknow units"#, opacity = .3
                      #, labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
            )  %>% addLayersControl(
              baseGroups = c("OpenStreetMap", "Esri.WorldImagery"),
              overlayGroups = c("Habitat suitability", "Surface resistance"),
              options = layersControlOptions(collapsed = FALSE)
            ) %>% clearBounds()
          
          rv$llmap <<- llmap <<- leafsurface
          updateLL(leafsurface)
          # leafsurface
          #llmap
          rv$llmap
          
          # pointssurface <<- leafsurface
          # output$ll_map_points <- renderLeaflet({pointssurface})
          # 
          # distancesurface <<- leafsurface
          # output$ll_map_dist <- renderLeaflet({distancesurface})
          
          
        } else {
          rv$log <- paste0(rv$log, '\n -- Error creating the "Surface resitance" TIF file')
          updateVTEXT(rv$log)
        }
      })
    }
  })
  
  
  ####### > POINTS  ------------------
  observeEvent(input$in_points_tif, {
    invisible(suppressWarnings(tryCatch(file.remove(c(rv$tifpathpts, rv$tifpathptsfix)), 
                                        error = function(e) NULL)))

    if(is.null(rv$inSurSessID)){
      pdebug(devug=devug,sep='\n',pre='-','rv$inSurSessID')
      (inSurSessID <- sessionIDgen())
      rv$inSurSessID <- inSurSessID
      pdebug(devug=devug,sep='\n',pre='-','rv$inSurSessID')
    }
    
    if(is.null(rv$inPointsSessID)){
      (inPointsSessID <- sessionIDgen())
      rv$inPointsSessID <- inPointsSessID
    }
    
    rv$tifpathpts <- paste0(tempFolder, '/in_points_', inPointsSessID, '.tif')
    file.copy(input$in_points_tif$datapath, rv$tifpathpts)
    
    
    # pdebug(devug=devug,sep='\n',pre='---H2S\n'," hs2rs_tif[]") # = = = = = = =  = = =  = = =  = = =  = = = 
    
    rv$log <- paste0(rv$log, '\nUpdating raster: making pixels squared and -9999 as no data');updateVTEXT(rv$log) # _______
    
    rv$tifpathptsfix <- paste0(tempFolder, '/in_surface_fixed_', rv$inSurSessID, '.tif')
    newtifPath_pts <- fitRaster2cola(inrasterpath = rv$tifpathpts, outrasterpath =  rv$tifpathptsfix)
    newtifPath_pts <- ifelse(is.na(newtifPath_pts), yes = rv$tifpathpts, no = newtifPath_pts)
    rv$newtifpathpts <- newtifPath_pts
    
    if (file.exists(rv$newtifpathpts)){
    rv$log <- paste0(rv$log, ' --- DONE');updateVTEXT(rv$log) # _______
      rv$tifready <- TRUE
      rv$tif <- newtifPath_pts
      
      output$ll_map_points <- renderLeaflet({
        
        newtif_pts <- raster(newtifPath_pts)
        rng_newtif_pts <- cellStats(newtif_pts, stat = range)
        ptsPal <<-  colorNumeric(palette = "viridis", reverse = TRUE,
                                 domain = rng_newtif_pts, na.color = "transparent")
        
        
        llmap <<- rv$llmap %>% removeImage(layerId = 'SurfaceResistance') %>% removeControl('legendSurface') %>% 
          addRasterImage(newtif_pts, colors = ptsPal, opacity = .7, 
                         group = "Surface resistance", layerId = 'SurfaceResistance') %>%
          addLegend(pal =  ptsPal, values = newtif_pts[], layerId = 'legendSurface',
                    position = 'topleft',
                    title= "Resistance"#, opacity = .3
                    #, labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
          ) %>% addLayersControl(
            baseGroups = c("OpenStreetMap", "Esri.WorldImagery"),
            overlayGroups = c("Habitat suitability", "Surface resistance"),
            options = layersControlOptions(collapsed = FALSE)
          ) %>% clearBounds()
        
        
        rv$llmap <<- llmap
        updateLL(llmap)
        # leafsurface
        #llmap
        rv$llmap
      })
    }
  })
  
  
  observeEvent(input$points_py, {
    if(!rv$tifready){
      rv$log <- paste0(rv$log, ' \n Creating points -- No raster yet!');updateVTEXT(rv$log) # _______
    } else {
      rv$log <- paste0(rv$log, ' \nCreating points');updateVTEXT(rv$log) # _______
      
      if(is.null(rv$inSurSessID)){
        pdebug(devug=devug,sep='\n',pre='-','rv$inSurSessID')
        (inSurSessID <- sessionIDgen())
        rv$inSurSessID <- inSurSessID
        pdebug(devug=devug,sep='\n',pre='-','rv$inSurSessID')
      }
      
      if(is.null(rv$inPointsSessID)){
        (inPointsSessID <- sessionIDgen())
        rv$inPointsSessID <- inPointsSessID
      }
      
      out_pts <- paste0(tempFolder, '/out_simpts_', rv$inSurSessID, '.shp')
      
      points_file <- points_shp(py = py, rv$tif, out_pts, 
                                as.numeric(input$in_points_3),
                                as.numeric(input$in_points_4),
                                as.numeric(input$in_points_5))
      
      rv$log <- paste0(rv$log, ' \nCreating points');updateVTEXT(rv$log) # _______
      
      if (!file.exists(points_file)){
        rv$log <- paste0(rv$log, '  --- Error creating points');updateVTEXT(rv$log) # _______
      } else {
        rv$pts <- points_file
        rv$ptsready <- TRUE
        
        output$ll_map_points <- renderLeaflet({
          
          #points_file <- "/data/temp/L2023090100204905file18e703e3d6298/out_simpts_J2023090100210305file18e7061e66c55.shp"
          points_shpO <- readOGR(points_file)
          points_shp <- spTransform(points_shpO, CRSobj = CRS("+proj=longlat +ellps=GRS80"))
          points_shp$ID <- 1:nrow(points_shp)
          #points_shp@data[, c('lng', 'lat')] <- points_shp@coords
          rv$shp <- points_shp
          
          rv$log <- paste0(rv$log, '  --- DONE');updateVTEXT(rv$log) # _______
          
          #temLL <- rv$llmap
          #save(temLL, file = '/data/tempR/ll.RData')
          #load('/data/tempR/ll.RData') # rv <- list(llmap = temLL); llmap = temLL
          
          
          llmap <<- rv$llmap %>% clearBounds() %>% clearGroup('Points') %>%
            ## Bug -- using removeMarker() not working, only one point. not use layerId in addMarkers
            addMarkers(data = points_shp, 
                       label = ~ID, group = 'Points') %>% 
            addLayersControl(
              baseGroups = c("OpenStreetMap", "Esri.WorldImagery"),
              overlayGroups = c('Points', "Habitat suitability", "Surface resistance"),
              options = layersControlOptions(collapsed = FALSE)
            ) 
          
          rv$llmap <<- llmap
          updateLL(llmap)
          # leafsurface
          #llmap
          rv$llmap
        })
      }
    }
  })
  
  ####### > DISTANCE  ------------------
  
  ##> vout_dist; ll_map_dist; dist_py; in_distance_3, 
  ##> in_distance_shp in_dist_tif, inDistSessID distmap newtifPath_dist newshpPath_dist
  # distmap = NULL, distrast = NULL, distshp = NULL,
  
  observeEvent(input$in_dist_tif, {
    invisible(suppressWarnings(tryCatch(file.remove(c(rv$tifpathdist, rv$newtifPath_dist)), 
                                        error = function(e) NULL)))
    
    if(is.null(rv$inSurSessID)){
      pdebug(devug=devug,sep='\n',pre='-','rv$inSurSessID')
      (inSurSessID <- sessionIDgen())
      rv$inSurSessID <- inSurSessID
      pdebug(devug=devug,sep='\n',pre='-','rv$inSurSessID')
    }
    
    if(is.null(rv$inDistSessID)){
      (inDistSessID <- sessionIDgen())
      rv$inDistSessID <- inDistSessID
    }
    
    
    rv$tifpathdist <- paste0(tempFolder, '/in_dist_', inDistSessID, '.tif')
    file.copy(input$in_dist_tif$datapath, rv$tifpathdist)
    
    # pdebug(devug=devug,sep='\n',pre='---H2S\n'," hs2rs_tif[]") # = = = = = = =  = = =  = = =  = = =  = = = 
    
    rv$log <- paste0(rv$log, '\nUpdating raster: making pixels squared and -9999 as no data');updateVTEXT(rv$log) # _______
    
    rv$tifpathdistfix <- paste0(tempFolder, '/in_dist_fixed_', rv$inDistSessID, '.tif')
    newtifPath_dist <- fitRaster2cola(inrasterpath = rv$tifpathdist, outrasterpath = rv$tifpathdistfix)
    newtifPath_dist <- ifelse(is.na(newtifPath_dist), yes = rv$tifpathpts, no = newtifPath_dist)
    rv$newtifPath_dist <- newtifPath_dist
    
    
    if (file.exists(rv$newtifPath_dist)){
      rv$log <- paste0(rv$log, ' --- DONE');updateVTEXT(rv$log) # _______
      rv$tifready <- TRUE
      rv$tif <- newtifPath_dist
      
      output$ll_map_dist <- renderLeaflet({
        
        newtif <- raster(newtifPath_dist)
        rng_newtif <- cellStats(newtif, stat = range)
        tifPal <<-  colorNumeric(palette = "viridis", reverse = TRUE,
                                 domain = rng_newtif, na.color = "transparent")
        
        
        llmap <<- rv$llmap %>% removeImage('SurfaceResistance')  %>% removeControl('legendSurface') %>% 
          addRasterImage(newtif, colors = tifPal, opacity = .7, 
                         group = "Surface resistance", layerId = 'SurfaceResistance') %>%
          addLegend(pal =  tifPal, values = newtif[], layerId = "legendSurface",
                    position = 'topleft',
                    title= "Resistance"#, opacity = .3
                    #, labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
          ) %>% addLayersControl(
            baseGroups = c("OpenStreetMap", "Esri.WorldImagery"),
            overlayGroups = c("Habitat suitability", "Surface resistance"),
            options = layersControlOptions(collapsed = FALSE)
          ) %>% clearBounds()
        
        
        rv$llmap <<- llmap
        updateLL(llmap)
        # leafsurface
        #llmap
        rv$llmap
      })
    }
  })
  
  
  ##> vout_dist; ll_map_dist; distance_py; in_distance_3, 
  ##> in_distance_shp in_dist_tif, inDistSessID distmap newtifPath_dist newshpPath_dist
  observeEvent(input$in_dist_shp, {
    
    pdebug(devug=devug,sep='\n',pre='--','names(input)', 'str(input$in_dist_shp)') # _____________
    
    invisible(suppressWarnings(tryCatch(file.remove(c(in_distance_shp, newin_distance_shp)), 
                                        error = function(e) NULL)))
    
    rv$log <- paste0(rv$log, '\nLoading shapefile');updateVTEXT(rv$log) # _______
    
    
    ## Create session IF if started from this tab
    if(is.null(rv$inSurSessID)){
      pdebug(devug=devug,sep='\n',pre='--','rv$inSurSessID') # _____________
      (inSurSessID <- sessionIDgen()) 
      rv$inSurSessID <- inSurSessID
      pdebug(devug=devug,sep='\n',pre='--','rv$inSurSessID') # _____________
    }
    
    
    if(is.null(rv$inDistSessID)){
      pdebug(devug=devug,sep='\n',pre='--','rv$inDistSessID') # _____________
      (inDistSessID <- sessionIDgen()) # rv <- list()
      rv$inDistSessID <- inDistSessID
      pdebug(devug=devug,sep='\n',pre='--','rv$inDistSessID') # _____________
    }
    
    
    pdebug(devug=devug,sep='\n',pre='--','is.null(rv$distrast)') # _____________
    
    
    
    if(!(rv$tifready)){
      rv$log <- paste0(rv$log, '\nSTOP: load a valid surface raster first');updateVTEXT(rv$log) # _______
    } else {
      
      inFiles <- input$in_dist_shp # 
      inFiles$newFile <- paste0(tempFolder, '/', basename(inFiles$name))
      pdebug(devug=devug,sep='\n',pre='--','(inFiles)', 'print(inFiles)') # _____________
      
      file.copy(inFiles$datapath, inFiles$newFile)
      
      inShp <<- loadShp(inFiles, tempFolder, rv$inDistSessID)
      pdebug(devug=devug,sep='\n',pre='--','is.null(rv$newtifPath_dist)', 'rv$newtifPath_dist') # _____________
      rv$ptsready <- TRUE
      rv$pts <- inShp$layer
      rv$shp <- inShp$shp
      
      rv$log <- paste0(rv$log, '\nShapefile loaded');updateVTEXT(rv$log) # _______
      
      distsurface0 <<- distsurface ## Create bkp if new load shp
      
      if (class(inShp$shp) == 'SpatialPointsDataFrame'){
        
        output$ll_map_dist <- renderLeaflet({
          rv$shp <- spTransform(inShp$shp, CRSobj = CRS("+proj=longlat +ellps=GRS80"))
          rv$shp$ID <- 1:nrow(rv$shp)
          
          llmap <<- rv$llmap  %>% removeMarker(layerId = 'Points') %>%
            addMarkers(data = rv$shp, label = ~ID, group = 'Points', layerId = 'Points')
          rv$llmap <<- llmap
          updateLL(llmap)
          # leafsurface
          #llmap
          rv$llmap
          
          
        })
      }
    }
  })
  
  observeEvent(input$dist_py, {
    pdebug(devug=devug,' rv$distshp','rv$distshp', 'rv$distrast', 'inShp$files') # _____________
    condDist <- 0
    if(!is.null(rv$distshp) & !is.null(rv$distrast)){
      if(rv$distshp == 1 & rv$distrast == 1 & !is.null(inShp$files)){
        if(length(inShp$files) != 0){
          condDist <- 1
        }
      }
    }
    
    if( condDist == 1){
      if(is.null(rv$inDistSessID)){
        pdebug(devug=devug,sep='\n',pre='--','rv$inDistSessID') # _____________
        (inDistSessID <- sessionIDgen()) 
        rv$inDistSessID <- inDistSessID
        pdebug(devug=devug,sep='\n',pre='--','rv$inDistSessID') # _____________
      }
      
      #input <- c(in_dist_3 = 25000)
      vout_dist <<- paste0(vout_dist, '\nGenerating matrix')
      isolate(output$vout_dist <- renderText({isolate(vout_dist)}))
      
      outcdmat <- paste0(tempFolder, '/out_cdmatrix_', rv$inDistSessID, '.csv')
      # outcdmat <- '/data/temp/G2023083001195205file35162c836424/out_cdmatrix_L2023083001200105file3516441548c2.csv'
      pdebug(devug=devug,'outcdmat') # _____________
      cdmat_file <- cdmat_py (py = py, inshp = inShp$layer, intif = rv$newtifPath_dist, 
                              outcsv = outcdmat, param3 = as.numeric(input$in_dist_3), param4 = 1)
      
      if(file.exists(outcdmat)){
        headMat <- data.table::fread(outcdmat, header = F)
        vout_dist <<- paste0(vout_dist, '\nMatrix generated, dim:', ncol(headMat), ' cols, ', nrow(headMat))
        isolate(output$vout <- renderText({isolate(vout_dist)}))
        output$dist_box1 <- renderValueBox({
          valueBox(
            "YES", "Ready", icon = icon("thumbs-up", lib = "glyphicon"),
            color = "green"
          )
        })
      }
    }
  })
  
  
  ####### > LCC  ------------------
  
  
  observeEvent(input$in_lcc_tif, {
    invisible(suppressWarnings(tryCatch(file.remove(c(rv$tifpathdist, rv$newtifPath_dist)), 
                                        error = function(e) NULL)))
    
    if(is.null(rv$inLccSessID)){
      (inLccSessID <- sessionIDgen())
      rv$inLccSessID <- inLccSessID
    }
    
    tifpathlcc <- paste0(tempFolder, '/in_lcc_', inLccSessID, '.tif')
    rv$tifpathlcc <- tifpathlcc
    file.copy(input$in_lcc_tif$datapath, rv$tifpathlcc)
    
    rv$log <- paste0(rv$log, '\nUpdating raster: making pixels squared and -9999 as no data');updateVTEXT(rv$log) # _______
    
    
    tifpathlccfix <- paste0(tempFolder, '/in_lcc_fixed_', rv$inlccSessID, '.tif')
    newtifPath_lcc <- fitRaster2cola(inrasterpath = rv$tifpathlcc, outrasterpath = rv$tifpathlccfix)
    newtifPath_lcc <- ifelse(is.na(newtifPath_lcc), yes = rv$tifpathlcc, no = rv$tifpathlccfix)
    
    if (file.exists(newtifPath_lcc)){
      rv$log <- paste0(rv$log, ' --- DONE');updateVTEXT(rv$log) # _______
      rv$tifready <- TRUE
      rv$tif <- newtifPath_lcc
      
      pdebug(devug=devug,sep='\n',pre='---- LOAD TIF LCC\n','rv$tifready', 'rv$tif', 'rv$inLccSessID') # _____________ 
      
      
      output$ll_map_lcc <- renderLeaflet({
        
        newtif <- raster(newtifPath_lcc)
        rng_newtif <- cellStats(newtif, stat = range)
        tifPal <<-  colorNumeric(palette = "viridis", reverse = TRUE,
                                 domain = rng_newtif, na.color = "transparent")
        
        
        llmap <<- rv$llmap %>% removeImage('SurfaceResistance')  %>% removeControl('legendSurface') %>% 
          addRasterImage(newtif, colors = tifPal, opacity = .7, 
                         group = "Surface resistance", layerId = 'SurfaceResistance') %>%
          addLegend(pal =  tifPal, values = newtif[], layerId = "legendSurface",
                    position = 'topleft',
                    title= "Resistance"#, opacity = .3
                    #, labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
          ) %>% addLayersControl(
            baseGroups = c("OpenStreetMap", "Esri.WorldImagery"),
            overlayGroups = c("Habitat suitability", "Surface resistance"),
            options = layersControlOptions(collapsed = FALSE)
          ) %>% clearBounds()
        
        
        rv$llmap <<- llmap
        updateLL(llmap)
        # leafsurface
        #llmap
        rv$llmap
      })
    }
  })
  
  
  ##> vout_lcc; ll_map_lcc; lccance_py; in_lccance_3, 
  ##> in_lccance_shp in_lcc_tif, inlccSessID lccmap newtifPath_lcc newshpPath_lcc
  observeEvent(input$in_lcc_shp, {
    
    invisible(suppressWarnings(tryCatch(file.remove(c(in_lcc_shp, newin_lcc_shp)), error = function(e) NULL)))
    
    rv$log <- paste0(rv$log, '\nLoading shapefile');updateVTEXT(rv$log) # _______
    
    ## Create session IF if started from this tab
    if(is.null(rv$inLccSessID)){
      (inLccSessID <<- sessionIDgen())
      rv$inLccSessID <- inLccSessID
    }
    
    pdebug(devug=devug,sep='\n',pre='--','rv$tifready', 'inLccSessID', 'rv$inLccSessID') # _____________ 
    
    
    if(!(rv$tifready)){
      rv$log <- paste0(rv$log, '\nSTOP: load a valid surface raster first');updateVTEXT(rv$log) # _______
    } else {
      
      inFiles <- input$in_lcc_shp #
      
      inFiles$newFile <- paste0(tempFolder, '/', basename(inFiles$name))
      pdebug(devug=devug,sep='\n',pre='\n--','print(inFiles)', 'inFiles$newFile', 'tempFolder') # _____________

      file.copy(inFiles$datapath, inFiles$newFile)
      #if(devug){save(inFiles, file = paste0(tempFolder, '/shpfiles.RData'))}
      
      inShp <<- loadShp(inFiles, tempFolder, rv$inlccSessID)


      if (class(inShp$shp) == 'SpatialPointsDataFrame'){
        
        rv$ptsready <- TRUE
        rv$pts <- inShp$layer
        rv$shp <- inShp$shp
        rv$log <- paste0(rv$log, '\nShapefile loaded');updateVTEXT(rv$log) # _______
        
        pdebug(devug=devug,sep='\n',pre='---- LOAD SHP LCC\n','rv$ptsready', 'rv$pts', 'rv$inLccSessID') # _____________ 
        
        
        output$ll_map_lcc <- renderLeaflet({
          rv$shp <- spTransform(inShp$shp, CRSobj = CRS("+proj=longlat +ellps=GRS80"))
          rv$shp$ID <- 1:nrow(rv$shp)
          
          llmap <<- rv$llmap  %>% clearGroup('Points') %>%  #removeMarker(layerId = 'Points') %>%
            addMarkers(data = rv$shp, label = ~ID, group = 'Points') 
          rv$llmap <<- llmap
          updateLL(llmap)
          # leafsurface
          #llmap
          rv$llmap
          
          
        })
      }
    }
  })
  
  observeEvent(input$lcc, {
    pdebug(devug=devug,sep='\n',pre='\n---- RUN LCC\n','rv$ptsready', 'rv$pts', 'rv$ptsready', 'rv$pts','rv$inLccSessID') # _____________ 
    condDist <- 0
    if(rv$ptsready & rv$tifready){
      condDist <- 1
    }
    
    cond <- condDist
    pdebug(devug=devug,' cond', 'condDist') # _____________
    cond <<- condDist
    pdebug(devug=devug,' cond') # _____________
    
    if( condDist == 1){
      #input <- c(in_dist_3 = 25000)
      rv$log <- paste0(rv$log, '\n Generating corridors');updateVTEXT(rv$log) # _______
      
      output$ll_map_lcc <- renderLeaflet({  
        
        out_lcc <- paste0(tempFolder, '/out_lcc_', rv$inLccSessID, '.tif')
        tStartLcc <- Sys.time()
        out_lcc <- lcc_py (py = py, inshp = rv$pts, intif = rv$tif, outtif = out_lcc,
                           param4 = as.numeric(input$in_lcc_4),
                           param5 = as.numeric(input$in_lcc_5),
                           param6 = as.numeric(input$in_lcc_6),
                           param7 = 1)
        tElapLcc <- Sys.time() - tStartLcc
        rv$log <- paste0(rv$log, ' - Time elapsed: ', tElapLcc);updateVTEXT(rv$log) # _______
        
        rv$out_lcc <- out_lcc
        
        pdebug(devug=devug,sep='\n',pre='\n \t |||| ','out_lcc', 'condDist') # _____________
        
        if(!file.exists(out_lcc)){
          rv$log <- paste0(rv$log, ' --- ERROR');updateVTEXT(rv$log) # _______
          rv$llmap
        } else {
          
          rv$log <- paste0(rv$log, ' --- DONE\n');updateVTEXT(rv$log) # _______
          
          
          newtif <- raster(out_lcc)
          rng_newtif <- cellStats(newtif, stat = range)
          tifPal <<-  colorNumeric(palette = "viridis", reverse = TRUE,
                                   domain = rng_newtif, na.color = "transparent")
          
          
          llmap <<- rv$llmap %>% removeImage('Corridor')  %>% removeControl('legendCorridor') %>% 
            addRasterImage(newtif, colors = tifPal, opacity = .7, 
                           group = "Surface resistance", layerId = 'Corridor') %>%
            addLegend(pal =  tifPal, values = newtif[], layerId = "legendCorridor",
                      position = 'topleft',
                      title= "Resistance"#, opacity = .3
                      #, labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
            ) %>% addLayersControl(
              baseGroups = c("OpenStreetMap", "Esri.WorldImagery"),
              overlayGroups = c('Points', "Habitat suitability", "Surface resistance", 'Corridor'),
              options = layersControlOptions(collapsed = FALSE)
            ) %>% clearBounds()
          
          
          rv$llmap <<- llmap
          updateLL(llmap)
          # leafsurface
          #llmap
          rv$llmap
        }
        
      })
    }
  })
  
  
  ####### > CRK  ------------------
  
  
  observeEvent(input$in_crk_tif, {
    invisible(suppressWarnings(tryCatch(file.remove(c(rv$tifpathcrk, rv$newtifPath_crk)), 
                                        error = function(e) NULL)))
    
    if(is.null(rv$inSurSessID)){
      pdebug(devug=devug,sep='\n',pre='-','rv$inSurSessID')
      (inSurSessID <- sessionIDgen())
      rv$inSurSessID <- inSurSessID
      pdebug(devug=devug,sep='\n',pre='-','rv$inSurSessID')
    }
    
    if(is.null(rv$incrkSessID)){
      (incrkSessID <- sessionIDgen())
      rv$incrkSessID <- incrkSessID
    }
    
    
    rv$tifpathcrk <- paste0(tempFolder, '/in_crk_', incrkSessID, '.tif')
    file.copy(input$in_crk_tif$datapath, rv$tifpathcrk)
    
    rv$log <- paste0(rv$log, '\nUpdating raster: making pixels squared and -9999 as no data');updateVTEXT(rv$log) # _______
    
    rv$tifpathcrkfix <- paste0(tempFolder, '/in_crk_fixed_', rv$incrkSessID, '.tif')
    newtifPath_crk <- fitRaster2cola(inrasterpath = rv$tifpathcrk, outrasterpath = rv$tifpathcrkfix)
    newtifPath_crk <- ifelse(is.na(newtifPath_crk), yes = rv$tifpathpts, no = newtifPath_crk)
    rv$newtifPath_crk <- newtifPath_crk
    
    
    if (file.exists(rv$newtifPath_crk)){
      rv$log <- paste0(rv$log, ' --- DONE');updateVTEXT(rv$log) # _______
      rv$tifready <- TRUE
      rv$tif <- newtifPath_crk
      
      output$ll_map_crk <- renderLeaflet({
        
        newtif <- raster(newtifPath_crk)
        rng_newtif <- cellStats(newtif, stat = range)
        tifPal <<-  colorNumeric(palette = "viridis", reverse = TRUE,
                                 domain = rng_newtif, na.color = "transparent")
        
        
        llmap <<- rv$llmap %>% removeImage('SurfaceResistance')  %>% removeControl('legendSurface') %>% 
          addRasterImage(newtif, colors = tifPal, opacity = .7, 
                         group = "Surface resistance", layerId = 'SurfaceResistance') %>%
          addLegend(pal =  tifPal, values = newtif[], layerId = "legendSurface",
                    position = 'topleft',
                    title= "Resistance"#, opacity = .3
                    #, labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
          ) %>% addLayersControl(
            baseGroups = c("OpenStreetMap", "Esri.WorldImagery"),
            overlayGroups = c("Habitat suitability", "Surface resistance"),
            options = layersControlOptions(collapsed = FALSE)
          ) %>% clearBounds()
        
        
        rv$llmap <<- llmap
        updateLL(llmap)
        # leafsurface
        #llmap
        rv$llmap
      })
    }
  })
  
  
  ##> vout_crk; ll_map_crk; crkance_py; in_crkance_3, 
  ##> in_crkance_shp in_crk_tif, incrkSessID crkmap newtifPath_crk newshpPath_crk
  observeEvent(input$in_crk_shp, {
    
    pdebug(devug=devug,sep='\n',pre='--','names(input)', 'str(input$in_crk_shp)') # _____________
    
    invisible(suppressWarnings(tryCatch(file.remove(c(in_crk_shp, newin_crk_shp)), 
                                        error = function(e) NULL)))
    
    rv$log <- paste0(rv$log, '\nLoading shapefile');updateVTEXT(rv$log) # _______
    
    
    ## Create session IF if started from this tab
    if(is.null(rv$inSurSessID)){
      pdebug(devug=devug,sep='\n',pre='--','rv$inSurSessID') # _____________
      (inSurSessID <- sessionIDgen()) 
      rv$inSurSessID <- inSurSessID
      pdebug(devug=devug,sep='\n',pre='--','rv$inSurSessID') # _____________
    }
    
    
    if(is.null(rv$incrkSessID)){
      pdebug(devug=devug,sep='\n',pre='--','rv$incrkSessID') # _____________
      (incrkSessID <- sessionIDgen()) # rv <- list()
      rv$incrkSessID <- incrkSessID
      pdebug(devug=devug,sep='\n',pre='--','rv$incrkSessID') # _____________
    }
    
    
    pdebug(devug=devug,sep='\n',pre='--','is.null(rv$crkrast)') # _____________
    
    
    
    if(!(rv$tifready)){
      rv$log <- paste0(rv$log, '\nSTOP: load a valid surface raster first');updateVTEXT(rv$log) # _______
    } else {
      
      inFiles <- input$in_crk_shp # 
      inFiles$newFile <- paste0(tempFolder, '/', basename(inFiles$name))
      pdebug(devug=devug,sep='\n',pre='--','(inFiles)', 'print(inFiles)') # _____________
      
      file.copy(inFiles$datapath, inFiles$newFile)
      
      inShp <<- loadShp(inFiles, tempFolder, rv$incrkSessID)
      pdebug(devug=devug,sep='\n',pre='--','is.null(rv$newtifPath_crk)', 'rv$newtifPath_crk') # _____________
      rv$ptsready <- TRUE
      rv$pts <- inShp$layer
      rv$shp <- inShp$shp
      
      rv$log <- paste0(rv$log, '\nShapefile loaded');updateVTEXT(rv$log) # _______
      
      #crksurface0 <<- crksurface ## Create bkp if new load shp
      
      if (class(inShp$shp) == 'SpatialPointsDataFrame'){
        
        output$ll_map_crk <- renderLeaflet({
          rv$shp <- spTransform(inShp$shp, CRSobj = CRS("+proj=longlat +ellps=GRS80"))
          rv$shp$ID <- 1:nrow(rv$shp)
          
          llmap <<- rv$llmap  %>% removeMarker(layerId = 'Points') %>%
            addMarkers(data = rv$shp, label = ~ID, group = 'Points', layerId = 'Points')
          rv$llmap <<- llmap
          updateLL(llmap)
          # leafsurface
          #llmap
          rv$llmap
          
          
        })
      }
    }
  })
  
  observeEvent(input$crk, {
    pdebug(devug=devug,' rv$distshp','rv$distshp', 'rv$distrast', 'inShp$files') # _____________
    condDist <- 0
    if(rv$ptsready & rv$tifready){
      condDist <- 1
    }
    
    if( condDist == 1){
      
      #input <- c(in_dist_3 = 25000)
      rv$log <- paste0(rv$log, '\n Generating corridors');updateVTEXT(rv$log) # _______
      
      
      out_crk <- paste0(tempFolder, '/out_crk_', rv$incrkSessID, '.csv')
      out_crk <- crk_py (py = py, inshp = rv$pts, intif = rv$pts, outtif = out_crk,
                         param4 = as.numeric(input$in_crk_4),
                         param5 = (input$in_crk_5),
                         param6 = 1)
      rv$out_crk <- out_crk
      
      if(!file.exists(out_crk)){
        rv$log <- paste0(rv$log, ' --- ERROR');updateVTEXT(rv$log) # _______
        
        
      } else {
        
        rv$log <- paste0(rv$log, ' --- DONE');updateVTEXT(rv$log) # _______
        
        output$ll_map_crk <- renderLeaflet({
          
          newtif <- raster(out_crk)
          rng_newtif <- cellStats(newtif, stat = range)
          tifPal <<-  colorNumeric(palette = "viridis", reverse = TRUE,
                                   domain = rng_newtif, na.color = "transparent")
          
          
          llmap <<- rv$llmap %>% removeImage('Kernel')  %>% removeControl('legendKernel') %>% 
            addRasterImage(newtif, colors = tifPal, opacity = .7, 
                           group = "Surface resistance", layerId = 'Kernel') %>%
            addLegend(pal =  tifPal, values = newtif[], layerId = "legendKernel",
                      position = 'topleft',
                      title= "Resistance"#, opacity = .3
                      #, labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
            ) %>% addLayersControl(
              
              overlayGroups = c('Points', "Habitat suitability", "Surface resistance", 'Corridor'),
              options = layersControlOptions(collapsed = FALSE)
            ) %>% clearBounds()
          
          
          rv$llmap <<- llmap
          updateLL(llmap)
          # leafsurface
          #llmap
          rv$llmap
        })
        
      }
    }
  })
}

shinyApp(ui, server)



###### LINUX SERVER COPY -------------

# sudo cp /home/shiny/connectscape/ /srv/shiny-server/cola -R
# sudo cp /home/shiny/connectscape/app.R /srv/shiny-server/cola/app.R
# sudo cp /home/shiny/connectscape/app.R /srv/shiny-server/cola2/app.R -R


# sudo rm /home/shiny/tmpR/leafSim.RDatasudo cp /home/vmuser/gedivis /srv/shiny-server/gedivis -R
# sudo rm /var/log/shiny-server/gedivis/*
# #sudo rm /srv/shiny-server/gedivis/*
#  sudo su - -c "R -e \"shinyParallel::installShinyParallel('/home/vmuser/gedivis/', max.sessions = 25)\"" # home/shinyusername/
# #sudo rm /srv/shiny-server/gedivis2 -R
# sudo cp /home/vmuser/gedivis /srv/shiny-server/gedivis2 -R
# sudo cat /var/log/shiny-server/gedivis_
