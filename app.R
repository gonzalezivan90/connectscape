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


source('/home/shiny/connectscape/cola_tools-copy.R')
tempPath <- '/data/temp/'; #dir.create(tempPath)
# debug insall order: htmltools >> shiny >> shinyWidgets

### time stamp -----
## Create temporal tif from ID Raster
tempID <- basename(tempfile())
timeMark <- gsub('[[:punct:]]| ', '', format(as.POSIXct(Sys.time(), tz="CET"), tz="America/Bogota",usetz=TRUE))
sessionID <- paste0(timeMark, '_', tempID)
tempFolder <- paste0(tempPath, '/', sessionID, '/')

tempFolder <- paste0(tempPath, '/2023082821124805_file5762732c645/')

dir.create(tempFolder)
print(paste('tempfile: ', tempFolder))
#tempPath <- sort(Sys.getenv(c("TEMP", 'TMP')), decreasing=T)[1]

py <- '/home/shiny/anaconda3/envs/cdujlab/bin/python'
reactShp <- reactiveValues(shp = FALSE,
                           leaf0 = leaflet() %>% addTiles() %>% 
                             addLayersControl(baseGroups = c("OpenStreetMap", "Esri.WorldImagery"),
                                              options = layersControlOptions(collapsed = FALSE)) %>%
                             addProviderTiles( "Esri.WorldImagery", group = "Esri.WorldImagery" ) %>%
                             setView(lng = -74, lat = 4.6, zoom = 10)
)

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


runS2RES <- function(py, intif, outtif, param1, param2, param3, param4, param5, param6, param7){
  src <- '/home/shiny/connecting-landscapes/src/s2res.py'
  datapath <- tempFolder # datapath = tempFolder
  
  
  timeMarkCDPOP <- gsub('[[:punct:]]| ', '', format(as.POSIXct(Sys.time(), tz="CET"), tz="America/Bogota",usetz=TRUE))
  (cmd_s2res <- paste0(py, ' ', src, ' ', datapath, ' ', intif, ' ', outtif, ' ', 
                 param1, ' ', param2, ' ', param3, ' ', param4, ' ', param5, 
                 ' ', param6, ' ', param7))
  
  return(list(newFiles = newFiles, cdpopPath = cdpopPath))
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
                  menuItem("Cost distance matrix", tabName = "tab_distance", icon = icon("border-all")),
                  
                  
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
                
                textInput("in_surf_3", "Min-grid:", ''),
                textInput("in_surf_4", "Max-grid:", ''),
                textInput("in_surf_5", "Max-resistance:", ''),
                textInput("in_surf_6", "Shape:", ''),
                textInput("in_surf_7", "No Data:", '')
        ),
        
        
        # tabItem('tab_example', 
        #         fluidPage( )
        # ),
        
        
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
  rv <- reactiveValues(data = NULL, orig=NULL, cdpopRun = NULL, out_cdpop_files = c(''))
  
  reactShp <- reactiveValues(shp = FALSE,
                             leaf0 = leaflet() %>% addTiles() %>% 
                               addLayersControl(baseGroups = c("OpenStreetMap", "Esri.WorldImagery"),
                                                options = layersControlOptions(collapsed = FALSE)) %>%
                               addProviderTiles( "Esri.WorldImagery", group = "Esri.WorldImagery" ) %>%
                               setView(lng = 25, lat = -21, zoom = 6)
                             
  )
  reactShp$leaf0 <<- leaflet() %>% addTiles() 
  
  output$loadMapLL <- renderLeaflet({
    reactShp$leaf0 %>%
      leaflet.extras::addDrawToolbar(targetGroup='draw', polylineOptions = FALSE,
                                     rectangleOptions = FALSE, circleOptions = FALSE,
                                     markerOptions = FALSE, circleMarkerOptions = FALSE,
                                     editOptions = leaflet.extras::editToolbarOptions())
  })
  
  vtext <- "Waiting for the habitat sutiability TIF"
  output$voutext <- renderText({isolate(vtext)})
  
  
  
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
  
  
  # renderDT ----------------------------------------------------------------
  
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
                         options = list(
                           paging =TRUE,
                           pageLength =  nrow(out_cdpop_file_X) 
                         )
        ))
    }
  })
  
  
  #### LOAD MAPS ----------------------
  
  observeEvent(input$in_sur_tif, {
    output$loadMapLL <- renderLeaflet({
      tifpath <- paste0(tempFolder, '/in_surface.tif')
      file.copy(input$in_sur_tif$datapath, tifpath)
      
      if(devug){ print(' ----- input$in_sur_tif'); print(input$in_sur_tif);
        print(tifpath); file.exists(tifpath)}
      
      vtext <<- paste0(vtext, '\nUpdating raster: square pixels and -9999 no data')
      isolate(output$voutext <- renderText({isolate(vtext)}))
      
      tifpathfixed <- paste0(tempFolder, '/in_surface_fixed.tif')
      
      newtifPath <- fitRaster2cola(inrasterpath = tifpath, outrasterpath = tifpathfixed)
      
      vtext <<- paste0(vtext, ' --- DONE')
      isolate(output$voutext <- renderText({isolate(vtext)}))
      #values$total <- values$total + 1
      
      newtif <- raster(newtifPath)
      rng_newtif <- c(newtif@data@min, newtif@data@max)
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
      
      if(devug){ print(' ----- input$in_sur_tif'); print(input$in_sur_tif)}
    })
  })
  
  
  
  
  observeEvent(input$surface_hs2rs, {
    
    
    
    
    newtif <- raster(newtifPath)
    hsPal <<-  colorNumeric(palette = "viridis", reverse = TRUE,
                            domain = newtif[], na.color = "transparent")
    
    output$loadMapLL <- renderLeaflet({
      vtext <<- paste0(vtext, '\nUpdating raster: square pixels and -9999 no data')
      leafsurface <<- leafsurface %>%
        addRasterImage(newtif, colors = hsPal, opacity = .7, group = "Habitat suitability") %>%
        addLegend(pal =  hsPal, values = newtif[],
                  position = 'topleft',
                  title= "Unknow units"#, opacity = .3
                  #, labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))
                  
        )
      ])
    
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
                         options = list(
                           paging =TRUE,
                           pageLength =  nrow(out_cdpop_file_X) 
                         )
        ))
    }
    })
  
  
  
  
  
  
  }
  
  shinyApp(ui, server)
  
  
  
  ###### LINUX SERVER COPY -------------
  
  # sudo cp /home/vmuser/gedivis /srv/shiny-server/gedivis -R
  # sudo cp /home/vmuser/gedivis/app.R /srv/shiny-server/gedivis/app.R
  
  
  # sudo rm /home/shiny/tmpR/leafSim.RDatasudo cp /home/vmuser/gedivis /srv/shiny-server/gedivis -R
  # sudo rm /var/log/shiny-server/gedivis/*
  # #sudo rm /srv/shiny-server/gedivis/*
  #  sudo su - -c "R -e \"shinyParallel::installShinyParallel('/home/vmuser/gedivis/', max.sessions = 25)\"" # home/shinyusername/
  # #sudo rm /srv/shiny-server/gedivis2 -R
  # sudo cp /home/vmuser/gedivis /srv/shiny-server/gedivis2 -R
  # sudo cat /var/log/shiny-server/gedivis_
  
  # "ecoID:924" forest Zulia