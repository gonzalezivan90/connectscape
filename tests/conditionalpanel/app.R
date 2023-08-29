### step by step version
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
  
  library(tidyverse)
  library(shiny)
  library(reshape2)
  library(DT)
  library(tibble)
}


tempPath <- '/data/temp/'; #dir.create(tempPath)
# debug insall order: htmltools >> shiny >> shinyWidgets

### time stamp -----
## Create temporal tif from ID Raster
tempID <- basename(tempfile())
timeMark <- gsub('[[:punct:]]| ', '', format(as.POSIXct(Sys.time(), tz="CET"), tz="America/Bogota",usetz=TRUE))
sessionID <- paste0(timeMark, '_', tempID)
tempFolder <- paste0(tempPath, '/', sessionID, '/')
dir.create(tempFolder)
print(paste('tempfile: ', tempFolder))
#tempPath <- sort(Sys.getenv(c("TEMP", 'TMP')), decreasing=T)[1]

py <- '/home/shiny/anaconda3/envs/cdujlab/bin/python'


debug <<- TRUE


runCDPOP <- function(py){
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
  datapath <- tempFolder
  vars <- paste0('inputvars.csv') # Only file name
  timeMarkCDPOP <- gsub('[[:punct:]]| ', '', format(as.POSIXct(Sys.time(), tz="CET"), tz="America/Bogota",usetz=TRUE))
  out <- paste0('cdpopout_', timeMarkCDPOP, '__')
  (cmd <- paste0(py, ' ', src, ' ', datapath, ' ', vars, ' ', out))
  
  #setwd(tempFolder)
  #file.copy('inputvars.csv', 'in.csv')
  #file.copy('xyA.csv', 'xy.csv')
  # system(cmd)
  
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
                  menuItem("Create source points", tabName = "tab_points", icon = icon("map-pin")),
                  menuItem("Cost distance matrix", tabName = "tab_distance", icon = icon("border-all")),
                  menuItem("CDPOP", tabName = "tab_cdpop", icon = icon("hippo")),
                  menuItem("Connectivity - corridors", tabName = "tab_corridors", icon = icon("route")),
                  
                  conditionalPanel(
                    'input.sidebarid == "tab_corridors"',
                    shiny::fileInput('in_corrpoints', 'Load point file Co', buttonLabel = 'Search', placeholder = 'No choose',
                                     accept=c('.csv','.txt'), multiple=FALSE),
                    shiny::fileInput('in_corrsurface', 'Load surface file', buttonLabel = 'Search', placeholder = 'No choose',
                                     accept=c('.csv','.txt'), multiple=FALSE)
                  ),
                  
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
        
        # tabhome tabsurface tab_points tab_distance tab_cdpop 
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
                                       fileInput("in_cdpop_cd", "CDmatric CSV File", accept = ".csv"),
                                       tableOutput("cdpop_files")),
                              
                              tabPanel("Run", tableOutput("cdpop_out"))
                  )
                )),
        
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

# server -----------------------------------------------------------------------

server <- function(input, output, session) {

    output$distPlot <- renderPlot({
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = "darkgray", border = "white", main = input$title)
  })
  
  
  
  ####### SRV CDPOP ------------------
  rv <- reactiveValues(data = NULL, orig=NULL)
  
  observeEvent(input$in_cdpop_par, {
    if(debug){ sprintf('cdpop_par: %s', input$in_cdpop_par)}
    file <- input$in_cdpop_par
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    rv$orig <- t(read.csv(file$datapath, header = input$header))
    colnames(rv$orig) <- paste0('Scen', 1:ncol(rv$orig))
    rv$data <- (rv$orig)
  })
  
  # in_cdpop_cd 
  # in_cdpop_age
  # in_cdpop_xy
  
  observeEvent(input$in_cdpop_xy, {
    if(debug){ sprintf('in_cdpop_cd: %s', input$in_cdpop_xy)}
    xyfile <- input$in_cdpop_cd
    ext <- tools::file_ext(xyfile$datapath)
    req(xyfile)
    validate(need(ext == "csv", "Please upload a csv file"))
    xyfile <<- (read.csv(xyfile$datapath, header = input$header))
    #colnames(rv$orig) <- paste0('Scen', 1:ncol(rv$orig))
    #rv$data <- (rv$orig)
    write.csv(xyfile, file = paste0(tempFolder, '/xy.csv'), row.names = FALSE)
  })

  observeEvent(input$in_cdpop_age {
    if(debug){ sprintf('in_cdpop_age: %s', input$in_cdpop_age)}
    
    agefile <- input$in_cdpop_age
    ext <- tools::file_ext(agefile$datapath)
    req(agefile)
    validate(need(ext == "csv", "Please upload a csv file"))
    age <<- (read.csv(agefile$datapath, header = input$header))
    #colnames(rv$orig) <- paste0('Scen', 1:ncol(rv$orig))
    write.csv(age, file = paste0(tempFolder, '/age.csv'), row.names = FALSE)
  })

  observeEvent(input$in_cdpop_cd, {
    if(debug){ sprintf('in_cdpop_cd: %s', input$in_cdpop_cd)}
    
    cdmatfile <- input$in_cdpop_cd
    ext <- tools::file_ext(cdmatfile$datapath)
    req(cdmat)
    validate(need(ext == "csv", "Please upload a csv file"))
    cdmat <<- (read.csv(cdmatfile$datapath, header = input$header))
    #colnames(rv$orig) <- paste0('Scen', 1:ncol(rv$orig))
    write.csv(cdmat, file = paste0(tempFolder, '/cdmat.csv'), row.names = FALSE)
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
    write.csv(writeParamCDPOP, file = paste0(tempFolder, '/params.csv'), row.names = FALSE)
    
    cond <- (ncol(cdmat) == nrow(cdmat)) &
      ncol(cdmat) > 2 &
      (nrow(cdmat) == nrow(xyfile))
    
    
    if( cond){
      output$cdpop_box1 <- renderValueBox({
        valueBox(
          "YES", "Ready", icon = icon("thumbs-up", lib = "glyphicon"),
          color = "green"
        )
      })
    }
  })
  
  ####
  
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