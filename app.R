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
  library(shinycssloaders)
  library(plotly) #
}

# debug insall order: htmltools >> shiny >> shinyWidgets


# install.packages('shinyWidgets')
# install.packages('shinydashboardPlus')
# install.packages('dashboardthemes')

# if ( identical ( unname(Sys.info()[c("sysname", 'nodename')]), c("Windows", 'HP-Z400')) ){
#   setwd('N:/Mi unidad/IG/server_IG/gedivis')
#   #setwd('N:/Mi unidad/IG/server_IG/gedivis/')
# }



# UI  ---------------
ui <- dashboardPage( 
  skin = 'green', 
  dashboardHeader( 
    ## UI Tags ------------------------------
    tags$li(a(href = 'https://goetzlab.rc.nau.edu/', target="_blank",
              img(src = 'geodelab.png',
                  title = "TNC Colombia", height = "50px"),
              style = "padding-top:0px; padding-bottom:0px;padding-left:0px;padding-right:0px;"),
            class = "dropdown"),
    title = "Connecting Landscapes",  titleWidth = 400
    ## UI sidebar ------------------------------
  ), ## End class
  
  # UI Panel  ---------------
  dashboardSidebar(
    #disable = FALSE,
    sidebarMenu(id = "sidebarid",
                #menuItem("Profiles L2b", tabName = "tab_profile2", startExpanded = TRUE,#),
                #https://fontawesome.com/search?q=map&o=r&m=free
                
                # menuItem("Home", tabName = "tabhome", icon = icon("house-user")),
                # 
                # menuItem("Habitat suitability <>\nresistance surface", tabName = "tabsurface", icon = icon("map-pin")),
                menuItem("Page 1", tabName = "page1"),
                menuItem("Page 2", tabName = "page2"),
                conditionalPanel(
                  'input.sidebarid == "page2"',
                  sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30),
                  selectInput("title", "Select plot title:", choices = c("Hist of x", "Histogram of x"))
                )
                
                # menuItem("Create source points", tabName = "tab_points", icon = icon("map-pin")),
                # menuItem("Cost distance matrix", tabName = "tab_distance", icon = icon("border-all")),
                # menuItem("CDPOP", tabName = "tab_cdpop", icon = icon("hippo")),
                # menuItem("Connectivity - corridors", tabName = "tab_corridors", icon = icon("route")),
                # menuItem("Connectivity - dispersal kernels", tabName = "tab_kernels", icon = icon("bezier-curve")),
                # menuItem("Plotting", tabName = "tab_plotting", icon = icon("image")),
                # menuItem("Mapping", tabName = "tab_Mapping", icon = icon("map")),
                # menuItem("Connectivity - prioritization", tabName = "tab_priori", icon = icon("trophy")),
                # menuItem("Landscape genetics mapping tools", tabName = "tab_genetics", icon = icon("route")),
                # menuItem("Run locally", tabName = "tablocal", icon = icon("code-fork"))
                
                
      # conditionalPanel(
      #   'input.sidebarid == "tab_surface"',
      #   # Shapefile
      #   shiny::fileInput('in_surface', 'Load resistance grid S', buttonLabel = 'Search', placeholder = 'No choose',
      #                    accept=c('.tif', '.rsg'), multiple = FALSE)
      # )
      
      # conditionalPanel(
      #   'input.sidebarid == "tab_points"',
      #   # Shapefile
      #   shiny::fileInput('in_points', 'Load point file P', buttonLabel = 'Search', placeholder = 'No choose',
      #                    accept=c('.shp','.dbf','.sbn','.sbx','.shx',".prj", '.zip', '.gpkg', '.SQLite', '.GeoJSON', '.xy'),
      #                    multiple=TRUE)),
      # conditionalPanel(
      #   'input.sidebarid == "tab_distance"',
      #   shiny::fileInput('in_points', 'Load point file', buttonLabel = 'Search', placeholder = 'No choose',
      #                    accept=c('.shp','.dbf','.sbn','.sbx','.shx',".prj", '.zip', '.gpkg', '.SQLite', '.GeoJSON', '.xy'),
      #                    multiple=TRUE)
      #   ),
      # conditionalPanel(
      #   'input.sidebarid %in% "tab_cdpop"',
      #   shiny::fileInput('in_cdpop', 'Load parameter file C', buttonLabel = 'Search', placeholder = 'No choose',
      #                    accept=c('.csv','.txt'), multiple=FALSE)
      # ),
      # 
      # conditionalPanel(
      #   'input.sidebarid == "tab_corridors"',
      #   shiny::fileInput('in_corrpoints', 'Load point file Co', buttonLabel = 'Search', placeholder = 'No choose',
      #                    accept=c('.csv','.txt'), multiple=FALSE)
      # ),
      # 
      # conditionalPanel(
      #   'input.sidebarid == "tab_corridors"',
      #   shiny::fileInput('in_corrsurface', 'Load point file Co', buttonLabel = 'Search', placeholder = 'No choose',
      #                    accept=c('.csv','.txt'), multiple=FALSE)
      # ),
      # 
      # conditionalPanel(
      #   'input.sidebarid %in% c("tab_kernels", "tab_plotting")',
      #   shiny::fileInput('in_corrpoints', 'Load point file Co', buttonLabel = 'Search', placeholder = 'No choose',
      #                    accept=c('.csv','.txt'), multiple=FALSE),
      #   shiny::fileInput('in_corrsurface', 'Load point file Co', buttonLabel = 'Search', placeholder = 'No choose',
      #                    accept=c('.csv','.txt'), multiple=FALSE)
      # ),
      
      
      
    )
  ),
  # 
  # rightsidebar = rightSidebar(
  #   background = "dark",
  #   uiOutput("side_bar"),
  #   title = "Right Sidebar"
  # ),
  
  dashboardBody( 
    
    tabItems(
      
      # page 1 ----
      tabItem(tabName = "page1", "Page 1 content. This page doesn't have any sidebar menu items."),
      # page 2 ----
      tabItem(tabName = "page2", 
              "Page 2 content. This page has sidebar meny items that are used in the plot below.",
              br(), br(),
              plotOutput("distPlot")),
      ######## UI viz ------------
      
      # tab_workgedi, tab_gedi, tab_worksimi, tab_similarity
      
      # tab_home
      # tab_surface
      # tab_points
      # tab_distance
      # tab_cdpop
      # tab_cdpop
      # tab_corridors
      # tab_kernels
      # tab_plotting
      # tab_Mapping
      # tab_priori
      # tab_genetics
      # tab_local
      
      tabItem('tab_home', 
              fluidPage(
                #includeMarkdown("md_intro.md")
                tabsetPanel(type = "pills",
                            tabPanel("Home", plotOutput("plot")),
                            tabPanel("Performance", verbatimTextOutput("summary")),
                            tabPanel("Showcase", tableOutput("table"))
                )
              )),
      
      tabItem('tab_surface', 
              # tabBox(  width = NULL,
              #   tabsetPanel(  type = 'pills',
              #     tabPanel( id = 'tab_intros', 
              #       title = 'Intro', ## Tomar índices calculados y ponerlos en mapas
              #       includeMarkdown("md_intro.md") ),
              #     
              #     tabPanel(
              #id = 'tab_gedi', 
              title = 'GEDI Vis', ## Consultar especies basado en un mapa
              
              
              fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                
                column( width = 6,
                        h6(),
                        leafletOutput("mapleaf", #height = "600px"
                        ) %>% withSpinner(color="#0dc5c1")
                ),
                column(width = 6, 
                       fluidRow( 
                         #h5('Click the biotic regions map >> click an ecosystem >> click up to 3 points >> Clear'),
                         column( width = 4, h5('1. Select an ecorregion in the map'), actionButton("clearMap", "Clear ecorregion") ),
                         column( width = 4, h5('2. Select an ecosystem'), actionButton("clearEco", "Clear ecosystem") ),
                         column( width = 4, h5('3. Select GEDI points'), actionButton("clearCur", "Clear curves") )
                       ),
                       br(),
                       highchartOutput("plotcurve", height = "600px") %>% withSpinner(color="#0dc5c1") )
              )
              # ) # close tab
              #### close tabs
              #     
              #   ) # tabsetPanel
              # ) # tabbox
      ),
      
      ######## UI GEDI viz2 ------------
      
      # tab_workgedi, tab_gedi, tab_worksimi, tab_similarity
      #tabItem('tab_workgedi2', includeMarkdown("md_intro.md")),
      tabItem('tab_gedi2', 
              
              fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                
                column( width = 6,
                        fluidRow( 
                          #h5('Click the biotic regions map >> click an ecosystem >> click up to 3 points >> Clear'),
                          column( width = 1),
                          column( width = 4, h5('1. Select an ecorregion in the map'), 
                                  actionButton("clearMap2", "Clear ecorregion") ),
                          column( width = 4, h5('2. Select an ecosystem'), 
                                  actionButton("clearEco2", "Clear ecosystem") ),
                          column( width = 3, h5('3. Select GEDI points'), 
                                  actionButton("clearCur2", "Clear curves") )
                        ),
                        h6(),
                        leafletOutput("mapleaf2", #height = "800px"
                        ) %>% withSpinner(color="#0dc5c1")
                ),
                column(width = 6, 
                       
                       fluidRow( 
                         column( width = 3,
                                 selectInput(inputId = "varSelected", label = "Variable:",
                                             choices =  c( 'PAI', 'PAVD', 'COV'), # 'RH',
                                             selected = 'RH')),
                         column( width = 3,
                                 selectInput(inputId = "eneSelected", label = "Energy:",
                                             choices =  c('Absolute', 'Proportional'), 
                                             selected = 'RH')),
                         column( width = 3, 
                                 sliderInput(inputId = "xAxisSlider", label = "X-range:", 
                                             min = 0, max = 100, value = 100, step = 1)),
                         column( width = 3, 
                                 sliderInput(inputId = "yAxisSlider", label = "Y-range:", 
                                             min = 0, max = 100, value = 100, step = 1))
                       ),
                       
                       highchartOutput("plotcurve2"
                                       , height = "800px"
                       ) %>% withSpinner(color="#0dc5c1") )
              )
              # ) # close tab
              #### close tabs
              #     
              #   ) # tabsetPanel
              # ) # tabbox
      ),
      
      ######## UI GEDI compare ------------
      
      tabItem('tab_compare', 
              tags$style(type = "text/css", "#mapleaf3 {height: calc(90vh - 90px) !important;}"),
              tags$style(type = "text/css", "#plotcurve3 {height: calc(90vh - 90px) !important;}"),
              
              fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                
                column( width = 6,
                        fluidRow( 
                          #h5('Click the biotic regions map >> click an ecosystem >> click up to 3 points >> Clear'),
                          column( width = 1),
                          column( width = 4, #h5('1. Select an ecosystem type:'),
                                  selectInput('selEco', label = '1. Select an ecosystem type:', 
                                              choices = c('Bosque'), selected = 'Bosque')),
                          column( width = 4, h5('2. Click regions in the map'), 
                                  actionButton("clearEco3", "Clear ecosystem") ),
                          column( width = 3, h5('3. Clear curves'),
                                  actionButton("clearCur3", "Clear curves") )
                        ),
                        h6(),
                        leafletOutput("mapleaf3", height = "800px"
                        ) %>% withSpinner(color="#0dc5c1")
                ),
                column(width = 6, 
                       
                       fluidRow( 
                         column( width = 3,
                                 selectInput(inputId = "varSelected3", label = "Variable:",
                                             choices =  c( 'PAI', 'PAVD', 'COV'), # 'RH',
                                             selected = 'RH')),
                         column( width = 3,
                                 selectInput(inputId = "eneSelected3", label = "Energy:",
                                             choices =  c('Absolute', 'Proportional'), 
                                             selected = 'RH')),
                         column( width = 3, 
                                 sliderInput(inputId = "xAxisSlider3", label = "x-range:", 
                                             min = 0, max = 100, value = 100, step = 1)),
                         column( width = 3, 
                                 sliderInput(inputId = "yAxisSlider3", label = "Y-range:", 
                                             min = 0, max = 100, value = 100, step = 1))
                       ),
                       
                       highchartOutput("plotcurve3"
                                       , height = "800px"
                       ) %>% withSpinner(color="#0dc5c1") 
                )
              )
              # ) # close tab
              #### close tabs
              #     
              #   ) # tabsetPanel
              # ) # tabbox
      ),
      
      
      
      ######## UI GEDI cluster ------------
      
      tabItem('tab_worksimi', includeMarkdown("md_introsimi.md")),
      
      tabItem("tab_similarity", 
              fluidRow(
                column(width = 2, selectInput("in_gedi", "GEDI type", c('2a', '2b'), selected = '2a')), #2a_2b
                column(width = 2, selectInput("in_bin", "Samples", c('A', 'B'), selected = 'A')),
                column(width = 2, fluidRow(br(), actionButton("go_showscatter", "Show in plot"))),
                column(width = 1, fluidRow(br(), actionButton("go_showmap", "Show in map"))),
                column(width = 1, fluidRow(br(), actionButton("go_reset", "Reset"))),
                column(width = 2, selectInput("in_x1", "Plot-X-axis", c('u1', 'u2'), selected = 'u1')),
                column(width = 2, selectInput("in_y2", "Plot-Y-axis", c('u1', 'u2'), selected = 'u2'))
              ),
              fluidRow(
                
                column(width = 6, 
                       fluidRow(column(width = 1),
                                column(width = 3, selectInput("in_r", "Pts color: Red", c('u1', 'u2'), selected = 'u1')),
                                column(width = 4, selectInput("in_g", "Pts color: Green", c('u1', 'u2'), selected = 'u2')),
                                column(width = 4, selectInput("in_b", "Pts color: Blue", c('u1', 'u2'), selected = 'u1')))
                       , 
                       leafletOutput("leafPts", height = "600px") %>% withSpinner(color="#0dc5c1")
                ),
                
                column(width = 6, 
                       fluidRow(plotlyOutput("plot", height = '80%') %>% withSpinner(color="#0dc5c1"),
                                plotlyOutput("barplot", height = '80%') %>% withSpinner(color="#0dc5c1"))
                       
                )
              )
      ) ,
      
      
      ######## UI heigth ------------
      
      tabItem(tabName = "tab_heightmap", 
              
              #bootstrapPage(
              #fluidPage(
              #fluidRow(
              fillPage(
                #column(width = 12,
                #tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                # leafletOutput("mapx",# width = "100%", height = "100%")
                #               height = 1000)
                
                #tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                #tags$style(type = "text/css", "#plotcurve2 {height: calc(90vh - 90px) !important;}"),
                leafletOutput("mapx", width="100%",height="1000px")#, width="100%", height = "100%")
                ,
                absolutePanel(top = 50, right = 20,
                              fluidRow(
                                column(2, br(), actionButton("go_height", "Clip map")),
                                column(3, selectInput("map_eco", "Eco. domain", c('All', 'Forest'), 
                                                      selected = 'All')),
                                column(3, selectInput("map_var", "Veg. variable", 
                                                      c('Canopy heigth',
                                                        'Canopy cover',
                                                        'fhdPai',
                                                        'PAI',
                                                        'RH50'), 
                                                      selected = 'Canopy heigth'))
                              ),
                              
                              
                              verbatimTextOutput('rglText') %>% withSpinner(color="#0dc5c1"),
                              #checkboxInput("scale_rgl", "Show legend", TRUE),
                              rglwidgetOutput("rglPlot")
                )
              )
      ) # close tab
      
      ######## UI close ------------
      
    ) # close tabItems
  ) # dashboardbody
)


# SERVER  ---------------
server <- function(input, output, session) {
  
  
} # close


shinyApp(ui = ui, server = server)


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