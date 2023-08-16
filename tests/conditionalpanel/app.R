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
  
}

# debug insall order: htmltools >> shiny >> shinyWidgets


# install.packages('shinyWidgets')
# install.packages('shinydashboardPlus')
# install.packages('dashboardthemes')

# if ( identical ( unname(Sys.info()[c("sysname", 'nodename')]), c("Windows", 'HP-Z400')) ){
#   setwd('N:/Mi unidad/IG/server_IG/gedivis')
#   #setwd('N:/Mi unidad/IG/server_IG/gedivis/')
# }

library(shiny)
library(shinydashboard)

# ui ---------------------------------------------------------------------------

ui <- dashboardPage(
  
  # title ----
  dashboardHeader(title = "Test Application"),
  
  # sidebar ----
  dashboardSidebar(
    sidebarMenu(id = "sidebarid",
                menuItem("Home", tabName = "tabhome", icon = icon("house-user")),
                menuItem("Habitat suitability <>\nresistance surface", tabName = "tabsurface", icon = icon("map-pin")),
                menuItem("Create source points", tabName = "tab_points", icon = icon("map-pin")),
                menuItem("Cost distance matrix", tabName = "tab_distance", icon = icon("border-all")),
                menuItem("CDPOP", tabName = "tab_cdpop", icon = icon("hippo")),
                menuItem("Connectivity - corridors", tabName = "tab_corridors", icon = icon("route")),
                
                conditionalPanel(
                  'input.sidebarid == "tab_corridors"',
                  sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30),
                  selectInput("title", "Select plot title:", choices = c("Hist of x", "Histogram of x"))
                ),
                
                # conditionalPanel(
                #   'input.sidebarid %in% c("tab_kernels", "tab_plotting")',
                #   shiny::fileInput('in_corrpoints', 'Load point file Co', buttonLabel = 'Search', placeholder = 'No choose',
                #                    accept=c('.csv','.txt'), multiple=FALSE),
                #   shiny::fileInput('in_corrsurface', 'Load point file Co', buttonLabel = 'Search', placeholder = 'No choose',
                #                    accept=c('.csv','.txt'), multiple=FALSE)
                # ),
                menuItem("Connectivity - dispersal kernels", tabName = "tab_kernels", icon = icon("bezier-curve")),
                menuItem("Plotting", tabName = "tab_plotting", icon = icon("image")),
                menuItem("Mapping", tabName = "tab_Mapping", icon = icon("map")),
                menuItem("Connectivity - prioritization", tabName = "tab_priori", icon = icon("trophy")),
                menuItem("Landscape genetics mapping tools", tabName = "tab_genetics", icon = icon("route")),
                menuItem("Run locally", tabName = "tablocal", icon = icon("code-fork")),
                
                menuItem("Page 1", tabName = "page1"),
                conditionalPanel(
                  'input.sidebarid == "page1"',
                  sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30),
                  selectInput("title", "Select plot title:", choices = c("Hist of x", "Histogram of x"))
                ),
                menuItem("Page 2", tabName = "page2")
                
    )
  ),
  
  # body ----
  dashboardBody(
    tabItems(
      # page 1 ----
      tabItem(tabName = "page1", "Page 1 content. This page doesn't have any sidebar menu items."),
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