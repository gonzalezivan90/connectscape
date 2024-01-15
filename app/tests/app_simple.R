### step by step version
## workign version meant to scale by steps

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
  library(maptools)
  #library(mongolite)#
  
  library(raster)
  library(RColorBrewer)
  library(rgdal)
  library(rgeos)
  library(rmarkdown)
  library(sf)
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
  library(viridis)
  
  #https://stackoverflow.com/questions/17107206/change-temporary-directory
  # write(paste0("TMP = '","/data/tempR" ,"'"), file=file.path(Sys.getenv('R_USER'), '.Renviron'))
  # write("TMP = /data/tempR", file=file.path('~/.Renviron'))
  # YES; R -e "write('TMP = /data/tempR', file=file.path('~/.Renviron'))"
  # NO R -e "write('TMP = \"/data/tempR\"', file=file.path('~/.Renviron'))"
  # NO R -e "write('TMP = "/data/tempR"', file=file.path(Sys.getenv('R_USER'), '.Renviron'))"
}

server <- function(input, output, session) {
  
  # Fill in the spot we created for a plot
  output$phonePlot <- renderPlot({
    
    # Render a barplot
    barplot(WorldPhones[,input$region]*1000, 
            main=input$region,
            ylab="Number of Telephones",
            xlab="Year")
  })
}


# Use a fluid Bootstrap layout
ui <- fluidPage(    
  
  # Give the page a title
  titlePanel("Telephones by region"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("region", "Region:", 
                  choices=colnames(WorldPhones)),
      hr(),
      helpText("Data from AT&T (1961) The World's Telephones.")
    ),
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput("phonePlot")  
    )
  )
)

shinyApp(ui, server)
