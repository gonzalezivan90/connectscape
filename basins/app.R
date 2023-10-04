library(shiny)
library(leaflet)
library(RColorBrewer)

# manually create a legend
legend <- function(values, palette, title, left_label, right_label, bins = 7) {
  
  # validate args
  stopifnot(!is.null(values))
  stopifnot(!is.null(palette))
  stopifnot(!is.null(title))
  stopifnot(!is.null(left_label))
  stopifnot(!is.null(right_label))
  
  # generate color palette using Bins (not sure if it's the best approach)
  # @reference: 
  # https://github.com/rstudio/leaflet/blob/c19b0fb9c60d5caf5f6116c9e30dba3f27a5288a/R/legend.R#L93
  pal <- colorNumeric(palette, values)
  cuts <- if (length(bins) == 1) pretty(values, n = bins) else bins
  n <- length(cuts)
  r <- range(values, na.rm = TRUE)
  # pretty cut points may be out of the range of `values`
  cuts <- cuts[cuts >= r[1] & cuts <= r[2]]
  colors <- pal(c(r[1], cuts, r[2]))
  
  # generate html list object using colors
  legend <- tags$ul(class = "legend")
  legend$children <- lapply(seq_len(length(colors)), function(color) {
    tags$li(
      class = "legend-item legend-color",
      style = paste0(
        "background-color:", colors[color]
      ),
    )
  })
  
  # add labels to list
  legend$children <- tagList(
    tags$li(
      class = "legend-item legend-label left-label",
      as.character(left_label)
    ),
    legend$children,
    tags$li(
      class = "legend-item legend-label right-label",
      as.character(right_label)
    )
  )
  
  # render legend with title
  return(
    tagList(
      tags$span(class = "legend-title", as.character(title)),
      legend
    )
  )
}

# ui
ui <- tagList(
  tags$head(
    tags$style(
      "html, body {
                width: 100%;
                height: 100%;
            }",
      ".legend-title {
                display: block;
                font-weight: bold;
            }",
      ".legend {
                list-style: none;
                padding: 0;
                display: flex;
                justify-content: center;
                align-items: center;
            }",
      ".legend-item {
                display: inline-block;
            }",
      ".legend-item.legend-label {
                margin: 0 8px;
            }",
      ".legend-item.legend-color {
                width: 24px;
                height: 16px;
            }"
    )
  ),
  bootstrapPage(
    leafletOutput("map", width = "100%", height = "100%"),
    absolutePanel(
      top = 10, right = 10,
      sliderInput("range", "Magnitudes", min(quakes$mag), max(quakes$mag),
                  value = range(quakes$mag), step = 0.1
      ),
      selectInput("colors", "Color Scheme",
                  rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
      ),
      checkboxInput("legend", "Show legend", TRUE)
    ),
    absolutePanel(
      bottom = 20,
      right = 10,
      width = "225px",
      uiOutput("map_legend"),
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    quakes[quakes$mag >= input$range[1] & quakes$mag <= input$range[2],]
  })
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, quakes$mag)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(quakes) %>%
      addTiles() %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~10^mag/10, weight = 1, color = "#777777",
                 fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
      )
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    if (input$legend) {
      output$map_legend <- renderUI({
        
        # build legend
        legend(
          values = filteredData()[["mag"]],
          palette = as.character(input$colors),
          title = "Mag",
          left_label = "0%",
          right_label = "100%"
        )
      })
    }
    if (!input$legend) {
      output$map_legend <- renderUI({
        tags$div("")
      })
    }
  })
}

shinyApp(ui, server)