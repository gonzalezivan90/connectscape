library(tidyverse)
library(shiny)
library(reshape2)
library(DT)
library(tibble)


(data <- 
    read.delim(sep = ' ', text = "ID Type Range
21 A1 B1 100
22 C1 D1 200"))

write.csv(data, 'a_splitme.csv')





# ###function for deleting the rows
# splitColumn <- function(data, column_name) {
#   newColNames <- c("Unmerged_type1", "Unmerged_type2")
#   newCols <- colsplit(data[[column_name]], " ", newColNames)
#   after_merge <- cbind(data, newCols)
#   after_merge[[column_name]] <- NULL
#   after_merge
# }
# ###_______________________________________________
# ### function for inserting a new column
# 
# fillvalues <- function(data, values, columName){
#   df_fill <- data
#   vec <- strsplit(values, ",")[[1]]
#   df_fill <- tibble::add_column(df_fill, newcolumn = vec, .after = columName)
#   df_fill
# }
# 
# ##function for removing the colum
# 
# removecolumn <- function(df, nameofthecolumn){
#   df[ , -which(names(df) %in% nameofthecolumn)]
# }
# 
# ### use a_splitme.csv for testing this program
# 
# 
# # UI ----------------------------------------------------------------------
# 
# ui <- fluidPage(
#   sidebarLayout(
#     sidebarPanel(
#       fileInput("file1", "Choose CSV File", accept = ".csv"),
#       checkboxInput("header", "Header", TRUE),
#       actionButton("Splitcolumn", "SplitColumn"),
#       uiOutput("selectUI"),
#       actionButton("deleteRows", "Delete Rows"),
#       textInput("textbox", label="Input the value to replace:"),
#       actionButton("replacevalues", label = 'Replace values'),
#       actionButton("removecolumn", "Remove Column"),
#       actionButton("Undo", 'Undo')
#     ),
#     mainPanel(
#       DTOutput("table1")
#     )
#   )
# )
# 
# 
# # SERVER ------------------------------------------------------------------
# 
# server <- function(session, input, output) {
#   rv <- reactiveValues(data = NULL, orig=NULL)
#   
#   observeEvent(input$file1, {
#     file <- input$file1
#     ext <- tools::file_ext(file$datapath)
#     
#     req(file)
#     
#     validate(need(ext == "csv", "Please upload a csv file"))
#     
#     rv$orig <- read.csv(file$datapath, header = input$header, )
#     rv$data <- rv$orig
#   })
#   
#   output$selectUI<-renderUI({
#     req(rv$data)
#     selectInput(inputId='selectcolumn', label='select column', choices = names(rv$data))
#   })
#   
#   #splitcolumn
#   observeEvent(input$Splitcolumn, {
#     rv$data <- splitColumn(rv$data, input$selectcolumn)
#   })
#   
#   #delterows
#   observeEvent(input$deleteRows,{
#     if (!is.null(input$table1_rows_selected)) {
#       rv$data <- rv$data[-as.numeric(input$table1_rows_selected),]
#     }
#   })
#   
#   
#   # renderDT ----------------------------------------------------------------
#   
#   output$table1 <- renderDT({
#     datatable(rv$data, editable = TRUE)
#   })
#   
#   observeEvent(input$table1_cell_edit, {
#     row  <- input$table1_cell_edit$row
#     clmn <- input$table1_cell_edit$col
#     rv$data[row, clmn] <- input$table1_cell_edit$value
#   })
#   
#   
#   observeEvent(input$replacevalues, {
#     rv$data <- fillvalues(rv$data, input$textbox, input$selectcolumn)
#   })
#   observeEvent(input$removecolumn, {
#     rv$data <- removecolumn(rv$data,input$selectcolumn)
#   })
#   observeEvent(input$Undo, {
#     rv$data <- rv$orig
#   })
# }
# 
# shinyApp(ui, server)


###function for deleting the rows
splitColumn <- function(data, column_name) {
  newColNames <- c("Unmerged_type1", "Unmerged_type2")
  newCols <- colsplit(data[[column_name]], " ", newColNames)
  after_merge <- cbind(data, newCols)
  after_merge[[column_name]] <- NULL
  after_merge
}
###_______________________________________________
### function for inserting a new column

fillvalues <- function(data, values, columName){
  df_fill <- data
  vec <- strsplit(values, ",")[[1]]
  df_fill <- tibble::add_column(df_fill, newcolumn = vec, .after = columName)
  df_fill
}

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

### use a_splitme.csv for testing this program


# UI ----------------------------------------------------------------------

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File", accept = ".csv"),
      checkboxInput("header", "Header", TRUE),
      #actionButton("Splitcolumn", "SplitColumn"),
      uiOutput("selectUI"),
      #actionButton("deleteRows", "Delete Rows"),
      #textInput("textbox", label="Input the value to replace:"),
      #actionButton("replacevalues", label = 'Replace values'),
      actionButton("addcolumn", "Add Column"),
      actionButton("removecolumn", "Remove last column"),
      actionButton("Undo", 'Undo')
    ),
    mainPanel(
      #DTOutput("table1")
      DT::dataTableOutput(outputId =  "table1")
      
    )
  )
)


# SERVER ------------------------------------------------------------------

server <- function(session, input, output) {
  rv <- reactiveValues(data = NULL, orig=NULL)
  
  observeEvent(input$file1, {
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    
    validate(need(ext == "csv", "Please upload a csv file"))
    
    rv$orig <- t(read.csv(file$datapath, header = input$header))
    colnames(rv$orig) <- paste0('Scen', 1:ncol(rv$orig))
    
    rv$data <- (rv$orig)
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
}

shinyApp(ui, server)