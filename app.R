library(tidyverse)
library(DT)
library(shiny)
library(shinyFiles)
tbl <- read_csv("tbl.csv")
unitPt <- c(tbl$points/tbl$serv)
dat <- select(tbl, item, cat, serv, unit, points) 
dat <- mutate(dat, unitPt = tbl$points/tbl$serv)
dat$unitPt <- round(dat$unitPt, 3 )

# ui ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ui <- fluidPage(
  fluidRow(
    column(1,
           h3("Menge:")), 
    column(1,
           numericInput("menge", "", min = 0, max = 330, value =  0, step = 1 )
    ),
    column(1,
           h3("Einheit:")), 
    column(1,
           h3(textOutput("unit"))), 
    column(1,
           h3("Punkte:")),
    column(1,
           h3(textOutput("total"))),
    column(2,
           h3("Übernehmen:")),
    column(2,
           h3(actionButton("submit","", icon =icon("fas fa-arrow-alt-circle-up")  ))) 
  ),
  
  fluidRow(
    column(12,
           DTOutput("x3"),
           #h5(textOutput("currentTime"))
    )
  )
)

# server ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
server <- function(input, output, session) {
  
  output$x3 = DT::renderDataTable(dat,
                                  filter= "top",
                                  plugins = "searchHighlight",
                                  extensions = 'Buttons',
                                  options = list(lengthChange = FALSE,
                                                 dom = 'Bfrtip',
                                                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                                 # autoWidth = TRUE,
                                                 # columnDefs = list(list(width = '50px', targets = c(2)),
                                                 #                   #list(width = '60px', targets = c(2)),
                                                 #                   list(width = '10px', targets = c(3:6))
                                                 #                   ),
                                                 initComplete = JS(
                                                   "function(settings, json) {",
                                                   "$(this.api().table().header()).css({'background-color': '#33cccc', 'color': '#fff'});",
                                                   "}")
                                  ),
  )
  #calculate total pt
  output$total = renderText(dat$unitPt[input$x3_rows_selected]*input$menge) 
  output$unit = renderText(dat$unit[input$x3_rows_selected])
  
  output$currentTime <- renderText({
    invalidateLater(1000, session)
    paste("Time", Sys.time())
    
    # input$goButton    -----
    server <- function(input, output) {
      output$distPlot <- renderPlot({
        # Take a dependency on input$goButton. This will run once initially,
        # because the value changes from NULL to 0.   
        
        # Use isolate() to avoid dependency on input$obs
        dist <- isolate(rnorm(input$obs))
        hist(dist)
        #----
      })
    }
    
    
    
  })
}
shinyApp(ui, server)


