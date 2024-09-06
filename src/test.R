library(shiny)
library(leaflet)
library(leaflet.extras)
library(mapview)
library(shinyWidgets)
library(DT)
library(bslib)
library(htmlwidgets)

setwd("../")

# Source helpers ----
source("./utils/helpers.R")

# Load data
trade_csv <- read.csv("./data/electricity-exports-and-imports-annual.csv",
                      header = TRUE, sep = ",")

# Prepare data using helpers.R
trade_df <- prepare_df(trade_csv)

# Initial coordinates
center_lat <- 59
center_long <- -106

# User interface ----
ui <- fluidPage(

  tags$head(
    tags$script(src = "leaflet-ant-path.js"),
    # tags$script(src = "leaflet-ant-path.js.map"),
    tags$script(src = "leaflet-ant-path.es6.js"),
    # tags$script(src = "leaflet-ant-path.es6.js.map"),
    tags$script(
      HTML("
function drawBezierCurve(map, start, end, control) {
    const antCurve = antPath([
    'M', [start],
    'C', [control, control],
    'Z', [end]
    ],
    {use: L.curve, color: 'yellow', fill: true});

    antCurve.addTo(map)
  }
    Shiny.addCustomMessageHandler('draw-bezier-curve', function(data) {
      var start = [data.start.lat, data.start.lon];
      var end = [data.end.lat, data.end.lon];
      var control = [data.control.lat, data.control.lon];
      
      drawBezierCurve(leafletMap, start, end, control, control);
    });
  ")
    )
  ),

  titlePanel("Electricity imports and exports across US-Canada Border"),

  sidebarLayout(
    sidebarPanel(
      helpText("This visualization allows the user to analyze 
      the flow of electricity to/from Canadian Provinces and 
      territories to/from US States."),

      selectInput("trade", "Select a trade to visualize",
                  c("Imports", "Exports")),

      selectizeInput("province", "Select or search a Province:",
                     choices = c("Alberta", "British Columbia", "Manitoba",
                                 "New Brunswick", "Newfoundland and Labrador",
                                 "Nova Scotia", "Ontario",
                                 "Prince Edward Island", "Quebec",
                                 "Saskatchewan"),
                     options = list(placeholder = "Search...")),

      selectizeInput("state", "Select or search a State:",
                     choices = us_sf$name,
                     options = list(placeholder = "Search...")),

      selectInput("year", "Select a year", 
                  choices = min(trade_df["Year"]):
                    as.numeric(format(Sys.Date(), "%Y"))),
    ),

    mainPanel(

      tabsetPanel(type = "tabs",

        tabPanel("Map", br(), leafletOutput("map")),

        tabPanel("Data", br(),  DTOutput("table")),

        tabPanel("Testing",
          br(),
          textOutput("current_selection"),
          br(),
          textOutput("previous_selection"),
          br(),
          textOutput("total"),
        )

      )
    )
  )
)

# Server logic
server <- function(input, output, session) {

  output$table <-  renderDT(
    {
      filter_df(trade_df, input$year, input$trade, input$province)
    }
  )

  output$map <- renderLeaflet(
    m <- leaflet() %>%
      addTiles() %>%
      setView(lng = center_long, lat = center_lat, zoom = 2.5)
  )

  observeEvent(input$province, {
    updateMap()
  })
  
  observeEvent(input$state, {
    updateMap()
  })

  popup <- paste0("<strong>", combined_sf$name ,"</strong>")

  # Highlight State or Province when hovering over it
  observe({
    leafletProxy("map", data = NULL) %>%
      # removeShape("name") %>%
      addProviderTiles("OpenStreetMap") %>%
      addPolygons(
        data = combined_sf,
        fillColor = "transparent",
        weight = 1,
        color = "grey",
        fillOpacity = 0,
        highlightOptions = highlightOptions(
          fillColor = "blue",
          weight = 3,
          color = "black",
          fillOpacity = 0.7
        ),
        layerId = ~name,
        group = ~region_type,
        popup = popup,
        label = lapply(1:nrow(combined_sf), function(i) {
          HTML(paste0(combined_sf$name[i], "<br>", "Total ",
                      input$trade, " (", input$year, "): ",
                      get_total(filter_df(trade_df, input$year,
                                          input$trade, combined_sf$name[i]))))
        })
      )
  })
  start <- list(lat = 34.0522, lon = -118.2437)
  end <- list(lat = 40.7128, lon = -74.0060)
  control <- list(lat=44, lon=-60)
  updateMap <- reactive({
    req(input$province, input$state)
    # session$sendCustomMessage(type="draw-bezier-curve", list(
      
    #   start=start,
    #   end=end,
    #   control=control
    # ))
   
  })
}

# Run the app
shinyApp(ui, server)