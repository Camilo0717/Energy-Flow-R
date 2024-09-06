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
    tags$script(src = "leaflet-bezier.js"),
    tags$script(src = "leaflet-ant-path.es6.js"),
    tags$script(src = "leaflet-ant-path.js"),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('drawBezier', function(data) {
        var options = {
          color: 'rgb(145, 146, 150)',
          fillColor: 'rgb(145, 146, 150)',
          dashArray: 8,
          opacity: 0.8,
          weight: '1',
          iconTravelLength: 0.5, // How far icon should go. 0.5 = 50%
          iconMaxWidth: 50,
          iconMaxHeight: 50,
          fullAnimatedTime: 7000, // animation time in ms
          easeOutPiece: 4, // animation easy out time in ms
          easeOutTime: 2500 // animation easy out time in ms
        };

        L.bezier({
          path: [
            [
              {lat: data.start.lat, lng: data.start.lon},
              {lat: data.end.lat, lng: data.end.lon}
            ]
          ],

          icon: {
            path: 'plane.png'
          }
        }, options).addTo(map);
      });
    "))
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
          textOutput("start_coordinates"),
          br(),
          textOutput("end_coordinates"),
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

  coordinates <- reactiveValues(start = NULL, end = NULL)

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

  updateMap <- reactive({
    req(input$province, input$state)

    province_data <- subset(flattened_ca, name == input$province)
    state_data <- subset(flattened_us, name == input$state)

    # coordinates$start <- province_data$centroid
    # coordinates$end <- state_data$centroid
    print("Start printing")
    print(province_data)
    print(state_data)
    print("End printing")

    # session$sendCustomMessage(type = "draw-bezier-curve", list(
    #   start = c(province_data$)
    # ))

    sendBezierMessage(session, province_data$lat, province_data$lon,
                      state_data$lat, state_data$lon)
  })

  # output$start_coordinates <- renderText({
  #   paste("Start Coordinates: Lat =",
  #         coordinates$start$lat, ", Lon =", coordinates$start$lon)
  # })

  # output$end_coordinates <- renderText({
  #   paste("End Coordinates: Lat =",
  #         coordinates$end$lat, ", Lon =", coordinates$end$lon)
  # })

}

# Run the app
shinyApp(ui, server)