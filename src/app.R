library(shiny)
library(leaflet)
library(leaflet.extras)
library(mapview)
library(shinyWidgets)
library(DT)
library(bslib)
library(htmlwidgets)
library(htmltools)

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

# Define the dependency
dependency_ant_path <- htmlDependency(
  name = "leaflet-ant-path",
  version = "1.0.0",
  src = c(href = "https://raw.githubusercontent.com/rubenspgcavalcante/leaflet-ant-path/master/dist/"),
  script = "leaflet-ant-path.js"
)

# User interface ----
ui <- fluidPage(

  tags$head(
    # tags$script(src = "leaflet-ant-path.js"),
    # tags$script(src = "leaflet-ant-path.es6.js"),
    tags$script(src = "leaflet-curve.js"),
    tags$script(HTML("
    var isRendered = false;

    Shiny.addCustomMessageHandler('drawBezier', function(data) {

      if (isRendered) {
        if (window.curvePaths) {
          window.curvePaths.forEach(function(path) {
            mapElem.removeLayer(path);
          });
        }

        window.curvePaths = data.map(function(curvePathData) {
          var options = {
            color: curvePathData.color || 'red',
            fill: false,
            weight: curvePathData.width || 2
          };
          var curvePath = L.curve(curvePathData.path, options);
          curvePath.addTo(mapElem);
          return curvePath;
        });

      }
    })
    ")),

  ),

  titlePanel("Electricity imports and exports across US-Canada Border"),

  sidebarLayout(
    sidebarPanel(
      helpText("Helper text."),

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

  # Define reactiveValues to store clicked information
  clicked_info <- reactiveValues(name = NULL, group = NULL)
  
  output$table <-  renderDT(
    {
      filter_df(trade_df, input$year, input$trade, input$province)
    }
  )

  output$map <- renderLeaflet(
    m <- leaflet() %>%
      addTiles() %>%
      setView(lng = center_long, lat = center_lat, zoom = 2.5)
      %>% htmlwidgets::onRender("
        function(el, x) {
          mapElem = this;
          isRendered = true;
          }
      ")
  )
    
  coordinates <- reactiveValues(start = NULL, end = NULL)

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
        # popup = paste0("<strong>", combined_sf$name ,"</strong>"),
        label = lapply(1:nrow(combined_sf), function(i) {
          HTML(paste0(combined_sf$name[i], "<br>", "Total ",
                      input$trade, " (", input$year, "): ",
                      get_total(filter_df(trade_df, input$year,
                                          input$trade, combined_sf$name[i]))))
        })
      )
  })

  observeEvent(input$map_shape_click, {
    click_info <- input$map_shape_click
    clicked_info$name <- click_info$id  # This will be the name of the state/province
    clicked_info$group <- click_info$group
    cat("Group:", clicked_info$group, "\n")
    cat("Name:", clicked_info$name, "\n")
    updateMap()
  })

  updateMap <- reactive({
    req(clicked_info$name, clicked_info$group)

    if (clicked_info$group == 'state') {
      start_data <- subset(flattened_us, name == clicked_info$name)
    } else {
      start_data <- subset(flattened_ca, name == clicked_info$name)
    }

    

    # Define a point outside North America for testing
    outside_na_lat <- 50  # Latitude for a point outside North America
    outside_na_lon <- 10  # Longitude for a point outside North America
    
    curvePaths <- list(
      # First line segment
      list(
        path = list('M', c(start_data$lat, start_data$lon), 'L', c(5, -75)),
        color = 'blue',  # Optional color parameter
        width = 4        # Width parameter
      ),
      # Second line segment starting from the same point
      list(
        path = list('M', c(start_data$lat, start_data$lon), 'L', c(outside_na_lat, outside_na_lon)),
        color = 'green', # Optional color parameter
        width = 6        # Width parameter
      )
    )
     
    session$sendCustomMessage("drawBezier", curvePaths)
  })
}

# Run the app
shinyApp(ui, server)