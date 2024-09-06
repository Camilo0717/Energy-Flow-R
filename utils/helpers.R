library(dplyr)
library(sf)
library(jsonlite)

# Rename columns for the Dataframe
columns <- c("Year", "Activity", "Source", "Destination",
            "Energy_MWh", "Value_CAD", "Price_CAD.MWh")

# Load GeoJSON data
canada_sf <- st_read("./data/ca-provinces.json")
us_sf <- st_read("./data/us-states.json")

# Load centroids data
centroids <- fromJSON("./data/centroids.json")

# Flatten centroids into a dataframe
nested_ca_df <- centroids[[2]][[1]]
nested_us_df <- centroids[[2]][[2]]

flatten_df <- function(df){
  flattened_df <- data.frame(
    name = df$name,
    lat = df$centroid$lat,
    lon = df$centroid$lon
  )
  return(flattened_df)
}

flattened_ca <- flatten_df(nested_ca_df)
flattened_us <- flatten_df(nested_us_df)

# Drop all columns except province/state name and geometry
canada_sf <- select(canada_sf, name, geometry)
us_sf <- select(us_sf, name, geometry)

# Add region type for merging
canada_sf <- canada_sf %>% mutate(region_type = "province")
us_sf <- us_sf %>% mutate(region_type = "state")

# Merge dataframes
combined_sf <- bind_rows(canada_sf, us_sf)

filter_df <- function(df, year, trade, region){
  if (trade == 'Imports') {
    return(df 
      %>% filter(Year == year, Activity == 'Imports', Destination == region))
  } else {
    return(df 
      %>% filter(Year == year, Activity == 'Exports', Source == region))
  }
}

get_total <- function(df){
  df <- df %>% filter (Source == "Total" | Destination == "Total")
  total <- df["Energy_MWh"]
  totalStr <- if_else(nrow(total) == 1, paste0(total, " MWh"), "N/A")
  return(totalStr)
}

get_destinations <- function(input_df){
  destinations <- input_df %>% distinct(Destination)
  destinations_list <- unlist(destinations$Destination)
  return(destinations_list)
}

get_sources <- function(input_df){
  sources <- input_df %>% distinct(Source)
  sources_list <- unlist(sources$Source)
  return(sources_list)
}

get_unique_sources_destinations <- function(input_df){
  sl <- get_sources(input_df)
  dl <- get_destinations(input_df)
  sdl <- c(sl, dl)
  
  return(unique(sdl))
}

# Declare RTOs from the dataset
RTOs <- c("Pennsylvania Jersey Maryland Power Pool", "New England-ISO", "Minn / N. Dakota")

# Get states in the dataset
get_states <- function(input_df){
  sdl <- get_unique_sources_destinations(input_df)
  
  states_in_df <- setdiff(sdl, RTOs)
  states_in_df <- states_in_df[states_in_df != "Total"]
  
  return(states_in_df)
}

# Prepare and clean dataset
prepare_df <- function(input_df){
  ## Change column names
  colnames(input_df) <- columns
  
  ## Eliminate entries with suppressed Destination
  input_df <- input_df %>% filter(Destination != "")
  
  ## Fix Quebec for Source and Destination columns
  input_df <- input_df %>% mutate(Source = if_else(Source == "Qu\xe9bec", "Quebec", Source))
  
  input_df <- input_df %>% mutate(Destination = if_else(Destination == "Qu\xe9bec", "Quebec", Destination))
  
  ## Drop price column
  input_df <- subset(input_df, select = -c(Price_CAD.MWh))
  
  return(input_df)
}

# Get a dataframe with tradeflow for the counterpart areas based on activity, year, and name of S/P
get_counterparts <- function(df, year, activity, group, name){

  df <- df %>% filter(Year == year)

  if (group == 'province') {
    if (activity == 'Imports') {
      df <- df %>% filter(Activity == activity, Destination == name)
    } else if (activity == 'Exports') {
      df <- df %>% filter(Activity == activity, Source == name)
    }
  } else if (group == 'state') {
    if (activity == 'Imports') {
      df <- df %>% filter(Activity == 'Exports', Destination == name)
    } else if (activity == 'Exports') {
      df <- df %>% filter(Activity == 'Imports', Source == name)
    } 
  }

  return(df)
}

# Calculate control points for the Bézier curves
calculate_control_points <- function(start, end) {

  # Determine if the flow goes North-South or viceversa
  if (start$lat > end$lat){
    # North -> South
    return(list(end$lon, start$lat))
  } else {
    # South -> North, or same latitude edge scenario
    return(list(start$lon, end$lat))
  }

}

sendBezierMessage <- function(session, start_lat, start_lon, end_lat, end_lon){
  data <- list(
    start = list(lat = start_lat, lon = start_lon),
    end = list(lat = end_lat, lon = end_lon)
  )
  session$sendCustomMessage(type = 'drawBezier', data)
  print("Sending Message")
}
