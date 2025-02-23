#' Visualize building geometries with Leaflet
#'
#' @importFrom leaflet leaflet addTiles addPolygons colorQuantile
#' @param building_sf sf object containing building geometries with height and building ID
#'
#' @importFrom leaflet leaflet addTiles addPolygons
#'
#' @return A leaflet map object showing building geometries
#' @export
visualize_buildings <- function(building_sf) {
  # Transform CRS for Leaflet (WGS84 - EPSG:4326)
  building_sf_4326 <- sf::st_transform(building_sf, crs = 4326)

  # Visualize with Leaflet
  leaflet(building_sf_4326) %>%
    addTiles() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(
      fillColor = ~colorQuantile("YlOrRd", height)(height),
      fillOpacity = 1,
      color = NA,
      weight = 1,
      popup = ~as.character(part_id)
      )
}



#' Run Interactive Shadow Map Application
#'
#' Starts a Shiny application to visualize shadow progression over a day.
#' It takes precomputed map layers containing buildings, shadows, and sunlight areas.
#'
#' @param map_layer A named list of sf objects for each hour, containing `buildings`, `shadows`, and `sunlight` geometries in the respective coordinate system.
#' @param date A character string representing the date (e.g., `"2025-02-18"`).
#'
#' @import shiny
#' @import leaflet
#' @import sf
#' @import shinyWidgets
#' @import lwgeom
#'
#' @return Launches a Shiny interactive map application.
#' @export
run_interact_shadow_map <- function(map_layer, date) {
  # S2 deaktivieren, falls Probleme auftreten
  sf_use_s2(FALSE)

  # Extract time labels and hours
  time_labels <- names(map_layer)
  time_hours <- sub(".* - (.*)", "\\1", time_labels)

  # Extract buildings and validate geometries
  all_buildings <- map_layer[[1]]$buildings

  if (is.null(all_buildings) || (is.data.frame(all_buildings) && nrow(all_buildings) == 0)) {
    stop("Error: No building data available in map_layer.")
  }

  # Repariere ungültige Geometrien
  all_buildings <- st_make_valid(all_buildings)
  all_buildings <- all_buildings[st_is_valid(all_buildings), ]

  # Entferne doppelte Punkte
  all_buildings <- st_simplify(all_buildings, dTolerance = 0.00001, preserveTopology = TRUE)

  # Transformiere zu WGS84 (EPSG:4326)
  all_buildings <- st_transform(all_buildings, crs = 4326)

  # Berechne Zentrum (Falls MULTIPOLYGON -> größtes Polygon)
  center_coords <- st_coordinates(st_centroid(all_buildings, of_largest_polygon = TRUE))
  center_lng <- mean(center_coords[, 1])
  center_lat <- mean(center_coords[, 2])

  # Shiny UI
  ui <- fluidPage(
    titlePanel(paste("Shadow Progression Over the Day:", date)),
    leafletOutput("map", height = "700px"),
    absolutePanel(
      id = "controls", class = "panel panel-default", fixed = FALSE,
      draggable = TRUE, bottom = 0.5, left = 20, width = 350,
      sliderTextInput(
        inputId = "time",
        label = "Time:",
        choices = time_hours,
        selected = time_hours[1],
        grid = TRUE,
        animate = animationOptions(interval = 2500, loop = TRUE)
      )
    )
  )

  # Shiny Server
  server <- function(input, output, session) {

    # Reactive function to get selected time data
    current_data <- reactive({
      time <- input$time
      matched_time <- time_labels[which(time_hours == time)]

      if (length(matched_time) == 0) return(NULL)

      data <- map_layer[[matched_time]]
      if (is.null(data)) return(NULL)

      # Repariere Geometrien, falls nötig
      data$buildings <- st_make_valid(data$buildings)
      data$buildings <- data$buildings[st_is_valid(data$buildings), ]
      data$buildings <- st_simplify(data$buildings, dTolerance = 0.00001, preserveTopology = TRUE)
      data$buildings <- st_transform(data$buildings, crs = 4326)

      data$sunlight  <- st_make_valid(data$sunlight)
      data$sunlight  <- data$sunlight[st_is_valid(data$sunlight), ]
      data$sunlight  <- st_transform(data$sunlight, crs = 4326)

      data$shadows   <- st_make_valid(data$shadows)
      data$shadows   <- data$shadows[st_is_valid(data$shadows), ]
      data$shadows   <- st_transform(data$shadows, crs = 4326)

      return(data)
    })

    # Render map
    output$map <- renderLeaflet({
      map_layer_data <- current_data()
      if (is.null(map_layer_data)) return(leaflet() %>% addTiles())

      center_coords <- st_coordinates(st_centroid(map_layer_data$buildings, of_largest_polygon = TRUE))
      center_lng <- mean(center_coords[, 1])
      center_lat <- mean(center_coords[, 2])

      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = center_lng, lat = center_lat, zoom = 16) %>%
        addPolygons(data = map_layer_data$buildings, color = NA, fillColor = "black", fillOpacity = 0.9, popup = "Building") %>%
        addPolygons(data = map_layer_data$sunlight, color = NA, fillColor = "yellow", fillOpacity = 0.4, popup = "Sunlight") %>%
        addPolygons(data = map_layer_data$shadows, color = NA, fillColor = "gray", fillOpacity = 0.4, popup = "Shadow")
    })
  }

  # Start app
  shinyApp(ui, server)
}




#' Create a time control HTML element for the map
#'
#' @param time POSIXct object representing the time
#' @return character string containing HTML for the time control
#' @export
create_time_control <- function(time) {
  formatted_date <- format(time, "%Y-%m-%d")
  formatted_time <- format(time, "%H:%M")
  time_zone <- format(time, "%Z")

  sprintf(
    '<div style="
      background-color: white;
      padding: 8px;
      border-radius: 4px;
      box-shadow: 0 2px 5px rgba(0,0,0,0.2);
      font-family: Arial, sans-serif;
      font-size: 14px;
      margin: 10px;
    ">
      <strong>Shadow Forecast</strong><br>
      Date: %s<br>
      Time: %s %s
    </div>',
    formatted_date,
    formatted_time,
    time_zone
  )
}





#' Create a map showing buildings, shadows, and sunlight areas
#'
#' @param buildings_union sf object containing the merged building polygons
#' @param shadows sf object containing shadow polygons
#' @param sunlight sf object containing sunlight areas
#' @param time POSIXct object representing the time
#' @importFrom leaflet setView
#' @return leaflet map object
#' @export
create_shadow_map <- function(buildings_union, shadows, sunlight, time) {
  # Transform to WGS84
  buildings_wgs84 <- st_transform(buildings_union, 4326)
  shadows_wgs84 <- st_transform(shadows, 4326)
  sunlight_wgs84 <- st_transform(sunlight, 4326)

  leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(
      data = sunlight_wgs84,
      fillColor = "#FFFF80",
      fillOpacity = 0.3,
      weight = 0,
      color=NA,
      group = "Sunlight"
    ) %>%
    addPolygons(
      data = shadows_wgs84,
      fillColor = "grey",
      fillOpacity = 0.3,
      weight = 0,
      color= NA,
      group = "Shadows"
    ) %>%
    addPolygons(
      data = buildings_wgs84,
      fillColor = "black",
      fillOpacity = 1,
      weight = 1,
      color = NA,
      group = "Buildings"
    ) %>%
    addLayersControl(
      overlayGroups = c("Sunlight", "Buildings", "Shadows"),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    addControl(
      html = create_time_control(time),
      position = "bottomright"
    ) %>%
    setView(
      lng = mean(st_coordinates(st_centroid(buildings_wgs84))[,1]),
      lat = mean(st_coordinates(st_centroid(buildings_wgs84))[,2]),
      zoom = 17
    )
}




#' Main function to process buildings and create an interactive shadow map
#'
#' This function processes building geometries, calculates shadow polygons for each building,
#' and creates an interactive shadow map using the Leaflet package. The process involves calculating
#' the convex hulls of building footprints and their respective shadow geometries, then dissolving
#' the individual shadow polygons. Sunlight areas are then computed, and finally, the shadow map is
#' generated based on the given time.
#'
#' The process is executed in batches to manage memory usage, and each stage of the process is timed
#' to provide performance feedback.
#'
#' @param buildings A simple features (sf) object containing building geometries, where:
#'   - The `geometry` column contains the building footprints (as `sfc` objects).
#'   - The `shadow_geometry` column contains the shadow geometries associated with each building.
#' @param time A POSIXct object representing the specific time at which the shadow calculation is to be
#'   performed. This time is used to adjust the shadow angles and determine shadow positions.
#' @param batch_size An integer specifying the number of buildings to process in each batch. Default is 50.
#'
#' @return A `leaflet` map object that visualizes the building shadows and sunlight areas, allowing the user
#'   to interactively explore the shadows based on the provided time.
#'
#' @details
#' The function sequentially performs the following steps:
#' 1. Processes building geometries.
#' 2. Computes the convex hulls of the union of each building's footprint and shadow geometry.
#' 3. Dissolves the individual shadow polygons to form a consolidated shadow area.
#' 4. Creates sunlight areas based on the processed buildings and shadow geometries.
#' 5. Generates an interactive Leaflet map displaying the calculated shadows and sunlight areas.
#'
#' During each stage, the function logs the processing time for transparency and performance monitoring.
#' If any stage fails (e.g., no valid shadow polygons are created), an error is thrown with a descriptive message.
#'
#' @importFrom leaflet leaflet addTiles addPolygons setView
#' @importFrom sf st_sf st_union
#' @export
#'
#' @examples
#' # Example of using the create_building_shadow_map function
#' # Assuming 'buildings_sf' is an existing sf object containing building and shadow geometries
#' shadow_map <- create_building_shadow_map(buildings_sf, time = Sys.time())
#'
#' # Display the shadow map
#' shadow_map
create_building_shadow_map <- function(buildings, time, batch_size = 50) {

  # Step 1: Process building geometries
  start_time <- Sys.time()
  processed_buildings <- process_buildings(buildings)
  processing_duration <- Sys.time() - start_time
  message("Time to process buildings: ", round(processing_duration, 2), " seconds")

  # Validate processed buildings
  if (is.null(processed_buildings) || nrow(processed_buildings) == 0) {
    stop("Error: No processed buildings available.")
  }

  # Step 2: Compute shadow hulls for each building
  individual_shadows <- compute_shadow_hulls(processed_buildings, batch_size)

  # Step 3: Dissolve individual shadow polygons into a consolidated shadow area
  shadows <- dissolve_shadow_polygons(individual_shadows)

  # Validate shadow polygons
  if (is.null(shadows) || length(shadows) == 0) {
    stop("Error: No shadow polygons created.")
  }

  # Step 4: Create sunlight areas based on processed buildings and shadow polygons
  message("Creating sunlight areas...")
  start_sunlight_creation <- Sys.time()
  sunlight <- create_sunlight_areas(processed_buildings, shadows)
  sunlight_creation_duration <- Sys.time() - start_sunlight_creation
  message("Time to create sunlight areas: ", round(sunlight_creation_duration, 2), " seconds")

  # Validate sunlight areas
  if (is.null(sunlight) || length(sunlight) == 0) {
    stop("Error: No sunlight areas created.")
  }

  # Step 5: Generate the interactive shadow map
  message("Creating shadow map...")
  start_map_creation <- Sys.time()
  map <- create_shadow_map(processed_buildings, shadows, sunlight, time)
  map_creation_duration <- Sys.time() - start_map_creation
  message("Time to create shadow map: ", round(map_creation_duration, 2), " seconds")

  # Validate map creation
  if (is.null(map)) {
    stop("Error: Shadow map not created.")
  }

  # Return the generated map
  return(map)
}
