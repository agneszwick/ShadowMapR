#' Visualize Building Geometries with Leaflet
#'
#' This function generates an interactive map using the `leaflet` package to visualize building geometries. Each building is represented by polygons, with colors indicating building heights. The map provides a popup showing the building ID when a user clicks on a building.
#'
#' @param building_sf sf object containing building geometries. This should be an `sf` object with columns for `height` (building height) and `part_id` (building ID or any other unique identifier for buildings).
#'
#' @importFrom leaflet leaflet addTiles addPolygons colorQuantile
#' @importFrom sf st_transform
#'
#' @return A `leaflet` map object displaying building geometries with colors based on height and popups showing building IDs.
#'
#' @details
#' The `visualize_buildings` function visualizes the spatial distribution of buildings with varying heights. The buildings are colored according to their height using a color palette from the `YlOrRd` scale. The map also includes a popup for each building that displays its `part_id` value (which could be used to uniquely identify each building).
#'
#' @examples
#' \dontrun{
#' # Assuming you have an sf object with building geometries, height, and part_id.
#' visualize_buildings(building_sf)
#' }
#'
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


#' Create a Map Visualizing Buildings, Shadows, and Sunlight Areas
#'
#' This function generates an interactive map using the `leaflet` package to visualize buildings, shadows, and sunlight areas.
#' The map allows users to toggle between different layers for buildings, shadows, and sunlight areas.
#' The function also adds a time control in the bottom-right corner of the map, which could be used for dynamic time-based visualizations.
#'
#' @param building_sf sf object containing merged building polygons (e.g., `sf` object with the geometry of buildings).
#' @param shadows sf object containing shadow polygons (e.g., `sf` object with the geometry representing shadow areas).
#' @param sunlight sf object containing sunlight polygons (e.g., `sf` object with the geometry representing areas under sunlight).
#' @param time POSIXct object representing the time of day when the map is generated (this could be used for dynamic updates or time-based visualization controls).
#'
#' @importFrom leaflet setView addProviderTiles addPolygons addLayersControl addControl
#' @importFrom sf st_transform st_coordinates st_centroid st_make_valid st_simplify
#'
#' @return A `leaflet` map object with layers for buildings, shadows, and sunlight, along with a time control at the bottom-right.
#'
#' @details
#' The `create_shadow_map` function provides a visual representation of buildings, shadows, and sunlight areas in a geographic region.
#' It allows users to interact with the map by toggling layers and exploring the spatial relationships between buildings and their shadows under different sunlight conditions.
#' The map is initialized at the centroid of the building polygons.
#'
#' @examples
#' \dontrun{
#' # Assuming you have `sf` objects: building_sf, shadows, sunlight, and a time object.
#' create_shadow_map(building_sf, shadows, sunlight, Sys.time())
#' }
#'
#' @export
create_shadow_map <- function(building_sf, shadows, sunlight, time) {

  # Ensure all geometries are valid and simplified
  building_sf <- st_make_valid(building_sf)
  building_sf <- st_simplify(building_sf, dTolerance = 0.0001)

  shadows <- st_make_valid(shadows)
  shadows <- st_simplify(shadows, dTolerance = 0.0001)

  sunlight <- st_make_valid(sunlight)
  sunlight <- st_simplify(sunlight, dTolerance = 0.0001)

  # Disable S2 processing
  sf::sf_use_s2(FALSE)

  # Transform to WGS84
  buildings_union <- st_union(building_sf)
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
      color = NA,
      group = "Sunlight"
    ) %>%
    addPolygons(
      data = shadows_wgs84,
      fillColor = "grey",
      fillOpacity = 0.3,
      weight = 0,
      color = NA,
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
      lng = mean(st_coordinates(st_centroid(buildings_wgs84))[, 1]),
      lat = mean(st_coordinates(st_centroid(buildings_wgs84))[, 2]),
      zoom = 16
    )
}
