#' Calculate Solar Position for a Full Day
#'
#' @param building_sf sf object containing building geometries
#' @param date Date object specifying the day for calculations
#' @return List of sf objects containing solar positions for each hour
#' @export
calculate_solar_day <- function(building_sf, date) {
  results <- list()
  for (hour in 0:23) {
    time <- as.POSIXct(paste(date, sprintf("%02d:00:00", hour)), tz = "UTC")
    solar_data <- calculate_solar(building_sf, time)
    if (any(solar_data$sol_elevation > 0)) {
      label <- sprintf("%s - %02d:00", date, hour)
      results[[label]] <- solar_data[solar_data$sol_elevation > 0, ]
    }
  }
  return(results)
}

#' Calculate hourly shadows
#' @param solar_results A list containing hourly solar data.
#' @return A list of calculated shadows for each hour.
#' @export
calculate_hourly_shadows <- function(solar_results) {
  # Apply the calculate_all_shadows function to each hourly solar data
  lapply(solar_results, function(hourly_data) {
    # Ensure that calculate_all_shadows is defined and works on the given data
    calculate_all_shadows(hourly_data)
  })
}

#' Create hourly shadow polygons
#'
#' @param shadows A list of shadows for each hour.
#' @param batch_size integer number of buildings to process in each batch (default = 50)
#' @return A named list of shadow polygons for each hour.
#' @export
create_hourly_shadow_polygons <- function(shadows, batch_size = 50) {
  setNames(
    lapply(names(shadows), function(hour) {
      processed_data <- process_buildings(shadows[[hour]])
      create_shadow_polygons(processed_data, batch_size)
    }),
    names(shadows)
  )
}


#' Create hourly sunlight areas
#' @param shadow_polygons A named list of shadow polygons for each hour.
#' @param building_sf An sf object containing building geometries.
#' @return A named list of sunlight areas for each hour.
#' @export
create_hourly_sunlight_areas <- function(shadow_polygons, building_sf) {
  setNames(
    lapply(names(shadow_polygons), function(hour) {
      create_sunlight_areas(shadow_polygons[[hour]], building_sf)
    }),
    names(shadow_polygons)
  )
}


#' Precompute map layers for each hour
#'
#' This function precomputes the final map layers for each hour. For each hour, it:
#'   - Calculates shadow polygons and sunlight areas,
#'   - Unions all building geometries
#'
#' @param solar_result A list of solar results with names corresponding to hours (e.g., "2025-02-20 - 07:00").
#' @param building_sf An sf object containing building geometries.
#' @param batch_size Integer, the number of buildings to process in each batch.
#'
#' @return A named list of lists. Each inner list contains three sf objects:
#'   buildings, shadows, and sunlight.
#' @export
precompute_map_layers <- function(solar_result, building_sf, batch_size = 50) {
  # Step 1: Calculate hourly shadow offsets
  hourly_shadow_offset <- calculate_hourly_shadows(solar_result)

  # Step 2: Create hourly shadow polygons from offsets
  hourly_shadows <- create_hourly_shadow_polygons(hourly_shadow_offset, batch_size)

  # Step 4: Create hourly sunlight areas
  hourly_sunareas <- create_hourly_sunlight_areas(hourly_shadows, building_sf)

  #Step x:
  building_sf <- st_union(building_sf)

  # Step x: Prepare the final map layers for each hour
  final_map_layers <- list()

  # Iterate over each hour and prepare the layers for that hour
  for (hour in names(hourly_shadows)) {
    # Get the corresponding polygons for this hour
    shadow_data <- hourly_shadows[[hour]]
    sunlight_data <- hourly_sunareas[[hour]]
    building_data <- building_sf  # The buildings are the same for all hours, use transformed building data

    # Create the map layer for the current hour
    final_map_layers[[hour]] <- list(
      buildings = building_data,
      shadows = shadow_data,
      sunlight = sunlight_data
    )
  }

  # Return the final map layers
  return(final_map_layers)
}


#' Run Interactive Shadow Map Application
#'
#' This function starts a Shiny application to visualize shadow progression over a day.
#' It takes precomputed map layers containing buildings, shadows, and sunlight areas.
#'
#' @param map_layer A named list of sf objects for each hour, containing `buildings`, `shadows`, and `sunlight` geometries in WGS84 (EPSG:4326).
#' @param date A character string representing the date (e.g., `"2025-02-18"`).
#'
#' @return Launches a Shiny interactive map application.
#' @export
run_interact_shadow_map <- function(map_layer, date) {
  # Extrahiere die Zeitlabels aus den Daten
  time_labels <- names(map_layer)

  # Extrahiere nur die Uhrzeit aus den Zeitlabels
  time_hours <- sub(".* - (.*)", "\\1", time_labels)

  # Berechnung des Mittelpunkts aller Gebäude
  all_buildings <- map_layer[[1]]$buildings
  center_coords <- st_coordinates(st_centroid(all_buildings))
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
        choices = time_hours,  # Zeige nur die Uhrzeiten im Slider
        selected = time_hours[1],  # Standardmäßig die erste Uhrzeit auswählen
        grid = TRUE,
        animate = animationOptions(interval = 2500, loop = TRUE)
      )
    )
  )

  # Shiny Server
  server <- function(input, output, session) {

    # Reaktive Funktion, die die Geometrien für die ausgewählte Zeit zurückgibt
    current_data <- reactive({
      time <- input$time  # Ausgewählte Zeit (nur Uhrzeit)

      # Finde den passenden Zeitstempel aus den Zeitlabels
      matched_time <- time_labels[which(time_hours == time)]

      # Überprüfen, ob die Zeit gefunden wurde
      if (length(matched_time) == 0) {
        return(NULL)  # Keine passenden Daten gefunden
      }

      # Hole die Daten für den gefundenen Zeitstempel
      data <- map_layer[[matched_time]]

      # Überprüfen, ob Daten vorhanden sind
      if (is.null(data)) {
        return(NULL)  # Keine gültigen Daten vorhanden
      }

      # Transformiere alle Geometrien von UTM (ETRS89) zu WGS84 (EPSG:4326)
      data$buildings <- st_transform(data$buildings, crs = 4326)
      data$sunlight <- st_transform(data$sunlight, crs = 4326)
      data$shadows <- st_transform(data$shadows, crs = 4326)

      return(data)  # Die transformierten Daten zurückgeben
    })

    # Karte rendern
    output$map <- renderLeaflet({
      map_layer_data <- current_data()  # Hole die aktuellen Geodaten basierend auf der Zeit

      # Überprüfe, ob map_layer_data NULL ist
      if (is.null(map_layer_data)) {
        return(leaflet() %>% addTiles())  # Wenn keine Daten vorhanden, gib leere Karte zurück
      }

      # Berechne den Mittelpunkt der Gebäude
      center_coords <- st_coordinates(st_centroid(map_layer_data$buildings))
      center_lng <- mean(center_coords[, 1])
      center_lat <- mean(center_coords[, 2])

      # Basiskarte erstellen mit CartoDB.Positron
      map <- leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%  # Verwenden von CartoDB.Positron
        setView(lng = center_lng, lat = center_lat, zoom = 16)

      # Gebäude, Sonnenlicht und Schatten hinzufügen
      map <- map %>%
        addPolygons(data = map_layer_data$buildings, color=NA, fillColor = "black", weight = 2, fillOpacity = 0.9, popup = "Building") %>%
        addPolygons(data = map_layer_data$sunlight, color=NA, fillColor = "yellow", weight = 2, fillOpacity = 0.4, popup = "Sunlight") %>%
        addPolygons(data = map_layer_data$shadows, color=NA, fillColor = "gray", weight = 2, fillOpacity = 0.4, popup = "Shadow")

      map
    })

    # Ausgabe der aktuell ausgewählten Zeit
    output$selected_time <- renderText({
      paste("Selected time:", input$time)
    })
  }

  # App starten
  shinyApp(ui, server)
}
