#' Process buildings and create shadow geometries
#' @param buildings sf object containing building geometries with a geometry column
#' @return sf object containing processed buildings
#' @export
process_buildings <- function(buildings) {
  message("Processing buildings...")

  processed_buildings <- buildings %>%
    mutate(
      geometry = st_make_valid(geometry)
    ) %>%
    filter(
      !st_is_empty(geometry)
    )

  return(processed_buildings)
}


#' Create shadow polygons from buildings
#'
#' @param buildings sf object containing processed buildings
#' @param batch_size integer number of buildings to process in each batch
#' @return sf object containing dissolved shadow polygons
#' @export
create_shadow_polygons <- function(buildings, batch_size = 50) {

  message("Creating shadow polygons...")
  final_polygons <- vector("list", nrow(buildings))
  final_part_ids <- vector("character", nrow(buildings))
  valid_count <- 0

  # Batchweise Verarbeitung
  for (batch_start in seq(1, nrow(buildings), by = batch_size)) {
    batch_end <- min(batch_start + batch_size - 1, nrow(buildings))
    message(sprintf("Processing batch %d to %d", batch_start, batch_end))

    # Verarbeitung jedes Gebäudes im Batch
    for (i in batch_start:batch_end) {
      current_part_id <- buildings$part_id[i]
      message(sprintf("Processing building %d (%s)", i, current_part_id))

      tryCatch({
        bldg_geom <- st_geometry(buildings$geometry[i])
        shadow_geom <- st_geometry(buildings$shadow_geometry[i])

        # Prüfe und repariere Geometrien
        if (!st_is_valid(bldg_geom) || !st_is_valid(shadow_geom)) {
          message("Invalid geometry detected, attempting to fix...")
          bldg_geom <- st_make_valid(bldg_geom)
          shadow_geom <- st_make_valid(shadow_geom)
        }

        # Vereinige Geometrien und berechne konvexe Hülle
        combined_geom <- st_union(c(bldg_geom, shadow_geom))
        hull <- st_convex_hull(combined_geom)

        # Validierung und Speicherung des Polygons
        if (st_is_valid(hull) && !st_is_empty(hull)) {
          valid_count <- valid_count + 1
          final_polygons[[valid_count]] <- hull
          final_part_ids[valid_count] <- current_part_id
          message("Successfully processed")
        } else {
          message("Invalid or empty hull created")
        }
      }, error = function(e) {
        message(sprintf("Error processing building %s: %s", current_part_id, e$message))
      })
    }
  }

  message(sprintf("Successfully processed %d buildings", valid_count))

  if (valid_count > 0) {
    final_polygons <- final_polygons[1:valid_count]
    final_part_ids <- final_part_ids[1:valid_count]

    # Erstellen des sf-Objekts und Auflösen der Polygone
    shadow_polygons <- st_sf(
      part_id = final_part_ids,
      geometry = st_sfc(do.call(c, final_polygons),
                        crs = st_crs(buildings))
    ) %>%
      summarise(geometry = st_union(geometry))

    return(shadow_polygons)
  } else {
    stop("No valid shadow polygons were created.")
  }
}


#' Create sunlight areas by subtracting buildings and shadows from a bounding box
#'
#' @param buildings sf object containing buildings
#' @param shadows sf object containing shadow polygons
#' @return sf object containing sunlight areas
#' @export
create_sunlight_areas <- function(buildings, shadows) {
  bbox <- st_bbox(buildings)
  bbox_poly <- st_as_sfc(bbox)

  buildings_union <- st_union(buildings$geometry)
  st_difference(bbox_poly, st_union(buildings_union, shadows$geometry))
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

#' Create an interactive map showing buildings, shadows, and sunlight areas
#'
#' @param buildings sf object containing buildings
#' @param shadows sf object containing shadow polygons
#' @param sunlight sf object containing sunlight areas
#' @param time POSIXct object representing the time
#' @return leaflet map object
#' @export
create_shadow_map <- function(buildings, shadows, sunlight, time) {
  # Transform to WGS84
  buildings_wgs84 <- st_transform(buildings, 4326)
  shadows_wgs84 <- st_transform(shadows, 4326)
  sunlight_wgs84 <- st_transform(sunlight, 4326)

  leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(
      data = sunlight_wgs84,
      fillColor = "#FFFF80",
      fillOpacity = 0.3,
      weight = 0,
      group = "Sunlight"
    ) %>%
    addPolygons(
      data = shadows_wgs84,
      fillColor = "grey",
      fillOpacity = 0.3,
      weight = 0,
      group = "Shadows"
    ) %>%
    addPolygons(
      data = buildings_wgs84,
      fillColor = "black",
      fillOpacity = 1,
      weight = 1,
      color = "white",
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
#' @param buildings sf object containing building geometries with shadow_geometry column
#' @param time POSIXct object representing the time for shadow calculation
#' @param batch_size integer number of buildings to process in each batch
#' @param cache logical whether to cache intermediate results
#' @return leaflet map object
#' @export
create_building_shadow_map <- function(buildings, time, batch_size = 50, cache = TRUE) {
  # Process buildings
  processed_buildings <- process_buildings(buildings, batch_size, cache)

  # Create shadow polygons
  shadows <- create_shadow_polygons(processed_buildings, batch_size, cache)

  # Create sunlight areas
  sunlight <- create_sunlight_areas(processed_buildings, shadows)

  # Create and return the map
  create_shadow_map(processed_buildings, shadows, sunlight, time)
}

# # Set the date and time for the shadow calculation
# time <- as.POSIXct(paste("2025-02-02", "10:00"), format = "%Y-%m-%d %H:%M")
#
# # Create and display the map
# map <- create_building_shadow_map(updated_buildings_with_shadows, time)
# print(map)  # This will display the map in your R viewer/browser
