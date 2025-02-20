#' Process buildings and create shadow geometries
#'
#' This function processes building geometries by checking for invalid geometries
#' and filtering empty ones. Invalid geometries are corrected using `st_make_valid`
#' and empty geometries are removed.
#'
#' @param buildings sf object containing building geometries with a geometry column
#' @importFrom sf st_make_valid st_is_empty st_geometry
#' @importFrom dplyr mutate filter
#' @return sf object containing processed buildings
#' @export
process_buildings <- function(buildings) {

  # Überprüfe, ob ungültige oder leere Geometrien vorhanden sind
  invalid_geom <- st_is_valid(st_geometry(buildings)) == FALSE
  empty_geom <- st_is_empty(st_geometry(buildings)) == TRUE

  message(sum(invalid_geom), " invalid geometries detected.")
  message(sum(empty_geom), " empty geometries detected.")

  # Falls ungültige oder leere Geometrien existieren, validiere und filtere nur diese
  if (sum(invalid_geom) > 0) {
    # message("Validating invalid geometries...")
    buildings[invalid_geom, "geometry"] <- st_make_valid(st_geometry(buildings[invalid_geom, ]))
  }

  # Filtere leere Geometrien
  if (sum(empty_geom) > 0) {
    # message("Filtering empty geometries...")
    buildings <- buildings[!empty_geom, ]
  }

  return(buildings)
}


#' Create shadow polygons from buildings
#'
#' @param buildings sf object containing processed buildings
#' @param batch_size integer number of buildings to process in each batch
#' @return sf object containing dissolved shadow polygons
#' @export
create_shadow_polygons <- function(buildings, batch_size) {
  message("Creating shadow polygons...")

  # Vorvalidierung aller Geometrien (falls noch nicht erfolgt)
  buildings$geometry <- st_make_valid(buildings$geometry)
  buildings$shadow_geometry <- st_make_valid(buildings$shadow_geometry)

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
      tryCatch({
        # Direkter Zugriff auf die Geometrien, da diese bereits validiert wurden
        bldg_geom <- buildings$geometry[i]
        shadow_geom <- buildings$shadow_geometry[i]

        # Vereinigung der Geometrien und Berechnung der konvexen Hülle in einem Schritt
        combined_geom <- st_union(bldg_geom, shadow_geom)
        hull <- st_convex_hull(combined_geom)

        # Speichern, falls das Ergebnis gültig und nicht leer ist
        if (st_is_valid(hull) && !st_is_empty(hull)) {
          valid_count <- valid_count + 1
          final_polygons[[valid_count]] <- hull
          final_part_ids[valid_count] <- current_part_id
        } else {
          message("Invalid or empty hull created for building ", current_part_id)
        }
      }, error = function(e) {
        message(sprintf("Error processing building %s: %s", current_part_id, e$message))
      })
    }
  }

  if (valid_count > 0) {
    final_polygons <- final_polygons[1:valid_count]
    final_part_ids <- final_part_ids[1:valid_count]

    # Erstellen des sf-Objekts und Zusammenführen der Polygone
    shadow_polygons <- st_sf(
      part_id = final_part_ids,
      geometry = st_sfc(do.call(c, final_polygons), crs = st_crs(buildings))
    ) %>%
      summarise(geometry = st_union(geometry))

    return(shadow_polygons)
  } else {
    stop("No valid shadow polygons were created.")
  }
}


#' Create sunlight areas by subtracting buildings and shadows from a bounding box
#'
#' @param buildings sf object containing building polygons
#' @param shadows sf object containing shadow polygons
#' @return sf object containing sunlight areas
#' @export
create_sunlight_areas <- function(buildings, shadows) {
  bbox <- st_as_sfc(st_bbox(buildings))  # Convert bounding box to polygon

  buildings_union <- st_union(buildings)  # Merge all buildings into one geometry
  shadows_union <- st_union(shadows)  # Merge all shadows into one geometry

  st_difference(bbox, st_union(buildings_union, shadows_union))  # Subtract buildings & shadows
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
#


#' Main function to process buildings and create an interactive shadow map
#'
#' @param buildings sf object containing building geometries with shadow_geometry column
#' @param time POSIXct object representing the time for shadow calculation
#' @param batch_size integer number of buildings to process in each batch
#' @return leaflet map object
#' @export
create_building_shadow_map <- function(buildings, time, batch_size = 50) {

  # Verarbeite Gebäude
  start_time <- Sys.time()
  processed_buildings <- process_buildings(buildings)
  processing_duration <- Sys.time() - start_time
  message("Time to process buildings: ", round(processing_duration, 2), " seconds")


  # Überprüfe, ob das processed_buildings korrekt ist
  if (is.null(processed_buildings) || nrow(processed_buildings) == 0) {
    stop("Error: No processed buildings available.")
  }

  # Erstelle Schattengrafiken
  shadows <- create_shadow_polygons(processed_buildings, batch_size)

  # Überprüfe, ob shadows korrekt sind
  if (is.null(shadows) || length(shadows) == 0) {
    stop("Error: No shadow polygons created.")
  }

  # Erstelle Sonnenbereiche
  message("Creating sunlight areas...")
  start_sunlight_creation <- Sys.time()
  sunlight <- create_sunlight_areas(processed_buildings, shadows)
  sunlight_creation_duration <- Sys.time() - start_sunlight_creation
  message("Time to create sunlight areas: ", round(sunlight_creation_duration, 2), " seconds")


  # Überprüfe, ob sunlight korrekt erstellt wurde
  if (is.null(sunlight) || length(sunlight) == 0) {
    stop("Error: No sunlight areas created.")
  }

  # Erstelle und gebe die Karte zurück
  message("Creating shadow map...")
  start_map_creation <- Sys.time()
  map <- create_shadow_map(processed_buildings, shadows, sunlight, time)
  map_creation_duration <- Sys.time() - start_map_creation
  message("Time to create shadow map: ", round(map_creation_duration, 2), " seconds")

  # Überprüfe, ob die Karte korrekt erstellt wurde
  if (is.null(map)) {
    stop("Error: Shadow map not created.")
  }

  return(map)
}
