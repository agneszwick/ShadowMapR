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

