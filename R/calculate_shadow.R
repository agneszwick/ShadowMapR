#' Calculate the shadow length based on building height and solar elevation angle
#' 
#' @param measuredHeight Numeric value representing the measured height of the building (in meters)
#' @param solar_elevation_angle Numeric value representing the solar elevation angle (in degrees)
#' @return Numeric value representing the shadow length (in meters)
calculate_shadow_length <- function(measuredHeight, solar_elevation_angle) {
  # Convert the solar elevation angle from degrees to radians
  solar_elevation_radians <- solar_elevation_angle * (pi / 180)

  # Calculate the shadow length using the formula
  h <- measuredHeight / tan(solar_elevation_radians)

  return(h)
}

#' Calculate the shadow points of a building based on solar position and shadow length
#' 
#' @param building sf object containing the geometry of a building (POLYGON geometry)
#' @param solar_pos Numeric vector of length 2, where the first element is the azimuth angle and the second element is the elevation angle of the sun (in degrees)
#' @param h Numeric value representing the shadow length (in meters), calculated based on the building's height and solar elevation angle
#' @return sf object containing the shadow polygon geometry of the building
calculate_shadow_points <- function(building, solar_pos, h) {
  # Extract the geometry of the building
  if (!inherits(building$geometry, "sfc_POLYGON")) {
    stop("The building geometry is not a valid POLYGON object.")
  }

  # Extract points from the geometry
  points <- st_coordinates(building$geometry)
  # Get unique points (remove duplicates and Z coordinates if present)
  points <- unique(points[, 1:2])

  # Initialize an empty list to store shadow points
  shadow_points_list <- list()

  # Loop through all points in the polygon and calculate the shadow
  for (i in 1:nrow(points)) {
    # Current point coordinates
    point <- points[i, ]

    # Calculate the sun vector (length = h, angle = solar_pos[1])
    azimuth_radians <- (solar_pos[1] - 180) * (pi / 180)  # Correct azimuth
    sun_vector <- c(h * cos(azimuth_radians), h * sin(azimuth_radians))

    # Calculate the shadow point by subtracting the sun vector from the point
    shadow_point <- point - sun_vector

    # Add the calculated shadow point to the list
    shadow_points_list[[i]] <- shadow_point
  }

  # Convert the list of shadow points to a matrix and close the polygon
  shadow_matrix <- do.call(rbind, shadow_points_list)
  shadow_matrix <- rbind(shadow_matrix, shadow_matrix[1,])  # Close the polygon

  # Convert to polygon geometry
  shadow_polygon <- st_polygon(list(shadow_matrix))

  return(st_sfc(shadow_polygon, crs = st_crs(building)))
}

#' Calculate shadow geometry for each building
#' 
#' @param buildings sf object containing building geometries and attributes such as height, solar azimuth, and solar elevation
#' @return sf object containing buildings with an additional column `shadow_geometry` representing the calculated shadow polygons for each building

calculate_all_shadows <- function(buildings) {
  # Compute shadow geometry for each building
  shadow_geometries <- lapply(1:nrow(buildings), function(i) {
    current_building <- buildings[i, ]

    # Extract solar azimuth, elevation, and building height
    solar_azimuth <- current_building$sol_azimuth
    solar_elevation <- current_building$sol_elevation
    measured_height <- current_building$height

    # Step 1: Calculate shadow length
    if (solar_elevation <= 0) {
      return(NULL)  # No shadow during sunset/night
    }
    h <- calculate_shadow_length(measured_height, solar_elevation)

    # Step 2: Calculate shadow points and return the shadow geometry
    tryCatch(
      calculate_shadow_points(current_building, c(solar_azimuth, solar_elevation), h),
      error = function(e) {
        message(sprintf("Error calculating shadow for building %s: %s", i, e$message))
        return(NULL)
      }
    )
  })

  # Convert the shadow geometries list into a single sfc column
  buildings$shadow_geometry <- st_sfc(do.call(c, shadow_geometries), crs = st_crs(buildings))

  return(buildings)
}

# # APPLY FUNCTIONS
# # Calculate shadows for all buildings
# updated_buildings_with_shadows <- calculate_all_shadows(building_sf)
# 
# # Output the first buildings with shadow geometry
# print(head(updated_buildings_with_shadows))
# 
# # Optional: Visualization of buildings with their shadows
# plot(st_geometry(updated_buildings_with_shadows), main = "Buildings with Shadows")
# plot(st_geometry(do.call(c, updated_buildings_with_shadows$shadow_geometry)), add = TRUE, col = "red", lwd = 2)
