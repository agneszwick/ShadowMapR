#' Calculate the shadow length based on building height and solar elevation angle
#'
#' This function calculates the shadow length of a building based on its height and the solar elevation angle.
#' The shadow length is calculated using the formula: shadow_length = height / tan(solar_elevation_angle).
#'
#' @param measuredHeight Numeric value representing the measured height of the building (in meters).
#' @param solar_elevation_angle Numeric value representing the solar elevation angle (in degrees).
#' @return Numeric value representing the shadow length (in meters).
#' @examples
#' \dontrun{
#' # Example usage
#' height <- 10
#' solar_elevation <- 45
#' shadow_length <- calculate_shadow_length(height, solar_elevation)
#' print(shadow_length)
#' }
calculate_shadow_length <- function(measuredHeight, solar_elevation_angle) {
  # Convert the solar elevation angle from degrees to radians
  solar_elevation_radians <- solar_elevation_angle * (pi / 180)

  # Calculate the shadow length using the formula
  h <- measuredHeight / tan(solar_elevation_radians)

  return(h)
}

#' Calculate the Shadow Points of a Building Based on Solar Position and Shadow Length
#'
#' This function calculates the shadow points of a building based on the solar position (azimuth and elevation)
#' and the shadow length. It takes an `sf` object containing the geometry of a building (POLYGON geometry),
#' the solar position, and the shadow length, and returns an `sf` object containing the shadow polygon geometry.
#' The function works by shifting the outline of the building in the direction of the shadow to create the shadow's corner points.
#'
#' @param building sf object containing the geometry of a building (POLYGON geometry).
#' @param solar_pos Numeric vector of length 2, where the first element is the solar azimuth (in degrees)
#'                  and the second element is the solar elevation (in degrees).
#' @param shadow_length Numeric value representing the shadow length (in meters), calculated based on the building's height and solar elevation angle.
#' @importFrom sf st_polygon st_sfc st_coordinates st_crs
#' @return sf object containing the shadow polygon geometry of the building.
#' @examples
#' \dontrun{
#' # Example usage
#' library(sf)
#'
#' # Create example building geometry
#' building <- st_sf(
#'   geometry = st_sfc(
#'     st_polygon(list(matrix(c(0, 0, 1, 1, 1, 0, 0, 0), ncol = 2, byrow = TRUE)))
#'   ),
#'   crs = 4326
#' )
#'
#' # Define solar position and shadow length
#' solar_pos <- c(180, 45)
#' shadow_length <- 10
#'
#' # Calculate shadow points
#' shadow_polygon <- calculate_shadow_points(building, solar_pos, shadow_length)
#' print(shadow_polygon)
#' }
calculate_shadow_points <- function(building, solar_pos, shadow_length) {
  # Check if the building geometry is valid
  if (!inherits(building$geometry, "sfc_POLYGON")) {
    stop("The building geometry is not a valid POLYGON object.")
  }

  # Extract solar azimuth and elevation from solar_pos
  sol_azimuth <- solar_pos[1]
  sol_elevation <- solar_pos[2]

  # Calculate the shadow azimuth
  shadow_azimuth <- (sol_azimuth + 180) %% 360

  # Convert azimuth to radians
  azimuth_radians_shadow <- shadow_azimuth * (pi / 180)

  # Extract building coordinates and remove duplicates
  points <- st_coordinates(building$geometry)
  points <- unique(points[, 1:2])

  # Initialize a list for shadow points
  shadow_points_list <- list()

  # Calculate shadow points for each building corner
  for (i in 1:nrow(points)) {
    point <- points[i, ]

    # Calculate shadow vector
    shadow_vector_x <- shadow_length * sin(azimuth_radians_shadow)
    shadow_vector_y <- shadow_length * cos(azimuth_radians_shadow)

    # Calculate shadow point
    shadow_point <- point + c(shadow_vector_x, shadow_vector_y)

    # Add shadow point to the list
    shadow_points_list[[i]] <- shadow_point
  }

  # Convert shadow points list to a matrix and close the polygon
  shadow_matrix <- do.call(rbind, shadow_points_list)
  shadow_matrix <- rbind(shadow_matrix, shadow_matrix[1,])

  # Create shadow polygon from the matrix
  shadow_polygon <- st_polygon(list(shadow_matrix))

  return(st_sfc(shadow_polygon, crs = st_crs(building)))
}


#' Calculate Shadow Geometry for Each Building
#'
#' This function calculates the shadow geometry for each building in an `sf` object based on the building's height,
#' solar azimuth, and solar elevation. It returns an `sf` object containing the buildings with an additional column
#' `shadow_geometry` representing the calculated shadow polygons for each building.
#'
#' @param buildings sf object containing building geometries and attributes such as height, solar azimuth, and solar elevation.
#' @return sf object containing buildings with an additional column `shadow_geometry` representing the calculated shadow polygons for each building.
#' @importFrom sf st_sfc st_crs
#' @examples
#' \dontrun{
#' # Example usage
#' library(sf)
#'
#' # Create example buildings with attributes
#' buildings <- st_sf(
#'   height = c(10, 15),
#'   sol_azimuth = c(180, 200),
#'   sol_elevation = c(45, 30),
#'   geometry = st_sfc(
#'     st_polygon(list(matrix(c(0, 0, 1, 1, 1, 0, 0, 0), ncol = 2, byrow = TRUE))),
#'     st_polygon(list(matrix(c(1, 1, 2, 2, 2, 1, 1, 1), ncol = 2, byrow = TRUE)))
#'   ),
#'   crs = 4326
#' )
#'
#' # Calculate shadow geometry for each building
#' buildings_with_shadows <- calculate_all_shadows(buildings)
#' print(buildings_with_shadows)
#' }
calculate_all_shadows <- function(buildings) {
  # Check if the necessary columns are present
  required_columns <- c("height", "sol_azimuth", "sol_elevation")
  missing_columns <- setdiff(required_columns, names(buildings))
  if (length(missing_columns) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing_columns, collapse = ", ")))
  }

  # Compute shadow geometry for each building
  shadow_geometries <- lapply(1:nrow(buildings), function(i) {
    current_building <- buildings[i, ]

    # Extract solar position and building height
    solar_azimuth <- current_building$sol_azimuth
    solar_elevation <- current_building$sol_elevation
    measured_height <- current_building$height

    # Calculate shadow length
    if (solar_elevation <= 0) {
      return(NULL)
    }
    shadow_length <- calculate_shadow_length(measured_height, solar_elevation)

    # Calculate shadow points and return shadow geometry
    tryCatch(
      calculate_shadow_points(current_building, c(solar_azimuth, solar_elevation), shadow_length),
      error = function(e) {
        message(sprintf("Error calculating shadow for building %s: %s", i, e$message))
        return(NULL)
      }
    )
  })

  # Convert the list of shadow geometries to a single sfc column
  buildings$shadow_geometry <- st_sfc(do.call(c, shadow_geometries), crs = st_crs(buildings))

  return(buildings)
}
