#' Calculate the Shadow Length Based on Building Height and Solar Elevation Angle
#'
#' This function calculates the shadow length of a building using its height and the solar elevation angle.
#' The formula used is: shadow_length = height / tan(solar_elevation_angle).
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
  # Convert solar elevation angle from degrees to radians
  solar_elevation_radians <- solar_elevation_angle * (pi / 180)

  # Compute shadow length
  h <- measuredHeight / tan(solar_elevation_radians)

  return(h)
}
#' Calculate the Shadow Polygon of a Building Based on Solar Position and Shadow Length
#'
#' This function calculates the shadow polygon of a building based on the solar position (azimuth and elevation)
#' and a given shadow length. The shadow is computed by applying a shadow vector to each corner of the building's
#' geometry. The shadow vector is determined by the solar azimuth, solar elevation, and shadow length, which
#' represents the distance the shadow extends from the building. This is useful for modeling the spatial extent of
#' shadows cast by buildings based on solar conditions.
#'
#' The function assumes that the building geometry is a valid polygon and that the shadow length has been
#' pre-calculated based on the building's height and solar elevation angle. The shadow polygon is constructed
#' by projecting the shadow vector from each vertex of the building geometry.
#'
#' @param building An `sf` object containing the geometry of a building, where the `geometry` column contains
#'   a `POLYGON` geometry representing the building's footprint.
#' @param solar_pos A numeric vector of length 2, where the first element is the solar azimuth (in degrees,
#'   relative to true north) and the second element is the solar elevation (in degrees, where 0 degrees is the
#'   horizon and positive values are above the horizon).
#' @param shadow_length A numeric value representing the length of the shadow (in meters). The shadow length
#'   is typically calculated based on the building's height and solar elevation angle, representing how far the
#'   shadow extends from the building.
#'
#' @return An `sf` object containing a single `POLYGON` geometry that represents the shadow of the building.
#'   The shadow polygon is the result of projecting shadow vectors from the building's corners based on the
#'   provided solar position and shadow length.
#'
#' @details
#' The shadow is calculated by computing a shadow vector for each corner of the building's geometry. The
#' shadow vector is derived from the solar azimuth and elevation, and the shadow length. The azimuth is adjusted
#' by adding 180 degrees to point in the opposite direction (away from the light source), and the result is used
#' to calculate the shadow's direction. Each corner of the building is extended by the shadow vector, and the
#' resulting points are connected to form a closed polygon, representing the shadow on the ground.
#'
#' The building geometry must be a valid `POLYGON` object for this function to work properly. The function also
#' assumes that the shadow length has been pre-calculated based on the building's height and solar angle.
#'
#' @importFrom sf st_polygon st_sfc st_coordinates st_crs
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' library(sf)
#'
#' # Create example building geometry (square)
#' building <- st_sf(
#'   geometry = st_sfc(
#'     st_polygon(list(matrix(c(0, 0, 1, 1, 1, 0, 0, 0), ncol = 2, byrow = TRUE)))
#'   ),
#'   crs = 4326
#' )
#'
#' # Define solar position (azimuth = 180°, elevation = 45°)
#' solar_pos <- c(180, 45)
#' shadow_length <- 10  # Example shadow length in meters
#'
#' # Calculate shadow polygon
#' shadow_polygon <- calculate_shadow_points(building, solar_pos, shadow_length)
#' plot(shadow_polygon)
#' }
#'
calculate_shadow_points <- function(building, solar_pos, shadow_length) {
  # Validate that the building geometry is a valid POLYGON
  if (!inherits(building$geometry, "sfc_POLYGON")) {
    stop("The building geometry must be a valid POLYGON object.")
  }

  # Extract solar azimuth and elevation
  sol_azimuth <- solar_pos[1]
  sol_elevation <- solar_pos[2]

  # Compute the shadow azimuth (opposite direction of the solar azimuth)
  shadow_azimuth <- (sol_azimuth + 180) %% 360

  # Convert azimuth to radians for calculation
  azimuth_radians_shadow <- shadow_azimuth * (pi / 180)

  # Extract building coordinates and remove duplicate points
  points <- st_coordinates(building$geometry)
  points <- unique(points[, 1:2])

  # Initialize a list to store the shadow points
  shadow_points_list <- list()

  # Compute shadow points by applying the shadow vector to each building corner
  for (i in 1:nrow(points)) {
    point <- points[i, ]

    # Calculate shadow vector components
    shadow_vector_x <- shadow_length * sin(azimuth_radians_shadow)
    shadow_vector_y <- shadow_length * cos(azimuth_radians_shadow)

    # Calculate the shadow point by applying the shadow vector
    shadow_point <- point + c(shadow_vector_x, shadow_vector_y)

    # Store the shadow point
    shadow_points_list[[i]] <- shadow_point
  }

  # Convert shadow points into a closed polygon
  shadow_matrix <- do.call(rbind, shadow_points_list)
  shadow_matrix <- rbind(shadow_matrix, shadow_matrix[1, ])  # Close the polygon

  # Create shadow polygon geometry
  shadow_polygon <- st_polygon(list(shadow_matrix))

  return(st_sfc(shadow_polygon, crs = st_crs(building)))
}


#' Compute the Shadow Offset for Each Building
#'
#' This function computes a shadow offset for each building in an `sf` object based on the building's height,
#' solar azimuth, and solar elevation. It applies a shadow vector of the correct length and angle to each corner
#' of the building, generating a new polygon that represents the shadow projection.
#'
#' @param building_sf An sf object containing building geometries and attributes such as height, solar azimuth, and solar elevation.
#' @return sf object containing buildings with an additional column `shadow_geometry` representing the calculated shadow polygons.
#' @importFrom sf st_sfc st_crs
#' @export
#' @examples
#' \dontrun{
#' library(sf)
#'
#' # Create example buildings with attributes
#' building_sf <- st_sf(
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
#' # Compute shadow geometry for each building
#' building_offset <- building_offset(building_sf)
#' print(building_offset)
#' }
building_offset <- function(building_sf) {
  # Check for required columns
  required_columns <- c("height", "sol_azimuth", "sol_elevation")
  missing_columns <- setdiff(required_columns, names(building_sf))
  if (length(missing_columns) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing_columns, collapse = ", ")))
  }

  # Compute shadow geometry for each building
  shadow_geometries <- lapply(1:nrow(building_sf), function(i) {
    current_building <- building_sf[i, ]

    # Extract solar position and building height
    solar_azimuth <- current_building$sol_azimuth
    solar_elevation <- current_building$sol_elevation
    measured_height <- current_building$height

    # Compute shadow length
    if (solar_elevation <= 0) {
      return(NULL)
    }
    shadow_length <- calculate_shadow_length(measured_height, solar_elevation)

    # Compute shadow polygon
    tryCatch(
      calculate_shadow_points(current_building, c(solar_azimuth, solar_elevation), shadow_length),
      error = function(e) {
        message(sprintf("Error calculating shadow for building %s: %s", i, e$message))
        return(NULL)
      }
    )
  })

  # Assign shadow geometries to buildings
  building_sf$shadow_geometry <- st_sfc(do.call(c, shadow_geometries), crs = st_crs(building_sf))

  return(building_sf)
}
