#' Calculate Centroids for Building Geometries
#'
#' This function calculates the centroids for the provided building geometries.
#' @param buildings sf object containing building geometries
#' @return sf object with centroids of the building geometries
#' @importFrom sf st_centroid
#' @export
calc_centroids <- function(buildings) {
  # Compute centroids for all buildings
  st_centroid(buildings)
}


#' Calculate Solar Position for Building Centroids
#'
#' @param centroids sf object containing centroids of building geometries
#' @param time POSIXct time for which to calculate the solar position
#' @param buildings sf object containing building geometries
#' @return sf object with solar azimuth and elevation added as attributes
#' @importFrom sf st_coordinates st_transform
#' @importFrom suntools solarpos
#' @export
calc_solar_pos <- function(centroids, time, buildings) {
  # Transform centroids to WGS84
  centroids_geo <- sf::st_transform(centroids, crs = 4326)

  # Get coordinates
  coords <- sf::st_coordinates(centroids_geo)

  # Create coordinate matrix (longitude, latitude)
  coord_matrix <- matrix(c(coords[, 1], coords[, 2]), ncol = 2, byrow = FALSE)
  colnames(coord_matrix) <- c("lon", "lat")

  # Compute solar positions
  sol_pos <- solarpos(coord_matrix, time)

  # Ensure output is a matrix
  if (!is.matrix(sol_pos)) stop("Solar positions format is incorrect.")

  # Add solar data to buildings
  buildings$sol_azimuth <- sol_pos[, 1]
  buildings$sol_elevation <- sol_pos[, 2]
  buildings$centroid_lon <- coord_matrix[, 1]
  buildings$centroid_lat <- coord_matrix[, 2]

  return(buildings)
}


#' Calculate Solar Position for Given Time
#'
#' This function calculates the solar position for the provided building data and time.
#' @param building_sf sf object containing building geometries
#' @param time POSIXct. The time for which to calculate the solar position.
#' @return sf object containing building geometries and solar positions
#' @export
calculate_solar <- function(building_sf, time) {
  # Berechnung der Zentroiden
  centroids <- calc_centroids(building_sf)

  # Berechnung des Sonnenstands
  result <- calc_solar_pos(centroids, time, building_sf)

  return(result)
}

