#' Calculate Centroids for Building Geometries
#'
#' This function calculates the centroids for the provided building geometries.
#' It takes an `sf` object containing building polygons and returns an `sf` object with the centroids of these polygons.
#' The centroids are calculated using the `st_centroid` function from the `sf` package.
#' The centroids are needed because the `suntools` package requires point coordinates to calculate the solar position.
#'
#' @param buildings sf object containing building geometries
#' @return sf object with centroids of the building geometries
#' @importFrom sf st_centroid
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' library(sf)
#'
#' # Create example building geometries
#' building_sf <- st_sf(
#'   part_id = c(1, 2),
#'   geometry = st_sfc(
#'     st_polygon(list(matrix(c(0, 0, 1, 1, 1, 0, 0, 0), ncol = 2, byrow = TRUE))),
#'     st_polygon(list(matrix(c(1, 1, 2, 2, 2, 1, 1, 1), ncol = 2, byrow = TRUE)))
#'   )
#' )
#'
#' # Calculate centroids
#' centroids_sf <- calc_centroids(building_sf)
#' print(centroids_sf)
#' }
calc_centroids <- function(buildings) {
  # Compute centroids for all buildings
  st_centroid(buildings)
}


#' Calculate Solar Position for Building Centroids
#'
#' This function calculates the solar position (azimuth and elevation) for the provided centroids of building geometries
#' at a specified time. It takes an `sf` object containing the centroids of building polygons and a `POSIXct` time object,
#' and returns an `sf` object with the solar azimuth and elevation added as attributes.
#' The solar position is calculated using the `solarpos` function from the `suntools` package.
#'
#' @param centroids sf object containing centroids of building geometries
#' @param time POSIXct time for which to calculate the solar position
#' @param buildings sf object containing building geometries
#' @return sf object with solar azimuth and elevation added as attributes
#' @importFrom sf st_coordinates st_transform
#' @importFrom suntools solarpos
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' library(sf)
#' library(suntools)
#'
#' # Create example centroids
#' centroids <- st_sf(
#'   part_id = c(1, 2),
#'   geometry = st_sfc(
#'     st_point(c(10.0, 20.0)),
#'     st_point(c(30.0, 40.0))
#'   ),
#'   crs = 4326
#' )
#'
#' # Specify the time for solar position calculation
#' time <- as.POSIXct("2025-02-21 12:00:00", tz = "UTC")
#'
#' # Calculate solar position
#' solar_position <- calc_solar_pos(centroids, time, centroids)
#' print(solar_position)
#' }
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
#' It takes an `sf` object containing building polygons, calculates the centroids of these polygons using the `calc_centroids` function,
#' and then calculates the solar position for these centroids using the `calc_solar_pos` function.
#' The centroids are needed because the `suntools` package requires point coordinates to calculate the solar position.
#' The user must define the time in the format `as.POSIXct("yyyy-mm-dd hh:mm:ss", tz = "Europe/Berlin")`.
#'
#' @param building_sf sf object containing building geometries
#' @param time POSIXct. The time for which to calculate the solar position, defined in the format `as.POSIXct("yyyy-mm-dd hh:mm:ss", tz = "Europe/Berlin")`.
#' @return sf object containing building geometries and solar positions
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' library(sf)
#' library(suntools)
#'
#' # Create example building geometries
#' building_sf <- st_sf(
#'   part_id = c(1, 2),
#'   geometry = st_sfc(
#'     st_polygon(list(matrix(c(0, 0, 1, 1, 1, 0, 0, 0), ncol = 2, byrow = TRUE))),
#'     st_polygon(list(matrix(c(1, 1, 2, 2, 2, 1, 1, 1), ncol = 2, byrow = TRUE)))
#'   ),
#'   crs = 4326
#' )
#'
#' # Specify the time for solar position calculation
#' time <- as.POSIXct("2025-02-21 12:00:00", tz = "Europe/Berlin")
#'
#' # Calculate solar position
#' solar_position <- sun_position(building_sf, time)
#' print(solar_position)
#' }
sun_position <- function(building_sf, time) {
  # Berechnung der Zentroiden
  centroids <- calc_centroids(building_sf)

  # Berechnung des Sonnenstands
  result <- calc_solar_pos(centroids, time, building_sf)

  return(result)
}
