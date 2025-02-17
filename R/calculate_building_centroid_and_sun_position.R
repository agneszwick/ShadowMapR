#' Calculate centroids of building geometries
#'
#' @param buildings sf object containing building geometries
#' @return sf object containing the centroids of the building geometries
#' @export
calc_centroids <- function(buildings) {
  # Compute centroids for all buildings
  st_centroid(buildings)
}

#' Calculate solar position for building centroids
#'
#' @param centroids sf object containing building centroids
#' @param time POSIXct object representing the time for solar position calculation
#' @param buildings sf object containing building geometries (to be updated with solar positions)
#' @return sf object containing building geometries along with calculated solar positions (azimuth and elevation)
#' @export
calc_solar_pos <- function(centroids, time, buildings) {
  # Transform centroids to WGS84
  centroids_geo <- st_transform(centroids, crs = 4326)
  coords <- st_coordinates(centroids_geo)

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

# # APPLY FUNCTIONS
# centroids <- calc_centroids(building_sf)
# time <- as.POSIXct("2024-02-02 09:00:00", tz = "UTC")
# building_sf <- calc_solar_pos(centroids, time, building_sf)
# print(building_sf)
