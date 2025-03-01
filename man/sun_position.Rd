% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_solar_pos.R
\name{sun_position}
\alias{sun_position}
\title{Calculate Solar Position for Given Time}
\usage{
sun_position(building_sf, time)
}
\arguments{
\item{building_sf}{sf object containing building geometries}

\item{time}{POSIXct. The time for which to calculate the solar position, defined in the format `as.POSIXct("yyyy-mm-dd hh:mm:ss", tz = "Europe/Berlin")`.}
}
\value{
sf object containing building geometries and solar positions
}
\description{
This function calculates the solar position for the provided building data and time.
It takes an `sf` object containing building polygons, calculates the centroids of these polygons using the `calc_centroids` function,
and then calculates the solar position for these centroids using the `calc_solar_pos` function.
The centroids are needed because the `suntools` package requires point coordinates to calculate the solar position.
The user must define the time in the format `as.POSIXct("yyyy-mm-dd hh:mm:ss", tz = "Europe/Berlin")`.
}
\examples{
\dontrun{
# Example usage
library(sf)
library(suntools)

# Create example building geometries
building_sf <- st_sf(
  part_id = c(1, 2),
  geometry = st_sfc(
    st_polygon(list(matrix(c(0, 0, 1, 1, 1, 0, 0, 0), ncol = 2, byrow = TRUE))),
    st_polygon(list(matrix(c(1, 1, 2, 2, 2, 1, 1, 1), ncol = 2, byrow = TRUE)))
  ),
  crs = 4326
)

# Specify the time for solar position calculation
time <- as.POSIXct("2025-02-21 12:00:00", tz = "Europe/Berlin")

# Calculate solar position
solar_position <- sun_position(building_sf, time)
print(solar_position)
}
}
