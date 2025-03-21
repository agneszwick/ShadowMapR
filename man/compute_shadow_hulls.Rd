% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_shadow_geometry.R
\name{compute_shadow_hulls}
\alias{compute_shadow_hulls}
\title{Compute Shadow Hulls for Buildings with Batch Processing}
\usage{
compute_shadow_hulls(buildings, batch_size = 50)
}
\arguments{
\item{buildings}{An `sf` object of class `sf` containing the geometries of buildings, where:
- The `geometry` column contains the building footprint geometries (as `sfc` objects).
- The `shadow_geometry` column contains the shadow geometry associated with each building.
- The `part_id` column contains a unique identifier for each building part.}

\item{batch_size}{The number of buildings to process in each batch. Default is 50.}
}
\value{
An `sf` object of class `sf` containing:
  - A `part_id` column that identifies the building parts.
  - A `geometry` column containing the convex hulls of the union of each building's footprint and shadow geometry,
    as `sfc` objects (simple features geometry list-column).
}
\description{
This function calculates the convex hull of each building's combined geometry (building footprint + shadow geometry).
The convex hull is a minimal enclosing polygon that contains all the points of the combined geometries. The function
processes each building in batches, merging the building footprint and its shadow geometry, then calculating the convex
hull of the combined geometry. Only valid and non-empty convex hulls are returned.
}
\details{
The function performs the following steps for each building:
- Retrieves the building and shadow geometries.
- Unions the building footprint with the shadow geometry.
- Computes the convex hull of the resulting unioned geometry.
- Checks if the resulting convex hull is valid and non-empty.
- Collects all valid convex hulls with their corresponding building part IDs for final output.

If any error occurs while processing a particular building, the function will skip that building and print an error message.
Additionally, if no valid convex hulls are generated, an error will be raised.

The function ensures that only valid and non-empty convex hulls are included in the result. If any hull is invalid
or empty, it is skipped, and a message is printed to inform the user. If no valid convex hulls are generated, the function
will stop and raise an error.

The `st_union` and `st_convex_hull` functions from the `sf` package are used to compute the combined geometry and convex hull.
The `st_is_valid` function is used to ensure the hull is a valid geometry, and `st_is_empty` is used to check if the resulting
hull is non-empty.
}
\examples{
# Example of using the compute_shadow_hulls function
# Assuming 'buildings_sf' is an existing sf object containing building and shadow geometries
shadow_hulls <- compute_shadow_hulls(buildings_sf)

# Plot the resulting shadow hulls
plot(shadow_hulls$geometry)

}
