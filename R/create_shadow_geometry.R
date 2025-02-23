#' Process Buildings by Validating and Filtering Geometries
#'
#' This function processes building and shadow geometries by checking for invalid geometries
#' and filtering out empty ones. Invalid geometries are corrected using `st_make_valid`
#' and empty geometries are removed. This ensures that the building and shadow geometries are valid
#' and ready for further processing.
#'
#' @param buildings sf object containing building geometries with a geometry column
#'  and shadow geometries with a shadow_geometry columnn.
#' @importFrom sf st_make_valid st_is_empty st_geometry st_is_valid
#' @importFrom dplyr mutate filter
#' @return sf object containing processed buildings with valid geometries.
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' # Create example building geometries
#' buildings <- st_sf(
#'   part_id = c(1, 2),
#'   geometry = st_sfc(
#'     st_polygon(list(matrix(c(0, 0, 1, 1, 1, 0, 0, 0), ncol = 2, byrow = TRUE))),
#'     st_polygon(list(matrix(c(1, 1, 2, 2, 2, 1, 1, 1), ncol = 2, byrow = TRUE)))
#'   ),
#'   shadow_geometry = st_sfc(
#'     st_polygon(list(matrix(c(0, 0, 1, 1, 1, 0, 0, 0), ncol = 2, byrow = TRUE))),
#'     st_polygon(list(matrix(c(1, 1, 2, 2, 2, 1, 1, 1), ncol = 2, byrow = TRUE)))
#'   ),
#'   crs = 4326
#' )
#'
#' # Process buildings to ensure valid geometries
#' processed_buildings <- process_buildings(buildings)
#' print(processed_buildings)
#' }
process_buildings <- function(buildings) {

  # Check for invalid or empty geometries
  invalid_geom_bld <- st_is_valid(st_geometry(buildings)) == FALSE
  invalid_geom_sh <- st_is_valid(st_geometry(buildings$shadow_geometry)) == FALSE

  empty_geom_bld <- st_is_empty(st_geometry(buildings)) == TRUE
  empty_geom_sh <- st_is_empty(st_geometry(buildings$shadow_geometry)) == TRUE

  message(sum(invalid_geom_bld), " invalid building geometries detected.")
  message(sum(invalid_geom_sh), " invalid shadow geometries detected.")
  message(sum(empty_geom_bld), " empty building geometries detected.")
  message(sum(empty_geom_sh), " empty shadow geometries detected.")

  # Validate invalid geometries
  if (sum(invalid_geom_bld) > 0) {
    buildings[invalid_geom_bld, "geometry"] <- st_make_valid(st_geometry(buildings[invalid_geom_bld, ]))
  }
  if (sum(invalid_geom_sh) > 0) {
    buildings[invalid_geom_sh, "shadow_geometry"] <- st_make_valid(st_geometry(buildings$shadow_geometry[invalid_geom_sh, ]))
  }

  # Filter out empty geometries
  if (sum(empty_geom_bld) > 0) {
    buildings <- buildings[!empty_geom_bld, ]
  }
  if (sum(empty_geom_sh) > 0) {
    buildings <- buildings[!empty_geom_sh, ]
  }

  return(buildings)
}


#' Compute Shadow Hulls for Buildings with Batch Processing
#'
#' This function calculates the convex hull of each building's combined geometry (building footprint + shadow geometry).
#' The convex hull is a minimal enclosing polygon that contains all the points of the combined geometries. The function
#' processes each building in batches, merging the building footprint and its shadow geometry, then calculating the convex
#' hull of the combined geometry. Only valid and non-empty convex hulls are returned.
#'
#' The function performs the following steps for each building:
#' - Retrieves the building and shadow geometries.
#' - Unions the building footprint with the shadow geometry.
#' - Computes the convex hull of the resulting unioned geometry.
#' - Checks if the resulting convex hull is valid and non-empty.
#' - Collects all valid convex hulls with their corresponding building part IDs for final output.
#'
#' If any error occurs while processing a particular building, the function will skip that building and print an error message.
#' Additionally, if no valid convex hulls are generated, an error will be raised.
#'
#' @param buildings An `sf` object of class `sf` containing the geometries of buildings, where:
#'   - The `geometry` column contains the building footprint geometries (as `sfc` objects).
#'   - The `shadow_geometry` column contains the shadow geometry associated with each building.
#'   - The `part_id` column contains a unique identifier for each building part.
#' @param batch_size The number of buildings to process in each batch. Default is 50.
#'
#' @return An `sf` object of class `sf` containing:
#'   - A `part_id` column that identifies the building parts.
#'   - A `geometry` column containing the convex hulls of the union of each building's footprint and shadow geometry,
#'     as `sfc` objects (simple features geometry list-column).
#'
#' @details The function ensures that only valid and non-empty convex hulls are included in the result. If any hull is invalid
#' or empty, it is skipped, and a message is printed to inform the user. If no valid convex hulls are generated, the function
#' will stop and raise an error.
#'
#' The `st_union` and `st_convex_hull` functions from the `sf` package are used to compute the combined geometry and convex hull.
#' The `st_is_valid` function is used to ensure the hull is a valid geometry, and `st_is_empty` is used to check if the resulting
#' hull is non-empty.
#'
#' @importFrom sf st_union st_convex_hull st_is_valid st_is_empty st_sfc st_crs
#' @export
#'
#' @examples
#' # Example of using the compute_shadow_hulls function
#' # Assuming 'buildings_sf' is an existing sf object containing building and shadow geometries
#' shadow_hulls <- compute_shadow_hulls(buildings_sf)
#'
#' # Plot the resulting shadow hulls
#' plot(shadow_hulls$geometry)
#'
compute_shadow_hulls <- function(buildings, batch_size = 50) {
  message("Computing shadow hulls...")

  shadow_hulls <- vector("list", nrow(buildings))
  part_ids <- vector("character", nrow(buildings))
  valid_count <- 0

  # Process in batches
  for (batch_start in seq(1, nrow(buildings), by = batch_size)) {
    batch_end <- min(batch_start + batch_size - 1, nrow(buildings))
    message(sprintf("Processing batch %d to %d", batch_start, batch_end))

    # Process each building in the batch
    for (i in batch_start:batch_end) {
      current_part_id <- buildings$part_id[i]
      tryCatch({
        bldg_geom <- buildings$geometry[i]
        shadow_geom <- buildings$shadow_geometry[i]

        # Union of geometries and convex hull calculation
        combined_geom <- st_union(bldg_geom, shadow_geom)
        hull <- st_convex_hull(combined_geom)

        # Save valid hulls
        if (st_is_valid(hull) && !st_is_empty(hull)) {
          valid_count <- valid_count + 1
          shadow_hulls[[valid_count]] <- hull
          part_ids[valid_count] <- current_part_id
        } else {
          message("Invalid or empty hull for building ", current_part_id)
        }
      }, error = function(e) {
        message(sprintf("Error processing building %s: %s", current_part_id, e$message))
      })
    }
  }

  if (valid_count > 0) {
    return(st_sf(
      part_id = part_ids[1:valid_count],
      geometry = st_sfc(do.call(c, shadow_hulls), crs = st_crs(buildings))
    ))
  } else {
    stop("No valid shadow hulls were created.")
  }
}


#' Dissolve Shadow Polygons into a Single Geometry
#'
#' This function dissolves multiple shadow hull polygons into a single unified geometry
#' by applying a union operation to all the shadow geometries in the input. It is useful for
#' combining overlapping or adjacent shadow areas into one boundary.
#'
#' @param shadow_hulls An `sf` object containing shadow hull geometries. The `geometry` column
#'   should include individual shadow polygons.
#'
#' @return An `sf` object with a single geometry representing the union of all shadow polygons.
#'
#' @details The function uses `st_union` to combine the geometries. If no valid geometries are present
#'   in the input, an error is raised.
#'
#' @importFrom sf st_union
#' @importFrom dplyr summarise
#' @export
#'
#' @examples
#' # Example usage:
#' dissolved_shadow <- dissolve_shadow_polygons(shadow_hulls_sf)
#' plot(dissolved_shadow$geometry)
#'
dissolve_shadow_polygons <- function(shadow_hulls) {
  message("Dissolving shadow polygons...")

  if (nrow(shadow_hulls) == 0) {
    stop("No shadow hulls available for dissolving.")
  }

  return(shadow_hulls %>%
           summarise(geometry = st_union(geometry)))
}



#' Create Sunlight Areas by Subtracting Buildings and Shadows from a Bounding Box
#'
#' This function creates sunlight areas by subtracting the building and shadow geometries from a bounding box.
#'
#' @param buildings sf object containing building polygons.
#' @param shadows sf object containing shadow polygons.
#' @return sf object containing sunlight areas.
#' @importFrom sf st_as_sfc st_bbox st_union st_difference
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' # Create example building and shadow geometries
#' buildings <- st_sf(
#'   geometry = st_sfc(
#'     st_polygon(list(matrix(c(0, 0, 1, 1, 1, 0, 0, 0), ncol = 2, byrow = TRUE))),
#'     st_polygon(list(matrix(c(1, 1, 2, 2, 2, 1, 1, 1), ncol = 2, byrow = TRUE)))
#'   ),
#'   crs = 4326
#' )
#' shadows <- st_sf(
#'   geometry = st_sfc(
#'     st_polygon(list(matrix(c(0, 0, 1, 1, 1, 0, 0, 0), ncol = 2, byrow = TRUE))),
#'     st_polygon(list(matrix(c(1, 1, 2, 2, 2, 1, 1, 1), ncol = 2, byrow = TRUE)))
#'   ),
#'   crs = 4326
#' )
#'
#' # Create sunlight areas
#' sunlight_areas <- create_sunlight_areas(buildings, shadows)
#' print(sunlight_areas)
#' }
create_sunlight_areas <- function(buildings, shadows) {
  bbox <- st_as_sfc(st_bbox(buildings))  # Convert bounding box to polygon

  buildings_union <- st_union(buildings)  # Merge all buildings into one geometry
  shadows_union <- st_union(shadows)  # Merge all shadows into one geometry

  st_difference(bbox, st_union(buildings_union, shadows_union))  # Subtract buildings & shadows
}
