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


#' Create Shadow Polygons from Buildings
#'
#' This function creates shadow polygons from building geometries. It processes the buildings in batches,
#' validates the geometries, and calculates the convex hull of the combined building and shadow geometries.
#' The resulting shadow polygons are then dissolved into a single geometry.
#'
#' The function works as follows:
#' 1. Processes the buildings in batches to manage memory usage and performance.
#' 2. For each building in the batch, it combines the building geometry with its shadow geometry.
#' 3. Calculates the convex hull of the combined geometry to create the shadow polygon.
#' 4. Collects all valid shadow polygons and dissolves them into a single geometry.
#'
#' @param buildings sf object containing processed buildings with valid geometries and shadow geometries.
#' @param batch_size integer number of buildings to process in each batch.
#' @return sf object containing dissolved shadow polygons.
#' @importFrom sf st_make_valid st_is_valid st_is_empty st_union st_convex_hull st_sfc st_crs
#' @importFrom dplyr summarise
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
#' # Create shadow polygons from buildings
#' shadow_polygons <- create_shadow_polygons(buildings, batch_size = 1)
#' print(shadow_polygons)
#' }
create_shadow_polygons <- function(buildings, batch_size) {
  message("Creating shadow polygons...")

  final_polygons <- vector("list", nrow(buildings))
  final_part_ids <- vector("character", nrow(buildings))
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

        # Save if valid and not empty
        if (st_is_valid(hull) && !st_is_empty(hull)) {
          valid_count <- valid_count + 1
          final_polygons[[valid_count]] <- hull
          final_part_ids[valid_count] <- current_part_id
        } else {
          message("Invalid or empty hull created for building ", current_part_id)
        }
      }, error = function(e) {
        message(sprintf("Error processing building %s: %s", current_part_id, e$message))
      })
    }
  }

  if (valid_count > 0) {
    final_polygons <- final_polygons[1:valid_count]
    final_part_ids <- final_part_ids[1:valid_count]

    # Create sf object and dissolve polygons
    shadow_polygons <- st_sf(
      part_id = final_part_ids,
      geometry = st_sfc(do.call(c, final_polygons), crs = st_crs(buildings))
    ) %>%
      summarise(geometry = st_union(geometry))

    return(shadow_polygons)
  } else {
    stop("No valid shadow polygons were created.")
  }
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
