#' Extract polygons from GML file
#'
#' This function reads a GML file, extracts polygons from `<gml:posList>` elements
#' within `<gml:Polygon>` elements, and returns the polygons as an `sf` object.
#' The function also extracts the `measuredHeight` attribute from the parent `<bldg:RoofSurface>` element.
#' This function is specifically designed for 3D building models LoD2 Germany (LoD2-DE).
#'
#' @param gml_file Path to the GML file
#' @param crs_code Character string. The coordinate reference system (CRS) code to be used.
#'
#' @import xml2 dplyr purrr sf
#'
#' @return A simple feature (sf) object containing polygons with their geometry, `part_id`, and `measuredHeight`
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' gml_file <- "path/to/your/file.gml"
#' crs_code <- "EPSG:25833"
#' polygons <- extract_gml_polygons(gml_file, crs_code)
#' print(polygons)
#' }
extract_gml_polygons <- function(gml_file, crs_code) {

  start_time <- Sys.time()
  message("CRS code detected: EPSG:", crs_code)
  message("Starting GML extraction...")

  # Read the GML file once
  gml_xml <- read_xml(gml_file)
  ns <- xml_ns(gml_xml)


  # Find all <gml:posList> elements
  poslist_nodes <- xml_find_all(gml_xml, ".//gml:MultiSurface//gml:Polygon//gml:exterior//gml:LinearRing//gml:posList", ns = ns)
  message("Filtered posList elements: ", length(poslist_nodes))

  # Function to extract coordinates and measuredHeight
  extract_xy_info <- function(poslist_node) {
    coords <- as.numeric(strsplit(xml_text(poslist_node), "\\s+")[[1]])
    if (length(coords) %% 3 != 0) {
      warning("[", format(Sys.time(), "%H:%M:%S"), "] The number of coordinates is not a multiple of 3!")
      return(NULL)
    }

    triplets <- matrix(coords, ncol = 3, byrow = TRUE)
    xy <- triplets[, 1:2, drop = FALSE]

    # Ensure polygon is closed
    if (!all(xy[1,] == xy[nrow(xy), ])) {
      xy <- rbind(xy, xy[1,])
    }

    # Find the parent <bldg:RoofSurface> element to extract measuredHeight
    parent_roof_surface <- xml_find_first(poslist_node, "ancestor::bldg:RoofSurface", ns = ns)
    measured_height_node <- xml_find_first(parent_roof_surface, ".//gen:stringAttribute[@name='Z_MAX']/gen:value", ns = ns)
    measured_height <- if (!is.null(measured_height_node)) as.numeric(xml_text(measured_height_node)) else NA

    return(list(xy = xy, measured_height = measured_height))
  }

  # Apply the extraction function sequentially
  info_list <- lapply(poslist_nodes, extract_xy_info)

  # Remove NULL results
  info_list <- Filter(Negate(is.null), info_list)

  # Create polygons from extracted coordinates
  polygons <- lapply(info_list, function(info) st_polygon(list(info$xy)))

  # Create a simple feature collection (sfc) with the polygons and CRS
  sfc_polygons <- st_sfc(polygons, crs = crs_code)

  message("Simple Feature Collection: ", length(poslist_nodes))

  # Extract measured_height from info_list
  measured_heights <- sapply(info_list, function(info) info$measured_height)

  # Create an sf object with measured_height and geometry
  sf_polygons <- st_sf(
    height = measured_heights,
    geometry = sfc_polygons
  )

  # Remove rows where measured_height is NA
  sf_polygons <- sf_polygons[!is.na(sf_polygons$height), ]

  # Add part_id column
  sf_polygons$part_id <- seq_len(nrow(sf_polygons))
  message("Simple Feature Collection cleaned: ", length(poslist_nodes))

  end_time <- Sys.time()
  message("GML extraction completed in ", round(difftime(end_time, start_time, units = "secs"), 2), " seconds.")

  return(sf_polygons)
}
