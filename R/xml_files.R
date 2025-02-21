#' Extract building data from an XML file
#'
#' This function reads an XML file containing 3D building models LoD0 Germany (LoD0-DE),
#' extracts building data including geometry and measured height, and returns the data as a data frame.
#' The function extracts the geometry from `<gml:posList>` elements within `<bldg:Building>` and `<bldg:BuildingPart>` elements,
#' and the `measuredHeight` attribute from `<bldg:measuredHeight>` elements.
#'
#' @param xml_file Character string, path to the XML file containing building data
#' @return Data frame containing extracted building data (building ID, part ID, geometry, height, and file name)
#' @importFrom xml2 read_xml xml_ns xml_find_all xml_attr xml_text xml_find_first
#' @importFrom dplyr %>% group_by summarise across where first
#' @importFrom sf st_as_sf st_union st_geometry_type st_cast st_is_empty st_transform st_crs
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' xml_file <- "path/to/your/file.xml"
#' building_data <- extract_xml_polygons(xml_file)
#' print(building_data)
#' }
extract_xml_polygons <- function(xml_file) {
  start_time <- Sys.time()
  xml <- read_xml(xml_file)
  ns <- xml_ns(xml)

  # Extract all buildings at once
  buildings <- xml_find_all(xml, ".//bldg:Building", ns = ns)

  # Pre-allocate lists for better memory efficiency
  result_list <- vector("list", length(buildings))

  for (i in seq_along(buildings)) {
    bldg <- buildings[[i]]
    bldg_id <- xml_attr(bldg, "gml:id", ns)

    # Extract all building parts at once
    parts <- xml_find_all(bldg, ".//bldg:consistsOfBuildingPart/bldg:BuildingPart", ns)

    if (length(parts) > 0) {
      # Process building parts
      part_data <- lapply(parts, function(part) {
        part_id <- as.character(xml_attr(part, "gml:id", ns))
        height <- suppressWarnings(as.numeric(xml_text(xml_find_first(part, ".//bldg:measuredHeight", ns))))
        geom_nodes <- xml_find_all(part, ".//gml:posList", ns)

        # Process all geometry nodes at once
        geom_data <- lapply(geom_nodes, function(node) {
          coords <- strsplit(xml_text(node), " ")[[1]]
          if (length(coords) %% 3 != 0) return(NULL)  # Remove invalid geometries
          data.frame(
            bldg_id = bldg_id,
            part_id = part_id,
            geometry = xml_text(node),
            height = height,
            file = basename(xml_file),
            stringsAsFactors = FALSE
          )
        })
        do.call(rbind, geom_data)
      })
      result_list[[i]] <- do.call(rbind, part_data)
    } else {
      # Process building without parts
      height <- suppressWarnings(as.numeric(xml_text(xml_find_first(bldg, ".//bldg:measuredHeight", ns))))
      geom_nodes <- xml_find_all(bldg, ".//gml:posList", ns)

      geom_data <- lapply(geom_nodes, function(node) {
        coords <- strsplit(xml_text(node), " ")[[1]]
        if (length(coords) %% 3 != 0) return(NULL)  # Remove invalid geometries
        data.frame(
          bldg_id = bldg_id,
          part_id = bldg_id,
          geometry = xml_text(node),
          height = height,
          file = basename(xml_file),
          stringsAsFactors = FALSE
        )
      })
      result_list[[i]] <- do.call(rbind, geom_data)
    }
  }

  # Combine all results at once
  result <- do.call(rbind, result_list)
  end_time <- Sys.time()
  message("XML extraction completed in ", round(difftime(end_time, start_time, units = "secs"), 2), " seconds.")
  return(result)
}



#' Convert building data to 2D polygons
#'
#' This function converts 3D building data to 2D polygons. It processes the geometry
#' by extracting the x and y coordinates from the 3D coordinates, ensuring that the polygons are closed,
#' and then creates a simple feature (sf) object with the specified coordinate reference system (CRS).
#' The function also simplifies the geometries to reduce complexity by removing unnecessary vertices.
#' The simplification is performed using the `st_simplify` function from the `sf` package.
#'
#' @param building_data Data frame containing building data with 3D coordinates
#' @param crs_code Character string, the coordinate reference system (CRS) code to be used
#' @param tolerance Numeric value for geometry simplification. A higher tolerance value will result in more simplified geometries by removing more vertices.
#'
#' @return sf object containing 2D building polygons in the specified CRS
#'
#' @importFrom sf st_as_sf st_simplify st_is_empty st_crs st_is_valid st_make_valid
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' building_data <- data.frame(
#'   geometry = c(
#'     "10.0 20.0 0.0 15.0 25.0 0.0 20.0 20.0 0.0 10.0 20.0 0.0",
#'     "30.0 40.0 0.0 35.0 45.0 0.0 40.0 40.0 0.0 30.0 40.0 0.0"
#'   ),
#'   stringsAsFactors = FALSE
#' )
#' crs_code <- "EPSG:25833"
#' polygons_2d <- convert_to_2D(building_data, crs_code)
#' print(polygons_2d)
#' }
convert_to_2D <- function(building_data, crs_code, tolerance = 0.1) {
  start_time <- Sys.time()

  # Vectorized geometry processing
  building_data$geometry <- gsub("([0-9.-]+) ([0-9.-]+) [0-9.-]+", "\\1 \\2", building_data$geometry)
  building_data$geometry <- paste0(
    "POLYGON((",
    gsub("([0-9.-]+ [0-9.-]+)(?= )", "\\1,", building_data$geometry, perl = TRUE),
    "))"
  )

  # Batch process geometries
  sf_obj <- st_as_sf(
    building_data,
    wkt = "geometry",
    crs = crs_code
  )

  # Debugging: Print the number of geometries before validation
  message("Number of geometries before validation: ", nrow(sf_obj))

  # Remove invalid geometries
  sf_obj <- sf_obj[st_is_valid(sf_obj$geometry), ]
  sf_obj$geometry <- st_make_valid(sf_obj$geometry)

  # Debugging: Print the number of geometries after validation
  message("Number of geometries after validation: ", nrow(sf_obj))

  # Simplify all geometries at once
  sf_obj <- st_simplify(sf_obj, dTolerance = tolerance)
  sf_obj <- sf_obj[!st_is_empty(sf_obj$geometry), ]

  # Debugging: Print the number of valid geometries
  message("Number of valid geometries after simplification: ", nrow(sf_obj))
  end_time <- Sys.time()
  message("Convert to 2D completed in ", round(difftime(end_time, start_time, units = "secs"), 2), " seconds.")
  return(sf_obj)
}

#' Clean building polygon geometries
#'
#' This function cleans building polygon geometries by grouping and summarizing them,
#' handling multipolygons, and removing invalid geometries. It groups the polygons by `part_id`,
#' summarizes the numeric attributes by their mean values, and retains the first value for character attributes.
#' Multipolygons are split into individual polygons, and invalid geometries are removed.
#'
#' @param building_sf sf object containing building polygons
#' @return sf object with cleaned polygons
#' @importFrom dplyr group_by summarise across where first
#' @importFrom sf st_union st_geometry_type st_cast st_is_empty
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' building_sf <- st_sf(
#'   part_id = c(1, 1, 2),
#'   geometry = st_sfc(
#'     st_polygon(list(matrix(c(0, 0, 1, 1, 1, 0, 0, 0), ncol = 2, byrow = TRUE))),
#'     st_polygon(list(matrix(c(1, 1, 2, 2, 2, 1, 1, 1), ncol = 2, byrow = TRUE))),
#'     st_multipolygon(list(
#'       list(matrix(c(2, 2, 3, 3, 3, 2, 2, 2), ncol = 2, byrow = TRUE)),
#'       list(matrix(c(3, 3, 4, 4, 4, 3, 3, 3), ncol = 2, byrow = TRUE))
#'     ))
#'   )
#' )
#' cleaned_sf <- clean_building_polygons(building_sf)
#' print(cleaned_sf)
#' }
clean_building_polygons <- function(building_sf) {
  start_time <- Sys.time()

  # Group and summarize in one operation
  result <- building_sf %>%
    group_by(part_id) %>%
    summarise(
      geometry = st_union(geometry),
      across(where(is.numeric), mean, na.rm = TRUE),
      across(where(is.character), first),
      .groups = "drop"
    )

  # Handle multipolygons more efficiently
  multi_idx <- st_geometry_type(result$geometry) == "MULTIPOLYGON"
  if (any(multi_idx)) {
    multi_polys <- result[multi_idx, ]
    single_polys <- result[!multi_idx, ]

    # Process multipolygons
    multi_polys <- st_cast(multi_polys, "POLYGON")
    multi_polys$part_id <- paste0(multi_polys$part_id, "_", seq_len(nrow(multi_polys)))

    # Combine results
    result <- rbind(single_polys, multi_polys)
  }

  # Remove invalid geometries
  result <- result[!st_is_empty(result$geometry), ]

  # Debugging: Print the number of valid geometries
  message("Number of valid geometries after cleaning: ", nrow(result))
  end_time <- Sys.time()
  message("Cleaning completed in ", round(difftime(end_time, start_time, units = "secs"), 2), " seconds.")
  return(result)
}
