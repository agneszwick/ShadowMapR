#' Extract building data from an XML file
#'
#' @param xml_file Character string, path to the XML file containing building data
#' @return Data frame containing extracted building data (building ID, part ID, geometry, height, and file name)
#' @examples
#' # Use example XML data bundled with the package
#' example_xml_path <- system.file("extdata", "example_data.xml", package = "ShadowMapR")
#' building_data <- extract_buildings(example_xml_path)
#' head(building_data)
#' @export


#' Extract building data from an XML file
#'
#' @param xml_file Character string, path to the XML file containing building data
#' @return Data frame containing extracted building data (building ID, part ID, geometry, height, and file name)
#' @export
extract_buildings <- function(xml_file) {
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
  gc() # Clean up memory
  return(result)
}

#' Convert 3D building geometries to 2D by ignoring the Z-coordinate
#'
#' @param building_data Data frame containing building data with geometry in WKT format
#' @param tolerance Numeric value specifying the tolerance for simplifying geometries (default is 0.1)
#' @return sf object containing the 2D geometries of the buildings
#' @export
convert_to_2D <- function(building_data, tolerance = 0.1) {
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
    crs = 25833
  )

  # Simplify all geometries at once
  sf_obj <- st_simplify(sf_obj, dTolerance = tolerance)
  sf_obj <- sf_obj[!st_is_empty(sf_obj$geometry), ]

  gc() # Clean up memory
  return(sf_obj)
}


plan(multisession)  # Nutze alle verfügbaren CPU-Kerne



#' Clean and process building geometries by merging parts and handling multipolygons
#'
#' @param building_sf sf object containing building geometries with part IDs
#' @return sf object with cleaned and processed geometries, where multipolygons are simplified and parts are merged
#' @export
clean_polygon <- function(building_sf) {
  # Group and summarize in one operation
  result <- building_sf %>%
    group_by(part_id) %>%
    summarise(
      geometry = st_union(geometry),
      across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), # <- Fix für Warnung 1
      across(where(is.character), first),
      .groups = "drop"
    )

  # Handle multipolygons more efficiently
  multi_idx <- st_geometry_type(result$geometry) == "MULTIPOLYGON"
  if (any(multi_idx)) {
    multi_polys <- result[multi_idx, ]
    single_polys <- result[!multi_idx, ]

    # Process multipolygons
    multi_polys <- st_cast(multi_polys, "POLYGON", group_or_split = TRUE) # <- Fix für Warnung 2
    multi_polys$part_id <- paste0(multi_polys$part_id, "_", seq_len(nrow(multi_polys)))

    # Combine results
    result <- rbind(single_polys, multi_polys)
  }

  # Remove invalid geometries
  result <- result[!st_is_empty(result$geometry), ]

  gc() # Clean up memory
  return(result)
}

#' Visualize building geometries with Leaflet
#'
#' @param building_sf sf object containing building geometries with height and building ID
#' @return A leaflet map object showing building geometries, colored by height
#' @export
visualize_buildings <- function(building_sf) {
  # Transform CRS for Leaflet (WGS84 - EPSG:4326)
  building_sf_4326 <- sf::st_transform(building_sf, crs = 4326)

  # Visualize with Leaflet
  leaflet(building_sf_4326) %>%
    addTiles() %>%
    addPolygons(
      fillColor = ~colorQuantile("YlOrRd", height)(height),
      fillOpacity = 0.5,
      color = "black",
      weight = 1,
      popup = ~paste("Building ID: ", bldg_id, "<br>", "Height: ", height)
    )
}

# # APPLY FUNCTIONS
# # Load and process selected XML files
# xml_files <- list.files("C:/Users/agnes/Documents/EAGLE/Introduction_Programming/Assignment_25/sunshine/LoD1",
#                         pattern = "\\.xml$", full.names = TRUE)
# # Filter files by specific filename endings

# filter_files <- function(file_list, endings) {
#   pattern <- paste0(endings, collapse = "|")
#   file_list[grepl(pattern, file_list)]
# }

# selected_files <- filter_files(xml_files, c("385_5814"))
# building_data <- map_dfr(selected_files, extract_buildings)
# # print(building_data)
#
# # Convert to sf
# building_sf <- convert_to_2D(building_data, tolerance = 0.1)
# print(building_sf)
#
# # Clean the data
# building_sf <- clean_polygon(building_sf)
#
# # Visualize the data
# visualize_buildings(building_sf)

