#' Extract building data from an XML file
#'
#' @param xml_file Character string, path to the XML file containing building data
#' @return Data frame containing extracted building data (building ID, part ID, geometry, height, and file name)
#' @importFrom xml2 read_xml xml_ns xml_find_all xml_attr xml_text xml_find_first
#' @importFrom dplyr %>% group_by summarise across where first
#' @importFrom sf st_as_sf st_union st_geometry_type st_cast st_is_empty st_transform st_crs
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
  return(result)
}

#' Convert building data to 2D polygons
#'
#' @param building_data Data frame containing building data
#' @param tolerance Numeric value for geometry simplification
#' @return sf object containing 2D building polygons
#' @importFrom sf st_as_sf st_simplify st_is_empty st_crs
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

  return(sf_obj)
}

#' Clean polygon geometries
#'
#' @param building_sf sf object containing building polygons
#' @return sf object with cleaned polygons
#' @importFrom dplyr group_by summarise across where first
#' @importFrom sf st_union st_geometry_type st_cast st_is_empty
#' @export
clean_polygon <- function(building_sf) {
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

  return(result)
}

#' Visualize building geometries with Leaflet
#'
#' @importFrom leaflet leaflet addTiles addPolygons colorQuantile
#' @param building_sf sf object containing building geometries with height and building ID
#' @return A leaflet map object showing building geometries, colored by height
#' @export
visualize_buildings <- function(building_sf) {
  # Transform CRS for Leaflet (WGS84 - EPSG:4326)
  building_sf_4326 <- sf::st_transform(building_sf, crs = 4326)

  # Visualize with Leaflet
  leaflet(building_sf_4326) %>%
    leaflet::addTiles() %>%
    leaflet::addPolygons(
      fillColor = ~leaflet::colorQuantile("YlOrRd", height)(height),
      fillOpacity = 0.5,
      color = "black",
      weight = 1,
      popup = ~paste("Building ID: ", bldg_id, "<br>", "Height: ", height)
    )
}
