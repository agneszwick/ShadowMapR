#' Load building data from a directory or file
#'
#' This function loads building data from a directory or file. For each file in the directory,
#' if the file is a GML file, it uses the `extract_gml_polygons` function.
#' If the file is an XML file, it applies the appropriate functions (`extract_xml_polygons`, `convert_to_2D`, and `clean_building_polygons`).
#'
#' @param path Character string. Path to the directory or file containing the files.
#' @param crs_code Character string. The coordinate reference system (CRS) code to be used.
#'
#' @import xml2 dplyr purrr sf
#' @return A combined simple feature (sf) object containing all building data in the coordinate system of the input file(s).
#' @export
#'
#' @examples
#' \dontrun{
#' # Load building data from a directory or file
#' path <- "path/to/your/directory_or_file"
#' crs_code <- "EPSG:25833"
#' building_data <- load_building_data(path, crs_code)
#' print(building_data)
#' }
load_building_data <- function(path, crs_code) {
  start_time <- Sys.time()

  # Check if the path is a directory
  if (dir.exists(path)) {
    # List all files in the directory
    files <- list.files(path, full.names = TRUE)
  } else {
    # Use the provided path as the single file
    files <- list(path)
  }

  # Handle empty directory
  if (length(files) == 0) {
    warning("No files found in the directory: ", path)
    return(NULL)
  }

  # Initialize a list to store the processed data
  all_building_data <- list()

  # Loop through all files and process them
  for (file_path in files) {
    # Get the file extension
    file_extension <- tolower(tools::file_ext(file_path))

    # Process GML files
    if (file_extension == "gml") {
      message("Processing GML file: ", file_path)
      building_data <- extract_gml_polygons(file_path, crs_code)
      if (!is.null(building_data)) {
        all_building_data <- append(all_building_data, list(building_data))
      }

      # Process XML files
    } else if (file_extension == "xml") {
      message("Processing XML file: ", file_path)

      file_start <- Sys.time()
      building_data <- extract_xml_polygons(file_path)
      file_end <- Sys.time()
      # message("Time taken for extracting buildings from ", basename(file_path), ": ", round(difftime(file_end, file_start, units = "secs"), 2), " seconds")

      # Convert to 2D
      convert_start <- Sys.time()
      building_sf <- convert_to_2D(building_data, crs_code, tolerance = 0.1)
      convert_end <- Sys.time()
      # message("Time taken for 2D conversion of ", basename(file_path), ": ", round(difftime(convert_end, convert_start, units = "secs"), 2), " seconds")

      # Clean the polygons
      clean_start <- Sys.time()
      cleaned_building_sf <- clean_building_polygons(building_sf)
      clean_end <- Sys.time()
      # message("Time taken for cleaning the polygons from ", basename(file_path), ": ", round(difftime(clean_end, clean_start, units = "secs"), 2), " seconds")

      if (!is.null(cleaned_building_sf)) {
        all_building_data <- append(all_building_data, list(cleaned_building_sf))
      }
    }
  }

  # Combine all processed data into one `sf` object
  if (length(all_building_data) == 0) {
    warning("No valid building data extracted from the files in ", path)
    return(NULL)
  }

  combined_building_data <- do.call(rbind, all_building_data)

  end_time <- Sys.time()
  return(combined_building_data)
}

#' Get example building data included with the package
#'
#' This function loads example building data included with the package.
#'
#' @return A simple feature (sf) object containing building geometries from the example dataset.
#' @export
#'
#' @examples
#' \dontrun{
#' # Get example building data
#' example_data <- get_example_data()
#' print(example_data)
#' }
get_example_data <- function() {
  # Get the path to example.xml in the package
  example_path <- system.file("extdata", "example.xml", package = "ShadowMapR")

  if (example_path == "") {
    stop("Example data not found. Please make sure the package is properly installed.")
  }

  # Use the directory, not the single file
  example_dir <- dirname(example_path)

  # Extract CRS code from the first file in the directory
  crs_code <- extract_crs(example_dir)
  message("CRS code detected: ", crs_code)

  # Load building data using the extracted CRS code
  return(load_building_data(example_dir, crs_code))
}
