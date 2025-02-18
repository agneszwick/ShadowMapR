#' Load and process building data from XML files
#'
#' @param file_path Character string. Path to a XML file or directory containing XML files
#' @param recursive Logical. If TRUE and file_path is a directory, search recursively for XML files
#' @return sf object containing building geometries
#' @importFrom xml2 read_xml
#' @export
load_building_data <- function(file_path, recursive = FALSE) {
  if (!file.exists(file_path)) {
    stop("File or directory does not exist: ", file_path)
  }

  # If it's a directory, find all XML files
  if (dir.exists(file_path)) {
    files <- list.files(
      path = file_path,
      pattern = "\\.xml$",
      full.names = TRUE,
      recursive = recursive
    )
    if (length(files) == 0) {
      stop("No XML files found in directory: ", file_path)
    }
  } else {
    # Single file
    if (!grepl("\\.xml$", file_path)) {
      stop("File must be an XML file: ", file_path)
    }
    files <- file_path
  }

  # Process each file
  building_data <- lapply(files, function(file) {
    message("Processing file: ", basename(file))
    extract_buildings(file)
  })

  # Combine all building data
  building_data <- do.call(rbind, building_data)

  # Convert to 2D and clean
  building_sf <- convert_to_2D(building_data, tolerance = 0.1)
  clean_polygon(building_sf)
}

#' Get example building data included with the package
#'
#' @return sf object containing building geometries from the example dataset
#' @export
get_example_data <- function() {
  # Get the path to example.xml in the package
  example_path <- system.file("extdata", "example.xml", package = "ShadowMapR")

  if (example_path == "") {
    stop("Example data not found. Please make sure the package is properly installed.")
  }

  # Load and process the example data
  message("Loading example data from: ", basename(example_path))
  load_building_data(example_path)
}
