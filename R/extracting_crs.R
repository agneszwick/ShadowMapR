#' Extract CRS from a file or the first file in a directory (GML or XML)
#'
#' This function extracts the coordinate reference system (CRS) from a GML or XML file,
#' reading the `srsName` attribute from the `<gml:Envelope>` element. If a directory is provided,
#' it uses the first file in the directory.
#'
#' @param path Path to the GML or XML file, or a directory containing such files
#'
#' @return The corresponding EPSG code, or NA if the CRS could not be determined
#' @importFrom xml2 read_xml xml_ns xml_find_first xml_attr
#' @export
extract_crs <- function(path) {
  # Check if the path is a directory
  if (dir.exists(path)) {
    # List all files in the directory
    files <- list.files(path, full.names = TRUE)

    # Check if there are any files in the directory
    if (length(files) == 0) {
      stop("No files found in the directory: ", path)
    }

    # Use the first file in the directory
    file_path <- files[1]
  } else {
    # Use the provided path as the file path
    file_path <- path
  }

  # Check if the file exists and is readable
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }
  if (!file.access(file_path, 4) == 0) {
    stop("File is not readable: ", file_path)
  }

  # Read the file as an XML document
  xml_doc <- tryCatch({
    read_xml(file_path)
  }, error = function(e) {
    stop("Failed to parse file: ", file_path, "\nError: ", e$message)
  })

  # Extract CRS from the <gml:Envelope> element
  srs_node <- xml_find_first(xml_doc, ".//gml:Envelope", ns = xml_ns(xml_doc))
  srs_name <- xml_attr(srs_node, "srsName", default = NA_character_)

  # Use the extract_epsg function to determine the EPSG code
  epsg_code <- extract_epsg(srs_name)

  if (is.na(epsg_code)) {
    stop("CRS could not be determined from the file!")
  }

  return(epsg_code)
}

#' Extract EPSG code from the srsName attribute
#'
#' This function extracts the EPSG code from a srsName string, supporting both
#' direct EPSG codes and known URN formats (e.g., "urn:adv:crs:ETRS89_UTM32").
#'
#' @param srs A character string containing the srsName attribute value
#'
#' @return The corresponding EPSG code, or NA if not found
#' @importFrom stringr str_match
#' @export
extract_epsg <- function(srs) {
  if (is.na(srs)) return(NA_integer_)

  # Direct EPSG match (e.g., "EPSG:25832")
  epsg_match <- str_match(srs, "EPSG:(\\d+)")
  if (!is.na(epsg_match[2])) return(as.integer(epsg_match[2]))

  # General pattern for URN formats (e.g., "urn:adv:crs:ETRS89_UTM32*DE_DHHN2016_NH")
  urn_match <- str_match(srs, "urn:[^:]+:crs:([^:*]+)")

  if (!is.na(urn_match[2])) {
    # Attempt to parse EPSG code if present in the URN (e.g., "ETRS89_UTM32")
    epsg_code <- parse_epsg_from_urn(urn_match[2])
    if (!is.na(epsg_code)) return(epsg_code)
  }

  # Return NA if no match was found
  warning("Unknown CRS format: ", srs)
  return(NA_integer_)
}

#' Parse EPSG code from URN-like formats
#'
#' This helper function parses the EPSG code from known URN formats like "ETRS89_UTM32".
#'
#' @param urn_segment A segment of the URN string (e.g., "ETRS89_UTM32")
#' @importFrom stringr str_match
#'
#' @return The corresponding EPSG code, or NA if no match is found
parse_epsg_from_urn <- function(urn_segment) {
  # General pattern matching for known coordinate systems in URNs
  epsg_match <- str_match(urn_segment, "(ETRS89_UTM32|ETRS89_UTM33)")

  # Match recognized UTM codes and return corresponding EPSG codes
  if (!is.na(epsg_match[2])) {
    switch(epsg_match[2],
           "ETRS89_UTM32" = return(25832),
           "ETRS89_UTM33" = return(25833))
  }

  # Return NA if no known EPSG match found
  return(NA_integer_)
}
