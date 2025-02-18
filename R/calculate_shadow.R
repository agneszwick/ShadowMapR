#' Calculate the shadow length based on building height and solar elevation angle
#'
#' @param measuredHeight Numeric value representing the measured height of the building (in meters)
#' @param solar_elevation_angle Numeric value representing the solar elevation angle (in degrees)
#' @return Numeric value representing the shadow length (in meters)
calculate_shadow_length <- function(measuredHeight, solar_elevation_angle) {
  # Convert the solar elevation angle from degrees to radians
  solar_elevation_radians <- solar_elevation_angle * (pi / 180)

  # Calculate the shadow length using the formula
  h <- measuredHeight / tan(solar_elevation_radians)

  return(h)
}

#' Calculate the shadow points of a building based on solar position and shadow length
#'
#' @param building sf object containing the geometry of a building (POLYGON geometry)
#' @param solar_pos Numeric vector of length 2, where the first element is the solar azimuth (in degrees)
#'                  and the second element is the solar elevation (in degrees)
#' @param shadow_length Numeric value representing the shadow length (in meters), calculated based on the building's height and solar elevation angle
#' @importFrom sf st_polygon st_sfc st_coordinates st_crs
#' @return sf object containing the shadow polygon geometry of the building
calculate_shadow_points <- function(building, solar_pos, shadow_length) {
  # Check if the building geometry is valid
  if (!inherits(building$geometry, "sfc_POLYGON")) {
    stop("The building geometry is not a valid POLYGON object.")
  }

  # Greife direkt auf die solar_pos zu oder auch direkt auf die entsprechenden Spalten
  # In diesem Beispiel verwenden wir solar_pos, wobei solar_pos[1] der Azimut und solar_pos[2] der Elevationwinkel ist.
  sol_azimuth <- solar_pos[1]
  sol_elevation <- solar_pos[2]

  # Berechne den Schatten-Azimut:
  # Der Schatten wird immer in die entgegengesetzte Richtung zum Sonnenstrahl geworfen.
  # Daher addieren wir 180 Grad zum Sonnenazimut und nehmen den Modulo 360, um den Wert in den Bereich 0-359 Grad zu bringen.
  shadow_azimuth <- (sol_azimuth + 180) %% 360

  # Um die trigonometrischen Funktionen in R zu verwenden, müssen wir den Azimut in Bogenmaß umrechnen.
  azimuth_radians_shadow <- shadow_azimuth * (pi / 180)

  # Extrahiere die Koordinaten des Gebäude-POLYGONS und entferne Duplikate sowie Z-Werte (falls vorhanden)
  points <- st_coordinates(building$geometry)
  points <- unique(points[, 1:2])

  # Initialisiere eine Liste für die Schattenpunkte
  shadow_points_list <- list()

  # Berechne für jeden Eckpunkt des Gebäudes den entsprechenden Schattenpunkt
  for (i in 1:nrow(points)) {
    # Aktueller Punkt
    point <- points[i, ]

    # Berechne den Schattenvektor:
    # Der Schattenvektor hat die Länge shadow_length und zeigt in die Richtung shadow_azimuth (in Bogenmaß).
    # Dabei gibt cos() den x-Anteil und sin() den y-Anteil.
    shadow_vector_x <- shadow_length * sin(azimuth_radians_shadow)
    shadow_vector_y <- shadow_length * cos(azimuth_radians_shadow)

    # Berechne den Schattenpunkt, indem du den Schattenvektor zu den Koordinaten des Punktes addierst.
    # (Je nachdem, ob der Schatten vom Gebäude weg oder in dessen Richtung liegen soll, kann hier subtrahiert oder addiert werden.
    # Hier nehmen wir an, dass der Schatten in die Richtung des Vektors liegt.)
    shadow_point <- point + c(shadow_vector_x, shadow_vector_y)

    # Füge den berechneten Schattenpunkt zur Liste hinzu
    shadow_points_list[[i]] <- shadow_point
  }

  # Konvertiere die Liste der Schattenpunkte in eine Matrix und schließe das Polygon (indem der erste Punkt erneut angehängt wird)
  shadow_matrix <- do.call(rbind, shadow_points_list)
  shadow_matrix <- rbind(shadow_matrix, shadow_matrix[1,])

  # Erstelle das Schattenpolygon aus der Matrix
  shadow_polygon <- st_polygon(list(shadow_matrix))

  return(st_sfc(shadow_polygon, crs = st_crs(building)))
}


#' Calculate shadow geometry for each building
#'
#' @param buildings sf object containing building geometries and attributes such as height, solar azimuth, and solar elevation
#' @return sf object containing buildings with an additional column `shadow_geometry` representing the calculated shadow polygons for each building
#' @importFrom sf st_sfc st_crs
calculate_all_shadows <- function(buildings) {
  # Überprüfe, ob die notwendigen Spalten vorhanden sind
  required_columns <- c("height", "sol_azimuth", "sol_elevation")
  missing_columns <- setdiff(required_columns, names(buildings))
  if (length(missing_columns) > 0) {
    stop(sprintf("Missing required columns: %s", paste(missing_columns, collapse = ", ")))
  }

  # Compute shadow geometry for each building
  shadow_geometries <- lapply(1:nrow(buildings), function(i) {
    current_building <- buildings[i, ]

    # Extrahiere Solarposition und Gebäudehöhe
    solar_azimuth <- current_building$sol_azimuth
    solar_elevation <- current_building$sol_elevation
    measured_height <- current_building$height

    # Step 1: Berechne die Schattenlänge
    # Wenn die Sonnenelevation <= 0 ist, gibt es keinen Schatten (Nacht oder Sonnenuntergang)
    if (solar_elevation <= 0) {
      return(NULL)
    }
    shadow_length <- calculate_shadow_length(measured_height, solar_elevation)

    # Step 2: Berechne die Schattenpunkte und gib die Geometrie des Schattens zurück
    tryCatch(
      calculate_shadow_points(current_building, c(solar_azimuth, solar_elevation), shadow_length),
      error = function(e) {
        message(sprintf("Error calculating shadow for building %s: %s", i, e$message))
        return(NULL)
      }
    )
  })

  # Konvertiere die Liste der Schattengeometrien in eine einzelne sfc-Spalte
  buildings$shadow_geometry <- st_sfc(do.call(c, shadow_geometries), crs = st_crs(buildings))

  return(buildings)
}
#
