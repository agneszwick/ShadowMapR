# ShadowMapR
ShadowMapR allows users to **calculate and visualize building shadows** based on sunlight exposure and shadowed areas using **XML or GML data**.

## Key Features
Designed for **3D building models** in **LoD0** and **LoD2** formats from Germany (*LoD0-DE and LoD2-DE*), this package provides functions to:

- Process `.xml` or `.gml` files containing building geometries (*POLYGON*) and height information
- Compute centroids of building geometries
- Determine solar position (*azimuth and elevation*) using `suntools`
- Calculate building shadows for a specific time or generate an hourly shadow progression over a day
- Generate an interactive map displaying sunlight and shadow areas using `leaflet`

## Usage
Users can analyze shadows at specific times or visualize their evolution throughout the day, making it ideal for urban planning, solar analysis, and environmental studies.

⚠ This package **does not** account for **terrain elevation** or **vegetation shadows**. It assumes all buildings are placed on a **flat surface**.

## Requirements

This package requires the following R packages:

- `dplyr`
- `future`
- `furrr`
- `ggplot2`
- `leaflet`
- `purrr`
- `sf`
- `stringr`
- `tibble`
- `tidyr`
- `xml2`
- `shinyWidgets`
- `suntools`
- `lubridate`
- `lwgeom`

## Limitations

- The file should contain no more than **5,000 buildings**, as processing times may exceed **2 minutes** for larger files. This was tested on a **Lenovo Ideapad SE40** with an **AMD Ryzen 7 3700U** CPU (4 cores, 8 threads, base clock 2.3 GHz, boost clock up to 4.0 GHz).
- If larger datasets need to be processed, it is recommended to filter the data beforehand or to distribute the computation across multiple cores (e.g., using `furrr::future_map()`).


## Installation

**From GitHub** 
```r
# List of required packages
packages <- c("devtools", "dplyr", "future", "furrr", "ggplot2", "leaflet", 
              "purrr", "remotes", "sf", "stringr", "shinyWidgets", "tibble", 
              "tidyr", "xml2", "lubridate", "lwgeom")

# Check which packages are missing
missing_packages <- packages[!(packages %in% installed.packages()[,"Package"])]

# Install only missing packages
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
} else {
  message("All packages are already installed!")
}

devtools::install_github("adokter/suntools")

remotes::install_github("agneszwick/ShadowMapR")

```
## Example 1: Building shadows for a specific time

```r
library(ShadowMapR)

# Use example file
building_sf <- get_example_data()

# # OR: Define file path
# file_path <- "path/to/your/xml/or/gml/file"

# # Extract CRS code
# crs_code <- extract_crs(file_path)

# # Process xml/gml file and create simple feature 
# building_sf <- load_building_data(file_path, crs_code)

# Visualize data  
visualize_buildings(building_sf)

# Define time 
time <- as.POSIXct("2025-02-18 15:00:00", tz = "Europe/Berlin")

# Calculate azimuth and elevation of sun at defined time
building_sf <- sun_position(building_sf, time)

# Calculate shadow offset
buildings_with_shadows <- building_offset(building_sf)

# Calculate shadow polygons
shadows <- shadow_polygons(buildings_with_shadows, batch_size=50)

# Create sunlight areas
sun_area <- create_sunlight_areas(building_sf, shadows)

# Display final map with sunny and shaded areas at defined time
create_shadow_map(building_sf, shadows, sun_area, time)
```

## Detailed Explanation 
### **`extract_crs`**

```r
# Load package
library(ShadowMapR)

# Define file path
file_path <- "path/to/your/xml/or/gml/file"

# Extract crs of file
crs_code <- extract_crs(file_path)
print(crs)
# (e.g.) 25832 
```


This function extracts the coordinate reference system (CRS) from a GML or XML file, reading the `srsName` attribute from the `<gml:Envelope>` element.

### **`load_building_data`**

```r
# Process xml/gml file and create simple feature 
building_sf <- load_building_data(file_path, crs_code)
```

@param *path*: Path to the directory or file containing the files.

@param *crs_code*: Character string. The coordinate reference system (CRS) code to be used.

- `.gml` file
  - `extract_gml_polygons`: reads a `.gml` file and extracts *geometry* and *measuredHeight* of buildings
  - @output: An `sf` with *part_id*, *geometry* and *height* column
 
- `.xml` file
  - `extract_xml_polygons`: Creates data frame of building data from an XML file
  - `convert_to_2D`: Creates simple feature of 2D building polygons
  - `clean_building_polygons`: Clean building polygon geometries by grouping and summarizing them
  - @output: An `sf` with *bldg_id*, *part_id*, *geometry*, *height* and *file* column

### **`get_example_data()`** 

```r
# Process example file
building_sf <- get_example_data()

# > print(building_sf)
# Simple feature collection with 91 features and 4 fields
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 370231.4 ymin: 5808336 xmax: 370921.3 ymax: 5808949
# Projected CRS: ETRS89 / UTM zone 33N
# # A tibble: 91 × 5
#    part_id                                                   geometry height bldg_id file 
#  * <chr>                                                <POLYGON [m]>  <dbl> <chr>   <chr>
#  1 DEBE06YYD00000lo_01 ((370455.6 5808769, 370461 5808772, 370462.2 …   3.37 DEBE06… exam…
#  2 DEBE06YYD00001AQ    ((370571.2 5808740, 370584.9 5808743, 370587.…   8.00 DEBE06… exam…
#  3 DEBE06YYD00001Ai_01 ((370551.1 5808764, 370559.8 5808766, 370562.…   7.99 DEBE06… exam…
#  4 DEBE06YYD00001Gw    ((370538.5 5808760, 370547.2 5808763, 370549.…   8.50 DEBE06… exam…
#  5 DEBE06YYD00001hw_01 ((370555 5808436, 370560 5808443, 370589.2 58…  21.2  DEBE06… exam…
#  6 DEBE06YYD00001hw_02 ((370582.5 5808413, 370589 5808423, 370600.5 …   5.27 DEBE06… exam…
#  7 DEBE06YYD00001hw_03 ((370574.9 5808452, 370581.2 5808461, 370582.…   7.49 DEBE06… exam…
#  8 DEBE06YYD00001hw_04 ((370584.6 5808399, 370590.2 5808407, 370606.…  12.2  DEBE06… exam…
#  9 DEBE06YYD00001hw_05 ((370554.2 5808439, 370557.6 5808444, 370558.…  10.2  DEBE06… exam…
# 10 DEBE06YYD00002gi    ((370395.3 5808737, 370395.5 5808738, 370396 …   6.37 DEBE06… exam…
# # ℹ 81 more rows
# # ℹ Use `print(n = ...)` to see more rows

```
This function sets the file path to example.xml, extracts the CRS code from the example `extract_crs` and runs `load_building_data`.

```r
visualize_buildings(building_sf)
```
This image is a screenshot of the interactive Leaflet map showing the visualization of the processed building data.

<img src="images/visualize_buildings.png" alt="Visualization Example">

### **`sun_position(building_sf, time)`**
```r
time <- as.POSIXct("yyyy-mm-dd hh:mm:ss", tz = "Europe/Berlin")
# time <- as.POSIXct("2025-02-18 15:00:00", tz = "Europe/Berlin")

building_sf <- sun_position(building_sf, time)

# > head(building_sf)
# Simple feature collection with 6 features and 8 fields
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 370455.6 ymin: 5808405 xmax: 370600.5 ymax: 5808772
# Projected CRS: ETRS89 / UTM zone 33N
# A tibble: 6 × 9
#  part_id                   geometry height bldg_id file  sol_azimuth sol_elevation centroid_lon
#  <chr>                <POLYGON [m]>  <dbl> <chr>   <chr>       <dbl>         <dbl>        <dbl>
# 1 DEBE06Y… ((370455.6 5808769, 3704…   3.37 DEBE06… exam…        221.          17.7         13.1
# 2 DEBE06Y… ((370571.2 5808740, 3705…   8.00 DEBE06… exam…        221.          17.7         13.1
# 3 DEBE06Y… ((370551.1 5808764, 3705…   7.99 DEBE06… exam…        221.          17.7         13.1
# 4 DEBE06Y… ((370538.5 5808760, 3705…   8.50 DEBE06… exam…        221.          17.7         13.1
# 5 DEBE06Y… ((370555 5808436, 370560…  21.2  DEBE06… exam…        221.          17.7         13.1
# 6 DEBE06Y… ((370582.5 5808413, 3705…   5.27 DEBE06… exam…        221.          17.7         13.1
# # ℹ 1 more variable: centroid_lat <dbl>
```
This function calculates the solar position for the provided building data and time. 

@param *building_sf*: An sf object containing building geometries.

@param *time*: A POSIXct object representing the time for which to calculate the solar position.

- `calc_centroids`: Calculates centroids of sf polygons
- `calc_solar_pos`: Calculates solar position (*sol_azimuth* and *sol_elevation*) of the centroids
  
@output: An `sf` object containing building geometries and solar positions (*sol_azimuth* and *sol_elevation*).

### **`building_offset`**

```r
buildings_with_shadows <- building_offset(building_sf)

# > head(buildings_with_shadows)
# Simple feature collection with 6 features and 8 fields
# Active geometry column: geometry
# Geometry type: POLYGON
# Dimension:     XY
# Bounding box:  xmin: 370455.6 ymin: 5808405 xmax: 370600.5 ymax: 5808772
# Projected CRS: ETRS89 / UTM zone 33N
# # A tibble: 6 × 10
#   part_id                                                                                     geometry height bldg_id file  sol_azimuth sol_elevation centroid_lon centroid_lat           shadow_geometry
#   <chr>                                                                                  <POLYGON [m]>  <dbl> <chr>   <chr>       <dbl>         <dbl>        <dbl>        <dbl>             <POLYGON [m]>
# 1 DEBE06YYD00000lo_01 ((370455.6 5808769, 370461 5808772, 370462.2 5808770, 370456.7 5808767, 370455.…   3.37 DEBE06… exam…        221.          17.7         13.1         52.4 ((370462.5 5808777, 3704…
# 2 DEBE06YYD00001AQ    ((370571.2 5808740, 370584.9 5808743, 370587.4 5808734, 370573.7 5808731, 37057…   8.00 DEBE06… exam…        221.          17.7         13.1         52.4 ((370587.6 5808759, 3706…
# 3 DEBE06YYD00001Ai_01 ((370551.1 5808764, 370559.8 5808766, 370562.3 5808758, 370561.7 5808757, 37056…   7.99 DEBE06… exam…        221.          17.7         13.1         52.4 ((370567.5 5808783, 3705…
# 4 DEBE06YYD00001Gw    ((370538.5 5808760, 370547.2 5808763, 370549.8 5808753, 370541.2 5808751, 37053…   8.50 DEBE06… exam…        221.          17.7         13.1         52.4 ((370556 5808780, 370564…
# 5 DEBE06YYD00001hw_01 ((370555 5808436, 370560 5808443, 370589.2 5808423, 370584.3 5808416, 370555 58…  21.2  DEBE06… exam…        221.          17.7         13.1         52.4 ((370598.6 5808486, 3706…
# 6 DEBE06YYD00001hw_02 ((370582.5 5808413, 370589 5808423, 370600.5 5808415, 370593.4 5808405, 370590.…   5.27 DEBE06… exam…        221.          17.7         13.1         52.4 ((370593.3 5808426, 3705…

plot(st_geometry(buildings_with_shadows$shadow_geometry), col = "grey")
plot(st_geometry(buildings_with_shadows), add = TRUE, col = "black", lwd = 2)
```
<img src="images/building_offset.png" alt="Building offset">

This function computes a shadow offset for each building in an `sf` object based on the building's height, solar azimuth, and solar elevation. It applies a shadow vector of the correct length and angle to each corner of the building, generating a new polygon that represents the shadow projection.

@param *buildings*: sf object containing building geometries and attributes such as *height*, *solar azimuth*, and *solar elevation*.

@output: An `sf` object containing buildings with an additional column *shadow_geometry* representing the calculated shadow polygons.


### **`shadow_polygons`**

```r
shadows <- shadow_polygons(buildings_with_shadows, batch_size=50)

# > print(shadows)
# Simple feature collection with 1 feature and 0 fields
# Geometry type: MULTIPOLYGON
# Dimension:     XY
# Bounding box:  xmin: 370231.4 ymin: 5808336 xmax: 370712.8 ymax: 5808923
# Projected CRS: ETRS89 / UTM zone 33N
#                         geometry
# 1 MULTIPOLYGON (((370415.7 58...


plot(st_geometry(shadows), col = "grey")
plot(st_geometry(building_sf), add = TRUE, col = "black", lwd = 2)
```
<img src="images/shadow_polygons.png" alt="Shadow Polygons">

This function processes building geometries, computes individual shadow hulls for each building, and then dissolves multiple shadow polygons into a single unified geometry.

@param *buildings*: An sf object containing building geometries and attributes such as *height*, *solar azimuth*, and *solar elevation*.

@param *batch_size* An integer value representing the batch size to process the shadow hulls in parts

@output: An `sf` object with a single geometry representing the union of all shadow polygons.

- `process_buildings`: Processes building and shadow geometries by checking for invalid geometriesand filtering out empty ones
- `compute_shadow_hulls`: Calculates the convex hull of the building and shadow geometries


### **`create_sunlight_areas`**

```r
sun_area <- create_sunlight_areas(building_sf, shadows)

# > print(sun_area)
# Geometry set for 1 feature 
# Geometry type: MULTIPOLYGON
# Dimension:     XY
# Bounding box:  xmin: 370231.4 ymin: 5808336 xmax: 370712.8 ymax: 5808923
# Projected CRS: ETRS89 / UTM zone 33N
# MULTIPOLYGON (((370231.4 5808336, 370231.4 5808...

plot(st_geometry(sun_area), col = "yellow")
plot(st_geometry(shadows), add=T, col = "grey")
plot(st_geometry(buildings_with_shadows), add = TRUE, col = "black", lwd = 2)
```
<img src="images/create_sun_areas.png" alt="Shadow Polygons">

This function creates sunlight areas by subtracting the building and shadow geometries from a bounding box.

### **`create_shadow_map`**

```r
create_shadow_map(building_sf, shadows, sun_area, time)
```
This image is a screenshot of the interactive Leaflet map displaying shadows, sunlight areas and buildings.
<img src="images/create_shadow_map.png" alt="Shadow Polygons">

This function generates an interactive map using the `leaflet` package to visualize buildings, shadows, and sunlight areas.


## Example 2: Generate an hourly shadow progression over a day
```r
library(ShadowMapR)

# Use example file
building_sf <- get_example_data()

# OR: Define file path
# file_path <- "path/to/your/xml/or/gml/file"

# Extract CRS code
# crs_code <- extract_crs(file_path)

# # Process xml/gml file and create simple feature
# building_sf <- load_building_data(file_path, crs_code)

# Visualize data
visualize_buildings(building_sf)

# Define date
date <- as.Date("2025-02-24")

# Calculate solar data for the day
solar_results <- calculate_solar_day(building_sf, date)

# Calculate hourly shadow offset
shadows <- calculate_hourly_shadow_offset(solar_results)

# Create hourly shadow polygons and sunlight areas
shadow_polygons <- create_hourly_shadow_polygons(shadows, batch=50)
sunlight_areas <- create_hourly_sunlight_areas(shadow_polygons, building_sf)

# Precompute map layers
map_layer <- precompute_map_layers(building_sf, shadow_polygons, sunlight_areas)

# Run interactive shadow map
hourly_shadow_progression(map_layer, date)
```

This image is a screenshot of the Shiny application.

<img src="images/hourly_shadow_progression.png" alt="Shadow Polygons">


In **Example 2**, each function is essentially the same as in **Example 1**. The key difference is that all the calculations are done for each hour of the day.

1. **`calculate_solar_day`**:  
   - The solar position (solar elevation and azimuth) is computed for each hour of the day using the `sun_position` function.

2. **`calculate_hourly_shadow_offset`**:  
   - Shadow offsets are calculated for each hour, accounting for changes in the solar position using the `building_offset` function.
   - This gives the shadow cast by buildings for each hour of the day.

3. **`create_hourly_shadow_polygons`**:  
   - This function creates the shadow cast by buildings for each hour of the day using the `shadow_polygons` function.

4. **`create_hourly_sunlight_areas`**:  
   - Calculates which areas remain in sunlight at each hour using the `create_sunlight_areas` function.

5. **`precompute_map_layers`**:  
   - Combines the hourly data for buildings, shadows, and sunlight into a structured map layer for each hour.
   - This results in a list of map layers, with each layer representing a specific hour of the day.

6. **`hourly_shadow_progression`**:  
   - The final output is a **Shiny application** that allows users to visualize the shadow progression over the course of a full day.
   - The application includes:
     - A **slider** to allow users to move through each hour of the day.
     - An **interactive map** that shows how the shadows and sunlight areas change as the day progresses.
    

