---
output:
  pdf_document: default
  html_document: default
---
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

⚠ **Limitations:**
This package **does not** account for **terrain elevation** or **vegetation shadows**. It assumes all buildings are placed on a **flat surface**.


## Installation

### From GitHub 
```r
# install.packages("remotes") # if not already installed
remotes::install_github("agneszwick/ShadowMapR")
```
## Example
### Use example file 
```r
# Load package
library(ShadowMapR)

# Process example file
building_sf <- get_example_data()
# >print(building_sf)
# EINFÜGEN!!!
```
**`get_example_data()`**
1. Set file path to example.xml
2. Extract CRS code from the example `extract_crs`
3. Following functions are for .xml-files (example.xml):
   - `extract_xml_polygons`: Creates data frame of building data from an XML file 
   - `convert_to_2D`: Creates simple feature of 2D building polygons
   - `clean_building_polygons`: Clean building polygon geometries by grouping and summarizing them

**Result:** Simple feature with *bldg_id*, *part_id*, *geometry*, *height* and *file* column

```r
visualize_buildings(building_sf)
```
This image is a screenshot of the interactive Leaflet map showing the visualization of the processed building data.

<img src="images/result_visualize_buildings.png" alt="Visualization Example" width="600" height="400">


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
**`sun_position(building_sf, time)`**
This function calculates the solar position for the provided building data and time. It takes an `sf` object containing building polygons, calculates the centroids of these polygons using the `calc_centroids` function, and then calculates the solar position for these centroids using the `calc_solar_pos` function.

**Input:**
- `building_sf`: An sf object containing building geometries.
- `time`: A POSIXct object representing the time for which to calculate the solar position.

**Output:**
- An `sf` object containing building geometries and solar positions (*sol_azimuth* and *sol_elevation*).


```r
building_offset <- calculate_all_shadows(building_sf)

# > head(building_offset)
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

plot(st_geometry(building_offset$shadow_geometry), col = "grey")
plot(st_geometry(building_offset), add = TRUE, col = "black", lwd = 2)

```
**`calculate_all_shadows`**
The function shifts the outline of a building by creating a vector for each corner in the direction of the shadow to create the shadow's corner points.
The vector is calculated based on the building's height, solar azimuth, and solar elevation. It returns an `sf` object containing the buildings with an additional column *shadow_geometry*.

**Input:**
- `buildings`: An `sf` object containing building geometries and attributes such as height, solar azimuth, and solar elevation.

**Output:**
- An `sf` object containing buildings with an additional column shadow_geometry representing the calculated shadow polygons for each building.




### Create shadow map with sun, shadow and building polygons
```r
shadow_map <- create_building_shadow_map(building_offset, time, batch_size = 100)
shadow_map
# This image is a screenshot of the interactive Leaflet map showing the visualization of the shadow and sunlight areas.
```
<img src="images/shadow_map.png" alt="Visualization Example" width="600" height="400">


### Use your own file(s)

```r
# Load package
library(ShadowMapR)

# Define file path
file_path <- "path/to/your/xml/or/gml/file"

# Extract crs of file
crs_code <- extract_crs(file_path)
print(crs)
# (e.g.) 25832 

# Process xml/gml file and create simple feature 
building_sf <- load_building_data(file_path, crs_code)

# Visualize building_sf in leaflet map
visualize_buildings(building_sf)
```
<<<<<<< Updated upstream
=======

### Calculate sun elevation and azimuth at defined time
```r
time <- as.POSIXct("yyyy-mm-dd hh:mm:ss", tz = "Europe/Berlin")
# e.g.
# time <- as.POSIXct("2025-02-18 15:00:00", tz = "Europe/Berlin")

building_sf <- sun_position(building_sf, time)
```
### Calculate building offset based on shadow length/position
```r
building_offset <- calculate_all_shadows(building_sf)
```

### Create shadow map with sun, shadow and building polygons
```r
shadow_map <- create_building_shadow_map(building_offset, time, batch_size = 100)
shadow_map
```
Functions
process_buildings
This function processes building geometries by checking for invalid geometries and filtering out empty ones. Invalid geometries are corrected using st_make_valid and empty geometries are removed. This ensures that the building geometries are valid and ready for further processing.

Input:

buildings: An sf object containing building geometries with a geometry column.
Output:

An sf object containing processed buildings with valid geometries.
create_shadow_polygons
This function creates shadow polygons from building geometries. It processes the buildings in batches, validates the geometries, and calculates the convex hull of the combined building and shadow geometries. The resulting shadow polygons are then dissolved into a single geometry.

Input:

buildings: An sf object containing processed buildings with valid geometries and shadow geometries.
batch_size: Integer number of buildings to process in each batch.
Output:

An sf object containing dissolved shadow polygons.
create_sunlight_areas
This function creates sunlight areas by subtracting the building and shadow geometries from a bounding box.

Input:

buildings: An sf object containing building polygons.
shadows: An sf object containing shadow polygons.
Output:

An sf object containing sunlight areas.
sun_position
This function calculates the solar position for the provided building data and time. It takes an sf object containing building polygons, calculates the centroids of these polygons using the calc_centroids function, and then calculates the solar position for these centroids using the calc_solar_pos function.

Input:

building_sf: An sf object containing building geometries.
time: A POSIXct object representing the time for which to calculate the solar position.
Output:

An sf object containing building geometries and solar positions.
calculate_all_shadows
This function calculates the shadow geometry for each building in an sf object based on the building's height, solar azimuth, and solar elevation. It returns an sf object containing the buildings with an additional column shadow_geometry representing the calculated shadow polygons for each building.

Input:

buildings: An sf object containing building geometries and attributes such as height, solar azimuth, and solar elevation.
Output:

An sf object containing buildings with an additional column shadow_geometry representing the calculated shadow polygons for each building.
create_building_shadow_map
This function creates a shadow map with sun, shadow, and building polygons.

Input:

building_offset: An sf object containing buildings with shadow geometries.
time: A POSIXct object representing the time for which to calculate the solar position.
batch_size: Integer number of buildings to process in each batch.
Output:

An sf object containing the shadow map.
License
This project is licensed under the GPL-3 License - see the LICENSE file for details.

Contributing
Please read CONTRIBUTING.md for details on our code of conduct, and the process for submitting pull requests to us.

Acknowledgments
Hat tip to anyone whose code was used
Inspiration
etc
>>>>>>> Stashed changes
