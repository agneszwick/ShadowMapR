# ShadowMapR
This package enables users to calculate and visualize building shadows, including sunlight exposure and shadowed areas, using XML data and geographical information.

It is specifically designed for 3D building models in LoD0 and LoD2 formats from Germany (LoD0-DE and LoD2-DE). The package provides functions to:

Calculate centroids of building geometries
Determine the solar position for these centroids
Generate and visualize shadow maps
Users can compute shadows for specific times or create an hourly progression of shadows throughout the day. The package also includes tools for interactive visualization of shadow maps using the {leaflet} package.

⚠ Limitations: This package does not account for terrain elevation or vegetation shadows. It assumes all buildings are placed on a flat surface.



## Installation

### From CRAN
```r
install.packages("yourpackage")
