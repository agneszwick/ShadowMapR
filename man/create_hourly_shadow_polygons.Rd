% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hourly_shadow_progression.R
\name{create_hourly_shadow_polygons}
\alias{create_hourly_shadow_polygons}
\title{Create Hourly Shadow Polygons}
\usage{
create_hourly_shadow_polygons(shadows, batch_size = 50)
}
\arguments{
\item{shadows}{A list of shadows for each hour, where each element corresponds to shadow
data for a specific hour. The shadows should have been calculated previously.}

\item{batch_size}{An integer specifying the number of buildings to process in each batch.
The default value is 50. Adjusting this value may improve performance depending
on the size of the dataset.}
}
\value{
A named list of shadow polygons for each hour. Each element in the list corresponds
        to an hour and contains the resulting shadow polygons calculated from the shadow data.
}
\description{
This function creates shadow polygons for each hour based on the provided shadow data.
It processes the shadows in batches, and for each batch, it calculates the corresponding
shadow polygons using the `shadow_polygons` function.
}
