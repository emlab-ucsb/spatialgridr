#' Create a spatial grid
#'
#' @description Creates a spatial grid, in `terra::rast()` of `sf` format, for areas within the boundaries provided
#'
#' @details
#' This function uses `sf::st_make_grid()` to create `sf` grids. The default ordering of this grid type is from bottom to top, left to right. In contrast, the `terra::rast()` grid is ordered from top to bottom, left to right. To preserve consistency across the data types, we have reordered `sf` grids to also fill from top to bottom, left to right.
#'
#' @param boundary `sf` object with boundary of the area(s) you want a grid for, e.g an EEZ or country. Boundaries can be obtained using `get_boundary()`
#' @param projection_crs a suitable crs for the area of interest
#' @param option the desired output format, either "raster", "sf_square" (vector), or "sf_hex" (vector); default is "raster"
#' @param resolution `numeric`; the desired grid cell resolution in units (usually metres or degrees) of the projection_crs: `sf::st_crs(projection_crs, parameters = TRUE)$units_gdal`
#' @param sf_method `string`. Only for `sf` grids:
#' * `"centroid"` a cell will be included in the grid if the centroid of the cell falls within the `boundary`, or if there is any `"overlap"` with the boundary. `"overlap"` will be significantly slower.
#'
#' @return A `terra::rast()` of `sf` grid with resolution and crs provided
#' @export
#'
#' @examples
#' # use get_boundary() to get a polygon of Samoa's Exclusive Economic Zone
#' samoa_eez <- get_boundary(name = "Samoa")
#' # You need a suitable projection for your area of interest, https://projectionwizard.org is useful for this purpose. For spatial planning, equal area projections are normally best.
#' samoa_projection <- '+proj=laea +lon_0=-172.5 +lat_0=0 +datum=WGS84 +units=m +no_defs'
#' # Create a grid with 5 km (5000 m) resolution covering the `samoa_eez` in a projection specified by `projection_crs`.
#' samoa_grid <- get_grid(boundary = samoa_eez, projection_crs = samoa_projection, resolution = 5000)

get_grid <- function(boundary, projection_crs, option = "raster", resolution = 5000, sf_method = "centroid"){

  # Add repeated errors for boundary
  if(!check_sf(boundary)) {
    stop("boundary must be an sf object")}

  if(!(option %in% c("raster", "sf_square", "sf_hex"))) stop("option must be either 'raster', 'sf_square' or 'sf_hex'")

  boundary <- boundary %>%
    sf::st_geometry() %>%
    sf::st_sf() %>%
    {if(sf::st_crs(boundary) == projection_crs) . else sf::st_transform(., projection_crs)}

  if(option == "raster") {
    boundary %>%
      terra::rast(resolution = resolution) %>%
      terra::rasterize(boundary, ., touches=FALSE, field = 1)

  } else{
    grid_out <- if(option == "sf_square") sf::st_make_grid(boundary, cellsize = resolution, square = TRUE) %>% sf::st_sf() else sf::st_make_grid(boundary, cellsize = resolution, square = FALSE) %>% sf::st_sf()

    if (sf_method == "centroid"){
      grid_intersect <- sf::st_centroid(grid_out)
    } else if (sf_method == "overlap"){
      grid_intersect <- grid_out
    }

    overlap <- sf::st_intersects(grid_intersect, boundary) %>%
      lengths() > 0
    grid_out[overlap,] %>%
      dplyr::bind_cols(sf::st_coordinates(sf::st_centroid(.)) %>%
                         as.data.frame() %>%
                         dplyr::select("X", "Y")) %>%
      dplyr::mutate(X = round(.data$X, digits = 4),
                    Y = round(.data$Y, digits = 4)) %>%
      dplyr::arrange(dplyr::desc(.data$Y), .data$X) %>%
      dplyr::select(-"X", -"Y")
  }
}
