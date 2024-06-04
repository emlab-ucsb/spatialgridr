#' Create a spatial grid
#'
#' @description Creates a spatial grid, in `terra::rast()` of `sf` format, for areas within the boundaries provided
#'
#' @details
#' For a `terra::rast()` format grid, each cell within the boundary is set to value 1, whereas cells outside are NA. For `sf` grids, the grid covers the boundary area and cells can be square (`output = "sf_square"`) or hexagonal (`output = "sf_hex"`). By default only cells whose centroids fall within the `boundary` are included in the grid. To include cells that touch the `boundary` use `touches = TRUE`.
#' `sf::st_make_grid()` is used to create `sf` grids. The default ordering of this grid type is from bottom to top, left to right. In contrast, the `terra::rast()` grid is ordered from top to bottom, left to right. To preserve consistency across the data types, we have reordered `sf` grids to also fill from top to bottom, left to right.
#'
#' @param boundary `sf` object with boundary of the area(s) you want a grid for, e.g an EEZ or country. Boundaries can be obtained using `get_boundary()`
#' @param resolution `numeric`; the desired grid cell resolution in same units (usually metres or degrees) as `crs`: `sf::st_crs(crs, parameters = TRUE)$units_gdal`
#' @param crs a coordinate reference system: `numeric` (e.g. EPSG code `4326`), `character` (e.g. `"+proj=longlat +datum=WGS84 +no_defs"`), or object of class `sf` or `sfc`. This is passed to `sf::st_crs()`
#' @param output `character` the desired output format, either `"raster"`, `"sf_square"` (vector), or `"sf_hex"` (vector); default is `"raster"`
#' @param touches `logical`. If `TRUE` all cells touched by the `boundary` will be included, and if `FALSE` only cells whose center point (centroid) falls within the `boundary` are included. Default is `FALSE`.
#'
#' @return A spatial grid in `output` format requested with `resolution` and `crs` provided
#' @export
#'
#' @examples
#' # use get_boundary() to get a polygon of Samoa's Exclusive Economic Zone
#' samoa_eez <- get_boundary(name = "Samoa")
#' # You need a suitable coordinate reference system (crs) for your area of interest, https://projectionwizard.org is useful for this purpose. For spatial planning, equal area projections are normally best.
#' samoa_projection <- '+proj=laea +lon_0=-172.5 +lat_0=0 +datum=WGS84 +units=m +no_defs'
#' # Create a grid with 5 km (5000 m) resolution covering the `samoa_eez` in a projection specified by `crs`.
#' samoa_grid <- get_grid(boundary = samoa_eez, resolution = 5000, crs = samoa_projection)

get_grid <- function(boundary, resolution = 5000, crs, output = "raster", touches = FALSE){

  if(!check_sf(boundary)) {
    stop("boundary must be an sf object")}

  if(!(output %in% c("raster", "sf_square", "sf_hex"))) stop("output must be either 'raster', 'sf_square' or 'sf_hex'")

  chosen_crs <- sf::st_crs(crs)

  boundary <- boundary %>%
    sf::st_geometry() %>%
    sf::st_sf() %>%
    {if(sf::st_crs(boundary) == chosen_crs) . else sf::st_transform(., chosen_crs)}

  if(output == "raster") {
    boundary %>%
      terra::rast(resolution = resolution) %>%
      terra::rasterize(boundary, ., touches = touches, field = 1)

  } else{
    grid_out <- if(output == "sf_square") sf::st_make_grid(boundary, cellsize = resolution, square = TRUE) %>% sf::st_sf() else sf::st_make_grid(boundary, cellsize = resolution, square = FALSE) %>% sf::st_sf()

    if (touches){
      grid_intersect <- grid_out
    } else{
      grid_intersect <- sf::st_centroid(grid_out)
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
