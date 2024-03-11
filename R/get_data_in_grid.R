#' Get gridded or cropped data from input data
#'
#' @param area_polygon `sf` polygon
#' @param spatial_grid `sf` or `terra::rast()` planning grid
#' @param dat `sf` or `terra::rast()` data to be gridded/ cropped
#' @param meth `character` method to use for for gridding/ resampling/ reprojecting raster data. If NULL (default), function checks if data values are binary (all 0, 1, NA, or NaN) in which case method is set to "mode" for sf output or "near" for raster output. If data is non-binary, method is set to "average" for sf output or "mean" for raster output. Note that different methods are used for sf and raster as `exactextractr::exact_extract()` is used for gridding to sf planning grid, whereas `terra::project()`/`terra::resample()` is used for transforming/ gridding raster data.
#' @param name `character` to name the data output
#' @param sf_col_layer_names `character` vector; name(s) of columns that contain the data to be gridded/ cropped in `sf` input data. If NULL, `sf` data is assumed to represent a single features, e.g. one habitat or species.
#' @param antimeridian `logical` can be set to true if the data to be extracted crosses the antimeridian and is in lon-lat (EPSG:4326) format. If set to `NULL` (default) the function will try to check if data spans the antimeridian and set this appropriately.
#' @param cov_fraction `numeric` cover fraction value between 0 and 1; if sf data is input and gridded data is required (i.e. a `spatial_grid` is provided), how much of each grid cell should be covered by an sf feature for it to be classified as that feature type
#'
#' @param return_cov_frac `logical` used only if sf data is input and gridded data is required (i.e. a `spatial_grid` is provided). If `TRUE` will return an `sf` object with the % coverage of each feature in each grid cell. `sf_col_layer_names` should be provided.
#'
#' @return `sf` or `terra::rast()` object; cropped and intersected data in same format as `dat` if  an `area_polygon` is provided, otherwise `sf` or `terra::rast()` gridded data depending on the format of the planning grid provided
#'
#' @export
#'
#' @examples
#' # knolls data for area of Pacific
#' knolls <- system.file("extdata", "ridges.rds", package = "spatialgridr") |> readRDS()
#' # an area of interest, in this case Samoa's Exclusive Economic Zone
#' samoa_eez <- system.file("extdata", "samoa_eez.rds", package = "spatialgridr") |> readRDS()
#'
#' # You need a suitable projection for your area of interest, https://projectionwizard.org is useful for this purpose. For spatial planning, equal area projections are normally best.
#' samoa_projection <- '+proj=laea +lon_0=-172.5 +lat_0=0 +datum=WGS84 +units=m +no_defs'
#'
#' # Create a planning grid with 5km sized planning units
#' planning_grid <- get_grid(area_polygon = samoa_eez, projection_crs = samoa_projection, resolution = 5000)
#' # Get knolls data, which is vector data in sf format, in the planning grid
#' knolls_gridded <- get_data_in_grid(spatial_grid = planning_grid, dat = knolls)
#' terra::plot(knolls_gridded)
#'
#' #Get some raster data on cold water corals for the same planning grid
#' cold_coral <- system.file("extdata", "cold_coral.tif", package = "spatialgridr") |> terra::rast()
#' coral_gridded <- get_data_in_grid(spatial_grid = planning_grid, dat = cold_coral)
#' terra::plot(coral_gridded)
get_data_in_grid <- function(area_polygon = NULL, spatial_grid = NULL, dat = NULL, meth = NULL, name = NULL, sf_col_layer_names = NULL, antimeridian = NULL, cov_fraction = 0.5, return_cov_frac = FALSE){
  if(is.null(dat)){
    stop("Please provide some input data")
  }
  check_grid_or_polygon(spatial_grid, area_polygon)

  dat <- data_from_filepath(dat)

  matching_crs <- check_matching_crs(area_polygon, spatial_grid, dat)

  antimeridian <- if(is.null(antimeridian)){
    sf_object <- if(is.null(spatial_grid)) area_polygon else{
      if(check_sf(spatial_grid)) spatial_grid else terra::as.polygons(spatial_grid) %>% sf::st_as_sf()
    }
    check_antimeridian(sf_object)
  } else antimeridian

#setting method for resampling, projecting, etc. a raster - should be 'near' for binary raster otherwise end up with non-binary values
#previously checking for unique values 0,1,NA, NaN but this is time consuming for global raster so get user to define if binary or not

  raster_cell_no_threshold <- 1e4

  if(!is.null(meth)){
    meth <- meth
  } else if(check_raster(dat)){
      meth <- dat %>%
        #take a sample if it is a large raster, and assume that no more than 50% of cells are NA otherwise this will fail
        {if(terra::ncell(dat)> raster_cell_no_threshold) terra::spatSample(., size = raster_cell_no_threshold/2, na.rm = TRUE) else terra::values(.)} %>%
        unlist() %>%
        unique() %>%
        {if(all(. %in% c(0,1,NA,NaN))) {
          if(check_raster(spatial_grid)) 'near' else 'mode'
        } else {
            if(check_raster(spatial_grid)) 'average' else 'mean'
          }
    }
  }

  if(!is.null(area_polygon)){
    get_raw_data(area_polygon, dat, meth, matching_crs, antimeridian)

  } else if(check_raster(dat)){
    ras_to_grid(dat, spatial_grid, matching_crs, meth, name, antimeridian)
    } else {
    sf_to_grid(dat, spatial_grid, matching_crs, name, sf_col_layer_names, antimeridian, cov_fraction, return_cov_frac)
  }

}
