#' Get gridded or cropped data from input data
#'
#' @param boundary `sf` object with boundary of the area(s) you want gridded data for, e.g an EEZ or country. Boundaries can be obtained using `get_boundary()`
#' @param spatial_grid `sf` or `terra::rast()` grid created using `get_grid()`
#' @param dat `sf` or `terra::rast()` data to be gridded/ cropped
#' @param meth `character` method to use for for gridding/ resampling/ reprojecting raster data. If NULL (default), function checks if data values are binary (all 0, 1, NA, or NaN) in which case method is set to "mode" for sf output or "near" for raster output. If data is non-binary, method is set to "average" for sf output or "mean" for raster output. Note that different methods are used for sf and raster as `exactextractr::exact_extract()` is used for gridding to sf spatial grid, whereas `terra::project()`/`terra::resample()` is used for transforming/ gridding raster data.
#' @param name `character` to name the data output; unless `feature_names` is supplied, in which case that column is used as the feature names
#' @param feature_names `character` (`sf` data only) column with feature names that will be used for grouping of input data. If NULL, `sf` data is assumed to represent a single features, e.g. one habitat or species.
#' @param antimeridian `logical` can be set to true if the `boundary` or `spatial_grid` for which data will be extracted crosses the antimeridian and the data source is in lon-lat (EPSG:4326) format. If set to `NULL` (default) the function will try to check if the antimeridian is crossed and set this appropriately. Note that if you are using an `boundary` or `spatial_grid` that crosses the antimeridian and have data that is not in lon-lat
#' @param cutoff `numeric` for `sf` gridded data only, i.e. an `sf` `spatial_grid` is provided. How much of each grid cell should be covered by an `sf` feature for it to be classified as that feature type (cover fraction value between 0 and 1). For example, if `cutoff = 0.5` (default), at least half of each grid cell has to be covered by a feature for the cell to be classified as that feature. If `NULL`, the % coverage of each feature in each grid cell is returned
#'
#' @param apply_cutoff `logical` (`sf` data only) if gridded output is required (i.e. a `spatial_grid` is provided), `FALSE` will return an `sf` object with the % coverage of each feature in each grid cell, as opposed to a binary presence/ absence. `feature_names` should be provided.
#'
#' @return `sf` or `terra::rast()` object; cropped and intersected data in same format as `dat` if  an `boundary` is provided, otherwise `sf` or `terra::rast()` gridded data depending on the format of the spatial grid provided
#'
#' @export
#'
#' @examples
#' # ridges data for area of Pacific
#' ridges <- system.file("extdata", "ridges.rds", package = "spatialgridr") |> readRDS()
#' # use get_boundary() to get Samoa's Exclusive Economic Zone
#' samoa_eez <- get_boundary(name = "Samoa")
#'
#' # You need a suitable projection for your area of interest, https://projectionwizard.org is useful for this purpose. If you are doing spatial planning, equal area projections are normally best.
#' samoa_projection <- '+proj=laea +lon_0=-172.5 +lat_0=0 +datum=WGS84 +units=m +no_defs'
#'
#' # Create a spatial grid with 5km square cells
#' samoa_grid <- get_grid(boundary = samoa_eez, resolution = 5000, crs = samoa_projection)
#' # Get ridges data, which is vector data in sf format, in the spatial grid
#' ridges_gridded <- get_data_in_grid(spatial_grid = samoa_grid, dat = ridges)
#' terra::plot(ridges_gridded)
#'
#' #Get some raster data on cold water corals for the same spatial grid
#' cold_coral <- system.file("extdata", "cold_coral.tif", package = "spatialgridr") |> terra::rast()
#' coral_gridded <- get_data_in_grid(spatial_grid = samoa_grid, dat = cold_coral)
#' terra::plot(coral_gridded)
get_data_in_grid <- function(spatial_grid = NULL, dat = NULL, raw = FALSE, meth = NULL, name = NULL, feature_names = NULL, antimeridian = NULL, cutoff = 0.5){
  if(is.null(dat)){
    stop("Please provide some input data")
  }
  check_grid_or_polygon(spatial_grid, boundary)

  dat <- data_from_filepath(dat)

  matching_crs <- check_matching_crs(boundary, spatial_grid, dat)

  antimeridian <- if(is.null(antimeridian)){
    sf_object <- if(is.null(spatial_grid)) boundary else{
      if(check_sf(spatial_grid)) spatial_grid else terra::as.polygons(spatial_grid) %>% sf::st_as_sf()
    }
    check_antimeridian(sf_object, dat)
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

  if(!is.null(boundary)){
    get_raw_data(boundary, dat, meth, matching_crs, antimeridian)

  } else if(check_raster(dat)){
    ras_to_grid(dat, spatial_grid, matching_crs, meth, name, antimeridian)
    } else {
    sf_to_grid(dat, spatial_grid, matching_crs, name, feature_names, antimeridian, cutoff, apply_cutoff)
  }

}
