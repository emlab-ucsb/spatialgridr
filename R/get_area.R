#' Get polygon(s) for an area of interest
#'
#' Marine and land areas can be obtained.  For marine areas, the `mrp_get` function from the `mregions2` package is used to retrieve an area (e.g. an EEZ) from [Marine Regions](https://marineregions.org/gazetteer.php). For land areas, the package [`rnaturalearth`](https://github.com/ropensci/rnaturalearth/) is used.
#'
#' @param name `character` name of the country or area. If `NULL` all areas are returned. If an incorrect `name` is input, a list of all possible names will be provided along with the error message.
#' @param type `character` the area type. Can be one of:
#' * `eez`: Exclusive Economic Zone (EEZ; 200nm), differ slightly from the the [UN Convention on the Law of the Sea (UNCLOS)](https://www.un.org/depts/los/convention_agreements/texts/unclos/part5.htm) definition because the archipelagic waters and the internal waters of a country are included
#' * `12nm`: 12 nautical miles zone (Territorial Seas), defined in [UNCLOS](https://www.un.org/Depts/los/convention_agreements/texts/unclos/part2.htm)
#' * `24nm`: 14 nautical miles zone (Contiguous Zone), defined in [UNCLOS](https://www.un.org/Depts/los/convention_agreements/texts/unclos/part2.htm)
#' * `ocean`: Global Oceans and Seas as compiled by the Flanders Marine Data Centre. Names are: "Arctic Ocean", "Baltic Sea", "Indian Ocean", "Mediterranean Region", "North Atlantic Ocean", "North Pacific Ocean", "South Atlantic Ocean", "South China and Easter Archipelagic Seas", "South Pacific Ocean", and "Southern Ocean".
#' * `countries`: country boundaries
#'
#' More details on the marine areas can be found on the [Marine Regions website](https://marineregions.org/sources.php), and for land area, the [Natural Earth website](https://www.naturalearthdata.com/features/). Note that this function retrieves data from Natural Earth at the highest resolution (1:10m).
#'
#' @param country_type `character` must be either `country` or `sovereign`. Some countries have many territories that it has jurisdiction over. For example, Australia, France and the U.K. have jurisdiction over many overseas islands. Using `sovereign` returns the main country and all the territories, whereas using `country` returns just the main country. More details about what is a country via the `rnaturalearth` package [vignette](https://cran.r-project.org/web/packages/rnaturalearth/vignettes/what-is-a-country.html)
#'
#' @return 'sf' object of the area requested
#' @export
#'
#' @examples
#' #Marine area examples:
#' if(require("mregions2")){
#'australia_mainland_eez <- get_area(name = "Australia")
#'plot(australia_mainland_eez["geometry"])
#'
#'#this includes all islands that Australia has jurisdiction over:
#'australia_including_territories_eez <- get_area(name = "Australia", country_type = "sovereign")
#'plot(australia_including_territories_eez["geometry"])
#' }
#'
#'#Land area examples:
#'if(require("rnaturalearth")){
#'australia_land <- get_area(name = "Australia", type = "countries")
#'plot(australia_land["geometry"])
#'
#'#this includes all islands that Australia has jurisdiction over:
#'australia_land_and_territories <- get_area(name = "Australia", type = "countries", country_type = "sovereign")
#'plot(australia_land_and_territories["geometry"])
#' }
get_area <- function(name = "Australia", type = "eez", country_type = "country"){
  # initial query types: country, eez, ocean, 12nm, 24nm

  mregions_types <- c("eez", "12nm", "24nm", "ocean")
  mregions_types_lookup <- c("eez", "eez_12nm", "eez_24nm", "goas")

  rnaturalearth_type <- c("countries")
  all_types <- c(mregions_types, rnaturalearth_type)

  country_types <- c("country", "sovereign")
  mregions_country_types_lookup <- c("territory1", "sovereign1")
  rnaturalearth_country_types_lookup <- c("countries", "sovereignty")

  if(!(type %in% all_types)) stop("'type' must be one of: ", paste(all_types, collapse = ", "))

  if(!(country_type %in% country_types) & type != "ocean") stop(message = "'country_type' must be one of: ", paste(country_types, collapse = ", "))

  if(type %in% mregions_types){
    rlang::check_installed("mregions2", reason = "to use `get_area()` to access marine areas", action = \(pkg, ...) remotes::install_github("lifewatch/mregions2"))
    query_type <- mregions_types_lookup[which(mregions_types == type)]

    if(is.null(name)) {
      message("You have requested all ", type, " areas, the download will take several minutes.")
      return(mregions2::mrp_get(query_type))
    }
    mregions_country_type <- ifelse(type == "ocean", "name", mregions_country_types_lookup[which(country_types == country_type)])

    #ifelse is only necessary until mregions2 package issue is fixed: https://github.com/lifewatch/mregions2/issues/23
    query_name_options <- if(type == "ocean"){c("Arctic Ocean", "Baltic Sea", "Indian Ocean", "Mediterranean Region", "North Atlantic Ocean", "North Pacific Ocean", "South Atlantic Ocean", "South China and Easter Archipelagic Seas", "South Pacific Ocean", "Southern Ocean")}else{ mregions2::mrp_col_unique(query_type, mregions_country_type) |>
      sort()}

    if(!(name %in% query_name_options)) {
      message("'name' is not a valid name. Please select one of the following: ")
      name <- select.list(choices = query_name_options)
    }

    eval(parse(text = paste0("mregions2::mrp_get(\"", query_type, "\", cql_filter = \"", mregions_country_type, " = '", name, "'\")")))
  } else{
    rlang::check_installed("rnaturalearth", reason = "to use `get_area()` to access land boundaries")
    rlang::check_installed("rnaturalearthhires", reason = "to use `get_area()` to access marine areas", action = \(pkg, ...) remotes::install_github("ropensci/rnaturalearthhires"))

    rnaturalearth_country_type <- rnaturalearth_country_types_lookup[which(country_types == country_type)]

    query_name_options <- rnaturalearthhires::countries10$ADMIN |>
      sort()

    if(!(name %in% query_name_options)) {
      message("'name' is not a valid name. Please select one of the following: ")
      name <- select.list(choices = query_name_options)
    }

    rnaturalearth::ne_countries(scale = 10, type = rnaturalearth_country_type, country = name)
  }
}
