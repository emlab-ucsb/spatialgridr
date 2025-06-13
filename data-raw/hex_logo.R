#create hex logo

devtools::load_all()

ridges <- readRDS(system.file("extdata", "ridges.rds", package = "spatialgridr"))

#load Samoa EEZ
samoa_eez <- get_boundary(name = "Samoa")

#equal area projection for Samoa obtained from https://projectionwizard.org
samoa_projection <- '+proj=laea +lon_0=-172.5 +lat_0=0 +datum=WGS84 +units=m +no_defs'

planning_grid_sf_coarse <- get_grid(boundary = samoa_eez, crs = samoa_projection, resolution = 30000, output = "sf_hex")

#grid the data
ridges_gridded_sf_coarse <- get_data_in_grid(spatial_grid = planning_grid_sf_coarse, dat = ridges, cutoff = 0.001)

my_pal <- c("#4dac26", "#e66101")

(p <- ggplot2::ggplot(data = ridges_gridded_sf_coarse) +
    ggplot2::geom_sf(ggplot2::aes(fill = factor(data))) +
    ggplot2::scale_fill_manual(values = my_pal, guide = "none")+
    ggplot2::theme_void())

hexSticker::sticker(p,
                    package = "spatialgridr",
                    p_size=20, s_x=1, s_y=0.7, s_width=1.4, s_height=1.2,
                    h_fill = "#2c7bb6",
                    h_color = "#315fc8ff",
                    filename = "data-raw/hex_logo.png")

hexSticker::sticker(p,
                    package = "spatialgridr",
                    p_size=7, s_x=1, s_y=0.7, s_width=1.4, s_height=1.2,
                    h_fill = "#2c7bb6",
                    h_color = "#315fc8ff",
                    filename = "data-raw/hex_logo.svg")
