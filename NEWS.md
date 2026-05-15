# spatialgridr (development version)


# spatialgridr 0.1
* Added robust function argument checks using `checkmate` package
* Replaced magrittr pipes with native pipes
* Removed dot operator usage
* Moved `rnaturalearth` and `mregions2` from Suggests to Imports as they are essential for boundary retrieval
* Removed `remotes` and `magrittr` dependencies

# spatialgridr 0.0.2.2

* Add NEWS.md
* Remove all `left = FALSE` arguments from `terra::rotate()` calls as no longer required
