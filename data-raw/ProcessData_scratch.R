# Convert SHP to RDA
# smaller and faster load times
# Erik.Leppo@tetratech.com
# 2025-01-14
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Packages ----
library(sf)

# HUC12, simple (bad)----
## Data ----
# fn_shp <- file.path("data-raw", "HUC12_simple", "HUC12_simple.shp")
fn_shp <- file.path("data-raw", "HUC12_CONUS_simple", "WBDSnapshot_National_CONUS.shp")
# ogr_shp <- rgdal::readOGR(dsn=fn_shp, layer="cbseg2003Combined2-latlong")
data_shp <- sf::st_read(dsn = fn_shp)
# fort_shp <- ggplot2::fortify(ogr_shp)

## Save as RDA for use in package----
HUC12_simple <- data_shp
usethis::use_data(HUC12_simple, overwrite = TRUE)

# document data!

# HUC02, CONUS simplify shared edges----
fn_shp <- file.path("data-raw", "HUC02_simple", "huc02_conus_simplifysharededges.shp")
data_shp <- sf::st_read(dsn = fn_shp)

# transform (4326 = long/lat)
data_shp2 <- sf::st_transform(data_shp, crs = 4326) 

## Save as RDA for use in package----
HUC02_simple2 <- data_shp2
usethis::use_data(HUC02_simple2, overwrite = TRUE)
