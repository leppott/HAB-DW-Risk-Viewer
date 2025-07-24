# Convert SHP to RDA
# smaller and faster load times
# Erik.Leppo@tetratech.com
# 2025-03-20
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Packages ----
library(sf)

# Data ----
fn_shp <- file.path("data-raw", "HUC12_centroid", "HUC12_centroid.shp")
# ogr_shp <- rgdal::readOGR(dsn=fn_shp, layer="cbseg2003Combined2-latlong")
data_shp <- sf::st_read(dsn = fn_shp)
# fort_shp <- ggplot2::fortify(ogr_shp)

# Save as RDA for use in package----
HUC12_centroid <- data_shp
usethis::use_data(HUC12_centroid, overwrite = TRUE)

# document data!


# ALTERNATE (RF only)
data_shp_rf <- data_shp[data_shp$HUC_12 %in% data_rf$HUC12, ]
HUC12_centroid_rf <- data_shp_rf
usethis::use_data(HUC12_centroid_rf, overwrite = TRUE)
