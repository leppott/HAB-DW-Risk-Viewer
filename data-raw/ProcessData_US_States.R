# Process Data - US State boundaries
# Erik.Leppo@tetratech.com
# 2025-08-13
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Avoid having to download each time shiny app is run
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tigris)
options(tigris_use_cache = TRUE)

# Download state boundaries
sf_states <- tigris::states(cb = TRUE, class = "sf")

# Change projection
# leaflet wants WGS84
sf_states <- sf::st_transform(sf_states, 4326) # 0.6 seconds

# Save to an RDS file
saveRDS(sf_states, 
        file = file.path("inst", 
                         "apps", 
                         "HAB-DW-Risk-Viewer", 
                         "data", 
                         "states_sf.rds"))

# # in GLOBAL to load it back
# sf_states <- readRDS("states_sf.rds")

