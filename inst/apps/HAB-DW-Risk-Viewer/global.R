# Shiny Global File

# Version ----
pkg_version <- "0.3.0.9004"

# Packages ----
library(shiny)
library(leaflet)
library(shinydashboard)
library(shinydashboardPlus) # only using for footer
library(shinyjs)
library(shinyBS) # nice buttons
# library(DT)
# # masks shinydashboardPlus::progressBar
# # masks shinyjs::alert
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(shinycssloaders) # spinner
library(ranger)
library(knitr)
library(rmarkdown)
library(sf)
library(classInt)
library(RColorBrewer)
library(ie2misc)
library(zip)
library(bsplus)  # tooltips

# library(tigris) # state layer (used in data-raw)
## other, no longer used
# library(readxl)
# library(httr)
# library(reshape2)
# library(shinyalert)#ok
# library(geojsonio) # geojson
# library(jsonlite)  # json
# library(terra)     # Raster images (TIF)
# #library(shinyWidgets)
# Model plots
# library(vip)
# ?error on Data Plots
# library(leaflet.extras2) # spinner for proxy

# Tabs ----
db_main_sb            <- source("external/db_main_sb.R", local = TRUE)$value
db_main_body          <- source("external/db_main_body.R", local = TRUE)$value
tab_code_about        <- source("external/tab_about.R", local = TRUE)$value
tab_code_map          <- source("external/tab_map.R", local = TRUE)$value
tab_code_selections   <- source("external/tab_selections.R", local = TRUE)$value
tab_code_resources    <- source("external/tab_resources.R", local = TRUE)$value
tab_code_troubleshoot <- source("external/tab_troubleshoot.R"
                                , local = TRUE)$value
# Global ----
dn_data <- "data"
dn_results <- "results"
dn_prediction <- "data_prediction"


# remove files
path_results <- file.path(dn_results)
fn_results <- list.files(path_results
                         , full.names = TRUE
                         , include.dirs = TRUE
                         , recursive = TRUE)
unlink(fn_results, recursive = TRUE) # to include dir use unlink instead of file.remove


# Data ----

# States Layer
sf_states <- readRDS(file.path("data", "states_sf.rds"))

# move to data-raw
# geojson_huc02 <- geojsonio::geojson_read(file.path(dn_data, "huc02.geojson")
#                                          , what = "sp")

# usethis::use_data(geojson_huc02, overwrite=TRUE)
# load(file.path(dn_data, "geojson_huc02.rda"))

# load(file.path(dn_data, "HUC02_simple2.rda"))

load(file.path(dn_data, "HUC12_simple.rda"))
# microbenchmark::microbenchmark(load(file.path(dn_data, "HUC12_simple.rda")), times = 3)
# mean = 1.6 seconds
# Change projection
# leaflet wants WGS84
HUC12_simple <- sf::st_transform(HUC12_simple, 4326) # 0.6 seconds

load(file.path(dn_data, "HUC12_centroid.rda"))
# # microbenchmark::microbenchmark(load(file.path(dn_data, "HUC12_centroid.rda")), times = 3)
# # mean = 1.1 seconds
# transform to WGS84 (for use with leaflet)
HUC12_centroid <- sf::st_transform(HUC12_centroid, 4326)
# Dummy Data for testing only, remove in production version
# HUC12_centroid$River_Risk <- runif(18)
# HUC12_centroid$Lake_Risk <- runif(18)
# HUC12_centroid$River_RF <- runif(18)
# HUC12_centroid$Lake_RF <- runif(18)

# 

# load(file.path(dn_data, "HUC12_simple_rf.rda"))

# test data
# geojson_huc02@data$River_Risk <- runif(22)
# geojson_huc02@data$Lake_Risk <- runif(22)
# geojson_huc02@data$River_RF <- runif(22)
# geojson_huc02@data$Lake_RF <- runif(22)
# HUC02_simple2$River_Risk <- runif(18)
# HUC02_simple2$Lake_Risk <- runif(18)
# HUC02_simple2$River_RF <- runif(18)
# HUC02_simple2$Lake_RF <- runif(18)
# HUC12_simple$River_Risk <- runif(18)
# HUC12_simple$Lake_Risk <- runif(18)
# HUC12_simple$River_RF <- runif(18)
# HUC12_simple$Lake_RF <- runif(18)


# load(file.path(dn_data, "HUC12_centroid_rf.rda"))
# HUC12_centroid_rf$River_Risk <- 42
# HUC12_centroid_rf$Lake_Risk <- 42
# HUC12_centroid_rf$River_RF <- 42
# HUC12_centroid_rf$Lake_RF <- 42

# load(file.path(dn_data, "HUC12_rf.rda"))
# HUC12_rf$River_Risk <- 42
# HUC12_rf$Lake_Risk <- 42
# HUC12_rf$River_RF <- 42
# HUC12_rf$Lake_RF <- 42


# microbenchmark::microbenchmark(geojson_huc02 <- 
#                                  geojsonio::geojson_read(file.path(dn_data,
#                                                                    "huc02.geojson"),
#                                                          what = "sp"),
#                                times = 3)
# min       lq     mean   median       uq      max neval
# 6.788116 6.824907 6.848884 6.861699 6.879269 6.896839     3

# Load RDA
# Unit: seconds
# expr      min       lq     mean
# load(file.path(dn_data, "geojson_huc02.rda")) 2.473405 2.497759 2.524774
# median       uq      max neval
# 2.522113 2.550458 2.578803     3


# geojson_huc02 <- readLines(file.path(dn_data, "huc02.geojson")) |>
#   paste(collapse = "\n") |>
#   jsonlite::fromJSON(simplifyVector = FALSE)

# raster_huc02 <- terra::rast(file.path(dn_data, 
#                                       "huc02_raster_image", 
#                                       "huc02_raster_image.tif"))
# microbenchmark::microbenchmark(raster_huc02 <- terra::rast(file.path(dn_data, 
#                                                                      "huc02_raster_image", 
#                                                                      "huc02_raster_image.tif")),
#                                times = 3)
# mean = 5.3 ms

# raster_huc12 <- terra::rast(file.path(dn_data,  
#                                       "huc12_raster.tif"))
# microbenchmark::microbenchmark(raster_huc12 <- terra::rast(file.path(dn_data,  
#                                                                      "huc12_raster.tif")),
#                                times = 3)
# mean = 3.6 ms


# Colors ----
pick_classint <- c("quantile", "equal", "pretty") #c("sd", "quantile", "equal", "pretty")
pick_pal <- c("PuOr", "BuPu", "OrRd", "PuBu", "RdPu") #RColorBrewer
pick_pal_names <- c("Purple_Orange", "Blue_Purple", "Orange_Red", "Purple_Blue", "Red_Purple")
pick_pal_change <- c("Red_Blue", "Purple_Green", "Orange_Green") # custom
pal_change_OrGn <- c("orange", "green")
# Color blind friendly
# https://colorbrewer2.org/#type=diverging&scheme=BrBG&n=5
pal_change_RdBu <- c("#ef8a62", "#67a9cf")  # red_blue (more orange than red)
pal_change_PuGn <- c("#af8dc3", "#7fbf7b") # purple_green

# Intervals ----
map_numclass <- 5
map_classint <- "pretty" # pretty picks its own number of breaks
map_pal <- "PuOr"
# map_ci_val_river_risk <- classInt::classIntervals(HUC12_simple$River_Risk,
#                                                   n = map_numclass,
#                                                   style = map_classint)
# map_ci_val_lake_risk <- classInt::classIntervals(HUC12_simple$Lake_Risk,
#                                                   n = map_numclass,
#                                                   style = map_classint)
# map_ci_val_river_rf <- classInt::classIntervals(HUC12_simple$River_RF,
#                                                   n = map_numclass,
#                                                   style = map_classint)
# map_ci_val_lake_rf <- classInt::classIntervals(HUC12_simple$Lake_RF,
#                                                  n = map_numclass,
#                                                  style = map_classint)
# if(map_classint == "pretty") {
#   
#   
# }## IF ~ pretty

# map_pal_col_river_risk <- RColorBrewer::brewer.pal(
#   n = length(map_ci_val_river_risk$brks),
#   name = map_pal)
# map_pal_col_lake_risk <- RColorBrewer::brewer.pal(
#   n = length(map_ci_val_lake_risk$brks),
#   name = map_pal)
# map_pal_col_river_rf <- RColorBrewer::brewer.pal(
#   n = length(map_ci_val_river_rf$brks),
#   name = map_pal)
# map_pal_col_lake_rf <- RColorBrewer::brewer.pal(
#   n = length(map_ci_val_lake_rf$brks),
#   name = map_pal)

# Map ----
# State Coordinates
df_coord_states <- read.csv(file.path("data",
                                      "us_states_geographic_boundaries.csv"))
# Bounding Box, CONUS
bbox_conus <- c(-124.7844079, 49.3457868, -66.9513812, 24.7433195)

# Bounding Box, western US (MS River)
#                 WA = west,  MN = north, LA = east, TX = south,  
bbox_westus <- c(-124.848974, 49.384358, -88.817017, 24.7433195)

# ggsave options
plot_device <- "png"
plot_height <- 7
plot_width  <- 9
plot_units  <- "in"
plot_scale  <- 1.25
plot_bg     <- "white"

# palette
pal_leaflet = "viridis"
leg_rnd <- 3

# Models ----
## Models----
rfr_cyan_model       <- readRDS(file.path("data", 
                                          "models", 
                                          "rfr_cyan_model.rds"))
rfr_DBP_model        <- readRDS(file.path("data", 
                                          "models", 
                                          "rfr_DBP_model.rds"))
rfr_DWOps_Risk_model <- readRDS(file.path("data", 
                                          "models", 
                                          "rfr_DWOps_Risk_model.rds"))
rfr_HABDW_Risk_model <- readRDS(file.path("data", 
                                          "models", 
                                          "rfr_HABDW_Risk_model.rds"))
rfr_lake_Risk_model  <- readRDS(file.path("data", 
                                          "models", 
                                          "rfr_lake_Risk_model.rds"))
rfr_Treat_Risk_model <- readRDS(file.path("data", 
                                          "models", 
                                          "rfr_Treat_Risk_model.rds"))
rfr_Viol_Risk_model  <- readRDS(file.path("data", 
                                          "models", 
                                          "rfr_Viol_Risk_model.rds"))
mod_var_types <- read.csv(file.path("data",
                                    "models",
                                    "model_variable_types.csv"))
## Models, Prediction Data ----
mod_pred_q1     <- read.csv(file.path("data",
                                      "models",
                                      "data_prediction",
                                      "Prediction_Dataset_1stQ.csv"))
mod_pred_q3     <- read.csv(file.path("data",
                                      "models",
                                      "data_prediction",
                                      "Prediction_Dataset_3rdQ.csv"))
mod_pred_min    <- read.csv(file.path("data",
                                      "models",
                                      "data_prediction",
                                      "Prediction_Dataset_min.csv"))
mod_pred_max    <- read.csv(file.path("data",
                                      "models",
                                      "data_prediction",
                                      "Prediction_Dataset_max.csv"))
mod_pred_mean   <- read.csv(file.path("data",
                                      "models",
                                      "data_prediction",
                                      "Prediction_Dataset_mean.csv"))
mod_pred_median <- read.csv(file.path("data",
                                      "models",
                                      "data_prediction",
                                      "Prediction_Dataset_median.csv"))
mod_pred_list <- list("1st quartile" = mod_pred_q1,
                      "3rd quartile" = mod_pred_q3,
                      min = mod_pred_min,
                      max = mod_pred_max,
                      mean = mod_pred_mean,
                      median = mod_pred_median
                      )
# used names from model_scenarios so can reference from radio button selections


model_models <- c("cyan",
                  "DBP",
                  "DWOps_Risk",
                  "HABDW_Risk",
                  "lake_Risk",
                  "Treat_Risk",
                  "Viol_Risk")

model_models_default <- "HABDW_Risk"

model_scenarios <- c("min",
                     "1st quartile",
                     "mean",
                     "median",
                     "3rd quartile",
                     "max")

model_scenarios_default <- "mean"

## Models, Var Imp ----
imp_n <- 20
# cols_modvar_display <- c("Row",
#                          "Model",
#                          "Effect_Type", 
#                          "Importance", 
#                          "Variable", 
#                          "Full_Name", 
#                          "Category")

# could prep and save these files
# but it doesn't take much to calculate

mod_varimp_cyan <- 
  data.frame(Variable = names(rfr_cyan_model$variable.importance),
             Importance = matrix(rfr_cyan_model$variable.importance)) |>
  dplyr::arrange(desc(Importance)) |>
  dplyr::mutate(Model = "cyan") |>
  dplyr::left_join(y = dplyr::select(mod_var_types, -Importance),
                   by = dplyr::join_by(Model, Variable)) |>
  dplyr::mutate(Row = dplyr::row_number()) |>
  dplyr::mutate(User_Select = "mean") |>
  dplyr::rename(Location = Effect_Type)

mod_varimp_dbp <- 
  data.frame(Variable = names(rfr_DBP_model$variable.importance),
             Importance = matrix(rfr_DBP_model$variable.importance)) |>
  dplyr::arrange(desc(Importance)) |>
  dplyr::mutate(Model = "dbp") |>
  dplyr::left_join(y = dplyr::select(mod_var_types, -Importance),
                   by = dplyr::join_by(Model, Variable)) |>
  dplyr::mutate(Row = dplyr::row_number()) |>
  dplyr::mutate(User_Select = "mean") |>
  dplyr::rename(Location = Effect_Type)

mod_varimp_dwops <- 
  data.frame(Variable = names(rfr_DWOps_Risk_model$variable.importance),
             Importance = matrix(rfr_DWOps_Risk_model$variable.importance)) |>
  dplyr::arrange(desc(Importance)) |>
  dplyr::mutate(Model = "dwops") |>
  dplyr::left_join(y = dplyr::select(mod_var_types, -Importance),
                   by = dplyr::join_by(Model, Variable)) |>
  dplyr::mutate(Row = dplyr::row_number()) |>
  dplyr::mutate(User_Select = "mean") |>
  dplyr::rename(Location = Effect_Type)

mod_varimp_habdw <- 
  data.frame(Variable = names(rfr_lake_Risk_model$variable.importance),
             Importance = matrix(rfr_lake_Risk_model$variable.importance)) |>
  dplyr::arrange(desc(Importance))|>
  dplyr::mutate(Model = "habdw") |>
  dplyr::left_join(y = dplyr::select(mod_var_types, -Importance),
                   by = dplyr::join_by(Model, Variable)) |>
  dplyr::mutate(Row = dplyr::row_number()) |>
  dplyr::mutate(User_Select = "mean") |>
  dplyr::rename(Location = Effect_Type)
  # dplyr::select(dplyr::all_of(cols_modvar_display))

mod_varimp_lake <- 
  data.frame(Variable = names(rfr_lake_Risk_model$variable.importance),
             Importance = matrix(rfr_lake_Risk_model$variable.importance)) |>
  dplyr::arrange(desc(Importance)) |>
  dplyr::mutate(Model = "lake") |>
  dplyr::left_join(y = dplyr::select(mod_var_types, -Importance),
                   by = dplyr::join_by(Model, Variable)) |>
  dplyr::mutate(Row = dplyr::row_number()) |>
  dplyr::mutate(User_Select = "mean") |>
  dplyr::rename(Location = Effect_Type)

mod_varimp_treat <- 
  data.frame(Variable = names(rfr_Treat_Risk_model$variable.importance),
             Importance = matrix(rfr_Treat_Risk_model$variable.importance)) |>
  dplyr::arrange(desc(Importance)) |>
  dplyr::mutate(Model = "treat") |>
  dplyr::left_join(y = dplyr::select(mod_var_types, -Importance),
                   by = dplyr::join_by(Model, Variable)) |>
  dplyr::mutate(Row = dplyr::row_number()) |>
  dplyr::mutate(User_Select = "mean") |>
  dplyr::rename(Location = Effect_Type)

mod_varimp_viol <- 
  data.frame(Variable = names(rfr_Viol_Risk_model$variable.importance),
             Importance = matrix(rfr_Viol_Risk_model$variable.importance)) |>
  dplyr::arrange(desc(Importance)) |>
  dplyr::mutate(Model = "viol") |>
  dplyr::left_join(y = dplyr::select(mod_var_types, -Importance),
                   by = dplyr::join_by(Model, Variable)) |>
  dplyr::mutate(Row = dplyr::row_number()) |>
  dplyr::mutate(User_Select = "mean") |>
  dplyr::rename(Location = Effect_Type)


## Models, Test ----
# Read in Model Test Datasets
rfr_test_cyan  <- read.csv(file.path("data",
                                     "models", 
                                     "rfr_test", 
                                     'RFR_test_cyan.csv'))
rfr_test_dbp   <- read.csv(file.path("data",
                                     "models",
                                     "rfr_test",
                                     'RFR_test_DBP.csv'))
rfr_test_dwops <- read.csv(file.path("data", 
                                     "models", 
                                     "rfr_test",
                                     'RFR_test_DWOpsR.csv'))
rfr_test_habdw <- read.csv(file.path("data",
                                     "models", 
                                     "rfr_test",
                                     'RFR_test_HABDW.csv'))
rfr_test_lake  <- read.csv(file.path("data", 
                                     "models", 
                                     "rfr_test",
                                     'RFR_test_lakeR.csv'))
rfr_test_treat <- read.csv(file.path("data", 
                                     "models", 
                                     "rfr_test",
                                     'RFR_test_TreatR.csv'))
rfr_test_viol  <- read.csv(file.path("data",
                                     "models", 
                                     "rfr_test",
                                     'RFR_test_ViolR.csv'))

