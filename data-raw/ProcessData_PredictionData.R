# Remove 4 entries from mean and median not in the other files
# use Min as the standard
# 2719 vs. 2715 records
# copy "fixed" files to Shiny app
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Erik.Leppo@tetratech.com
# 2025-08-21
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Packages ----

# Read Files -----
mod_pred_min    <- read.csv(file.path("data-raw",
                                      "data_prediction",
                                      "Prediction_Dataset_min.csv"))
mod_pred_mean    <- read.csv(file.path("data-raw",
                                       "data_prediction",
                                       "Prediction_Dataset_mean_2719.csv"))
mod_pred_median  <- read.csv(file.path("data-raw",
                                       "data_prediction",
                                       "Prediction_Dataset_median_2719.csv"))

# Munge ----
## Remove Rows
huc_good <- mod_pred_min$HUC12
mod_pred_mean_fix <- mod_pred_mean[mod_pred_mean$HUC12 %in% huc_good, ]
mod_pred_median_fix <- mod_pred_median[mod_pred_median$HUC12 %in% huc_good, ]
## Add Columns
mod_pred_mean_fix$HABs_Treat_Score2 <- 0
mod_pred_median_fix$HABs_Treat_Score2 <- 0

# Save ----
write.csv(mod_pred_mean_fix,
          file.path("inst",
                    "apps",
                    "HAB-DW-Risk-Viewer",
                    "data",
                    "models",
                    "data_prediction",
                    "Prediction_Dataset_mean.csv"),
          row.names = FALSE)
write.csv(mod_pred_median_fix,
          file.path("inst",
                    "apps",
                    "HAB-DW-Risk-Viewer",
                    "data",
                    "models",
                    "data_prediction",
                    "Prediction_Dataset_median.csv"),
          row.names = FALSE)

