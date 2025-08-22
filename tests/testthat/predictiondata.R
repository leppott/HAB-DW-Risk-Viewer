# QC prediction data for Shiny app
# Ensure have same parameters for each model and parameter value
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Erik.Leppo@tetratech.com
# 2025-08-21
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 7 models
# 6 parameter values (one file each)

# reuse code from Global.R in shiny app
# 1. load models
# 2. load prediction data files
# 3. generate variables for each model
# 4. TEST if those variables are in each of the prediction data files

# Columns ----
testthat::test_that("ORWA, pickfiles, filenames", {

  ## Models ----
  rfr_cyan_model       <- readRDS(file.path("inst",
                                            "apps",
                                            "HAB-DW-Risk-Viewer",
                                            "data", 
                                            "models", 
                                            "rfr_cyan_model.rds"))
  rfr_DBP_model        <- readRDS(file.path("inst",
                                            "apps",
                                            "HAB-DW-Risk-Viewer",
                                            "data", 
                                            "models", 
                                            "rfr_DBP_model.rds"))
  rfr_DWOps_Risk_model <- readRDS(file.path("inst",
                                            "apps",
                                            "HAB-DW-Risk-Viewer",
                                            "data", 
                                            "models", 
                                            "rfr_DWOps_Risk_model.rds"))
  rfr_HABDW_Risk_model <- readRDS(file.path("inst",
                                            "apps",
                                            "HAB-DW-Risk-Viewer",
                                            "data", 
                                            "models", 
                                            "rfr_HABDW_Risk_model.rds"))
  rfr_lake_Risk_model  <- readRDS(file.path("inst",
                                            "apps",
                                            "HAB-DW-Risk-Viewer",
                                            "data", 
                                            "models", 
                                            "rfr_lake_Risk_model.rds"))
  rfr_Treat_Risk_model <- readRDS(file.path("inst",
                                            "apps",
                                            "HAB-DW-Risk-Viewer",
                                            "data", 
                                            "models", 
                                            "rfr_Treat_Risk_model.rds"))
  rfr_Viol_Risk_model  <- readRDS(file.path("inst",
                                            "apps",
                                            "HAB-DW-Risk-Viewer",
                                            "data", 
                                            "models", 
                                            "rfr_Viol_Risk_model.rds"))
  
  ## Prediction Data ----
  mod_pred_q1     <- read.csv(file.path("inst",
                                        "apps",
                                        "HAB-DW-Risk-Viewer",
                                        "data",
                                        "models",
                                        "data_prediction",
                                        "Prediction_Dataset_1stQ.csv"))
  mod_pred_q3     <- read.csv(file.path("inst",
                                        "apps",
                                        "HAB-DW-Risk-Viewer",
                                        "data",
                                        "models",
                                        "data_prediction",
                                        "Prediction_Dataset_3rdQ.csv"))
  mod_pred_min    <- read.csv(file.path("inst",
                                        "apps",
                                        "HAB-DW-Risk-Viewer",
                                        "data",
                                        "models",
                                        "data_prediction",
                                        "Prediction_Dataset_min.csv"))
  mod_pred_max    <- read.csv(file.path("inst",
                                        "apps",
                                        "HAB-DW-Risk-Viewer",
                                        "data",
                                        "models",
                                        "data_prediction",
                                        "Prediction_Dataset_max.csv"))
  mod_pred_mean   <- read.csv(file.path("inst",
                                        "apps",
                                        "HAB-DW-Risk-Viewer",
                                        "data",
                                        "models",
                                        "data_prediction",
                                        "Prediction_Dataset_mean.csv"))
  mod_pred_median <- read.csv(file.path("inst",
                                        "apps",
                                        "HAB-DW-Risk-Viewer",
                                        "data",
                                        "models",
                                        "data_prediction",
                                        "Prediction_Dataset_median.csv"))
  
  ## Models, Variables ----
  mod_varimp_cyan <- 
    data.frame(Variable = names(rfr_cyan_model$variable.importance),
               Importance = matrix(rfr_cyan_model$variable.importance)) |>
    dplyr::arrange(desc(Importance)) |>
    dplyr::pull(Variable)
  
  mod_varimp_dbp <- 
    data.frame(Variable = names(rfr_DBP_model$variable.importance),
               Importance = matrix(rfr_DBP_model$variable.importance)) |>
    dplyr::arrange(desc(Importance)) |>
    dplyr::pull(Variable)
  
  mod_varimp_dwops <- 
    data.frame(Variable = names(rfr_DWOps_Risk_model$variable.importance),
               Importance = matrix(rfr_DWOps_Risk_model$variable.importance)) |>
    dplyr::arrange(desc(Importance)) |>
    dplyr::pull(Variable)
  
  mod_varimp_habdw <- 
    data.frame(Variable = names(rfr_lake_Risk_model$variable.importance),
               Importance = matrix(rfr_lake_Risk_model$variable.importance)) |>
    dplyr::arrange(desc(Importance))|>
    dplyr::pull(Variable)
  
  mod_varimp_lake <- 
    data.frame(Variable = names(rfr_lake_Risk_model$variable.importance),
               Importance = matrix(rfr_lake_Risk_model$variable.importance)) |>
    dplyr::arrange(desc(Importance)) |>
    dplyr::pull(Variable)
  
  mod_varimp_treat <- 
    data.frame(Variable = names(rfr_Treat_Risk_model$variable.importance),
               Importance = matrix(rfr_Treat_Risk_model$variable.importance)) |>
    dplyr::arrange(desc(Importance)) |>
    dplyr::pull(Variable)
  
  mod_varimp_viol <- 
    data.frame(Variable = names(rfr_Viol_Risk_model$variable.importance),
               Importance = matrix(rfr_Viol_Risk_model$variable.importance)) |>
    dplyr::arrange(desc(Importance)) |>
    dplyr::pull(Variable)
    
  
  ## Data, Variables ----
  var_pred_q1 <- names(mod_pred_q1)
  var_pred_q3 <- names(mod_pred_q3)
  var_pred_min <- names(mod_pred_min)
  var_pred_max <- names(mod_pred_max)
  var_pred_mean <- names(mod_pred_mean)
  var_pred_median <- names(mod_pred_median)
  
  ## test ----
  ### cyan ----
  testthat::expect_true(all(mod_varimp_cyan %in% var_pred_q1))
  testthat::expect_true(all(mod_varimp_cyan %in% var_pred_q3))
  testthat::expect_true(all(mod_varimp_cyan %in% var_pred_min))
  testthat::expect_true(all(mod_varimp_cyan %in% var_pred_max))
  testthat::expect_true(all(mod_varimp_cyan %in% var_pred_mean))
  testthat::expect_true(all(mod_varimp_cyan %in% var_pred_median))
  ### dbp ----
  testthat::expect_true(all(mod_varimp_dbp %in% var_pred_q1))
  testthat::expect_true(all(mod_varimp_dbp %in% var_pred_q3))
  testthat::expect_true(all(mod_varimp_dbp %in% var_pred_min))
  testthat::expect_true(all(mod_varimp_dbp %in% var_pred_max))
  testthat::expect_true(all(mod_varimp_dbp %in% var_pred_mean))
  testthat::expect_true(all(mod_varimp_dbp %in% var_pred_median))
  ### dwops ----
  testthat::expect_true(all(mod_varimp_dwops %in% var_pred_q1))
  testthat::expect_true(all(mod_varimp_dwops %in% var_pred_q3))
  testthat::expect_true(all(mod_varimp_dwops %in% var_pred_min))
  testthat::expect_true(all(mod_varimp_dwops %in% var_pred_max))
  testthat::expect_true(all(mod_varimp_dwops %in% var_pred_mean))
  testthat::expect_true(all(mod_varimp_dwops %in% var_pred_median))
  ### habdw ----
  testthat::expect_true(all(mod_varimp_habdw %in% var_pred_q1))
  testthat::expect_true(all(mod_varimp_habdw %in% var_pred_q3))
  testthat::expect_true(all(mod_varimp_habdw %in% var_pred_min))
  testthat::expect_true(all(mod_varimp_habdw %in% var_pred_max))
  testthat::expect_true(all(mod_varimp_habdw %in% var_pred_mean))
  testthat::expect_true(all(mod_varimp_habdw %in% var_pred_median))
  ### lake ----
  testthat::expect_true(all(mod_varimp_lake %in% var_pred_q1))
  testthat::expect_true(all(mod_varimp_lake %in% var_pred_q3))
  testthat::expect_true(all(mod_varimp_lake %in% var_pred_min))
  testthat::expect_true(all(mod_varimp_lake %in% var_pred_max))
  testthat::expect_true(all(mod_varimp_lake %in% var_pred_mean))
  testthat::expect_true(all(mod_varimp_lake %in% var_pred_median))
  ### treat ----
  testthat::expect_true(all(mod_varimp_treat %in% var_pred_q1))
  testthat::expect_true(all(mod_varimp_treat %in% var_pred_q3))
  testthat::expect_true(all(mod_varimp_treat %in% var_pred_min))
  testthat::expect_true(all(mod_varimp_treat %in% var_pred_max))
  testthat::expect_true(all(mod_varimp_treat %in% var_pred_mean))
  testthat::expect_true(all(mod_varimp_treat %in% var_pred_median))
  ### viol ----
  testthat::expect_true(all(mod_varimp_viol %in% var_pred_q1))
  testthat::expect_true(all(mod_varimp_viol %in% var_pred_q3))
  testthat::expect_true(all(mod_varimp_viol %in% var_pred_min))
  testthat::expect_true(all(mod_varimp_viol %in% var_pred_max))
  testthat::expect_true(all(mod_varimp_viol %in% var_pred_mean))
  testthat::expect_true(all(mod_varimp_viol %in% var_pred_median))
  
  ## Mismatches ----
  df_missing <- data.frame(model = NA,
                           stat = NA,
                           variable = NA)

  ### cyan ----
  mod_name      <- "cyan"
  var_mod_test  <- mod_varimp_cyan
  #
  pred_name     <- "q1"
  var_pred_test <- var_pred_q1
  var_nonmatch  <- var_mod_test[!var_mod_test %in% var_pred_test]
  msg <- paste0("\nModel variables (",
                mod_name,
                ") not in prediction data (",
                pred_name,
                "):\n",
                paste(var_nonmatch, collapse = "\n"),
                "\n\n")
  message(msg)
  df_test <- data.frame(model = mod_name,
                        stat = pred_name,
                        variable = var_nonmatch)
  df_missing <- rbind(df_missing, df_test)
  #  
  pred_name     <- "q3"
  var_pred_test <- var_pred_q1
  var_nonmatch  <- var_mod_test[!var_mod_test %in% var_pred_test]
  msg <- paste0("\nModel variables (",
                mod_name,
                ") not in prediction data (",
                pred_name,
                "):\n",
                paste(var_nonmatch, collapse = "\n"),
                "\n\n")
  message(msg)
  df_test <- data.frame(model = mod_name,
                        stat = pred_name,
                        variable = var_nonmatch)
  df_missing <- rbind(df_missing, df_test)
  #  
  pred_name     <- "min"
  var_pred_test <- var_pred_q1
  var_nonmatch  <- var_mod_test[!var_mod_test %in% var_pred_test]
  msg <- paste0("\nModel variables (",
                mod_name,
                ") not in prediction data (",
                pred_name,
                "):\n",
                paste(var_nonmatch, collapse = "\n"),
                "\n\n")
  message(msg)
  df_test <- data.frame(model = mod_name,
                        stat = pred_name,
                        variable = var_nonmatch)
  df_missing <- rbind(df_missing, df_test)
  #  
  pred_name     <- "max"
  var_pred_test <- var_pred_q1
  var_nonmatch  <- var_mod_test[!var_mod_test %in% var_pred_test]
  msg <- paste0("\nModel variables (",
                mod_name,
                ") not in prediction data (",
                pred_name,
                "):\n",
                paste(var_nonmatch, collapse = "\n"),
                "\n\n")
  message(msg)
  df_test <- data.frame(model = mod_name,
                        stat = pred_name,
                        variable = var_nonmatch)
  df_missing <- rbind(df_missing, df_test)
  #  
  pred_name     <- "mean"
  var_pred_test <- var_pred_q1
  var_nonmatch  <- var_mod_test[!var_mod_test %in% var_pred_test]
  msg <- paste0("\nModel variables (",
                mod_name,
                ") not in prediction data (",
                pred_name,
                "):\n",
                paste(var_nonmatch, collapse = "\n"),
                "\n\n")
  message(msg)
  df_test <- data.frame(model = mod_name,
                        stat = pred_name,
                        variable = var_nonmatch)
  df_missing <- rbind(df_missing, df_test)
  #  
  pred_name     <- "median"
  var_pred_test <- var_pred_q1
  var_nonmatch  <- var_mod_test[!var_mod_test %in% var_pred_test]
  msg <- paste0("\nModel variables (",
                mod_name,
                ") not in prediction data (",
                pred_name,
                "):\n",
                paste(var_nonmatch, collapse = "\n"),
                "\n\n")
  message(msg)
  df_test <- data.frame(model = mod_name,
                        stat = pred_name,
                        variable = var_nonmatch)
  df_missing <- rbind(df_missing, df_test)
  ### dbp ----
  mod_name      <- "dbp"
  var_mod_test  <- mod_varimp_dbp
  #
  pred_name     <- "q1"
  var_pred_test <- var_pred_q1
  var_nonmatch  <- var_mod_test[!var_mod_test %in% var_pred_test]
  msg <- paste0("\nModel variables (",
                mod_name,
                ") not in prediction data (",
                pred_name,
                "):\n",
                paste(var_nonmatch, collapse = "\n"),
                "\n\n")
  message(msg)
  df_test <- data.frame(model = mod_name,
                        stat = pred_name,
                        variable = var_nonmatch)
  df_missing <- rbind(df_missing, df_test)
  #  
  pred_name     <- "q3"
  var_pred_test <- var_pred_q1
  var_nonmatch  <- var_mod_test[!var_mod_test %in% var_pred_test]
  msg <- paste0("\nModel variables (",
                mod_name,
                ") not in prediction data (",
                pred_name,
                "):\n",
                paste(var_nonmatch, collapse = "\n"),
                "\n\n")
  message(msg)
  df_test <- data.frame(model = mod_name,
                        stat = pred_name,
                        variable = var_nonmatch)
  df_missing <- rbind(df_missing, df_test)
  #  
  pred_name     <- "min"
  var_pred_test <- var_pred_q1
  var_nonmatch  <- var_mod_test[!var_mod_test %in% var_pred_test]
  msg <- paste0("\nModel variables (",
                mod_name,
                ") not in prediction data (",
                pred_name,
                "):\n",
                paste(var_nonmatch, collapse = "\n"),
                "\n\n")
  message(msg)
  df_test <- data.frame(model = mod_name,
                        stat = pred_name,
                        variable = var_nonmatch)
  df_missing <- rbind(df_missing, df_test)
  #  
  pred_name     <- "max"
  var_pred_test <- var_pred_q1
  var_nonmatch  <- var_mod_test[!var_mod_test %in% var_pred_test]
  msg <- paste0("\nModel variables (",
                mod_name,
                ") not in prediction data (",
                pred_name,
                "):\n",
                paste(var_nonmatch, collapse = "\n"),
                "\n\n")
  message(msg)
  df_test <- data.frame(model = mod_name,
                        stat = pred_name,
                        variable = var_nonmatch)
  df_missing <- rbind(df_missing, df_test)
  #  
  pred_name     <- "mean"
  var_pred_test <- var_pred_q1
  var_nonmatch  <- var_mod_test[!var_mod_test %in% var_pred_test]
  msg <- paste0("\nModel variables (",
                mod_name,
                ") not in prediction data (",
                pred_name,
                "):\n",
                paste(var_nonmatch, collapse = "\n"),
                "\n\n")
  message(msg)
  df_test <- data.frame(model = mod_name,
                        stat = pred_name,
                        variable = var_nonmatch)
  df_missing <- rbind(df_missing, df_test)
  #  
  pred_name     <- "median"
  var_pred_test <- var_pred_q1
  var_nonmatch  <- var_mod_test[!var_mod_test %in% var_pred_test]
  msg <- paste0("\nModel variables (",
                mod_name,
                ") not in prediction data (",
                pred_name,
                "):\n",
                paste(var_nonmatch, collapse = "\n"),
                "\n\n")
  message(msg)
  df_test <- data.frame(model = mod_name,
                        stat = pred_name,
                        variable = var_nonmatch)
  df_missing <- rbind(df_missing, df_test)
  ### dwops ----
  mod_name      <- "dwops"
  var_mod_test  <- mod_varimp_dwops
  #
  pred_name     <- "q1"
  var_pred_test <- var_pred_q1
  var_nonmatch  <- var_mod_test[!var_mod_test %in% var_pred_test]
  msg <- paste0("\nModel variables (",
                mod_name,
                ") not in prediction data (",
                pred_name,
                "):\n",
                paste(var_nonmatch, collapse = "\n"),
                "\n\n")
  message(msg)
  df_test <- data.frame(model = mod_name,
                        stat = pred_name,
                        variable = var_nonmatch)
  df_missing <- rbind(df_missing, df_test)
  #  
  pred_name     <- "q3"
  var_pred_test <- var_pred_q1
  var_nonmatch  <- var_mod_test[!var_mod_test %in% var_pred_test]
  msg <- paste0("\nModel variables (",
                mod_name,
                ") not in prediction data (",
                pred_name,
                "):\n",
                paste(var_nonmatch, collapse = "\n"),
                "\n\n")
  message(msg)
  df_test <- data.frame(model = mod_name,
                        stat = pred_name,
                        variable = var_nonmatch)
  df_missing <- rbind(df_missing, df_test)
  #  
  pred_name     <- "min"
  var_pred_test <- var_pred_q1
  var_nonmatch  <- var_mod_test[!var_mod_test %in% var_pred_test]
  msg <- paste0("\nModel variables (",
                mod_name,
                ") not in prediction data (",
                pred_name,
                "):\n",
                paste(var_nonmatch, collapse = "\n"),
                "\n\n")
  message(msg)
  df_test <- data.frame(model = mod_name,
                        stat = pred_name,
                        variable = var_nonmatch)
  df_missing <- rbind(df_missing, df_test)
  #  
  pred_name     <- "max"
  var_pred_test <- var_pred_q1
  var_nonmatch  <- var_mod_test[!var_mod_test %in% var_pred_test]
  msg <- paste0("\nModel variables (",
                mod_name,
                ") not in prediction data (",
                pred_name,
                "):\n",
                paste(var_nonmatch, collapse = "\n"),
                "\n\n")
  message(msg)
  df_test <- data.frame(model = mod_name,
                        stat = pred_name,
                        variable = var_nonmatch)
  df_missing <- rbind(df_missing, df_test)
  #  
  pred_name     <- "mean"
  var_pred_test <- var_pred_q1
  var_nonmatch  <- var_mod_test[!var_mod_test %in% var_pred_test]
  msg <- paste0("\nModel variables (",
                mod_name,
                ") not in prediction data (",
                pred_name,
                "):\n",
                paste(var_nonmatch, collapse = "\n"),
                "\n\n")
  message(msg)
  df_test <- data.frame(model = mod_name,
                        stat = pred_name,
                        variable = var_nonmatch)
  df_missing <- rbind(df_missing, df_test)
  #  
  pred_name     <- "median"
  var_pred_test <- var_pred_q1
  var_nonmatch  <- var_mod_test[!var_mod_test %in% var_pred_test]
  msg <- paste0("\nModel variables (",
                mod_name,
                ") not in prediction data (",
                pred_name,
                "):\n",
                paste(var_nonmatch, collapse = "\n"),
                "\n\n")
  message(msg)
  df_test <- data.frame(model = mod_name,
                        stat = pred_name,
                        variable = var_nonmatch)
  df_missing <- rbind(df_missing, df_test)
  ### habdw ----
  mod_name      <- "habdw"
  var_mod_test  <- mod_varimp_habdw
  #
  pred_name     <- "q1"
  var_pred_test <- var_pred_q1
  var_nonmatch  <- var_mod_test[!var_mod_test %in% var_pred_test]
  msg <- paste0("\nModel variables (",
                mod_name,
                ") not in prediction data (",
                pred_name,
                "):\n",
                paste(var_nonmatch, collapse = "\n"),
                "\n\n")
  message(msg)
  df_test <- data.frame(model = mod_name,
                        stat = pred_name,
                        variable = var_nonmatch)
  df_missing <- rbind(df_missing, df_test)
  #  
  pred_name     <- "q3"
  var_pred_test <- var_pred_q1
  var_nonmatch  <- var_mod_test[!var_mod_test %in% var_pred_test]
  msg <- paste0("\nModel variables (",
                mod_name,
                ") not in prediction data (",
                pred_name,
                "):\n",
                paste(var_nonmatch, collapse = "\n"),
                "\n\n")
  message(msg)
  df_test <- data.frame(model = mod_name,
                        stat = pred_name,
                        variable = var_nonmatch)
  df_missing <- rbind(df_missing, df_test)
  #  
  pred_name     <- "min"
  var_pred_test <- var_pred_q1
  var_nonmatch  <- var_mod_test[!var_mod_test %in% var_pred_test]
  msg <- paste0("\nModel variables (",
                mod_name,
                ") not in prediction data (",
                pred_name,
                "):\n",
                paste(var_nonmatch, collapse = "\n"),
                "\n\n")
  message(msg)
  df_test <- data.frame(model = mod_name,
                        stat = pred_name,
                        variable = var_nonmatch)
  df_missing <- rbind(df_missing, df_test)
  #  
  pred_name     <- "max"
  var_pred_test <- var_pred_q1
  var_nonmatch  <- var_mod_test[!var_mod_test %in% var_pred_test]
  msg <- paste0("\nModel variables (",
                mod_name,
                ") not in prediction data (",
                pred_name,
                "):\n",
                paste(var_nonmatch, collapse = "\n"),
                "\n\n")
  message(msg)
  df_test <- data.frame(model = mod_name,
                        stat = pred_name,
                        variable = var_nonmatch)
  df_missing <- rbind(df_missing, df_test)
  #  
  pred_name     <- "mean"
  var_pred_test <- var_pred_q1
  var_nonmatch  <- var_mod_test[!var_mod_test %in% var_pred_test]
  msg <- paste0("\nModel variables (",
                mod_name,
                ") not in prediction data (",
                pred_name,
                "):\n",
                paste(var_nonmatch, collapse = "\n"),
                "\n\n")
  message(msg)
  df_test <- data.frame(model = mod_name,
                        stat = pred_name,
                        variable = var_nonmatch)
  df_missing <- rbind(df_missing, df_test)
  #  
  pred_name     <- "median"
  var_pred_test <- var_pred_q1
  var_nonmatch  <- var_mod_test[!var_mod_test %in% var_pred_test]
  msg <- paste0("\nModel variables (",
                mod_name,
                ") not in prediction data (",
                pred_name,
                "):\n",
                paste(var_nonmatch, collapse = "\n"),
                "\n\n")
  message(msg)
  df_test <- data.frame(model = mod_name,
                        stat = pred_name,
                        variable = var_nonmatch)
  df_missing <- rbind(df_missing, df_test)
  ### lake ----
  mod_name      <- "lake"
  var_mod_test  <- mod_varimp_lake
  #
  pred_name     <- "q1"
  var_pred_test <- var_pred_q1
  var_nonmatch  <- var_mod_test[!var_mod_test %in% var_pred_test]
  msg <- paste0("\nModel variables (",
                mod_name,
                ") not in prediction data (",
                pred_name,
                "):\n",
                paste(var_nonmatch, collapse = "\n"),
                "\n\n")
  message(msg)
  df_test <- data.frame(model = mod_name,
                        stat = pred_name,
                        variable = var_nonmatch)
  df_missing <- rbind(df_missing, df_test)
  #  
  pred_name     <- "q3"
  var_pred_test <- var_pred_q1
  var_nonmatch  <- var_mod_test[!var_mod_test %in% var_pred_test]
  msg <- paste0("\nModel variables (",
                mod_name,
                ") not in prediction data (",
                pred_name,
                "):\n",
                paste(var_nonmatch, collapse = "\n"),
                "\n\n")
  message(msg)
  df_test <- data.frame(model = mod_name,
                        stat = pred_name,
                        variable = var_nonmatch)
  df_missing <- rbind(df_missing, df_test)
  #  
  pred_name     <- "min"
  var_pred_test <- var_pred_q1
  var_nonmatch  <- var_mod_test[!var_mod_test %in% var_pred_test]
  msg <- paste0("\nModel variables (",
                mod_name,
                ") not in prediction data (",
                pred_name,
                "):\n",
                paste(var_nonmatch, collapse = "\n"),
                "\n\n")
  message(msg)
  df_test <- data.frame(model = mod_name,
                        stat = pred_name,
                        variable = var_nonmatch)
  df_missing <- rbind(df_missing, df_test)
  #  
  pred_name     <- "max"
  var_pred_test <- var_pred_q1
  var_nonmatch  <- var_mod_test[!var_mod_test %in% var_pred_test]
  msg <- paste0("\nModel variables (",
                mod_name,
                ") not in prediction data (",
                pred_name,
                "):\n",
                paste(var_nonmatch, collapse = "\n"),
                "\n\n")
  message(msg)
  df_test <- data.frame(model = mod_name,
                        stat = pred_name,
                        variable = var_nonmatch)
  df_missing <- rbind(df_missing, df_test)
  #  
  pred_name     <- "mean"
  var_pred_test <- var_pred_q1
  var_nonmatch  <- var_mod_test[!var_mod_test %in% var_pred_test]
  msg <- paste0("\nModel variables (",
                mod_name,
                ") not in prediction data (",
                pred_name,
                "):\n",
                paste(var_nonmatch, collapse = "\n"),
                "\n\n")
  message(msg)
  df_test <- data.frame(model = mod_name,
                        stat = pred_name,
                        variable = var_nonmatch)
  df_missing <- rbind(df_missing, df_test)
  #  
  pred_name     <- "median"
  var_pred_test <- var_pred_q1
  var_nonmatch  <- var_mod_test[!var_mod_test %in% var_pred_test]
  msg <- paste0("\nModel variables (",
                mod_name,
                ") not in prediction data (",
                pred_name,
                "):\n",
                paste(var_nonmatch, collapse = "\n"),
                "\n\n")
  message(msg)
  df_test <- data.frame(model = mod_name,
                        stat = pred_name,
                        variable = var_nonmatch)
  df_missing <- rbind(df_missing, df_test)
  ### treat ----
  mod_name      <- "treat"
  var_mod_test  <- mod_varimp_treat
  #
  pred_name     <- "q1"
  var_pred_test <- var_pred_q1
  var_nonmatch  <- var_mod_test[!var_mod_test %in% var_pred_test]
  msg <- paste0("\nModel variables (",
                mod_name,
                ") not in prediction data (",
                pred_name,
                "):\n",
                paste(var_nonmatch, collapse = "\n"),
                "\n\n")
  message(msg)
  df_test <- data.frame(model = mod_name,
                        stat = pred_name,
                        variable = var_nonmatch)
  df_missing <- rbind(df_missing, df_test)
  #  
  pred_name     <- "q3"
  var_pred_test <- var_pred_q1
  var_nonmatch  <- var_mod_test[!var_mod_test %in% var_pred_test]
  msg <- paste0("\nModel variables (",
                mod_name,
                ") not in prediction data (",
                pred_name,
                "):\n",
                paste(var_nonmatch, collapse = "\n"),
                "\n\n")
  message(msg)
  df_test <- data.frame(model = mod_name,
                        stat = pred_name,
                        variable = var_nonmatch)
  df_missing <- rbind(df_missing, df_test)
  #  
  pred_name     <- "min"
  var_pred_test <- var_pred_q1
  var_nonmatch  <- var_mod_test[!var_mod_test %in% var_pred_test]
  msg <- paste0("\nModel variables (",
                mod_name,
                ") not in prediction data (",
                pred_name,
                "):\n",
                paste(var_nonmatch, collapse = "\n"),
                "\n\n")
  message(msg)
  df_test <- data.frame(model = mod_name,
                        stat = pred_name,
                        variable = var_nonmatch)
  df_missing <- rbind(df_missing, df_test)
  #  
  pred_name     <- "max"
  var_pred_test <- var_pred_q1
  var_nonmatch  <- var_mod_test[!var_mod_test %in% var_pred_test]
  msg <- paste0("\nModel variables (",
                mod_name,
                ") not in prediction data (",
                pred_name,
                "):\n",
                paste(var_nonmatch, collapse = "\n"),
                "\n\n")
  message(msg)
  df_test <- data.frame(model = mod_name,
                        stat = pred_name,
                        variable = var_nonmatch)
  df_missing <- rbind(df_missing, df_test)
  #  
  pred_name     <- "mean"
  var_pred_test <- var_pred_q1
  var_nonmatch  <- var_mod_test[!var_mod_test %in% var_pred_test]
  msg <- paste0("\nModel variables (",
                mod_name,
                ") not in prediction data (",
                pred_name,
                "):\n",
                paste(var_nonmatch, collapse = "\n"),
                "\n\n")
  message(msg)
  df_test <- data.frame(model = mod_name,
                        stat = pred_name,
                        variable = var_nonmatch)
  df_missing <- rbind(df_missing, df_test)
  #  
  pred_name     <- "median"
  var_pred_test <- var_pred_q1
  var_nonmatch  <- var_mod_test[!var_mod_test %in% var_pred_test]
  msg <- paste0("\nModel variables (",
                mod_name,
                ") not in prediction data (",
                pred_name,
                "):\n",
                paste(var_nonmatch, collapse = "\n"),
                "\n\n")
  message(msg)
  df_test <- data.frame(model = mod_name,
                        stat = pred_name,
                        variable = var_nonmatch)
  df_missing <- rbind(df_missing, df_test)
  ### viol ----
  mod_name      <- "viol"
  var_mod_test  <- mod_varimp_viol
  #
  pred_name     <- "q1"
  var_pred_test <- var_pred_q1
  var_nonmatch  <- var_mod_test[!var_mod_test %in% var_pred_test]
  msg <- paste0("\nModel variables (",
                mod_name,
                ") not in prediction data (",
                pred_name,
                "):\n",
                paste(var_nonmatch, collapse = "\n"),
                "\n\n")
  message(msg)
  df_test <- data.frame(model = mod_name,
                        stat = pred_name,
                        variable = var_nonmatch)
  df_missing <- rbind(df_missing, df_test)
  #  
  pred_name     <- "q3"
  var_pred_test <- var_pred_q1
  var_nonmatch  <- var_mod_test[!var_mod_test %in% var_pred_test]
  msg <- paste0("\nModel variables (",
                mod_name,
                ") not in prediction data (",
                pred_name,
                "):\n",
                paste(var_nonmatch, collapse = "\n"),
                "\n\n")
  message(msg)
  df_test <- data.frame(model = mod_name,
                        stat = pred_name,
                        variable = var_nonmatch)
  df_missing <- rbind(df_missing, df_test)
  #  
  pred_name     <- "min"
  var_pred_test <- var_pred_q1
  var_nonmatch  <- var_mod_test[!var_mod_test %in% var_pred_test]
  msg <- paste0("\nModel variables (",
                mod_name,
                ") not in prediction data (",
                pred_name,
                "):\n",
                paste(var_nonmatch, collapse = "\n"),
                "\n\n")
  message(msg)
  df_test <- data.frame(model = mod_name,
                        stat = pred_name,
                        variable = var_nonmatch)
  df_missing <- rbind(df_missing, df_test)
  #  
  pred_name     <- "max"
  var_pred_test <- var_pred_q1
  var_nonmatch  <- var_mod_test[!var_mod_test %in% var_pred_test]
  msg <- paste0("\nModel variables (",
                mod_name,
                ") not in prediction data (",
                pred_name,
                "):\n",
                paste(var_nonmatch, collapse = "\n"),
                "\n\n")
  message(msg)
  df_test <- data.frame(model = mod_name,
                        stat = pred_name,
                        variable = var_nonmatch)
  df_missing <- rbind(df_missing, df_test)
  #  
  pred_name     <- "mean"
  var_pred_test <- var_pred_q1
  var_nonmatch  <- var_mod_test[!var_mod_test %in% var_pred_test]
  msg <- paste0("\nModel variables (",
                mod_name,
                ") not in prediction data (",
                pred_name,
                "):\n",
                paste(var_nonmatch, collapse = "\n"),
                "\n\n")
  message(msg)
  df_test <- data.frame(model = mod_name,
                        stat = pred_name,
                        variable = var_nonmatch)
  df_missing <- rbind(df_missing, df_test)
  #  
  pred_name     <- "median"
  var_pred_test <- var_pred_q1
  var_nonmatch  <- var_mod_test[!var_mod_test %in% var_pred_test]
  msg <- paste0("\nModel variables (",
                mod_name,
                ") not in prediction data (",
                pred_name,
                "):\n",
                paste(var_nonmatch, collapse = "\n"),
                "\n\n")
  message(msg)
  df_test <- data.frame(model = mod_name,
                        stat = pred_name,
                        variable = var_nonmatch)
  df_missing <- rbind(df_missing, df_test)
  #
  ## save missing ----
  write.csv(df_missing,
            "missing_params.csv",
            row.names = FALSE)
})## Test ~ Columns

