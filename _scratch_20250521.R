# Testing
# Erik.Leppo@tetratech.com
# 20240521
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Top Ten Variables by Importance

## HABDW_Risk
mod_name <- "HABDW_Risk"
mod_varimp <- data.frame(Variable = names(rfr_HABDW_Risk_model$variable.importance),
                         Importance = matrix(rfr_HABDW_Risk_model$variable.importance))
mod_varimp <- mod_varimp[order(mod_varimp[, "Importance"], decreasing = TRUE), ]
mod_varimp_10 <- mod_varimp[1:10, ]
mod_varimp_10
ggplot2::ggplot(mod_varimp_10, ggplot2::aes(x = reorder(Variable, Importance),
                                            y = Importance)) +
  ggplot2::geom_bar(stat = "identity") + 
  ggplot2::coord_flip() +
  ggplot2::theme_bw() +
  ggplot2::labs(x = "Model Variables (top ten)",
                title = mod_name)
  

## DWOps_Risk
mod_name <- "DWOps_Risk"
mod_varimp <- data.frame(Variable = names(rfr_DWOps_Risk_model$variable.importance),
                         Importance = matrix(rfr_DWOps_Risk_model$variable.importance))
mod_varimp <- mod_varimp[order(mod_varimp[, "Importance"], decreasing = TRUE), ]
mod_varimp_10 <- mod_varimp[1:10, ]
mod_varimp_10
ggplot2::ggplot(mod_varimp_10, ggplot2::aes(x = reorder(Variable, Importance), 
                                            y = Importance)) +
  ggplot2::geom_bar(stat = "identity") + 
  ggplot2::coord_flip() +
  ggplot2::theme_bw() +
  ggplot2::labs(x = "Model Variables (top ten)",
                title = mod_name)

## lake_Risk
mod_name <- "lake_Risk"
mod_varimp <- data.frame(Variable = names(rfr_lake_Risk_model$variable.importance),
                         Importance = matrix(rfr_lake_Risk_model$variable.importance))
mod_varimp <- mod_varimp[order(mod_varimp[, "Importance"], decreasing = TRUE), ]
mod_varimp_10 <- mod_varimp[1:10, ]
mod_varimp_10
ggplot2::ggplot(mod_varimp_10, ggplot2::aes(x = reorder(Variable, Importance), 
                                            y = Importance)) +
  ggplot2::geom_bar(stat = "identity") + 
  ggplot2::coord_flip() +
  ggplot2::theme_bw() +
  ggplot2::labs(x = "Model Variables (top ten)",
                title = mod_name)


## Model 
river_mean <- read.csv(file.path("data", "models", "Prediction_Dataset_mean.csv"))
# 33 variables, 2:34
rfr_HABDW_Risk_model$num.independent.variables
mod_input <- river_mean[1, 2:34]
pred <- predict(rfr_HABDW_Risk_model, data = mod_input)


mod_varimp_all <- dplyr::bind_rows(list(mod_varimp_cyan, 
                                        mod_varimp_dbp, 
                                        mod_varimp_dwops, 
                                        mod_varimp_habdw, 
                                        mod_varimp_lake,
                                        mod_varimp_treat,
                                        mod_varimp_viol))
write.csv(mod_varimp_all, file = "model_variable_types.csv")
