# Testing
# Erik.Leppo@tetratech.com
# 20240521
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Top Ten Variables by Importance----

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


# Map ----

## bbox----
sel_map_zoom <- "UT"
if (sel_map_zoom == "US") {
  bbox_zoom <- bbox_conus
} else if (sel_map_zoom == "US west") {
  bbox_zoom <- bbox_westus
} else {
  df_zoom <- df_coord_states |>
    filter(Postal_Abbreviation == sel_map_zoom)
  bbox_zoom <- c(df_zoom$Min_Longitude,
                 df_zoom$Max_Latitude, 
                 df_zoom$Max_Longitude, 
                 df_zoom$Min_Latitude)
}## IF ~ sel_map_zoom

## crop ----
sf::sf_use_s2(FALSE) # get error this isn't best but gets around error
HUC12_simple_zoom_crop <- sf::st_crop(HUC12_simple,
                                 sf::st_bbox(
                                   c(xmin = bbox_zoom[1],
                                     xmax = bbox_zoom[3],
                                     ymin = bbox_zoom[4],
                                     ymax = bbox_zoom[2])))
# microbenchmark, mean = 0.371 seconds

## intersect ----
sf_bbox_zoom <- sf::st_as_sfc(
  sf::st_bbox(
    c(xmin = bbox_zoom[1],
      xmax = bbox_zoom[3],
      ymin = bbox_zoom[4],
      ymax = bbox_zoom[2]),
    crs = sf::st_crs(HUC12_simple)))
sf::sf_use_s2(FALSE) # get error this isn't best but gets around error
HUC12_simple_zoom_intersect <- sf::st_intersection(HUC12_simple,
                                         sf_bbox_zoom)
# microbenchmark; mean = 0.256 ms + 374 ms = 0.374 s

## leaflet ----
leaflet::leaflet() |>
  leaflet::addTiles() |>
  leaflet::addProviderTiles("CartoDB.Positron", group="Positron") |>
  addProviderTiles(providers$OpenStreetMap, group = "Open Street Map") |>
  addProviderTiles(providers$Esri.WorldImagery, group = "ESRI World Imagery") |>
  # addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") |>
  # Layers, Control
  addLayersControl(baseGroups = c("Positron",
                                  "Open Street Map",
                                  "ESRI World Imagery")) |>
  # HUC Layer
  # leaflet::addPolygons(data = HUC12_simple,
  #             popup = ~paste0("HUC12: ", huc12, as.character("<br>"),
  #                             "Name: ", name, as.character("<br>"),
  #                             "Results: ", input$map_results, as.character("<br>"),
  #                             "Waterbody: ", input$map_water),
  #             weight = 1,
  #             color = "black",
  #             fillColor = "blue",
  #             smoothFactor = 5) |>
  leaflet::addPolygons(data = HUC12_simple_zoom_crop,
                       popup = ~paste0("HUC12: ", HUC_12, as.character("<br>"),
                                       "Name: ", HU_12_NAME, as.character("<br>"),
                                       # "Model: ", input$map_results, as.character("<br>"),
                                       # "Waterbody: ", input$map_water, as.character("<br>"),
                                       "Results, River Risk:", round(River_Risk, 1), as.character("<br>"),
                                       "Results, Lake Risk:", round(Lake_Risk, 1), as.character("<br>"),
                                       "Results, River RF:", round(River_RF, 1), as.character("<br>"),
                                       "Results, Lake RF:", round(Lake_RF, 1)
                                       ),
                       weight = 1,
                       color = "darkgray",
                       # fillColor = "skyblue",
                       # fillColor = ~sel_user_pal(),
                       fillColor = colorNumeric(
                         palette = "viridis",
                         domain = HUC12_simple$"River_Risk")(HUC12_simple$"River_Risk"),
                       smoothFactor = 0,
                       highlightOptions = highlightOptions(bringToFront = TRUE,
                                                           color = "darkgreen",
                                                           fillColor = "green",
                                                           weight = 3)) |>
  
  # addRasterImage(raster_huc12, opacity = 0.8) |>
  # Legend
  # addLegend("bottomleft",
  #           colors = "green",
  #           labels = "HUC02",
  #           values = NA) |>
  # Layers
  # Mini map
  leaflet::addMiniMap(toggleDisplay = TRUE) |>
  # Bounds
  fitBounds(bbox_westus[1], 
            bbox_westus[2], 
            bbox_westus[3], 
            bbox_westus[4]) |>
  addPolygons(data = sf_states,
              fillColor = NULL,
              color = "black",
              weight = 1.5,
              opacity = 1,
              fillOpacity = 0,
              group = "States")
  # leaflet::addCircleMarkers(data = HUC12_centroid,
  #                           lng = ~Longitude,
  #                           lat = ~Latitude,
  #                           color = "darkgray",
  #                           fillColor = colorNumeric(
  #                             palette = "viridis",
  #                             domain = HUC12_centroid$"River_Risk")(HUC12_centroid$"River_Risk"),
  #                           group = "HUC12")
  #                           # popup = ~paste0("HUC12: ", HUC_12, as.character("<br>"),
  #                           #                 "Name: ", HU_12_NAME, as.character("<br>"),
  #                           #                 "Model: ", input$map_results, as.character("<br>"),
  #                           #                 "Waterbody: ", input$map_water, as.character("<br>"),
  #                           #                 "Results, River Risk:", round(River_Risk, 1), as.character("<br>"),
  #                           #                 "Results, Lake Risk:", round(Lake_Risk, 1), as.character("<br>"),
  #                           #                 "Results, River RF:", round(River_RF, 1), as.character("<br>"),
  #                           #                 "Results, Lake RF:", round(Lake_RF, 1)
  #                           # ))
