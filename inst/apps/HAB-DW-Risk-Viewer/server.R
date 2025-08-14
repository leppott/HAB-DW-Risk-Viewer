#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Packages ----
# library(shiny)

# Server ----
function(input, output, session) {

  ## Map, Base ----
  output$map_huc <- renderLeaflet({

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
      # Mini map
      leaflet::addMiniMap(toggleDisplay = TRUE) |>
      # Bounds
      fitBounds(bbox_conus[1], 
                bbox_conus[2], 
                bbox_conus[3], 
                bbox_conus[4])
      
  })## map_huc
  
  # User Variable
  sel_user_var <- reactive({    
    
    if(input$map_water == "River") {
      suv1 <- "River"
    } else if (input$map_water == "Random Forest") {
      suv1 <- "Lake"
    }## IF ~ map_results
    
    if(input$map_results == "Risk") {
      suv2 <- "Risk"
    } else if (input$map_results == "Random Forest") {
      suv2 <- "RF"
    }## IF ~ map_results
    
    suv <- paste(suv1, suv2, sep = "_")
    
    return(suv)
    
  })
  
  sel_user_pal <- reactive({
    colorNumeric(
      palette = "viridis",
      domain = HUC12_simple[[sel_user_var]]
    )
    
  })
  
  
  # Map, Update ----
  # update map based on user selections
  # tied to Update button
  # https://rstudio.github.io/leaflet/shiny.html
  # need a reactive to trigger, use map update button
  observeEvent(input$but_map_update, {
    # shiny::withProgress({
    
      # ### 00, Initialize ----
      # prog_detail <- "Create Map..."
      # message(paste0("\n", prog_detail))
      # 
      # # Number of increments
      # prog_n <- 9
      # prog_sleep <- 0.25
      
      # check user selections
      
      # If waterbody = River, pop up with 
      
      # create a new map?\
      
      ## 01, change view ----
      # prog_detail <- "Change Tab"
      # message(paste0("\n", prog_detail))
      # # Increment the progress bar, and update the detail text.
      # incProgress(1/prog_n, detail = prog_detail)
      # Sys.sleep(prog_sleep)
      
      # updateTabItems(session, "tabs", "Map Selections")
      updateTabsetPanel(session, "inSelections", "Map")
   
      ## 02, Layer Type ----
      # prog_detail <- "Define Layer"
      # message(paste0("\n", prog_detail))
      # # Increment the progress bar, and update the detail text.
      # incProgress(1/prog_n, detail = prog_detail)
      # Sys.sleep(prog_sleep)
      
      # mapdatatype <- "polygon_simple" 
      # "polygon_simple", "centroid", "polygon_rf", "centroid_rf"
      if (input$rad_map_layertype == "points") {
        mapdatatype <- "centroid" 
      } else if (input$rad_map_layertype == "polygons") {
        mapdatatype <- "polygon_simple"
      }## IF ~ layer type
      
      ## 03, Map, bbox ----
      # prog_detail <- "Define Zoom"
      # message(paste0("\n", prog_detail))
      # # Increment the progress bar, and update the detail text.
      # incProgress(1/prog_n, detail = prog_detail)
      # Sys.sleep(prog_sleep)
      
      sel_map_zoom <- input$map_zoom
      if (sel_map_zoom == "CONUS") {
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
    
      ## 04, Data, User ----
      # prog_detail <- "Define Data"
      # message(paste0("\n", prog_detail))
      # # Increment the progress bar, and update the detail text.
      # incProgress(1/prog_n, detail = prog_detail)
      # Sys.sleep(prog_sleep)
      
      sel_map_model <- input$map_model
      
      if(sel_map_model == "cyan") {
        mod_varimp_user <- mod_varimp_cyan
        rfr_ranger <- rfr_cyan_model 
      } else if (sel_map_model == "DBP") {
        mod_varimp_user <- mod_varimp_dbp
        rfr_ranger <- rfr_DBP_model 
      } else if (sel_map_model == "DWOps_Risk") {
        mod_varimp_user <- mod_varimp_dwops
        rfr_ranger <- rfr_DWOps_Risk_model 
      } else if (sel_map_model == "HABDW_Risk") {
        mod_varimp_user <- mod_varimp_habdw
        rfr_ranger <- rfr_HABDW_Risk_model 
      } else if (sel_map_model == "lake_Risk") {
        mod_varimp_user <- mod_varimp_lake
        rfr_ranger <- rfr_lake_Risk_model 
      } else if (sel_map_model == "Treat_Risk") {
        mod_varimp_user <- mod_varimp_treat
        rfr_ranger <- rfr_Treat_Risk_model 
      } else if (sel_map_model == "Viol_Risk") {
        mod_varimp_user <- mod_varimp_viol
        rfr_ranger <- rfr_Viol_Risk_model 
      }## IF ~ sel_map_model
      
      ## 05, Scenario - User Select----
      # prog_detail <- "Define Scenario"
      # message(paste0("\n", prog_detail))
      # # Increment the progress bar, and update the detail text.
      # incProgress(1/prog_n, detail = prog_detail)
      # Sys.sleep(prog_sleep)
      
      
      mod_varimp_user[1, "User_Select"] <- input$map_radio_p01
      mod_varimp_user[2, "User_Select"] <- input$map_radio_p02
      mod_varimp_user[3, "User_Select"] <- input$map_radio_p03
      mod_varimp_user[4, "User_Select"] <- input$map_radio_p04
      mod_varimp_user[5, "User_Select"] <- input$map_radio_p05
      mod_varimp_user[6, "User_Select"] <- input$map_radio_p06
      mod_varimp_user[7, "User_Select"] <- input$map_radio_p07
      mod_varimp_user[8, "User_Select"] <- input$map_radio_p08
      mod_varimp_user[9, "User_Select"] <- input$map_radio_p09
      mod_varimp_user[10, "User_Select"] <- input$map_radio_p10
      mod_varimp_user[11, "User_Select"] <- input$map_radio_p11
      mod_varimp_user[12, "User_Select"] <- input$map_radio_p12
      mod_varimp_user[13, "User_Select"] <- input$map_radio_p13
      mod_varimp_user[14, "User_Select"] <- input$map_radio_p14
      mod_varimp_user[15, "User_Select"] <- input$map_radio_p15
      mod_varimp_user[16, "User_Select"] <- input$map_radio_p16
      mod_varimp_user[17, "User_Select"] <- input$map_radio_p17
      mod_varimp_user[18, "User_Select"] <- input$map_radio_p18
      mod_varimp_user[19, "User_Select"] <- input$map_radio_p19
      mod_varimp_user[20, "User_Select"] <- input$map_radio_p20
 
      # TEMP - create model input file
      # variable == mod_varimp_user[1, "Variable"]
      # user_select == mod_varimp_user[1, "User_Select"]
      # user_values == mod_scen_list [["1st quartile"]][, "Lake_Chla"]
      # update values based on user selections
      mod_scen_user <- mod_scen_mean # default
      #
      # 01
      var_num <- 1
      update_variable    <- mod_varimp_user[var_num, "Variable"]
      update_user_select <- mod_varimp_user[var_num, "User_Select"]
      mod_scen_user[, update_variable] <- 
        mod_scen_list[[update_user_select]][, update_variable]
      # 02
      var_num <- 2
      update_variable    <- mod_varimp_user[var_num, "Variable"]
      update_user_select <- mod_varimp_user[var_num, "User_Select"]
      mod_scen_user[, update_variable] <- 
        mod_scen_list[[update_user_select]][, update_variable]
      # 03
      var_num <- 3
      update_variable    <- mod_varimp_user[var_num, "Variable"]
      update_user_select <- mod_varimp_user[var_num, "User_Select"]
      mod_scen_user[, update_variable] <- 
        mod_scen_list[[update_user_select]][, update_variable]
      # 04
      var_num <- 4
      update_variable    <- mod_varimp_user[var_num, "Variable"]
      update_user_select <- mod_varimp_user[var_num, "User_Select"]
      mod_scen_user[, update_variable] <- 
        mod_scen_list[[update_user_select]][, update_variable]
      # 05
      var_num <- 5
      update_variable    <- mod_varimp_user[var_num, "Variable"]
      update_user_select <- mod_varimp_user[var_num, "User_Select"]
      mod_scen_user[, update_variable] <- 
        mod_scen_list[[update_user_select]][, update_variable]
      # 06
      var_num <- 6
      update_variable    <- mod_varimp_user[var_num, "Variable"]
      update_user_select <- mod_varimp_user[var_num, "User_Select"]
      mod_scen_user[, update_variable] <- 
        mod_scen_list[[update_user_select]][, update_variable]
      # 07
      var_num <- 7
      update_variable    <- mod_varimp_user[var_num, "Variable"]
      update_user_select <- mod_varimp_user[var_num, "User_Select"]
      mod_scen_user[, update_variable] <- 
        mod_scen_list[[update_user_select]][, update_variable]
      # 08
      var_num <- 8
      update_variable    <- mod_varimp_user[var_num, "Variable"]
      update_user_select <- mod_varimp_user[var_num, "User_Select"]
      mod_scen_user[, update_variable] <- 
        mod_scen_list[[update_user_select]][, update_variable]
      # 09
      var_num <- 9
      update_variable    <- mod_varimp_user[var_num, "Variable"]
      update_user_select <- mod_varimp_user[var_num, "User_Select"]
      mod_scen_user[, update_variable] <- 
        mod_scen_list[[update_user_select]][, update_variable]
      # 10
      var_num <- 10
      update_variable    <- mod_varimp_user[var_num, "Variable"]
      update_user_select <- mod_varimp_user[var_num, "User_Select"]
      mod_scen_user[, update_variable] <- 
        mod_scen_list[[update_user_select]][, update_variable]
      # 11
      var_num <- 11
      update_variable    <- mod_varimp_user[var_num, "Variable"]
      update_user_select <- mod_varimp_user[var_num, "User_Select"]
      mod_scen_user[, update_variable] <- 
        mod_scen_list[[update_user_select]][, update_variable]
      # 12
      var_num <- 12
      update_variable    <- mod_varimp_user[var_num, "Variable"]
      update_user_select <- mod_varimp_user[var_num, "User_Select"]
      mod_scen_user[, update_variable] <- 
        mod_scen_list[[update_user_select]][, update_variable]
      # 13
      var_num <- 13
      update_variable    <- mod_varimp_user[var_num, "Variable"]
      update_user_select <- mod_varimp_user[var_num, "User_Select"]
      mod_scen_user[, update_variable] <- 
        mod_scen_list[[update_user_select]][, update_variable]
      # 14
      var_num <- 14
      update_variable    <- mod_varimp_user[var_num, "Variable"]
      update_user_select <- mod_varimp_user[var_num, "User_Select"]
      mod_scen_user[, update_variable] <- 
        mod_scen_list[[update_user_select]][, update_variable]
      # 15
      var_num <- 15
      update_variable    <- mod_varimp_user[var_num, "Variable"]
      update_user_select <- mod_varimp_user[var_num, "User_Select"]
      mod_scen_user[, update_variable] <- 
        mod_scen_list[[update_user_select]][, update_variable]
      # 16
      var_num <- 16
      update_variable    <- mod_varimp_user[var_num, "Variable"]
      update_user_select <- mod_varimp_user[var_num, "User_Select"]
      mod_scen_user[, update_variable] <- 
        mod_scen_list[[update_user_select]][, update_variable]
      # 17
      var_num <- 17
      update_variable    <- mod_varimp_user[var_num, "Variable"]
      update_user_select <- mod_varimp_user[var_num, "User_Select"]
      mod_scen_user[, update_variable] <- 
        mod_scen_list[[update_user_select]][, update_variable]
      # 18
      var_num <- 18
      update_variable    <- mod_varimp_user[var_num, "Variable"]
      update_user_select <- mod_varimp_user[var_num, "User_Select"]
      mod_scen_user[, update_variable] <- 
        mod_scen_list[[update_user_select]][, update_variable]
      # 19
      var_num <- 19
      update_variable    <- mod_varimp_user[var_num, "Variable"]
      update_user_select <- mod_varimp_user[var_num, "User_Select"]
      mod_scen_user[, update_variable] <- 
        mod_scen_list[[update_user_select]][, update_variable]
      # 20
      var_num <- 20
      update_variable    <- mod_varimp_user[var_num, "Variable"]
      update_user_select <- mod_varimp_user[var_num, "User_Select"]
      mod_scen_user[, update_variable] <- 
        mod_scen_list[[update_user_select]][, update_variable]
      
      # save
      # ok to resave file
      write.csv(mod_varimp_user, 
                file.path("results", 
                          "model_variable_importance.csv"),
                row.names = FALSE)
      write.csv(mod_scen_user, 
                file.path("results", 
                          "model_scenario_user_values.csv"),
                row.names = FALSE)
  
      ## 06, Model Predict ----
      # prog_detail <- "Model Predict"
      # message(paste0("\n", prog_detail))
      # # Increment the progress bar, and update the detail text.
      # incProgress(1/prog_n, detail = prog_detail)
      # Sys.sleep(prog_sleep)

      # model == rfr_ranger (based on user selection)
      # prediction data == mean data with user mods for top 20
      mod_pred_data <- mod_scen_user
      # remove NA
      mod_pred_data_naomit <- na.omit(mod_pred_data)
      # drop HUC12
      mod_pred_data_naomit_rm_huc12 <- mod_pred_data_naomit
      mod_pred_data_naomit_rm_huc12[, "HUC12"] <- NULL
      # run model
      pred_user <- as.data.frame(predict(rfr_ranger,
                                         data = mod_pred_data_naomit_rm_huc12))
      # Add HUC12
      pred_user_results <- cbind(HUC12 = mod_pred_data_naomit[, "HUC12"],
                                 pred_user)
      
      ## 07, Save ----
      # prog_detail <- "Save Predictions"
      # message(paste0("\n", prog_detail))
      # # Increment the progress bar, and update the detail text.
      # incProgress(1/prog_n, detail = prog_detail)
      # Sys.sleep(prog_sleep)
      
      write.csv(pred_user_results,
                file.path("results", "predictions_user.csv"),
                row.names = FALSE)
      
      ## MERGE results with HUC12
      # inside of leaflet
  
  
      ## 08, update leaflet ----
      # prog_detail <- "Update Map"
      # message(paste0("\n", prog_detail))
      # # Increment the progress bar, and update the detail text.
      # incProgress(1/prog_n, detail = prog_detail)
      # Sys.sleep(prog_sleep)
  
      # prep data for join
      mod_pred_data <- mod_pred_data |>
        dplyr::mutate(HUC12 = as.character(HUC12))
      pred_user_results <- pred_user_results |>
        dplyr::mutate(HUC12 = as.character(HUC12))
     
      # Class Intervals
      # Apply class intervals
      
      len_pred <- length(unique(pred_user_results$prediction))
      if (len_pred == 1) {
        map_ci_val <- 1
        # minimum is 3, else get warning
        map_pal_col <- RColorBrewer::brewer.pal(
          n = max(3, 1), 
          name = map_pal)
      } else {
        map_ci_val <- classInt::classIntervals(
          pred_user_results$prediction,
          n = min(map_numclass, len_pred),
          style = map_classint)
        map_pal_col <- RColorBrewer::brewer.pal(
          n = length(map_ci_val$brks),
          name = map_pal)
      }## IF ~ len_pred
      
      
      
      if(mapdatatype == "centroid") {
  
        ### centroid ----
  
        sf::sf_use_s2(FALSE) # get error this isn't best but gets around error
        
        # add user selections and model results
        data_proxy <- HUC12_centroid |>
          dplyr::left_join(mod_pred_data,
                           by = c("HUC_12" = "HUC12")) |>
          dplyr::left_join(pred_user_results,
                           by = c("HUC_12" = "HUC12"))
  
        
        
        # Crop Data
        if (input$rad_map_crop == "Yes" & sel_map_zoom != "CONUS") {
           # crop data to selected state
          # data_proxy <- sf::st_crop(HUC12_centroid,
          #                                  sf::st_bbox(
          #                                    c(xmin = bbox_zoom[1],
          #                                      xmax = bbox_zoom[3],
          #                                      ymin = bbox_zoom[4],
          #                                      ymax = bbox_zoom[2])))
          # intersect to selected state
          sf_states_zoom <- sf_states |>
            filter(STUSPS == sel_map_zoom)
          data_proxy <- data_proxy |>
            sf::st_intersection(sf_states_zoom)
        } else {
          # # crop to western US
          # data_proxy <- sf::st_crop(HUC12_centroid,
          #                                    sf::st_bbox(
          #                                      c(xmin = bbox_westus[1],
          #                                        xmax = bbox_westus[3],
          #                                        ymin = bbox_westus[4],
          #                                        ymax = bbox_westus[2])))
          # CONUS
          # data_proxy <- HUC12_centroid
        }## IF ~ rad_map_crop
        
      
        leafletProxy("map_huc") |> #, data = data_proxy) |>
          # add spinner
          # addSpinner() |>
          # clean up map before adding to it
          clearControls() |>
          clearShapes() |>
          clearMarkers() |>
          # States
          addPolygons(data = sf_states,
                      fillColor = NULL,
                      color = "black",
                      weight = 1.5,
                      opacity = 1,
                      fillOpacity = 0,
                      group = "States") |>
          # Add HUC
          leaflet::addCircleMarkers(data = data_proxy,
                                    lng = ~Longitude,
                                    lat = ~Latitude,
                                    color = "darkgray",
                                    fillColor = colorNumeric(
                                      palette = "viridis",
                                      domain = data_proxy$"prediction")(data_proxy$"prediction"),
                                    group = "HUC12",
                                    popup = ~paste0("HUC12: ", HUC_12, as.character("<br>"),
                                                    "Name: ", HU_12_NAME, as.character("<br>"),
                                                    # "Model: ", input$map_results, as.character("<br>"),
                                                    # "Waterbody: ", input$map_water, as.character("<br>"),
                                                    "Prediction:", round(prediction, 1)
                                                    )## paste0 ~ popup
                                    ) |>
          # # Layers, Control
          addLayersControl(baseGroups = c("Positron",
                                          "Open Street Map",
                                          "ESRI World Imagery"),
                           overlayGroups = c("HUC12", "States")) |>
          # # Bounds
          fitBounds(bbox_zoom[1],
                    bbox_zoom[2],
                    bbox_zoom[3],
                    bbox_zoom[4]) # |>
          
        # Spinner, Stop
        # stopSpinner()
        # Layers, Hide
        #hideGroup("HUC12") # if hide by default not sure when loaded
        # Zoom in 
        # setView(zoom = 7) # needs lat long
        
        
      } else if (mapdatatype == "centroid_rf") {
        ### centroid_rf ----
        leafletProxy("map_huc", data = HUC12_centroid_rf) |>
          # add spinner
          # addSpinner() |>
          # clean up map before adding to it
          clearControls() |>
          clearShapes() |>
          clearMarkers() |>
          # Add HUC
          leaflet::addCircleMarkers(lng = ~Longitude,
                                    lat = ~Latitude,
                                    color = "darkgray",
                                    fillColor = colorNumeric(
                                      palette = "viridis",
                                      domain = HUC12_centroid$"River_Risk")(HUC12_centroid$"River_Risk"),
                                    group = "HUC12",
                                    popup = ~paste0("HUC12: ", HUC_12, as.character("<br>"),
                                                    "Name: ", HU_12_NAME, as.character("<br>"),
                                                    "Model: ", input$map_results, as.character("<br>"),
                                                    "Waterbody: ", input$map_water, as.character("<br>"),
                                                    "Results, River Risk:", round(River_Risk, 1), as.character("<br>"),
                                                    "Results, Lake Risk:", round(Lake_Risk, 1), as.character("<br>"),
                                                    "Results, River RF:", round(River_RF, 1), as.character("<br>"),
                                                    "Results, Lake RF:", round(Lake_RF, 1)
                                    )) |>
          # Layers, Control
          addLayersControl(baseGroups = c("Positron",
                                          "Open Street Map",
                                          "ESRI World Imagery"),
                           overlayGroups = "HUC12") |>
          # Bounds
          fitBounds(bbox_zoom[1], 
                    bbox_zoom[2], 
                    bbox_zoom[3], 
                    bbox_zoom[4])
        # Spinner, Stop
        # stopSpinner()
        # Layers, Hide
        #hideGroup("HUC12") # if hide by default not sure when loaded
        # Zoom in 
        # setView(zoom = 7) # needs lat long
        
        
      } else if (mapdatatype == "polygon_simple") {
        ### polygon_simple ----

        sf::sf_use_s2(FALSE) # get error this isn't best but gets around error
        
        # add user selections and model results
        data_proxy <- HUC12_simple |>
          dplyr::left_join(mod_pred_data,
                           by = c("HUC_12" = "HUC12")) |>
          dplyr::left_join(pred_user_results,
                           by = c("HUC_12" = "HUC12"))
        
        # ** ALWAYS ** Crop polygon data
        # Crop Data 
        if (sel_map_zoom != "CONUS") {
        # crop data to selected state
        # data_proxy <- sf::st_crop(HUC12_simple,
        #                                  sf::st_bbox(
        #                                    c(xmin = bbox_zoom[1],
        #                                      xmax = bbox_zoom[3],
        #                                      ymin = bbox_zoom[4],
        #                                      ymax = bbox_zoom[2])))
        # intersect to selected state
          sf_states_zoom <- sf_states |>
            filter(STUSPS == sel_map_zoom)
          data_proxy <- data_proxy |>
            sf::st_intersection(sf_states_zoom)
        } else {
          # CONUS
          # data_proxy <- HUC12_simple
        }## IF ~ sel_map_zoom
        
        leaflet::leafletProxy("map_huc") |> # , data = HUC12_simple) |>
          # clean up map before adding to it
          clearControls() |>
          clearShapes() |>
          clearMarkers() |>
          # States
          addPolygons(data = sf_states,
                      fillColor = NULL,
                      color = "black",
                      weight = 1.5,
                      opacity = 1,
                      fillOpacity = 0,
                      group = "States") |>
          # Add HUC
           leaflet::addPolygons(data = data_proxy,
                                group = "HUC12",
                                popup = ~paste0("HUC12: ", HUC_12, as.character("<br>"),
                                                "Name: ", HU_12_NAME, as.character("<br>"),
                                                # "Model: ", input$map_results, as.character("<br>"),
                                                # "Waterbody: ", input$map_water, as.character("<br>"),
                                                "Prediction:", round(prediction, 1)
                                                ),
                                 weight = 1,
                                 color = "darkgray",
                                 # fillColor = "skyblue",
                                 # fillColor = ~sel_user_pal(),
                                fillColor = colorNumeric(
                                  palette = "viridis",
                                  domain = data_proxy$"prediction")(data_proxy$"prediction"),
                                # smoothFactor = 0,
                                highlightOptions = highlightOptions(bringToFront = TRUE,
                                                                    color = "darkgreen",
                                                                    fillColor = "green",
                                                                    weight = 3)
                                ) |>
          # Layers, Control
          addLayersControl(baseGroups = c("Positron",
                                          "Open Street Map",
                                          "ESRI World Imagery"),
                           overlayGroups = c("HUC12", "States")) |>
          # Bounds
          fitBounds(bbox_zoom[1],
                    bbox_zoom[2],
                    bbox_zoom[3],
                    bbox_zoom[4])
  #       # # Layers, Hide
  #       # #hideGroup("HUC12") # if hide by default not sure when loaded
  #       # # Zoom in 
  #       # # setView(zoom = 7) # needs lat long
        
      } else if (mapdatatype == "polygon_rf") {
        ### polygon_rf ----
        leafletProxy("map_huc", data = HUC12_rf) |>
          # add spinner
          # addSpinner() |>
          # clean up map before adding to it
          clearControls() |>
          clearShapes() |>
          clearMarkers() |>
          # Add HUC
          leaflet::addPolygons(data = HUC12_simple,
                               group = "HUC12",
                               popup = ~paste0("HUC12: ", huc12, as.character("<br>"),
                                               "Name: ", name, as.character("<br>"),
                                               "Scenario: ", input$map_water_model, as.character("<br>"),
                                               # "Model: ", input$map_results, as.character("<br>"),
                                               # "Waterbody: ", input$map_water, as.character("<br>"),
                                               # "Endpoint: ", input$map_endpoint, as.character("<br>"),
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
                                 domain = HUC12_rf$"River_Risk")(HUC12_rf$"River_Risk"),
                               smoothFactor = 0,
                               highlightOptions = highlightOptions(bringToFront = TRUE,
                                                                   color = "darkgreen",
                                                                   fillColor = "green",
                                                                   weight = 3)) |>
          # Layers, Control
          addLayersControl(baseGroups = c("Positron",
                                          "Open Street Map",
                                          "ESRI World Imagery"),
                           overlayGroups = "HUC12") |>
          # Bounds
          fitBounds(bbox_zoom[1], 
                    bbox_zoom[2], 
                    bbox_zoom[3], 
                    bbox_zoom[4])
        # Spinner, Stop
        # stopSpinner()
        # Layers, Hide
        #hideGroup("HUC12") # if hide by default not sure when loaded
        # Zoom in 
        # setView(zoom = 7) # needs lat long
        
      }## IF ~ maptype
      
      ## 09, update summary  ----
      # prog_detail <- "Update Summary"
      # message(paste0("\n", prog_detail))
      # # Increment the progress bar, and update the detail text.
      # incProgress(1/prog_n, detail = prog_detail)
      # Sys.sleep(prog_sleep)
      
      ## 10, Plot, CDF ----
      # data subsetted above
      p_summ_cdf <- ggplot2::ggplot(data_proxy,
                               ggplot2::aes(x = prediction)) +
        ggplot2::stat_ecdf(geom = "step")+
        ggplot2::labs(x = "Risk Score",
                      y = "Proportion",
                      title = sel_map_zoom) + 
        ggplot2::theme_bw()
      # save
      fn_p <- file.path(dn_results, 
                        paste0("plot_summ_cdf.", plot_device))
      ggplot2::ggsave(fn_p, 
                      plot = p_summ_cdf,
                      height = plot_height,
                      width = plot_width,
                      units = plot_units,
                      # scale = plot_scale,
                      bg = plot_bg)
      
      ## 11, Plot, Box ----
      p_summ_box <- ggplot2::ggplot(data_proxy,
                                    ggplot2::aes(y = prediction)) +
        ggplot2::geom_boxplot() +
        ggplot2::labs(title = sel_map_zoom,
                      y = "Prediction") + 
        ggplot2::theme_bw() +
        ggplot2::theme(axis.text.x = ggplot2::element_blank(),
              axis.ticks.x = ggplot2::element_blank())
      # save
      fn_p <- file.path(dn_results, 
                        paste0("plot_summ_box.", plot_device))
      ggplot2::ggsave(fn_p, 
                      plot = p_summ_box,
                      height = plot_height,
                      width = plot_width,
                      units = plot_units,
                      # scale = plot_scale,
                      bg = plot_bg)
      
      ## 12, Table----
      
      df_summ_stat <- data.frame(
        location = sel_map_zoom,
        mean = mean(data_proxy$prediction,na.rm = TRUE),
        median = median(data_proxy$prediction,na.rm = TRUE),
        min = min(data_proxy$prediction,na.rm = TRUE),
        max = max(data_proxy$prediction,na.rm = TRUE),
        Quartile1st = quantile(data_proxy$prediction, 0.25, na.rm = TRUE),
        Quartile3rd = quantile(data_proxy$prediction, 0.75, na.rm = TRUE))
      
     fn_tbl <- file.path(dn_results, "summary_stats.csv")
     write.csv(df_summ_stat,
               fn_tbl,
               row.names = FALSE)
      
    # })## withProgress
  })## observer ~ but_map_update
  
  ## Summary, State ----
  # CDF, Bar charts, box plots, etc. 
  
  ### CDF ----
  # plot_sum_cdf <- eventReactive(input$but_map_range, {
 #  output$plot_summ_cdf <- renderPlot({
  output$plot_summ_cdf <- renderImage({ 
    
    # # respstate is merged scenario and HUC data
    # # scenarios
    # 
    # sel_map_zoom <- input$map_zoom
    # 
    # # may not use if/then in final, 
    # # use user selection on variables for model
    # sel_map_model <- input$map_model
    # sel_map_water <- input$map_water
    # 
    # if (sel_map_zoom == "" | sel_map_zoom == "CONUS") {
    #   # For all states
    #   p_cdf <- ggplot2::ggplot(scenarios, 
    #                            ggplot2::aes(scenarios[, varname])) + 
    #     ggplot2::stat_ecdf(geom = "step")+
    #     ggplot2::labs(x = "Risk Score", 
    #                   y = "Proportion",
    #                   title = "CONUS")
    # } else {
    #   # Subset out Stat
    #   state_sub = resp_state[resp_state$STATE == sel_map_zoom, ]
    #   state_sub = state_sub[complete.cases(state_sub[,c("HUC12")]),] 
    #   
    #   # Subset out variable:
    #   varname = sel_map_model #'Pred_HABDW_Risk_mean'
    #   state_sub2 = state_sub[, c('HUC12', varname)]
    #   #names(state_sub2)[2] = 'VARIABLE'
    #   
    #   # For specified State
    #   p_cdf_<- ggplot2::ggplot(state_sub, 
    #                            ggplot2::aes(state_sub[, varname])) + 
    #     ggplot2::stat_ecdf(geom = "step")+
    #     ggplot2::labs(x = "Risk Score", 
    #                   y = "Proportion",
    #                   title = sel_map_zoom)
    # }## IF ~ sel_map_zoom
    # 
    # return(p_cdf)
    
    # plot.ecdf(rnorm(24))
    
    fn_p <- file.path(dn_results, 
                      paste0("plot_summ_cdf.", plot_device))
    list(src = fn_p,
         contentType = 'image/png',
         alt = "CDF plot of user model predictions.")
    
  }, deleteFile = FALSE)## plot_summ_cdf
  
  ### box ----
  output$plot_summ_box <- renderPlot({ 
    # # Remove Cyan and DBP 
    # 
    # state_sub. = state_sub %>% 
    #   dplyr::select(-contains(c("DBP","Cyan"))) %>% 
    #   tidyr::drop_na(HUC12)
    # 
    # # Select all mean, median, 1stQ, etc.
    # BOX_category1 = state_sub. %>% 
    #   dplyr::select(contains(c('HUC12',"mean"))) %>%
    #   tidyr::drop_na(HUC12)
    # 
    # # Select single variable but keep mean, median, max, etc.
    # BOX_category2 = state_sub. %>%
    #   dplyr::select(contains(c('HUC12',"HABDW"))) %>% 
    #   tidyr::drop_na(HUC12)
    # 
    # BOX_category1. = reshape2::melt(BOX_category1, id = 'HUC12')
    # BOX_category2. = reshape2::melt(BOX_category2, id = 'HUC12')
    # 
    # order1 = c('Pred_HABDW_Risk_mean',
    #            'Pred_HABDW_Risk_min',
    #            'Pred_HABDW_Risk_1stQ',
    #            'Pred_HABDW_Risk_median',
    #            'Pred_HABDW_Risk_3rdQ',
    #            'Pred_HABDW_Risk_max')
    # 
    # 
    # # Plot the different variables together
    # ggplot2::ggplot(BOX_category1., 
    #                 ggplot2::aes(y=value,
    #            x=as.factor(variable), 
    #            fill=as.factor(variable)))+
    #   ggplot2::geom_boxplot()+
    #   #scale_y_log10()+
    #   ggplot2::ggtitle( "Surface Water")+
    #   ggplot2::labs(x="", y="Risk")+
    #   ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))+#puts title in center
    #   ggplot2::theme(legend.position="none")+
    #   ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))+
    #   #theme(panel.background = element_blank())+ # remove grey background
    #   ggplot2::theme(axis.line = ggplot2::element_line(colour = "black")) # add axis lines
    # 
    # # Change order of categories
    # BOX_category2.$variable <- factor(BOX_category2.$variable, 
    #                                   levels = order1)
    # 
    
    # 
    # boxplot(mtcars)
    # title("box plot")
    
     })
  
  
  ### box single variable----
  output$plot_summ_box_singlevar <- renderImage({ 
  
    # # Plot single variable
    # ggplot(BOX_category2., 
    #        aes(y=value,
    #            x=as.factor(variable), 
    #            fill=as.factor(variable)))+
    #   geom_boxplot()+
    #   #scale_y_log10()+
    #   #ggtitle( "Response")+
    #   labs(x="",y="Risk")+
    #   theme(plot.title = element_text(hjust = 0.5))+#puts title in center
    #   theme(legend.position="none")+
    #   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    #   #theme(panel.background = element_blank())+ # remove grey background
    #   theme(axis.line = element_line(colour = "black")) # add axis lines
    
    # boxplot(mtcars)
    # title("box plot, single variable")
    
    fn_p <- file.path(dn_results, 
                      paste0("plot_summ_box.", plot_device))
    list(src = fn_p,
         contentType = 'image/png',
         alt = "Box plot of user model predictions.")
    
  }, deleteFile = FALSE)
  
  ### table----
  
  # Table of Mean Min Max for different variables
  output$table_summ <- renderTable({
    
    # # respstate is merged scenario and HUC data
    # 
    # sel_map_zoom <- input$map_zoom
    # 
    # # may not use if/then in final, 
    # # use user selection on variables for model
    # sel_map_model <- input$map_model
    # sel_map_water <- input$map_water
    # 
    # # if (sel_map_zoom == "" | sel_map_zoom == "CONUS") {
    # #   # For all states
    # #   state_sub = resp_state
    # # } else {
    # #   # For specified State
    # #   # Subset out Stat
    # #   state_sub = resp_state[resp_state$STATE == sel_map_zoom, ]
    # #   #names(state_sub2)[2] = 'VARIABLE'
    # # }## IF ~ sel_map_zoom
    # # 
    # # state_sub = state_sub[complete.cases(state_sub[,c("HUC12")]),] 
    # # 
    # # # Subset out variable:
    # # varname = sel_map_model #'Pred_HABDW_Risk_mean'
    # # state_sub2 = state_sub[, c('HUC12', varname)]
    # # 
    # # temptable = data.frame(
    # #   mean = mean(state_sub[,varname],na.rm = TRUE),
    # #   median = median(state_sub[,varname],na.rm = TRUE),
    # #   min = min(state_sub[,varname],na.rm = TRUE),
    # #   max = max(state_sub[,varname],na.rm = TRUE),
    # #   Quartile1st = quantile(state_sub[,varname], 0.25, na.rm = TRUE),
    # #   Quartile3rd =quantile(state_sub[,varname], 0.75, na.rm = TRUE))
    # 
    # mtcars
    
    fn_tbl <- file.path(dn_results, "summary_stats.csv")

    validate(
      need(fn_tbl, "Update map to create table.")
    )## validate
    
    if (file.exists(fn_tbl)) {
      read.csv(fn_tbl)
    }## IF ~ file.exists
    
  })
  ## Model Performance ----
  
  ### Mod Perf, Plot ----
  output$plot_model_perf <- renderPlot({
    
    # may not use if/then in final, use user selection on variables for model
    sel_map_model <- input$map_model
    sel_map_water <- input$map_water
    
    if(sel_map_model == "cyan") {
      rfr_ranger <- rfr_cyan_model 
      tester1 <- rfr_test_cyan
    } else if (sel_map_model == "DBP") {
      rfr_ranger <- rfr_DBP_model 
      tester1 <- rfr_test_dbp
    } else if (sel_map_model == "DWOps_Risk") {
      rfr_ranger <- rfr_DWOps_Risk_model 
      tester1 <- rfr_test_dwops
    } else if (sel_map_model == "HABDW_Risk") {
      rfr_ranger <- rfr_HABDW_Risk_model 
      tester1 <- rfr_test_habdw
    } else if (sel_map_model == "lake_Risk") {
      rfr_ranger <- rfr_lake_Risk_model 
      tester1 <- rfr_test_lake
    } else if (sel_map_model == "Treat_Risk") {
      rfr_ranger <- rfr_Treat_Risk_model 
      tester1 <- rfr_test_treat
    } else if (sel_map_model == "Viol_Risk") {
      rfr_ranger <- rfr_Viol_Risk_model 
      tester1 <- rfr_test_viol
    }## IF ~ sel_map_model
    
    # R2 Testing Set
    yobs_tester <- as.numeric(unlist(tester1[,'Result_Risk']))
    ypred_tester <- predict(rfr_ranger, data = tester1)
    # r2_tester <- r2(yobs_tester, as.numeric(ypred_tester$predictions))
    
    tempDF <- data.frame(yobs = yobs_tester, ypred = ypred_tester$predictions)
    
    p_mod_summ <- tempDF |>
      ggplot2::ggplot(ggplot2::aes(yobs, ypred)) +
      ggplot2::geom_point(size = 1, color = 'blue') +
      ggplot2::geom_smooth(method = 'lm' , color = 'grey') +
      ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
      ggplot2::scale_x_continuous(expand = c(0, 0), limits = c(0, NA)) +
      ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
      ggplot2::labs(title = "Random Forest Observed vs Predicted",
                    x = "Observed ",
                    y = "Predicted ",
                    caption = sel_map_model) +
      ggplot2::theme_bw() +
      ggplot2::theme(text = ggplot2::element_text(size = 15,
                                                  face = "bold",
                                                  color = "blue"),
                     plot.title = ggplot2::element_text(hjust = 0.5, 
                                                        size = 15))
    # save
    fn_plot <- file.path("results", "plot_model_summary.png")
    ggplot2::ggsave(fn_plot,
                    height = plot_height,
                    width = plot_width,
                    units = plot_units,
                    bg = plot_bg)
    
    # output
    p_mod_summ
    
  })## plot_modperf
  
  
  ### Mod Perf, Table ----
  output$table_model_perf <- renderTable({
    
    sel_map_model <- input$map_model
    
    if(sel_map_model == "cyan") {
      rfr_ranger <- rfr_cyan_model 
      tester1 <- rfr_test_cyan
    } else if (sel_map_model == "DBP") {
      rfr_ranger <- rfr_DBP_model 
      tester1 <- rfr_test_dbp
    } else if (sel_map_model == "DWOps_Risk") {
      rfr_ranger <- rfr_DWOps_Risk_model 
      tester1 <- rfr_test_dwops
    } else if (sel_map_model == "HABDW_Risk") {
      rfr_ranger <- rfr_HABDW_Risk_model 
      tester1 <- rfr_test_habdw
    } else if (sel_map_model == "lake_Risk") {
      rfr_ranger <- rfr_lake_Risk_model 
      tester1 <- rfr_test_lake
    } else if (sel_map_model == "Treat_Risk") {
      rfr_ranger <- rfr_Treat_Risk_model 
      tester1 <- rfr_test_treat
    } else if (sel_map_model == "Viol_Risk") {
      rfr_ranger <- rfr_Viol_Risk_model 
      tester1 <- rfr_test_viol
    }## IF ~ sel_map_model
    
    r2_report    <- rfr_ranger$r.squared
    error_report <- rfr_ranger$prediction.error 
    
    # function to calculate r-squared
    r2 <- function(y,pred){
      1 - var(y - pred) / var(y)
    }
    
    # R2 Testing Set
    yobs_tester <- as.numeric(unlist(tester1[,'Result_Risk']))
    ypred_tester <- predict(rfr_ranger, data = tester1)
    r2_tester <- r2(yobs_tester, as.numeric(ypred_tester$predictions))
    
    #Root Mean Squared Error (RMSE)
    rmse <- sqrt(mean((ypred_tester$predictions-yobs_tester)^2))
    
    # Mean Bias
    bias <- (sum(ypred_tester$predictions) - sum(yobs_tester)) / length(yobs_tester) 
    
    #standard deviation of the error
    SD <- sd(ypred_tester$predictions - yobs_tester)
    
    # Nash-Sutcliffe efficiency coefficient 
    nse2 <- ie2misc::vnse(ypred_tester$predictions, yobs_tester, na.rm = TRUE) 
    
    table_col_1 <- c("r.squared (reported by Ranger function)",
                     "prediction.error (reported by Ranger function)",
                     "R squared (testing set)",
                     "Root Mean Squared Error",
                     "Mean Bias",
                     "Standard deviation of the error",
                     "Nash-Sutcliffe efficiency coefficient")
    
    table_col_2 <- c(r2_report, error_report, r2_tester, rmse,bias, SD, nse2)
    table_col_2 <- formatC(table_col_2, digits = 4, format = "f")
    
    table_to_print <- data.frame(cbind(table_col_1, table_col_2))
    colnames(table_to_print) <- c("Model Uncertainty Metrics","Values")
    
    # save
    write.csv(table_to_print, 
              file.path("results", "model_summary_metrics.csv"),
              row.names = FALSE)
    
    # output
    table_to_print
  }, 
  type = "html",
  bordered = TRUE,
  striped = TRUE,
  align = "c"
  )
  
  # observe({
  #   # user selection
  #   user_water <- input$map_water
  #   user_results <- input$map_results
  #   
  #   # update map
  #   leaflet::leafletProxy("map") |>
  #     leaflet::clearShapes() |>
  #     leaflet::addPolygons(data = HUC12_simple,
  #                          # color = "black",
  #                          fill = "blue",
  #                          popup = ~paste0("HUC12: ", huc12, as.character("<br>"),
  #                                          "Name: ", name, as.character("<br>"),
  #                                          "Results: ", user_results, as.character("<br>"),
  #                                          "Waterbody: ", user_water))
  #   
  #   
  #   
  #   
  # })## observe ~ water
  
  # Model, Var Imp ----
  # output$plot_model_rfr_Lake_Risk <- renderPlot({
  #   vip::vip(rfr_lake_Risk_model) + 
  #     ggplot2::theme_bw() + 
  #     ggplot2::labs(title = "rfr_lake_Risk_model")
  #   # plotly::ggplotly(p)
  # })
  
  ### Model, Var Imp, Plot ----
  output$plot_model_varimp <- renderPlot({
    sel_map_model <- input$map_model
    sel_map_water <- input$map_water
    if(sel_map_model == "cyan") {
      mod_name <- mod_varimp_cyan
    } else if (sel_map_model == "DBP") {
      mod_name <- mod_varimp_dbp
    } else if (sel_map_model == "DWOps_Risk") {
      mod_name <- mod_varimp_dwops
    } else if (sel_map_model == "HABDW_Risk") {
      mod_name <- mod_varimp_habdw
    } else if (sel_map_model == "lake_Risk") {
      mod_name <- mod_varimp_lake
    } else if (sel_map_model == "Treat_Risk") {
      mod_name <- mod_varimp_treat
    } else if (sel_map_model == "Viol_Risk") {
      mod_name <- mod_varimp_viol
    }## IF ~ sel_map_model
    #
    # Top N Variable Importance and Plot
    p_varimp <- mod_name |>
      dplyr::slice(1:imp_n) |>
      ggplot2::ggplot(ggplot2::aes(x = reorder(Variable, Importance),
                                   y = Importance)) +
        ggplot2::geom_bar(stat = "identity") + 
        ggplot2::coord_flip() +
        ggplot2::theme_bw(base_size = 18) +
        ggplot2::labs(x = "Model Variables",
                      title = sel_map_model,
                      subtitle = paste0("Top ", imp_n, " Variables"),
                      caption = paste0("Waterbody: ", sel_map_water))
    
    # save
    fn_plot <- file.path("results", "plot_variable_importance.png")
    ggplot2::ggsave(fn_plot,
                    height = plot_height,
                    width = plot_width,
                    units = plot_units,
                    bg = plot_bg)
    
    # output
    p_varimp
  })## renderPlot
  
  ### Model, Var Imp, Table ----
  output$table_model_varimp <- renderTable({
    sel_map_model <- input$map_model
    if(sel_map_model == "cyan") {
      mod_varimp_user <- mod_varimp_cyan
    } else if (sel_map_model == "DBP") {
      mod_varimp_user <- mod_varimp_dbp
    } else if (sel_map_model == "DWOps_Risk") {
      mod_varimp_user <- mod_varimp_dwops
    } else if (sel_map_model == "HABDW_Risk") {
      mod_varimp_user <- mod_varimp_habdw
    } else if (sel_map_model == "lake_Risk") {
      mod_varimp_user <- mod_varimp_lake
    } else if (sel_map_model == "Treat_Risk") {
      mod_varimp_user <- mod_varimp_treat
    } else if (sel_map_model == "Viol_Risk") {
      mod_varimp_user <- mod_varimp_viol
    }## IF ~ sel_map_model
    
    # Scenario - User Select
    mod_varimp_user[ 1, "User_Select"] <- input$map_radio_p01
    mod_varimp_user[ 2, "User_Select"] <- input$map_radio_p02
    mod_varimp_user[ 3, "User_Select"] <- input$map_radio_p03
    mod_varimp_user[ 4, "User_Select"] <- input$map_radio_p04
    mod_varimp_user[ 5, "User_Select"] <- input$map_radio_p05
    mod_varimp_user[ 6, "User_Select"] <- input$map_radio_p06
    mod_varimp_user[ 7, "User_Select"] <- input$map_radio_p07
    mod_varimp_user[ 8, "User_Select"] <- input$map_radio_p08
    mod_varimp_user[ 9, "User_Select"] <- input$map_radio_p09
    mod_varimp_user[10, "User_Select"] <- input$map_radio_p10
    mod_varimp_user[11, "User_Select"] <- input$map_radio_p11
    mod_varimp_user[12, "User_Select"] <- input$map_radio_p12
    mod_varimp_user[13, "User_Select"] <- input$map_radio_p13
    mod_varimp_user[14, "User_Select"] <- input$map_radio_p14
    mod_varimp_user[15, "User_Select"] <- input$map_radio_p15
    mod_varimp_user[16, "User_Select"] <- input$map_radio_p16
    mod_varimp_user[17, "User_Select"] <- input$map_radio_p17
    mod_varimp_user[18, "User_Select"] <- input$map_radio_p18
    mod_varimp_user[19, "User_Select"] <- input$map_radio_p19
    mod_varimp_user[20, "User_Select"] <- input$map_radio_p20
    
    # TEMP - create model input file
    # variable == mod_varimp_user[1, "Variable"]
    # user_select == mod_varimp_user[1, "User_Select"]
    # user_values == mod_scen_list [["1st quartile"]][, "Lake_Chla"]
    # update values based on user selections
    mod_scen_user <- mod_scen_mean # default
    #
    # 01
    var_num <- 1
    update_variable    <- mod_varimp_user[var_num, "Variable"]
    update_user_select <- mod_varimp_user[var_num, "User_Select"]
    mod_scen_user[, update_variable] <- 
      mod_scen_list[[update_user_select]][, update_variable]
    # 02
    var_num <- 2
    update_variable    <- mod_varimp_user[var_num, "Variable"]
    update_user_select <- mod_varimp_user[var_num, "User_Select"]
    mod_scen_user[, update_variable] <- 
      mod_scen_list[[update_user_select]][, update_variable]
    # 03
    var_num <- 3
    update_variable    <- mod_varimp_user[var_num, "Variable"]
    update_user_select <- mod_varimp_user[var_num, "User_Select"]
    mod_scen_user[, update_variable] <- 
      mod_scen_list[[update_user_select]][, update_variable]
    # 04
    var_num <- 4
    update_variable    <- mod_varimp_user[var_num, "Variable"]
    update_user_select <- mod_varimp_user[var_num, "User_Select"]
    mod_scen_user[, update_variable] <- 
      mod_scen_list[[update_user_select]][, update_variable]
    # 05
    var_num <- 5
    update_variable    <- mod_varimp_user[var_num, "Variable"]
    update_user_select <- mod_varimp_user[var_num, "User_Select"]
    mod_scen_user[, update_variable] <- 
      mod_scen_list[[update_user_select]][, update_variable]
    # 06
    var_num <- 6
    update_variable    <- mod_varimp_user[var_num, "Variable"]
    update_user_select <- mod_varimp_user[var_num, "User_Select"]
    mod_scen_user[, update_variable] <- 
      mod_scen_list[[update_user_select]][, update_variable]
    # 07
    var_num <- 7
    update_variable    <- mod_varimp_user[var_num, "Variable"]
    update_user_select <- mod_varimp_user[var_num, "User_Select"]
    mod_scen_user[, update_variable] <- 
      mod_scen_list[[update_user_select]][, update_variable]
    # 08
    var_num <- 8
    update_variable    <- mod_varimp_user[var_num, "Variable"]
    update_user_select <- mod_varimp_user[var_num, "User_Select"]
    mod_scen_user[, update_variable] <- 
      mod_scen_list[[update_user_select]][, update_variable]
    # 09
    var_num <- 9
    update_variable    <- mod_varimp_user[var_num, "Variable"]
    update_user_select <- mod_varimp_user[var_num, "User_Select"]
    mod_scen_user[, update_variable] <- 
      mod_scen_list[[update_user_select]][, update_variable]
    # 10
    var_num <- 10
    update_variable    <- mod_varimp_user[var_num, "Variable"]
    update_user_select <- mod_varimp_user[var_num, "User_Select"]
    mod_scen_user[, update_variable] <- 
      mod_scen_list[[update_user_select]][, update_variable]
    # 11
    var_num <- 11
    update_variable    <- mod_varimp_user[var_num, "Variable"]
    update_user_select <- mod_varimp_user[var_num, "User_Select"]
    mod_scen_user[, update_variable] <- 
      mod_scen_list[[update_user_select]][, update_variable]
    # 12
    var_num <- 12
    update_variable    <- mod_varimp_user[var_num, "Variable"]
    update_user_select <- mod_varimp_user[var_num, "User_Select"]
    mod_scen_user[, update_variable] <- 
      mod_scen_list[[update_user_select]][, update_variable]
    # 13
    var_num <- 13
    update_variable    <- mod_varimp_user[var_num, "Variable"]
    update_user_select <- mod_varimp_user[var_num, "User_Select"]
    mod_scen_user[, update_variable] <- 
      mod_scen_list[[update_user_select]][, update_variable]
    # 14
    var_num <- 14
    update_variable    <- mod_varimp_user[var_num, "Variable"]
    update_user_select <- mod_varimp_user[var_num, "User_Select"]
    mod_scen_user[, update_variable] <- 
      mod_scen_list[[update_user_select]][, update_variable]
    # 15
    var_num <- 15
    update_variable    <- mod_varimp_user[var_num, "Variable"]
    update_user_select <- mod_varimp_user[var_num, "User_Select"]
    mod_scen_user[, update_variable] <- 
      mod_scen_list[[update_user_select]][, update_variable]
    # 16
    var_num <- 16
    update_variable    <- mod_varimp_user[var_num, "Variable"]
    update_user_select <- mod_varimp_user[var_num, "User_Select"]
    mod_scen_user[, update_variable] <- 
      mod_scen_list[[update_user_select]][, update_variable]
    # 17
    var_num <- 17
    update_variable    <- mod_varimp_user[var_num, "Variable"]
    update_user_select <- mod_varimp_user[var_num, "User_Select"]
    mod_scen_user[, update_variable] <- 
      mod_scen_list[[update_user_select]][, update_variable]
    # 18
    var_num <- 18
    update_variable    <- mod_varimp_user[var_num, "Variable"]
    update_user_select <- mod_varimp_user[var_num, "User_Select"]
    mod_scen_user[, update_variable] <- 
      mod_scen_list[[update_user_select]][, update_variable]
    # 19
    var_num <- 19
    update_variable    <- mod_varimp_user[var_num, "Variable"]
    update_user_select <- mod_varimp_user[var_num, "User_Select"]
    mod_scen_user[, update_variable] <- 
      mod_scen_list[[update_user_select]][, update_variable]
    # 20
    var_num <- 20
    update_variable    <- mod_varimp_user[var_num, "Variable"]
    update_user_select <- mod_varimp_user[var_num, "User_Select"]
    mod_scen_user[, update_variable] <- 
      mod_scen_list[[update_user_select]][, update_variable]
    
    # mod_scen_user 
    # updated to "mean" values 
    # top 20 variables updated to user selections
    
    # save
    write.csv(mod_varimp_user, 
              file.path("results", "model_variable_importance.csv"),
              row.names = FALSE)
    write.csv(mod_scen_user, 
              file.path("results", "model_scenario_user_values.csv"),
              row.names = FALSE)
    
    # output
    mod_varimp_user
  })
  
 
  # Misc ----
  output$str_water <- renderText({
    paste0("Waterbody: ", input$map_water)
  })
  
  output$str_model <- renderText({
    paste0("Model: ", input$map_model)
  })
  

  # Radio  ----
  observeEvent(input$map_model, {
    sel_map_model <- input$map_model
    rnd_imp <- 3

  if (sel_map_model == "cyan") {
    ### Lab, cyan ----  
    mod_radio <- mod_varimp_cyan
    lab_radio_01 <- paste0(mod_radio[1, 1],
                           # " (Importance = ", 
                           # round(mod_radio[1, 2], rnd_imp),
                           # ")",                      
                           " [Location = ",
                           mod_radio[1, 4],
                           "]")
    lab_radio_02 <- paste0(mod_radio[2, 1],
                           # " (Importance = ", 
                           # round(mod_radio[2, 2], rnd_imp),
                           # ")",                       
                           " [Location = ",
                           mod_radio[2, 4],
                           "]")
    lab_radio_03 <- paste0(mod_radio[3, 1],
                           # " (Importance = ", 
                           # round(mod_radio[3, 2], rnd_imp),
                           # ")",                       
                           " [Location = ",
                           mod_radio[3, 4],
                           "]")
    lab_radio_04 <- paste0(mod_radio[4, 1],
                           # " (Importance = ", 
                           # round(mod_radio[4, 2], rnd_imp),
                           # ")",                     
                           " [Location = ",
                           mod_radio[4, 4],
                           "]")
    lab_radio_05 <- paste0(mod_radio[5, 1],
                           # " (Importance = ", 
                           # round(mod_radio[5, 2], rnd_imp),
                           # ")",                         
                           " [Location = ",
                           mod_radio[5, 4],
                           "]")
    lab_radio_06 <- paste0(mod_radio[6, 1],
                           # " (Importance = ", 
                           # round(mod_radio[6, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[6, 4],
                           "]")
    lab_radio_07 <- paste0(mod_radio[7, 1],
                           # " (Importance = ", 
                           # round(mod_radio[7, 2], rnd_imp),
                           # ")",                        
                           " [Location = ",
                           mod_radio[7, 4],
                           "]")
    lab_radio_08 <- paste0(mod_radio[8, 1],
                           # " (Importance = ", 
                           # round(mod_radio[8, 2], rnd_imp),
                           # ")",                         
                           " [Location = ",
                           mod_radio[8, 4],
                           "]")
    lab_radio_09 <- paste0(mod_radio[9, 1],
                           # " (Importance = ", 
                           # round(mod_radio[9, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[9, 4],
                           "]")
    lab_radio_10 <- paste0(mod_radio[10, 1],
                           # " (Importance = ", 
                           # round(mod_radio[10, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[9, 4],
                           "]")
    lab_radio_11 <- paste0(mod_radio[11, 1],
                           # " (Importance = ", 
                           # round(mod_radio[11, 2], rnd_imp),
                           # ")",                           
                           " [Location = ",
                           mod_radio[11, 4],
                           "]")
    lab_radio_12 <- paste0(mod_radio[12, 1],
                           # " (Importance = ", 
                           # round(mod_radio[12, 2], rnd_imp),
                           # ")",                           
                           " [Location = ",
                           mod_radio[12, 4],
                           "]")
    lab_radio_13 <- paste0(mod_radio[13, 1],
                           # " (Importance = ", 
                           # round(mod_radio[13, 2], rnd_imp),
                           # ")",                           
                           " [Location = ",
                           mod_radio[13, 4],
                           "]")
    lab_radio_14 <- paste0(mod_radio[14, 1],
                           # " (Importance = ", 
                           # round(mod_radio[14, 2], rnd_imp),
                           # ")",                         
                           " [Location = ",
                           mod_radio[14, 4],
                           "]")
    lab_radio_15 <- paste0(mod_radio[15, 1],
                           # " (Importance = ", 
                           # round(mod_radio[15, 2], rnd_imp),
                           # ")",                         
                           " [Location = ",
                           mod_radio[15, 4],
                           "]")
    lab_radio_16 <- paste0(mod_radio[16, 1],
                           # " (Importance = ", 
                           # round(mod_radio[16, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[16, 4],
                           "]")
    lab_radio_17 <- paste0(mod_radio[17, 1],
                           " (Importance = ", 
                           round(mod_radio[17, 2], rnd_imp),
                           ")",                         
                           " [Location = ",
                           mod_radio[17, 4],
                           "]")
    lab_radio_18 <- paste0(mod_radio[18, 1],
                           # " (Importance = ", 
                           # round(mod_radio[18, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[18, 4],
                           "]")
    lab_radio_19 <- paste0(mod_radio[19, 1],
                           # " (Importance = ", 
                           # round(mod_radio[19, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[19, 4],
                           "]")
    lab_radio_20 <- paste0(mod_radio[20, 1],
                           # " (Importance = ", 
                           # round(mod_radio[20, 2], rnd_imp),
                           # ")",                         
                           " [Location = ",
                           mod_radio[20, 4],
                           "]")
  } else if (sel_map_model == "DBP") {
    ### Lab, dbp ----
    mod_radio <- mod_varimp_dbp
    lab_radio_01 <- paste0(mod_radio[1, 1],
                           # " (Importance = ", 
                           # round(mod_radio[1, 2], rnd_imp),
                           # ")",                         
                           " [Location = ",
                           mod_radio[1, 4],
                           "]")
    lab_radio_02 <- paste0(mod_radio[2, 1],
                           # " (Importance = ", 
                           # round(mod_radio[2, 2], rnd_imp),
                           # ")",                         
                           " [Location = ",
                           mod_radio[2, 4],
                           "]")
    lab_radio_03 <- paste0(mod_radio[3, 1],
                           # " (Importance = ", 
                           # round(mod_radio[3, 2], rnd_imp),
                           # ")",                           
                           " [Location = ",
                           mod_radio[3, 4],
                           "]")
    lab_radio_04 <- paste0(mod_radio[4, 1],
                           # " (Importance = ", 
                           # round(mod_radio[4, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[4, 4],
                           "]")
    lab_radio_05 <- paste0(mod_radio[5, 1],
                           # " (Importance = ", 
                           # round(mod_radio[5, 2], rnd_imp),
                           # ")",                         
                           " [Location = ",
                           mod_radio[5, 4],
                           "]")
    lab_radio_06 <- paste0(mod_radio[6, 1],
                           # " (Importance = ", 
                           # round(mod_radio[6, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[6, 4],
                           "]")
    lab_radio_07 <- paste0(mod_radio[7, 1],
                           # " (Importance = ", 
                           # round(mod_radio[7, 2], rnd_imp),
                           # ")",                           
                           " [Location = ",
                           mod_radio[7, 4],
                           "]")
    lab_radio_08 <- paste0(mod_radio[8, 1],
                           # " (Importance = ", 
                           # round(mod_radio[8, 2], rnd_imp),
                           # ")",                           
                           " [Location = ",
                           mod_radio[8, 4],
                           "]")
    lab_radio_09 <- paste0(mod_radio[9, 1],
                           # " (Importance = ", 
                           # round(mod_radio[9, 2], rnd_imp),
                           # ")",                            
                           " [Location = ",
                           mod_radio[9, 4],
                           "]")
    lab_radio_10 <- paste0(mod_radio[10, 1],
                           # " (Importance = ", 
                           # round(mod_radio[10, 2], rnd_imp),
                           # ")",                            
                           " [Location = ",
                           mod_radio[9, 4],
                           "]")
    lab_radio_11 <- paste0(mod_radio[11, 1],
                           # " (Importance = ", 
                           # round(mod_radio[11, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[11, 4],
                           "]")
    lab_radio_12 <- paste0(mod_radio[12, 1],
                           # " (Importance = ", 
                           # round(mod_radio[12, 2], rnd_imp),
                           # ")",                           
                           " [Location = ",
                           mod_radio[12, 4],
                           "]")
    lab_radio_13 <- paste0(mod_radio[13, 1],
                           # " (Importance = ", 
                           # round(mod_radio[13, 2], rnd_imp),
                           # ")",                           
                           " [Location = ",
                           mod_radio[13, 4],
                           "]")
    lab_radio_14 <- paste0(mod_radio[14, 1],
                           # " (Importance = ", 
                           # round(mod_radio[14, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[14, 4],
                           "]")
    lab_radio_15 <- paste0(mod_radio[15, 1],
                           # " (Importance = ", 
                           # round(mod_radio[15, 2], rnd_imp),
                           # ")",                           
                           " [Location = ",
                           mod_radio[15, 4],
                           "]")
    lab_radio_16 <- paste0(mod_radio[16, 1],
                           # " (Importance = ", 
                           # round(mod_radio[16, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[16, 4],
                           "]")
    lab_radio_17 <- paste0(mod_radio[17, 1],
                           # " (Importance = ", 
                           # round(mod_radio[17, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[17, 4],
                           "]")
    lab_radio_18 <- paste0(mod_radio[18, 1],
                           # " (Importance = ", 
                           # round(mod_radio[18, 2], rnd_imp),
                           # ")",                         
                           " [Location = ",
                           mod_radio[18, 4],
                           "]")
    lab_radio_19 <- paste0(mod_radio[19, 1],
                           # " (Importance = ", 
                           # round(mod_radio[19, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[19, 4],
                           "]")
    lab_radio_20 <- paste0(mod_radio[20, 1],
                           # " (Importance = ", 
                           # round(mod_radio[20, 2], rnd_imp),
                           # ")",                         
                           " [Location = ",
                           mod_radio[20, 4],
                           "]")
  } else if (sel_map_model == "DWOps_Risk") {
    ### Lab, dwops ----
    mod_radio <- mod_varimp_dwops
    lab_radio_01 <- paste0(mod_radio[1, 1],
                           # " (Importance = ", 
                           # round(mod_radio[1, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[1, 4],
                           "]")
    lab_radio_02 <- paste0(mod_radio[2, 1],
                           # " (Importance = ", 
                           # round(mod_radio[2, 2], rnd_imp),
                           # ")",                           
                           " [Location = ",
                           mod_radio[2, 4],
                           "]")
    lab_radio_03 <- paste0(mod_radio[3, 1],
                           # " (Importance = ", 
                           # round(mod_radio[3, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[3, 4],
                           "]")
    lab_radio_04 <- paste0(mod_radio[4, 1],
                           # " (Importance = ", 
                           # round(mod_radio[4, 2], rnd_imp),
                           # ")",                           
                           " [Location = ",
                           mod_radio[4, 4],
                           "]")
    lab_radio_05 <- paste0(mod_radio[5, 1],
                           # " (Importance = ", 
                           # round(mod_radio[5, 2], rnd_imp),
                           # ")",                           
                           " [Location = ",
                           mod_radio[5, 4],
                           "]")
    lab_radio_06 <- paste0(mod_radio[6, 1],
                           # " (Importance = ", 
                           # round(mod_radio[6, 2], rnd_imp),
                           # ")",                           
                           " [Location = ",
                           mod_radio[6, 4],
                           "]")
    lab_radio_07 <- paste0(mod_radio[7, 1],
                           # " (Importance = ", 
                           # round(mod_radio[7, 2], rnd_imp),
                           # ")",                         
                           " [Location = ",
                           mod_radio[7, 4],
                           "]")
    lab_radio_08 <- paste0(mod_radio[8, 1],
                           # " (Importance = ", 
                           # round(mod_radio[8, 2], rnd_imp),
                           # ")",                         
                           " [Location = ",
                           mod_radio[8, 4],
                           "]")
    lab_radio_09 <- paste0(mod_radio[9, 1],
                           # " (Importance = ", 
                           # round(mod_radio[9, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[9, 4],
                           "]")
    lab_radio_10 <- paste0(mod_radio[10, 1],
                           # " (Importance = ", 
                           # round(mod_radio[10, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[9, 4],
                           "]")
    lab_radio_11 <- paste0(mod_radio[11, 1],
                           # " (Importance = ", 
                           # round(mod_radio[11, 2], rnd_imp),
                           # ")",                         
                           " [Location = ",
                           mod_radio[11, 4],
                           "]")
    lab_radio_12 <- paste0(mod_radio[12, 1],
                           # " (Importance = ", 
                           # round(mod_radio[12, 2], rnd_imp),
                           # ")",                        
                           " [Location = ",
                           mod_radio[12, 4],
                           "]")
    lab_radio_13 <- paste0(mod_radio[13, 1],
                           # " (Importance = ", 
                           # round(mod_radio[13, 2], rnd_imp),
                           # ")",                        
                           " [Location = ",
                           mod_radio[13, 4],
                           "]")
    lab_radio_14 <- paste0(mod_radio[14, 1],
                           # " (Importance = ", 
                           # round(mod_radio[14, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[14, 4],
                           "]")
    lab_radio_15 <- paste0(mod_radio[15, 1],
                           # " (Importance = ", 
                           # round(mod_radio[15, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[15, 4],
                           "]")
    lab_radio_16 <- paste0(mod_radio[16, 1],
                           # " (Importance = ", 
                           # round(mod_radio[16, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[16, 4],
                           "]")
    lab_radio_17 <- paste0(mod_radio[17, 1],
                           # " (Importance = ", 
                           # round(mod_radio[17, 2], rnd_imp),
                           # ")",                           
                           " [Location = ",
                           mod_radio[17, 4],
                           "]")
    lab_radio_18 <- paste0(mod_radio[18, 1],
                           # " (Importance = ", 
                           # round(mod_radio[18, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[18, 4],
                           "]")
    lab_radio_19 <- paste0(mod_radio[19, 1],
                           # " (Importance = ", 
                           # round(mod_radio[19, 2], rnd_imp),
                           # ")",                           
                           " [Location = ",
                           mod_radio[19, 4],
                           "]")
    lab_radio_20 <- paste0(mod_radio[20, 1],
                           # " (Importance = ", 
                           # round(mod_radio[20, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[20, 4],
                           "]")
  } else if (sel_map_model == "HABDW_Risk") {
    ### Lab, habdw ----
    mod_radio <- mod_varimp_habdw
    lab_radio_01 <- paste0(mod_radio[1, 1],
                           # " (Importance = ", 
                           # round(mod_radio[1, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[1, 4],
                           "]")
    lab_radio_02 <- paste0(mod_radio[2, 1],
                           # " (Importance = ", 
                           # round(mod_radio[2, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[2, 4],
                           "]")
    lab_radio_03 <- paste0(mod_radio[3, 1],
                           # " (Importance = ", 
                           # round(mod_radio[3, 2], rnd_imp),
                           # ")",                         
                           " [Location = ",
                           mod_radio[3, 4],
                           "]")
    lab_radio_04 <- paste0(mod_radio[4, 1],
                           # " (Importance = ", 
                           # round(mod_radio[4, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[4, 4],
                           "]")
    lab_radio_05 <- paste0(mod_radio[5, 1],
                           # " (Importance = ", 
                           # round(mod_radio[5, 2], rnd_imp),
                           # ")",                         
                           " [Location = ",
                           mod_radio[5, 4],
                           "]")
    lab_radio_06 <- paste0(mod_radio[6, 1],
                           # " (Importance = ", 
                           # round(mod_radio[6, 2], rnd_imp),
                           # ")",                         
                           " [Location = ",
                           mod_radio[6, 4],
                           "]")
    lab_radio_07 <- paste0(mod_radio[7, 1],
                           # " (Importance = ", 
                           # round(mod_radio[7, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[7, 4],
                           "]")
    lab_radio_08 <- paste0(mod_radio[8, 1],
                           # " (Importance = ", 
                           # round(mod_radio[8, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[8, 4],
                           "]")
    lab_radio_09 <- paste0(mod_radio[9, 1],
                           # " (Importance = ", 
                           # round(mod_radio[9, 2], rnd_imp),
                           # ")",                           
                           " [Location = ",
                           mod_radio[9, 4],
                           "]")
    lab_radio_10 <- paste0(mod_radio[10, 1],
                           # " (Importance = ", 
                           # round(mod_radio[10, 2], rnd_imp),
                           # ")",                           
                           " [Location = ",
                           mod_radio[9, 4],
                           "]")
    lab_radio_11 <- paste0(mod_radio[11, 1],
                           # " (Importance = ", 
                           # round(mod_radio[11, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[11, 4],
                           "]")
    lab_radio_12 <- paste0(mod_radio[12, 1],
                           # " (Importance = ", 
                           # round(mod_radio[12, 2], rnd_imp),
                           # ")",                         
                           " [Location = ",
                           mod_radio[12, 4],
                           "]")
    lab_radio_13 <- paste0(mod_radio[13, 1],
                           # " (Importance = ", 
                           # round(mod_radio[13, 2], rnd_imp),
                           # ")",                       
                           " [Location = ",
                           mod_radio[13, 4],
                           "]")
    lab_radio_14 <- paste0(mod_radio[14, 1],
                           # " (Importance = ", 
                           # round(mod_radio[14, 2], rnd_imp),
                           # ")",                        
                           " [Location = ",
                           mod_radio[14, 4],
                           "]")
    lab_radio_15 <- paste0(mod_radio[15, 1],
                           # " (Importance = ", 
                           # round(mod_radio[15, 2], rnd_imp),
                           # ")",                        
                           " [Location = ",
                           mod_radio[15, 4],
                           "]")
    lab_radio_16 <- paste0(mod_radio[16, 1],
                           # " (Importance = ", 
                           # round(mod_radio[16, 2], rnd_imp),
                           # ")",                         
                           " [Location = ",
                           mod_radio[16, 4],
                           "]")
    lab_radio_17 <- paste0(mod_radio[17, 1],
                           # " (Importance = ", 
                           # round(mod_radio[17, 2], rnd_imp),
                           # ")",                        
                           " [Location = ",
                           mod_radio[17, 4],
                           "]")
    lab_radio_18 <- paste0(mod_radio[18, 1],
                           # " (Importance = ", 
                           # round(mod_radio[18, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[18, 4],
                           "]")
    lab_radio_19 <- paste0(mod_radio[19, 1],
                           # " (Importance = ", 
                           # round(mod_radio[19, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[19, 4],
                           "]")
    lab_radio_20 <- paste0(mod_radio[20, 1],
                           # " (Importance = ", 
                           # round(mod_radio[20, 2], rnd_imp),
                           # ")",                         
                           " [Location = ",
                           mod_radio[20, 4],
                           "]")
  } else if(sel_map_model == "lake_Risk") {
    ### Lab, lake ----
    mod_radio <- mod_varimp_lake
    lab_radio_01 <- paste0(mod_radio[1, 1],
                           # " (Importance = ", 
                           # round(mod_radio[1, 2], rnd_imp),
                           # ")",                         
                           " [Location = ",
                           mod_radio[1, 4],
                           "]")
    lab_radio_02 <- paste0(mod_radio[2, 1],
                           # " (Importance = ", 
                           # round(mod_radio[2, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[2, 4],
                           "]")
    lab_radio_03 <- paste0(mod_radio[3, 1],
                           # " (Importance = ", 
                           # round(mod_radio[3, 2], rnd_imp),
                           # ")",                         
                           " [Location = ",
                           mod_radio[3, 4],
                           "]")
    lab_radio_04 <- paste0(mod_radio[4, 1],
                           # " (Importance = ", 
                           # round(mod_radio[4, 2], rnd_imp),
                           # ")",                        
                           " [Location = ",
                           mod_radio[4, 4],
                           "]")
    lab_radio_05 <- paste0(mod_radio[5, 1],
                           # " (Importance = ", 
                           # round(mod_radio[5, 2], rnd_imp),
                           # ")",                         
                           " [Location = ",
                           mod_radio[5, 4],
                           "]")
    lab_radio_06 <- paste0(mod_radio[6, 1],
                           # " (Importance = ", 
                           # round(mod_radio[6, 2], rnd_imp),
                           # ")",                         
                           " [Location = ",
                           mod_radio[6, 4],
                           "]")
    lab_radio_07 <- paste0(mod_radio[7, 1],
                           # " (Importance = ", 
                           # round(mod_radio[7, 2], rnd_imp),
                           # ")",                        
                           " [Location = ",
                           mod_radio[7, 4],
                           "]")
    lab_radio_08 <- paste0(mod_radio[8, 1],
                           # " (Importance = ", 
                           # round(mod_radio[8, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[8, 4],
                           "]")
    lab_radio_09 <- paste0(mod_radio[9, 1],
                           # " (Importance = ", 
                           # round(mod_radio[9, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[9, 4],
                           "]")
    lab_radio_10 <- paste0(mod_radio[10, 1],
                           # " (Importance = ", 
                           # round(mod_radio[10, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[9, 4],
                           "]")
    lab_radio_11 <- paste0(mod_radio[11, 1],
                           # " (Importance = ", 
                           # round(mod_radio[11, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[11, 4],
                           "]")
    lab_radio_12 <- paste0(mod_radio[12, 1],
                           # " (Importance = ", 
                           # round(mod_radio[12, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[12, 4],
                           "]")
    lab_radio_13 <- paste0(mod_radio[13, 1],
                           # " (Importance = ", 
                           # round(mod_radio[13, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[13, 4],
                           "]")
    lab_radio_14 <- paste0(mod_radio[14, 1],
                           # " (Importance = ", 
                           # round(mod_radio[14, 2], rnd_imp),
                           # ")",                           
                           " [Location = ",
                           mod_radio[14, 4],
                           "]")
    lab_radio_15 <- paste0(mod_radio[15, 1],
                           # " (Importance = ", 
                           # round(mod_radio[15, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[15, 4],
                           "]")
    lab_radio_16 <- paste0(mod_radio[16, 1],
                           # " (Importance = ", 
                           # round(mod_radio[16, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[16, 4],
                           "]")
    lab_radio_17 <- paste0(mod_radio[17, 1],
                           # " (Importance = ", 
                           # round(mod_radio[17, 2], rnd_imp),
                           # ")",                        
                           " [Location = ",
                           mod_radio[17, 4],
                           "]")
    lab_radio_18 <- paste0(mod_radio[18, 1],
                           # " (Importance = ", 
                           # round(mod_radio[18, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[18, 4],
                           "]")
    lab_radio_19 <- paste0(mod_radio[19, 1],
                           # " (Importance = ", 
                           # round(mod_radio[19, 2], rnd_imp),
                           # ")",                         
                           " [Location = ",
                           mod_radio[19, 4],
                           "]")
    lab_radio_20 <- paste0(mod_radio[20, 1],
                           # " (Importance = ", 
                           # round(mod_radio[20, 2], rnd_imp),
                           # ")",                        
                           " [Location = ",
                           mod_radio[20, 4],
                           "]")
  } else if (sel_map_model == "Treat_Risk") {
    ### Lab, treat ----
    mod_radio <- mod_varimp_treat
    lab_radio_01 <- paste0(mod_radio[1, 1],
                           # " (Importance = ", 
                           # round(mod_radio[1, 2], rnd_imp),
                           # ")",                        
                           " [Location = ",
                           mod_radio[1, 4],
                           "]")
    lab_radio_02 <- paste0(mod_radio[2, 1],
                           # " (Importance = ", 
                           # round(mod_radio[2, 2], rnd_imp),
                           # ")",                         
                           " [Location = ",
                           mod_radio[2, 4],
                           "]")
    lab_radio_03 <- paste0(mod_radio[3, 1],
                           # " (Importance = ", 
                           # round(mod_radio[3, 2], rnd_imp),
                           # ")",                        
                           " [Location = ",
                           mod_radio[3, 4],
                           "]")
    lab_radio_04 <- paste0(mod_radio[4, 1],
                           # " (Importance = ", 
                           # round(mod_radio[4, 2], rnd_imp),
                           # ")",                         
                           " [Location = ",
                           mod_radio[4, 4],
                           "]")
    lab_radio_05 <- paste0(mod_radio[5, 1],
                           # " (Importance = ", 
                           # round(mod_radio[5, 2], rnd_imp),
                           # ")",                         
                           " [Location = ",
                           mod_radio[5, 4],
                           "]")
    lab_radio_06 <- paste0(mod_radio[6, 1],
                           # " (Importance = ", 
                           # round(mod_radio[6, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[6, 4],
                           "]")
    lab_radio_07 <- paste0(mod_radio[7, 1],
                           # " (Importance = ", 
                           # round(mod_radio[7, 2], rnd_imp),
                           # ")",                           
                           " [Location = ",
                           mod_radio[7, 4],
                           "]")
    lab_radio_08 <- paste0(mod_radio[8, 1],
                           # " (Importance = ", 
                           # round(mod_radio[8, 2], rnd_imp),
                           # ")",                      
                           " [Location = ",
                           mod_radio[8, 4],
                           "]")
    lab_radio_09 <- paste0(mod_radio[9, 1],
                           # " (Importance = ", 
                           # round(mod_radio[9, 2], rnd_imp),
                           # ")",                           
                           " [Location = ",
                           mod_radio[9, 4],
                           "]")
    lab_radio_10 <- paste0(mod_radio[10, 1],
                           # " (Importance = ", 
                           # round(mod_radio[10, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[11, 4],
                           "]")
    lab_radio_11 <- paste0(mod_radio[11, 1],
                           # " (Importance = ", 
                           # round(mod_radio[11, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[11, 4],
                           "]")
    lab_radio_12 <- paste0(mod_radio[12, 1],
                           # " (Importance = ", 
                           # round(mod_radio[12, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[12, 4],
                           "]")
    lab_radio_13 <- paste0(mod_radio[13, 1],
                           # " (Importance = ", 
                           # round(mod_radio[13, 2], rnd_imp),
                           # ")",                         
                           " [Location = ",
                           mod_radio[13, 4],
                           "]")
    lab_radio_14 <- paste0(mod_radio[14, 1],
                           # " (Importance = ", 
                           # round(mod_radio[14, 2], rnd_imp),
                           # ")",                           
                           " [Location = ",
                           mod_radio[14, 4],
                           "]")
    lab_radio_15 <- paste0(mod_radio[15, 1],
                           # " (Importance = ", 
                           # round(mod_radio[15, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[15, 4],
                           "]")
    lab_radio_16 <- paste0(mod_radio[16, 1],
                           # " (Importance = ", 
                           # round(mod_radio[16, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[16, 4],
                           "]")
    lab_radio_17 <- paste0(mod_radio[17, 1],
                           # " (Importance = ", 
                           # round(mod_radio[17, 2], rnd_imp),
                           # ")",                         
                           " [Location = ",
                           mod_radio[17, 4],
                           "]")
    lab_radio_18 <- paste0(mod_radio[18, 1],
                           # " (Importance = ", 
                           # round(mod_radio[18, 2], rnd_imp),
                           # ")",                           
                           " [Location = ",
                           mod_radio[18, 4],
                           "]")
    lab_radio_19 <- paste0(mod_radio[19, 1],
                           # " (Importance = ", 
                           # round(mod_radio[19, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[19, 4],
                           "]")
    lab_radio_20 <- paste0(mod_radio[20, 1],
                           # " (Importance = ", 
                           # round(mod_radio[20, 2], rnd_imp),
                           # ")",                         
                           " [Location = ",
                           mod_radio[20, 4],
                           "]")
  } else if (sel_map_model == "Viol_Risk") {
    ### Lab, viol ----
    mod_radio <- mod_varimp_viol
    lab_radio_01 <- paste0(mod_radio[1, 1],
                           # " (Importance = ", 
                           # round(mod_radio[1, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[1, 4],
                           "]")
    lab_radio_02 <- paste0(mod_radio[2, 1],
                           # " (Importance = ", 
                           # round(mod_radio[2, 2], rnd_imp),
                           # ")",                         
                           " [Location = ",
                           mod_radio[2, 4],
                           "]")
    lab_radio_03 <- paste0(mod_radio[3, 1],
                           # " (Importance = ", 
                           # round(mod_radio[3, 2], rnd_imp),
                           # ")",                        
                           " [Location = ",
                           mod_radio[3, 4],
                           "]")
    lab_radio_04 <- paste0(mod_radio[4, 1],
                           # " (Importance = ", 
                           # round(mod_radio[4, 2], rnd_imp),
                           # ")",                       
                           " [Location = ",
                           mod_radio[4, 4],
                           "]")
    lab_radio_05 <- paste0(mod_radio[5, 1],
                           # " (Importance = ", 
                           # round(mod_radio[5, 2], rnd_imp),
                           # ")",                        
                           " [Location = ",
                           mod_radio[5, 4],
                           "]")
    lab_radio_06 <- paste0(mod_radio[6, 1],
                           # " (Importance = ", 
                           # round(mod_radio[6, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[6, 4],
                           "]")
    lab_radio_07 <- paste0(mod_radio[7, 1],
                           # " (Importance = ", 
                           # round(mod_radio[7, 2], rnd_imp),
                           # ")",                         
                           " [Location = ",
                           mod_radio[7, 4],
                           "]")
    lab_radio_08 <- paste0(mod_radio[8, 1],
                           # " (Importance = ", 
                           # round(mod_radio[8, 2], rnd_imp),
                           # ")",                       
                           " [Location = ",
                           mod_radio[8, 4],
                           "]")
    lab_radio_09 <- paste0(mod_radio[9, 1],
                           # " (Importance = ", 
                           # round(mod_radio[9, 2], rnd_imp),
                           # ")",                         
                           " [Location = ",
                           mod_radio[9, 4],
                           "]")
    lab_radio_10 <- paste0(mod_radio[10, 1],
                           # " (Importance = ", 
                           # round(mod_radio[10, 2], rnd_imp),
                           # ")",                            
                           " [Location = ",
                           mod_radio[11, 4],
                           "]")
    lab_radio_11 <- paste0(mod_radio[11, 1],
                           # " (Importance = ", 
                           # round(mod_radio[11, 2], rnd_imp),
                           # ")",                            
                           " [Location = ",
                           mod_radio[11, 4],
                           "]")
    lab_radio_12 <- paste0(mod_radio[12, 1],
                           # " (Importance = ", 
                           # round(mod_radio[12, 2], rnd_imp),
                           # ")",                           
                           " [Location = ",
                           mod_radio[12, 4],
                           "]")
    lab_radio_13 <- paste0(mod_radio[13, 1],
                           # " (Importance = ", 
                           # round(mod_radio[13, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[13, 4],
                           "]")
    lab_radio_14 <- paste0(mod_radio[14, 1],
                           # " (Importance = ", 
                           # round(mod_radio[14, 2], rnd_imp),
                           # ")",                           
                           " [Location = ",
                           mod_radio[14, 4],
                           "]")
    lab_radio_15 <- paste0(mod_radio[15, 1],
                           # " (Importance = ", 
                           # round(mod_radio[15, 2], rnd_imp),
                           # ")",                          
                           " [Location = ",
                           mod_radio[15, 4],
                           "]")
    lab_radio_16 <- paste0(mod_radio[16, 1],
                           # " (Importance = ", 
                           # round(mod_radio[16, 2], rnd_imp),
                           # ")",                        
                           " [Location = ",
                           mod_radio[16, 4],
                           "]")
    lab_radio_17 <- paste0(mod_radio[17, 1],
                           # " (Importance = ", 
                           # round(mod_radio[17, 2], rnd_imp),
                           # ")",                    
                           " [Location = ",
                           mod_radio[17, 4],
                           "]")
    lab_radio_18 <- paste0(mod_radio[18, 1],
                           # " (Importance = ", 
                           # round(mod_radio[18, 2], rnd_imp),
                           # ")",                   
                           " [Location = ",
                           mod_radio[18, 4],
                           "]")
    lab_radio_19 <- paste0(mod_radio[19, 1],
                           # " (Importance = ", 
                           # round(mod_radio[19, 2], rnd_imp),
                           # ")",                      
                           " [Location = ",
                           mod_radio[19, 4],
                           "]")
    lab_radio_20 <- paste0(mod_radio[20, 1],
                           # " (Importance = ", 
                           # round(mod_radio[20, 2], rnd_imp),
                           # ")",
                           " [Location = ",
                           mod_radio[20, 4],
                           "]")
  } else {
    lab_radio_01 <- "p01"
    lab_radio_02 <- "p02"
    lab_radio_03 <- "p03"
    lab_radio_04 <- "p04"
    lab_radio_05 <- "p05"
    lab_radio_06 <- "p06"
    lab_radio_07 <- "p07"
    lab_radio_08 <- "p08"
    lab_radio_09 <- "p09"
    lab_radio_10 <- "p10"
    lab_radio_11 <- "p11"
    lab_radio_12 <- "p12"
    lab_radio_13 <- "p13"
    lab_radio_14 <- "p14"
    lab_radio_15 <- "p15"
    lab_radio_16 <- "p16"
    lab_radio_17 <- "p17"
    lab_radio_18 <- "p18"
    lab_radio_19 <- "p19"
    lab_radio_20 <- "p20"
  }## IF ~ sel_map_water
    
    updateRadioButtons(session,
                       "map_radio_p01",
                       label = lab_radio_01)
    updateRadioButtons(session,
                       "map_radio_p02",
                       label = lab_radio_02)
    updateRadioButtons(session,
                       "map_radio_p03",
                       label = lab_radio_03)
    updateRadioButtons(session,
                       "map_radio_p04",
                       label = lab_radio_04)
    updateRadioButtons(session,
                       "map_radio_p05",
                       label = lab_radio_05)
    updateRadioButtons(session,
                       "map_radio_p06",
                       label = lab_radio_06)
    updateRadioButtons(session,
                       "map_radio_p07",
                       label = lab_radio_07)
    updateRadioButtons(session,
                       "map_radio_p08",
                       label = lab_radio_08)
    updateRadioButtons(session,
                       "map_radio_p09",
                       label = lab_radio_09)
    updateRadioButtons(session,
                       "map_radio_p10",
                       label = lab_radio_10)
    updateRadioButtons(session,
                       "map_radio_p11",
                       label = lab_radio_11)
    updateRadioButtons(session,
                       "map_radio_p12",
                       label = lab_radio_12)
    updateRadioButtons(session,
                       "map_radio_p13",
                       label = lab_radio_13)
    updateRadioButtons(session,
                       "map_radio_p14",
                       label = lab_radio_14)
    updateRadioButtons(session,
                       "map_radio_p15",
                       label = lab_radio_15)
    updateRadioButtons(session,
                       "map_radio_p16",
                       label = lab_radio_16)
    updateRadioButtons(session,
                       "map_radio_p17",
                       label = lab_radio_17)
    updateRadioButtons(session,
                       "map_radio_p18",
                       label = lab_radio_18)
    updateRadioButtons(session,
                       "map_radio_p19",
                       label = lab_radio_19)
    updateRadioButtons(session,
                       "map_radio_p20",
                       label = lab_radio_20)
  })
    
  observeEvent(input$but_p_change_all, {
    # Change all parameter radio buttons
    updateRadioButtons(session,
                       "map_radio_p01",
                       selected = input$p_val_all)
    updateRadioButtons(session,
                       "map_radio_p02",
                       selected = input$p_val_all)
    updateRadioButtons(session,
                       "map_radio_p03",
                       selected = input$p_val_all)
    updateRadioButtons(session,
                       "map_radio_p04",
                       selected = input$p_val_all)
    updateRadioButtons(session,
                       "map_radio_p05",
                       selected = input$p_val_all)
    updateRadioButtons(session,
                       "map_radio_p06",
                       selected = input$p_val_all)
    updateRadioButtons(session,
                       "map_radio_p07",
                       selected = input$p_val_all)
    updateRadioButtons(session,
                       "map_radio_p08",
                       selected = input$p_val_all)
    updateRadioButtons(session,
                       "map_radio_p09",
                       selected = input$p_val_all)
    updateRadioButtons(session,
                       "map_radio_p10",
                       selected = input$p_val_all)
    updateRadioButtons(session,
                       "map_radio_p11",
                       selected = input$p_val_all)
    updateRadioButtons(session,
                       "map_radio_p12",
                       selected = input$p_val_all)
    updateRadioButtons(session,
                       "map_radio_p13",
                       selected = input$p_val_all)
    updateRadioButtons(session,
                       "map_radio_p14",
                       selected = input$p_val_all)
    updateRadioButtons(session,
                       "map_radio_p15",
                       selected = input$p_val_all)
    updateRadioButtons(session,
                       "map_radio_p16",
                       selected = input$p_val_all)
    updateRadioButtons(session,
                       "map_radio_p17",
                       selected = input$p_val_all)
    updateRadioButtons(session,
                       "map_radio_p18",
                       selected = input$p_val_all)
    updateRadioButtons(session,
                       "map_radio_p19",
                       selected = input$p_val_all)
    updateRadioButtons(session,
                       "map_radio_p20",
                       selected = input$p_val_all)
  })## but_p_change_all
  

}## server logic