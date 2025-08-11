# Map Panel

function() {
  # tabPanel(
  # sidebarLayout(
    # sidebarPanel(
    #   width = 2,
    #   h2("Map Data"),
    #   p("Render points on a map."),
    #   br(),

      

    #   
    #   
    #   
    # #   , h4("A. Upload a file.")
    # #   , p("If no file name showing below repeat 'Import File' in the left sidebar.")
    # #   , p(textOutput("fn_input_display_map"))
    # #   
    # #   , h4("B. Define Data Type")
    # #   , uiOutput("UI_map_datatype")
    # #   
    # #   , h4("C. Define Column Names")
    # #   , uiOutput("UI_map_col_xlong")
    # #   , uiOutput("UI_map_col_ylat")
    # #   , uiOutput("UI_map_col_sampid")
    # #   # , uiOutput("UI_map_col_keep")
    # #   
    # #   
    # #   , hr()
    # #   , includeHTML(file.path("www", "rmd_html", "ShinyHTML_Map.html"))
    # #   
    # 
    # 
    # )## sidebarPanel ~ END
    # 
    # , mainPanel(
      tabsetPanel(type = "tabs",
                  id = "inSelections",
                  tabPanel(title = "Selections",
                           h3("Update Map"),
                           p(paste0("After making changes below click the button here to update the map. ",
                                    "Adding the HUC12 layers takes about 1 minute.")),
                           bsButton("but_map_update", "Update Map"),
                           shinyBS::bsTooltip(id = "but_map_update",
                                              title = paste0("Clicking this button will",
                                                             " generate a map with the user",
                                                             " selections on this page."),
                                              placement = "right"),
                           # hr(),
                           h3("1. Scale"),
                           fluidRow(column(4, 
                                           p(paste0("Choose a scale at which to view the map and summarize model output. ",
                                                    "Select 'CONUS' to see the entire dataset or select a state to zoom in and get detailed summary.")),
                                           selectInput("map_zoom",
                                                       "State:",
                                                       choices = c("", 
                                                                   "CONUS",
                                                                   df_coord_states[, "Postal_Abbreviation"]),
                                                       selected = "")
                                           ),
                                    column(2,
                                           radioButtons("rad_map_layertype",
                                                        "Layer Type",
                                                        choices = c("points", "polygons"),
                                                        selected = "points")
                                           ),
                                    column(3, 
                                           radioButtons("rad_map_crop",
                                                        "Crop data to selected state?",
                                                        choices = c("Yes", "No"),
                                                        selected = "Yes")
                                           )
                                    ),
                           # hr(),
                           h3("2. Model Choice"),
                           p("Choose whether to model lakes, rivers, or both."),
                           fluidRow(column(width = 6,
                                           selectInput("map_water",
                                                       "Waterbody:",
                                                       # future have Lake or River
                                                       choices = c("Lake"), 
                                                       selected = "Lake"),
                                           selectInput("map_model",
                                                       "Model:",
                                                       choices = model_models,
                                                       selected = model_models_default),
                           h3("3. Scenarios"),
                           p(paste0("Choose whether to update Risk calculations based on the minimum, ",
                                     "1st quartile, mean, median, 3rd quartile, or maximum value ",
                                     "from the population of available data for each of the top 20 variables. ",
                                     "Select each variable individually or go to the dropdown menu and click ",
                                     "'Change Parameters' below to select all at once.")),
                           selectInput("p_val_all",
                                       "Parameter Model Value:",
                                       choices = model_scenarios,
                                       selected = model_scenarios_default),
                           shinyBS::bsButton("but_p_change_all", 
                                             "Change Value Top 20 Parameters Below"),
                           shinyBS::bsTooltip(id = "but_p_change_all",
                                              title = paste0("Clicking this button will",
                                                             " change all 20 variables below",
                                                             " to the same model value as ",
                                                             " specified in the selection box above."),
                                              placement = "right"),
                           radioButtons("map_radio_p01",
                                        label = "p01",
                                        choices = model_scenarios,
                                        selected = model_scenarios_default,
                                        inline = TRUE),
                           radioButtons("map_radio_p02",
                                        "p02",
                                        choices = model_scenarios,
                                        selected = model_scenarios_default,
                                        inline = TRUE),
                           radioButtons("map_radio_p03",
                                        "p03",
                                        choices = model_scenarios,
                                        selected = model_scenarios_default,
                                        inline = TRUE),
                           radioButtons("map_radio_p04",
                                        "p04",
                                        choices = model_scenarios,
                                        selected = model_scenarios_default,
                                        inline = TRUE),
                           radioButtons("map_radio_p05",
                                        "p05",
                                        choices = model_scenarios,
                                        selected = model_scenarios_default,
                                        inline = TRUE),
                           radioButtons("map_radio_p06",
                                        "p06",
                                        choices = model_scenarios,
                                        selected = model_scenarios_default,
                                        inline = TRUE),
                           radioButtons("map_radio_p07",
                                        "p07",
                                        choices = model_scenarios,
                                        selected = model_scenarios_default,
                                        inline = TRUE),
                           radioButtons("map_radio_p08",
                                        "p08",
                                        choices = model_scenarios,
                                        selected = model_scenarios_default,
                                        inline = TRUE),
                           radioButtons("map_radio_p09",
                                        "p09",
                                        choices = model_scenarios,
                                        selected = model_scenarios_default,
                                        inline = TRUE),
                           radioButtons("map_radio_p10",
                                        "p10",
                                        choices = model_scenarios,
                                        selected = model_scenarios_default,
                                        inline = TRUE),
                           radioButtons("map_radio_p11",
                                        "p11",
                                        choices = model_scenarios,
                                        selected = model_scenarios_default,
                                        inline = TRUE),
                           radioButtons("map_radio_p12",
                                        "p12",
                                        choices = model_scenarios,
                                        selected = model_scenarios_default,
                                        inline = TRUE),
                           radioButtons("map_radio_p13",
                                        "p13",
                                        choices = model_scenarios,
                                        selected = model_scenarios_default,
                                        inline = TRUE),
                           radioButtons("map_radio_p14",
                                        "p14",
                                        choices = model_scenarios,
                                        selected = model_scenarios_default,
                                        inline = TRUE),
                           radioButtons("map_radio_p15",
                                        "p15",
                                        choices = model_scenarios,
                                        selected = model_scenarios_default,
                                        inline = TRUE),
                           radioButtons("map_radio_p16",
                                        "p16",
                                        choices = model_scenarios,
                                        selected = model_scenarios_default,
                                        inline = TRUE),
                           radioButtons("map_radio_p17",
                                        "p17",
                                        choices = model_scenarios,
                                        selected = model_scenarios_default,
                                        inline = TRUE),
                           radioButtons("map_radio_p18",
                                        "p18",
                                        choices = model_scenarios,
                                        selected = model_scenarios_default,
                                        inline = TRUE),
                           radioButtons("map_radio_p19",
                                        "p19",
                                        choices = model_scenarios,
                                        selected = model_scenarios_default,
                                        inline = TRUE),
                           radioButtons("map_radio_p20",
                                        "p20",
                                        choices = model_scenarios,
                                        selected = model_scenarios_default,
                                        inline = TRUE)
                           ), # Column A
                    # column(width = 4,
                    #        p("Use button to change the displayed parameters (top 20 by model influence)."),
                    #        hr()
                    #        ) # Column B
                    ), # fluidRow
                           
                   ), # tabPanel ~ Selections
          tabPanel(title = "Model Info",
                   p("Model information based on user selections."),
                   h3("Scenario"),
                   p(textOutput("str_water")),
                   p(textOutput("str_model")),
                   fluidRow(
                     column(6,
                            h4("Variable Importance"),
                            plotOutput("plot_model_varimp"),
                            tableOutput("table_model_varimp")
                            ),
                     column(1),
                     column(5,
                            h4("Model Performance"),
                            plotOutput("plot_model_perf"),
                            tableOutput("table_model_perf")
                            )
                   )
                   ),
          tabPanel(title = "Summary",
                   p(em("Summary information based on user selections after clicking 'Update Map'.")),
                   hr(),
                   
                   ), ## tabPanel ~ Summary
          tabPanel(title = "Map",
                   p(em("Map based on user selections after clicking 'Update Map'.")),
                   withSpinner(leafletOutput("map_huc",
                                             height = "85vh"))
          ) ## tabPanel ~ Map
      )## tabsetPanel ~ END
    # )## mainPanel ~ END
  # )##sidebarLayout ~ END
   # )## tabPanel ~ END
}##FUNCTION ~ END
