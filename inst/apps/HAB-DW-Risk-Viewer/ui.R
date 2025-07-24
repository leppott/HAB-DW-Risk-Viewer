#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# library(shiny)

# UI ----
dashboardPage(
  header = dashboardHeader(title = "HAB DW Risk Viewer"),
  sidebar = dashboardSidebar(db_main_sb("leftsdiebarmenu")),
  body = dashboardBody(db_main_body("dbBody")),
  footer = dashboardFooter(left = pkg_version,
                      right = "https:://github.com/leppott/HAB-DW-Risk-Viewer")
  
)## dashboardPage

# # UI, old ----
# # Define UI for application that draws a histogram
# fluidPage(
#   
#   ## CSS----
#   theme = "styles.css",
#   
#   ## Tags----
#   shiny::includeCSS(file.path("www", "tags_ContDataQC.R")),
#   
#   ## EPA Header----
#   # https://www.epa.gov/themes/epa_theme/pattern-lab/patterns/pages-standalone-template/pages-standalone-template.rendered.html
#   # shiny::includeHTML(file.path("www", "header_TADA.html")),
#   # shiny::includeHTML(file.path("www", "header_ContDataQC.html")),
# 
#   ## Application title ----
#   titlePanel("HAB DW Risk Viewer"),
# 
#   ## Side bar ----
#   # Sidebar with a slider input for number of bins
#   sidebarLayout(
#       sidebarPanel(
#         ### Selections ----
#         selectInput("map_water",
#                     "Waterbody:",
#                     choices = c("River", "Lake"),
#                     selected = "River"),
#         selectInput("map_results",
#                     "Model:",
#                     choices = c("Random Forest", "Risk Score"),
#                     selected = "Random Forest"),
#         selectInput("map_water_model",
#                     "Waterbody - Model:",
#                     choices = c("River - Random Forest", 
#                                 "River - Risk Score",
#                                 "Lake - Random Forest",
#                                 "Lake - Risk Score"),
#                     selected = "River - Random Forest"),
# 
#         width = 2
#       ),##sidebarPanel
# 
#       ## Main Panel ----
#       # Show a plot of the generated distribution
#       mainPanel(
#         ### plot ----
#           # plotOutput("distPlot"),
#           leafletOutput("map_huc")
#       )## mainPanel
#   ), ## sidebarLayout
#   
#   ## EPA footer----
#   # shiny::includeHTML(file.path("www", "footer_TADA.html"))
#   # shiny::includeHTML(file.path("www", "footer_ContDataQC.html"))
# )## fluidPage
