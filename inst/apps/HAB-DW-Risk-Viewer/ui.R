#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# 20250811, USEPA template from ContDataQC

# UI, basic ----
# dashboardPage(
#   header = dashboardHeader(title = "HAB DW Risk Viewer"),
#   sidebar = dashboardSidebar(db_main_sb("leftsdiebarmenu")),
#   body = dashboardBody(db_main_body("dbBody")),
#   footer = dashboardFooter(left = pkg_version,
#                       right = "https:://github.com/leppott/HAB-DW-Risk-Viewer")
# )## dashboardPage

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# UI, TADA Style----
shinyUI(
  fluidPage(

    ## CSS----
    theme = "styles.css",

    ## Tags----
    shiny::includeCSS(file.path("www", "tags_ContDataQC.R")),

    ## EPA Header----
    # https://www.epa.gov/themes/epa_theme/pattern-lab/patterns/pages-standalone-template/pages-standalone-template.rendered.html
    # shiny::includeHTML(file.path("www", "header_TADA.html")),
    shiny::includeHTML(file.path("www", "header_ContDataQC.html")),

    ## Application title ----
    titlePanel("HAB DW Risk Viewer"),

    ## MAIN
    dashboardPage(
      header  = dashboardHeader(title = "HAB DW Risk Viewer"),
      sidebar = dashboardSidebar(db_main_sb("leftsdiebarmenu")),
      body    = dashboardBody(db_main_body("dbBody")) ,
      footer  = dashboardFooter(
        # left = pkg_version ,
        # right = "https:://github.com/leppott/HAB-DW-Risk-Viewer"
        )
    ),## dashboardPage
    
    ## EPA footer----
    #shiny::includeHTML(file.path("www", "footer_TADA.html"))
    shiny::includeHTML(file.path("www", "footer_ContDataQC.html"))
))## END
