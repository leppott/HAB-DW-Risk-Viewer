#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# library(shiny)

# Define UI for application that draws a histogram
fluidPage(
  
  # CSS
  theme = "styles.css",
  
  # Tags
  shiny::includeCSS(file.path("www", "tags_ContDataQC.R")),
  
  # EPA Header
  # https://www.epa.gov/themes/epa_theme/pattern-lab/patterns/pages-standalone-template/pages-standalone-template.rendered.html
  # shiny::includeHTML(file.path("www", "header_TADA.html")),
  shiny::includeHTML(file.path("www", "header_ContDataQC.html")),

  # Application title
  titlePanel("Old Faithful Geyser Data"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
      sidebarPanel(
          sliderInput("bins",
                      "Number of bins:",
                      min = 1,
                      max = 50,
                      value = 30)
      ),

      # Show a plot of the generated distribution
      mainPanel(
          plotOutput("distPlot")
      )
  ),
  
  # EPA footer
  # shiny::includeHTML(file.path("www", "footer_TADA.html"))
  shiny::includeHTML(file.path("www", "footer_ContDataQC.html"))
)
