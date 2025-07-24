# Main

# tabs
# sourced in global.R
# ref in db_main_body.R
# menu in db_main_sb.R

function(id) {
  
  tabItems(
    tabItem(tabName = "tab_about"
            , tab_code_about())
    , tabItem(tabName = "tab_map"
              , tab_code_map())
    , tabItem(tabName = "tab_resources"
              , tab_code_resources())
    , tabItem(tabName = "tab_troubleshoot"
              , tab_code_troubleshoot())
  )## tabItems
  
}## FUNCTION ~ END


# body <- dashboardBody(
#   tabItems(
#     tabItem(tabName = "tab_about", h2("About"))
#     , tabItem(tabName = "tab_import", h2("Import"))
#   )## tabItems
# )## dashboardBody ~ END