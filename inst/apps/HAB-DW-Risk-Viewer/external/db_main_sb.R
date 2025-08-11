#Sidebar----

# tabs
# sourced in global.R
# ref in db_main_body.R
# menu in db_main_sb.R

#sb_main <- function(id) {
function(id) {
  dashboardSidebar(
    width = 275
    # , HTML("&nbsp;&nbsp;<font size=5><b>Steps</b></font>")
    #Steps, do *not* need to be done sequentially----
    , sidebarMenu(id = id
                  , menuItem(text = "About"
                             , tabName = "tab_about"
                             )## menuItem ~ About
                  , menuItem(text = "Map"
                             , tabName = "tab_map"
                             )## menuItem ~ Map
                  , menuItem(text = "Relevant Resources"
                             , tabName = "tab_resources"
                             )
                  , menuItem(text = "Troubleshooting"
                             , tabName = "tab_troubleshoot"
                             )
    )## sidebarMenu
  )## dashboardSidebar
}## FUNCTION