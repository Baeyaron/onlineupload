#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import rms
#' @import cowplot
#' @import fitdistrplus
#' @import BeSS
#' @import ggplot2
#' @import DT
#' @import lm.beta
#' @import reshape
#' @import pROC
#' @import survival
#' @import openxlsx
#' @import shinydashboard
#' @import shinyjs
#' @noRd
app_ui <- function() {
  sidebar <- dashboardSidebar(
    width = 300,
    fileInput("file",  c("Choose Feature .CSV File"), accept = c(".csv"),placeholder = "No file selected"),
    uiOutput("Sidebar")
  )

  body <- dashboardBody(
    useShinyjs(),
    uiOutput("TABUI")
  )
  dashboardPage(
    dashboardHeader(title = "Best subset selection (BeSS)"),
    sidebar,
    body,
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "BeSS"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
