#' main ui
#' @export
HLApp_ui <- function() {
  shiny::fluidPage(
    theme = bslib::bs_theme(version = 5, bootswatch = "flatly", "card-border-width" = "0", "card-box-shadow" = "none"),
    shinyjs::useShinyjs(),
    shiny::tags$head(shiny::tags$meta(name = "robots", content = "noindex, nofollow")),
    shiny::titlePanel(texts$main_title),
    shiny::HTML("&nbsp;&nbsp;&nbsp;&nbsp;"),
    loginUI("login"),
   # verbatimTextOutput("testing"),
    bslib::page_navbar(id = "app_navbar",
      navbar_options = bslib::navbar_options(collapsible = FALSE),
      bslib::nav_panel("PromiscuityApp", value = "prom", shiny::uiOutput("promUI")),
      bslib::nav_panel("Submotif explorer", value = "submotif", shiny::uiOutput("submUI")),
      bslib::nav_panel("Submotif comparison", value = "submotif_comp", shiny::uiOutput("submCompUI")),
      bslib::nav_panel("HLA translate", value = "translate", shiny::uiOutput("transUI")),
      footer = shiny::fluidRow(
        shiny::HTML("&nbsp;&nbsp;&nbsp;"),
        shiny::hr(size = 1),
        shiny::p(texts$app_footer)
      )
    )
  )
}