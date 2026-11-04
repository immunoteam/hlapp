#' main ui
#' @export
HLApp_ui <- function() {
  shiny::fluidPage(
    tags$head(tags$title("HLApp – HLA Promiscuity Analysis"),
              tags$style(HTML("
              
      /* Main bslib navigation panels: inactive text */
      .navbar .navbar-nav .nav-link {
        color: #ffffff !important;
      }
      
      /* Active main navigation panel text */
      .navbar .navbar-nav .nav-link.active,
      .navbar .navbar-nav .nav-link[aria-current=\"page\"] {
        color: #00a0fe !important;
        font-weight: bold;
      }

      /* Tabset panel text */
      .nav-tabs .nav-link {
        color: #777777 !important;
      }

      /* Active tab text */
      .nav-tabs .nav-link.active {
        color: #00a0fe !important;
        font-weight: bold;
      }
    "))
              ),
    
    theme = bslib::bs_theme(version = 5, bootswatch = "flatly", 
                            "card-border-width" = "0", "card-box-shadow" = "none"),
    shinyjs::useShinyjs(),
    shiny::tags$head(shiny::tags$meta(name = "robots", content = "noindex, nofollow")),
    img(src = "img/logo2.png", height = "100px"),
    shiny::HTML("&nbsp;&nbsp;&nbsp;&nbsp;"),
    # loginUI("login"),
    # verbatimTextOutput("testing"),
    bslib::page_navbar(
      id = "app_navbar",
      navbar_options = bslib::navbar_options(collapsible = FALSE),
      bslib::nav_panel("Calculate HLA metrics", value = "prom", shiny::uiOutput("promUI")),
      bslib::nav_panel("Submotif explorer", value = "submotif", shiny::uiOutput("submUI")),
      bslib::nav_panel("Submotif comparison", value = "submotif_comp", shiny::uiOutput("submCompUI")),
      bslib::nav_panel("Translate allele formats", value = "translate", shiny::uiOutput("transUI")),
      bslib::nav_panel("User Guide", value = "userguide", shiny::uiOutput("guideUI")),
      footer = shiny::fluidRow(
        shiny::HTML("&nbsp;&nbsp;&nbsp;"),
        shiny::hr(size = 1),
        shiny::p(texts$app_footer)
      )
    )
  )
}
