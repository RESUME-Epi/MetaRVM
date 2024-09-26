#' Launch the Shiny App
#'
#' @export
run_app <- function() {
  app_dir <- system.file("app", package = "MetaRVM")
  shiny::runApp(app_dir, display.mode = "normal")
}
