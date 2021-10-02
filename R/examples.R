#' Run the shinydashboard-based example
#'
#' @return No return value, a shiny app is launched.
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#'   shinylogin::runExample()
#' }
#' @export
runExample <- function() {
  appDir <- system.file("shiny-examples", "shinydashboard", package = "shinylogin")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `shinylogin`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
