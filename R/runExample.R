#' Execute the Shiny App for bullet investigation
#'
#' This shiny app would expect a tibble object called `shiny.tt` in the user's working environment.
#'
#' shiny.tt should have variables: `x3p`, `scan_id`, `crosscut`, `ccdata`, `grooves`.
#'
#' To have this shiny app run properly, `shiny.tt` should at least include `grooves` as a variable
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' bulletxtrctr::runExample()
#' }

runExample <- function() {
  appDir <- system.file("shiny-examples", "shiny_bullet", package = "bulletxtrctr")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
