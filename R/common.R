# common.R: Common UI code and data structures shared by multiple shinylogin authentication scenarios

.button_logout_ID <- "button_logout"

#' Shiny UI module for the “log out” button
#'
#' If your app doesn't want to have a logged-out state at all, e.g.
#' because it is sitting behind some kind of corporate SSO, then just
#' don't call this function at all.
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @param label label for the logout button
#' @param icon An optional \code{\link[shiny]{icon}} to appear on the button.
#' @param class bootstrap class for the logout button
#' @param style css styling for the logout button
#'
#' @return Shiny UI action button
logoutUI <- function(id, label = "Log out", icon = NULL, class = "btn-danger", style = "color: white;") {
  shinyjs::hidden(
    shiny::actionButton(shiny::NS(id, .button_logout_ID), label, icon = icon, class = class, style = style)
  )
}

#' The rock bottom basics of a shinylogin server
#'
#' @return A `shiny::ReactiveValues` object `user`, with fields
#'     `user$logged_in`, a boolean indicating whether there has been a
#'     successful login or not; and `user()$info`, containing personal
#'     information about the logged-in user
serve_shinylogin <- function() {
    user <- shiny::reactiveValues(logged_in = FALSE, info = NULL)

    ## Log out button is visible iff the user is logged in. (Harmless
    ## dead code if `logoutUI()` is not called.)
    shiny::observe({
        shinyjs::toggle(id = .button_logout_ID, condition = user$logged_in)
    })

    user
}
