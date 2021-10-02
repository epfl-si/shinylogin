## core.R: Common UI code, data structures and state machines shared by multiple shinylogin authentication scenarios
##
## You do *not* need the `core.*` functions for plain old password
## authentication. Those are exported for the benefit of other
## shinylogin.* modules only.

.button_logout_ID <- "button_logout"

#' Shiny UI module for the “log out” button
#'
#' 💡 Usage of the `shinylogin::core.` functions is reserved to
#' “complex” use cases, such as building a shinylogin extension
#' package. You do *not* need this function for an in-app password-based
#' authentication flow.
#'
#' If your app or authentication scheme doesn't want to have a
#' logged-out state at all, e.g. because it is sitting behind some
#' kind of corporate SSO, then just don't call this function.
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @param label label for the logout button
#' @param icon An optional \code{\link[shiny]{icon}} to appear on the button.
#' @param class bootstrap class for the logout button
#' @param style css styling for the logout button
#'
#' @return Shiny UI action button
#'
#' @export
core.logoutUI <- function(id, label = "Log out", icon = NULL, class = "btn-danger", style = "color: white;") {
  shinyjs::hidden(
    shiny::actionButton(shiny::NS(id, .button_logout_ID), label, icon = icon, class = class, style = style)
  )
}

#' The rock bottom basics of a shinylogin server
#'
#' 💡 Usage of the `shinylogin::core.` functions is reserved to
#' “complex” use cases, such as building a shinylogin extension
#' package. You do *not* need this function for an in-app password-based
#' authentication flow.
#'
#' @param reload_on_logout Whether the app force a session reload on logout, as a Boolean
#'
#' @return A `shiny::ReactiveValues` object `user`, with the following fields and methods:
#'     - `user$state()` — A read-only reactive variable containing the following fields:
#'     - `user$state()$logged_in` — A boolean indicating whether there has been a successful login or not
#'     - `user$state()$info` — Personal information about the logged-in user
#'     - `user$addLoginDetails(lst)` — Add the items of `lst` to `user$info`. If said `user$info$username` exists afterwards, set `user$state()$logged_in`
#'     - `user$logout()` — Unset `user$info` and set `user$state()$logged_in` to `FALSE`
#'
#' @export
core.serve <- function(input, output, session, reload_on_logout = FALSE) {
    user <- shiny::reactiveValues(logged_in = FALSE, info = NULL)

    ## Log out button is visible iff the user is logged in. (Harmless
    ## dead code if `core.logoutUI()` is not called.)
    shiny::observe({
        shinyjs::toggle(id = .button_logout_ID, condition = user$logged_in)
    })

    ## Logout sequence
    logout_callbacks <- list()  # Last-in, first-out
    onLogout <- function(expr) {
        logout_callbacks <<- c(list(enquo(expr)), logout_callbacks)
    }
    if (reload_on_logout) {
        onLogout({ session$reload() })
    }
    logout <- function() {
        work <- promises::promise_resolve(TRUE)
        for(quosure in logout_callbacks) {
            work <- promises::`%...>%`(work, { rlang::eval_tidy(quosure) })
        }
        work <- promises::`%...>%`(work, {
            user$info <- NULL
            user$logged_in <- FALSE
        })
    }

    ## Log out button triggers logout sequence
    shiny::observeEvent(input$button_logout, { logout() })

    list(
        state = shiny::reactive({ shiny::reactiveValuesToList(user) }),

        addLoginDetails = function(more_info) {
            if (! is.null(user$info)) {
                user$info <- c(user$info, more_info)
            } else {
                user$info <- more_info
            }
            if (! is.null(user$info$username)) {
                user$logged_in <- TRUE
            }
        },

        onLogout = onLogout,
        logout = logout
    )
}

#' Like the `%...>%` operator from the promises package, except that if the LHS is not
#' a promise, it will still work
#'
#' 💡 Usage of the `shinylogin::core.` functions is reserved to
#' “complex” use cases, such as building a shinylogin extension
#' package. You do *not* need this infix operator for an in-app password-based
#' authentication flow.
#'
#' @export
`%then%` <- function(lhs, rhs) {
    if (! promises::is.promising(lhs)) {
        lhs <- promises::promise_resolve(lhs)
    }
    ## Stolen from the implementation of `%...>%` without understanding a darn thing!
    parent <- parent.frame()
    env <- new.env(parent = parent)
    parts <- match.call()
    func <- promises:::pipeify_rhs(parts[[3L]], env)
    promises::then(lhs, func)
}

#' Create and return a sequence of unique Shiny IDs.
#'
#' 💡 Usage of the `shinylogin::core.` functions is reserved to
#' “complex” use cases, such as building a shinylogin extension
#' package. You do *not* need this function for an in-app password-based
#' authentication flow.
#'
#' @export
core.newIDSequence <- function(stem) {
    uniqueID <- 0
    list(nextId = function() {
        uniqueID <<- uniqueID + 1
        sprintf("%s_%d", stem, uniqueID)
    })
}
