#' Code hoisted out of shinyauthR, pending refactoring

requireNamespace(c("shiny", "shinyjs", "dplyr", "tibble"))
## And a slatering of sodium, but we'll fix that later

#' login server module
#'
#' Shiny authentication module for use with \link{loginUI}
#'
#' This module uses shiny's new \link[shiny]{moduleServer} method as opposed to the \link[shiny]{callModule}
#' method used by the now deprecated \link{login} function and must be called differently in your app.
#' For details on how to migrate see the 'Migrating from callModule to moduleServer' section of
#' \href{https://shiny.rstudio.com/articles/modules.html}{Modularizing Shiny app code}.
#'
#' @param id 	An ID string that corresponds with the ID used to call the module's UI function
#' @param auth  An object returned by e.g. \link{htpasswdAuth}
#' @param user_col bare (unquoted) or quoted column name containing user names
#' @param reload_on_logout should app force a session reload on logout?
#' @param cookie_logins enable automatic logins via browser cookies?
#' @param sessionid_col bare (unquoted) or quoted column name containing session ids
#' @param cookie_getter a function that returns a data.frame with at least two columns: user and session
#' @param cookie_setter a function with two parameters: user and session.  The function must save these to a database.
#'
#' @return The module will return a reactive 2 element list to your main application.
#'   First element \code{user_auth} is a boolean indicating whether there has been
#'   a successful login or not. Second element \code{info} will be the data frame provided
#'   to the function, filtered to the row matching the successfully logged in username.
#'   When \code{user_auth} is FALSE \code{info} is NULL.
#'
#' @importFrom rlang :=
#'
legacy_loginServer <- function(id,
                        auth,
                        user_col,
                        reload_on_logout = FALSE,
                        cookie_logins = FALSE,
                        sessionid_col,
                        cookie_getter,
                        cookie_setter) {

  randomString <- function(n = 64) {
    paste(
      sample(x = c(letters, LETTERS, 0:9), size = n, replace = TRUE),
      collapse = ""
    )
  }

  if (cookie_logins && (missing(cookie_getter) | missing(cookie_setter) | missing(sessionid_col))) {
    stop("if cookie_logins = TRUE, cookie_getter, cookie_setter and sessionid_col must be provided")
  } else {
    try_class_sc <- try(class(sessionid_col), silent = TRUE)
    if (try_class_sc == "character") {
      sessionid_col <- rlang::sym(sessionid_col)
    }
  }

  shiny::moduleServer(
    id,
    function(input, output, session) {

      credentials <- shiny::reactiveValues(user_auth = FALSE, info = NULL)
      # We want to avoid flashing a useless login prompt if we have a
      # valid cookie, but at session initialization time we can't know
      # immediately whether that will the case. Note that this
      # reactiveValue is kept on the server side and not transmitted
      # over the websocket:
      .auth_state <- shiny::reactiveValues(settled = ! cookie_logins)

      # Synchronize visibility of the login / logout controls
      shiny::observe({
        shinyjs::toggle(id = "button_logout", condition = .auth_state$settled && credentials$user_auth)
        shinyjs::toggle(id = "panel_login", condition = .auth_state$settled && !credentials$user_auth)
      })

      shiny::observeEvent(input$button_logout, {
        if (cookie_logins) {
          shinyjs::js$rmcookie()
        }

        if (reload_on_logout) {
          session$reload()
        } else {
          shiny::updateTextInput(session, "password", value = "")
          credentials$user_auth <- FALSE
          credentials$info <- NULL
        }
      })

      if (cookie_logins) {

        # possibility 1: login through a present valid cookie
        # first, check for a cookie once javascript is ready
        shiny::observeEvent(shiny::isTruthy(shinyjs::js$getcookie()), {
          shinyjs::js$getcookie()
        })
        # second, once cookie is found try to use it
        shiny::observeEvent(input$jscookie, {
          # Regardless of the outcome below, by the end of this “game
          # turn” of the async event loop, it will be time to show
          # either the login or the logout UI:
          .auth_state$settled <- TRUE

          # Stop now if cookie is a dud for any reason:...
          shiny::req(
            credentials$user_auth == FALSE,     ## ... if already logged in,
            is.null(input$jscookie) == FALSE,   ## ... if no cookie,
            nchar(input$jscookie) > 0           ## ... if cookie is empty
          )

          cookie_data <- dplyr::filter(cookie_getter(), {{sessionid_col}} == input$jscookie)

          if (nrow(cookie_data) != 1) {
            shinyjs::js$rmcookie()
          } else {
            # if valid cookie, we reset it to update expiry date
            .userid <- dplyr::pull(cookie_data, {{user_col}})
            .sessionid <- randomString()

            shinyjs::js$setcookie(.sessionid)

            cookie_setter(.userid, .sessionid)

            cookie_data <- utils::head(dplyr::filter(cookie_getter(), {{sessionid_col}} == .sessionid, {{user_col}} == .userid))

            credentials$user_auth <- TRUE
            credentials$info <- cookie_data
          }
        })

      }

      # possibility 2: login through login button
      shiny::observeEvent(input$button_login, {
        # if user name row and password name row are same, credentials are valid
        user_id <- auth$checkPassword(input$user_name, input$password)
        if (! is.null(user_id)) {
          credentials$user_auth <- TRUE
          credentials$info <- tibble::tibble(user = user_id)

          if (cookie_logins) {
            .sessionid <- randomString()
            shinyjs::js$setcookie(.sessionid)
            cookie_setter(user_id, .sessionid)
            cookie_data <- dplyr::filter(dplyr::select(cookie_getter(), -{{user_col}}), {{sessionid_col}} == .sessionid)
            if (nrow(cookie_data) == 1) {
              credentials$info <- dplyr::bind_cols(credentials$info, cookie_data)
            }
          }

        } else { # if not valid temporarily show error message to user
          shinyjs::toggle(id = "error", anim = TRUE, time = 1, animType = "fade")
          shinyjs::delay(5000, shinyjs::toggle(id = "error", anim = TRUE, time = 1, animType = "fade"))
        }
      })

      # return reactive list containing auth boolean and user information
      shiny::reactive({
        shiny::reactiveValuesToList(credentials)
      })

    }
  )
}

#' logout UI module
#'
#' Shiny UI Module for use with \link{logoutServer}
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @param label label for the logout button
#' @param icon An optional \code{\link[shiny]{icon}} to appear on the button.
#' @param class bootstrap class for the logout button
#' @param style css styling for the logout button
#'
#' @return Shiny UI action button
legacy_logoutUI <- function(id, label = "Log out", icon = NULL, class = "btn-danger", style = "color: white;") {
  ns <- shiny::NS(id)

  shinyjs::hidden(
    shiny::actionButton(ns("button_logout"), label, icon = icon, class = class, style = style)
  )
}
