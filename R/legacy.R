#' Code hoisted out of shinyauthR, pending refactoring

requireNamespace(c("glue", "shiny", "shinyjs", "dplyr"))
## And a slatering of sodium, but we'll fix that later

#' Generate required Javascript
#'
#' This is used to generate the Javascript which gets and sets the cookies in the
#' user's browser.  We need to glue it together to handle the variable expiry time and
#' to get the correct module namespace.  This function is used internally only.
#'
#' @param id name of input object to hold the cookie value
#' @param expire_days number of days to ask browser to retain cookie
#'
#' @return Character string of Javascript code.
#' @noRd
#'
#' @examples
#' \dontrun{
#' shinyjs::extendShinyjs(
#'   text = js_cookie_to_r_code(ns("jscookie")),
#'   functions = c("getcookie", "setcookie", "rmcookie")
#' )
#' }
js_cookie_to_r_code <- function(id, expire_days = 7) {
  glue::glue(.open = "{{{", .close = "}}}", '
	  shinyjs.getcookie = function(params) {
	    var cookie = Cookies.get("shinylogin");
	    if (typeof cookie !== "undefined") {
	      Shiny.setInputValue("{{{id}}}", cookie);
	    } else {
	      var cookie = "";
	      Shiny.setInputValue("{{{id}}}", cookie);
	    }
	  }
	  shinyjs.setcookie = function(params) {
	    Cookies.set("shinylogin", escape(params), { expires: {{{expire_days}}} });
	    Shiny.setInputValue("{{{id}}}", params);
	  }
	  shinyjs.rmcookie = function(params) {
	    Cookies.remove("shinylogin");
	    Shiny.setInputValue("{{{id}}}", "");
	  }')
}

#' Javascript code for managing cookies
#'
#' This code is minified from the GitHub project js-cookie/js-cookie.  We include it here
#' to make the shinylogin package more self-contained
#' @noRd
jscookie_script <- function() {
  shiny::includeScript(system.file("js-cookie/js-cookie.js", package = "shinylogin"))
}
#' login UI module
#'
#' Shiny UI Module for use with \link{loginServer}
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @param title header title for the login panel
#' @param user_title label for the user name text input
#' @param pass_title label for the password text input
#' @param login_title label for the login button
#' @param error_message message to display after failed login
#' @param additional_ui additional shiny UI element(s) to add below login button. Wrap multiple inside \code{shiny::tagList()}
#' @param cookie_expiry number of days to request browser to retain login cookie
#'
#' @return Shiny UI login panel with user name text input, password text input and login action button.
legacy_loginUI <- function(id,
                    title = "Please log in",
                    user_title = "User Name",
                    pass_title = "Password",
                    login_title = "Log in",
                    error_message = "Invalid username or password!",
                    additional_ui = NULL,
                    cookie_expiry = 7) {
  ns <- shiny::NS(id)

  shinyjs::hidden(
    shiny::div(
      id = ns("panel"),
      style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
      shiny::wellPanel(
        shinyjs::useShinyjs(),
        jscookie_script(),
        shinyjs::extendShinyjs(text = js_cookie_to_r_code(ns("jscookie"), expire_days = cookie_expiry), functions = c("getcookie", "setcookie", "rmcookie")),
        shinyjs::extendShinyjs(text = js_return_click(ns("password"), ns("button")), functions = c()),
        shiny::tags$h2(title, class = "text-center", style = "padding-top: 0;"),
        shiny::textInput(ns("user_name"), shiny::tagList(shiny::icon("user"), user_title)),
        shiny::passwordInput(ns("password"), shiny::tagList(shiny::icon("unlock-alt"), pass_title)),
        shiny::div(
          style = "text-align: center;",
          shiny::actionButton(ns("button"), login_title, class = "btn-primary", style = "color: white;")
        ),
        additional_ui,
        shinyjs::hidden(
          shiny::div(
            id = ns("error"),
            shiny::tags$p(
              error_message,
              style = "color: red; font-weight: bold; padding-top: 5px;",
              class = "text-center"
            )
          )
        )
      )
    )
  )
}

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
#' @param data data frame or tibble containing user names, passwords and other user data
#' @param user_col bare (unquoted) or quoted column name containing user names
#' @param pwd_col bare (unquoted) or quoted column name containing passwords
#' @param sodium_hashed have the passwords been hash encrypted using the sodium package? defaults to FALSE
#' @param log_out [reactive] supply the returned reactive from \link{logoutServer} here to trigger a user logout
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
                        data,
                        user_col,
                        pwd_col,
                        sodium_hashed = FALSE,
                        log_out = shiny::reactiveVal(),
                        reload_on_logout = FALSE,
                        cookie_logins = FALSE,
                        sessionid_col,
                        cookie_getter,
                        cookie_setter) {

  # if colnames are strings convert them to symbols
  try_class_uc <- try(class(user_col), silent = TRUE)
  if (try_class_uc == "character") {
    user_col <- rlang::sym(user_col)
  }

  try_class_pc <- try(class(pwd_col), silent = TRUE)
  if (try_class_pc == "character") {
    pwd_col <- rlang::sym(pwd_col)
  }

  if (cookie_logins && (missing(cookie_getter) | missing(cookie_setter) | missing(sessionid_col))) {
    stop("if cookie_logins = TRUE, cookie_getter, cookie_setter and sessionid_col must be provided")
  } else {
    try_class_sc <- try(class(sessionid_col), silent = TRUE)
    if (try_class_sc == "character") {
      sessionid_col <- rlang::sym(sessionid_col)
    }
  }

  # ensure all text columns are character class
  data <- dplyr::mutate_if(data, is.factor, as.character)

  shiny::moduleServer(
    id,
    function(input, output, session) {

      credentials <- shiny::reactiveValues(user_auth = FALSE, info = NULL, cookie_already_checked = FALSE)

      shiny::observeEvent(log_out(), {
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

      shiny::observe({
        if (cookie_logins) {
          if (credentials$user_auth) {
            shinyjs::hide(id = "panel")
          } else if (credentials$cookie_already_checked) {
            shinyjs::show(id = "panel")
          }
        } else {
          shinyjs::toggle(id = "panel", condition = !credentials$user_auth)
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
          credentials$cookie_already_checked <- TRUE

          # if already logged in or cookie missing, ignore change in input$jscookie
          shiny::req(
            credentials$user_auth == FALSE,
            is.null(input$jscookie) == FALSE,
            nchar(input$jscookie) > 0
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
            credentials$info <- dplyr::bind_cols(
              dplyr::filter(data, {{user_col}} == .userid),
              dplyr::select(cookie_data, -{{user_col}})
            )
          }
        })

      }

      # possibility 2: login through login button
      shiny::observeEvent(input$button, {

        # check for match of input username to username column in data
        row_username <- which(dplyr::pull(data, {{user_col}}) == input$user_name)

        if (length(row_username)) {
          row_password <- dplyr::filter(data, dplyr::row_number() == row_username)
          row_password <- dplyr::pull(row_password, {{pwd_col}})
          if (sodium_hashed) {
            password_match <- sodium::password_verify(row_password, input$password)
          } else {
            password_match <- identical(row_password, input$password)
          }
        } else {
          password_match <- FALSE
        }

        # if user name row and password name row are same, credentials are valid
        if (length(row_username) == 1 && password_match) {

          credentials$user_auth <- TRUE
          credentials$info <- dplyr::filter(data, {{user_col}} == input$user_name)

          if (cookie_logins) {
            .sessionid <- randomString()
            shinyjs::js$setcookie(.sessionid)
            cookie_setter(input$user_name, .sessionid)
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
    shiny::actionButton(ns("button"), label, icon = icon, class = class, style = style)
  )
}

#' Javascript code to trigger login button with return key
#'
#' @param idpassword name of password input, with correct namespace
#' @param idbutton name of action button, with correct namespace
#' @return Character string of Javascript code.
#' @noRd
js_return_click <- function(idpassword, idbutton) {
  glue::glue(.open = "{{{", .close = "}}}", '
	  $(document).keyup(function(event) {
    if ($("#{{{idpassword}}}").is(":focus") && (event.keyCode == 13)) {
        $("#{{{idbutton}}}").click();
    }
    });')
}

#' logout server module
#'
#' Shiny authentication module for use with \link{logoutUI}
#'
#' This module uses shiny's new \link[shiny]{moduleServer} method as opposed to the \link[shiny]{callModule}
#' method used by the now deprecated \link{login} function and must be called differently in your app.
#' For details on how to migrate see the 'Migrating from callModule to moduleServer' section of
#' \href{https://shiny.rstudio.com/articles/modules.html}{Modularizing Shiny app code}.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function
#' @param active \code{reactive} supply the returned \code{user_auth} boolean reactive from \link{loginServer}
#'   here to hide/show the logout button
#' @param ... arguments passed to \link[shinyjs]{toggle}
#'
#' @return Reactive boolean, to be supplied as the \code{log_out} argument of the
#'   \link{loginServer} module to trigger the logout process
legacy_logoutServer <- function(id, active, ...) {
  shiny::moduleServer(
    id,
    function (input, output, session) {
      shiny::observe({
        shinyjs::toggle(id = "button", condition = active(), ...)
      })

      # return reactive logout button tracker
      shiny::reactive({
        input$button
      })
    }
  )
}

#' Generate random string
#'
#' This function is used to generate random session ids.
#'
#' @param n length of string
#'
#' @return A random character string.
#' @noRd
randomString <- function(n = 64) {
  paste(
    sample(x = c(letters, LETTERS, 0:9), size = n, replace = TRUE),
    collapse = ""
  )
}
