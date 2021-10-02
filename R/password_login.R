# password_login.R: Password-based login UI and server-side code, with cookies as a side serving

requireNamespace(c("promises", "glue", "shiny", "shinyjs"))

#' Combined UI and server for password-based login
#'
#' The UI for the login form shows traditional login and password
#' fields. The shiny server is responsible for manipulating the
#' password, checking it, and computing user information like
#' permission levels. The shinylogin package does provide help for the
#' first two tasks, by means of `auth` helpers; it also has the
#' capability to persistently remember who is who using HTTP cookies.
#' However, computing permissions is on you, the app author.
#'
#' @param auth     An object returned by e.g. \link{htpasswdAuth} with a `checkPassword` method
#' @param cookie_store  An object returned by e.g. \link{inMemoryCookieStore}, or NULL if no persistent session mechanism is to be used
#' @param reload_on_logout Whether the app force a session reload on logout, as a Boolean
#'
#' @export
passwordLogin <- function(auth, cookie_store = NULL, reload_on_logout = FALSE) {
    id <- .ids$nextId()

    list(
        id = id,
        loginUI = function(title = "Please log in",
                           user_title = "User Name",
                           pass_title = "Password",
                           login_title = "Log in",
                           error_message = "Invalid username or password!",
                           additional_ui = NULL) {
            password_login_ui(id, title, user_title, pass_title,
                            login_title, error_message, additional_ui,
                            cookie_js_ui = `if`(is.null(cookie_store), NULL,
                                                  cookie_js_ui(id = id,
                                                               expire_days = cookie_store$expire_days)))
        },

        logoutUI = function(label = "Log out", icon = NULL, class = "btn-danger",
                            style = "color: white;") {
            core.logoutUI(id, label, icon, class, style)
        },

        loginServer = function() {
            shiny::moduleServer(
                id,
                function(input, output, session) {
                    user <- serve_password_login(
                        input, output, session,
                        auth$checkPassword, cookie_store, reload_on_logout)

                    ## Grant app code a low-privilege facet:
                    list(user = user$state, logout = user$logout)
                })
        })
}

.ids <- core.newIDSequence("passwordLogin")

#' Authenticate users out of a bcrypt htpasswd file
#'
#' ⚠ This is not just any htpasswd file; it must have been created
#' using with the "htpasswd -B" command
#'
#' @export
htpasswdAuth <- function(path) {
    htpasswd <- read.csv(path, sep=":", header = FALSE, col.names = c("user", "hashed_password"))

    function_exists <- function(package, funcname) {
        tryCatch({
            utils::getFromNamespace(funcname, package)
            TRUE
        }, error = function(...) { FALSE })
    }

    list(checkPassword = function(username, password) {
        htpasswd_line <- htpasswd[which(htpasswd$user == username),]
        if (nrow(htpasswd_line) != 1) return(NULL)

        hash <- htpasswd_line$hashed_password

        if (startsWith(hash, "$7$") && nchar(hash) == 101) {
            requireNamespace("sodium")
            if (sodium::password_verify(hash, password)) {
                return(username)
            } else {
                return(NULL)
            }
        }

        if (startsWith(hash, "$2y$")) {
            ## Assume bcrypt and `htpasswd -B`
            substr(hash, 0, 4) <- "$2a$"
        }

        if (startsWith(hash, "$2a$")) {
            requireNamespace("bcrypt")
            if (bcrypt::checkpw(password, hash)) {
                return(username)
            } else {
                return(NULL)
            }
        }

        stop(sprintf("Unsupported hash format in %s for user %s", path, username))
    })
}

#' Body of the `$loginServer` returned by \link{passwordLogin}
#'
#' This module uses shiny's new \link[shiny]{moduleServer} method as opposed to the \link[shiny]{callModule}
#' method used by the now deprecated \link{login} function and must be called differently in your app.
#' For details on how to migrate see the 'Migrating from callModule to moduleServer' section of
#' \href{https://shiny.rstudio.com/articles/modules.html}{Modularizing Shiny app code}.
#'
#' @param id 	An ID string that corresponds with the ID used to call the module's UI function
#' @param checkPassword  A function that takes login and password, and returns either NULL for login failure, or user information as a list, or a promise to either of the above
#' @param cookie_store  An object returned by e.g. \link{inMemoryCookieStore}, or NULL if no persistent session mechanism is to be used
#' @param reload_on_logout Whether the app force a session reload on logout, as a Boolean
#'
#' @return The `user` object from \link{core.serve}
#'
#' @importFrom promises %...>%
serve_password_login <- function(input, output, session, checkPassword, cookie_store = NULL, reload_on_logout = FALSE) {
    user <- core.serve(input, output, session, reload_on_logout = reload_on_logout)

    if (! is.null(cookie_store)) {
        cookies <- serve_cookie_login(input, cookie_store, user)
    } else {
        cookies <- NULL
    }

    ## Synchronize visibility of the login panel. We want to avoid flashing
    ## a useless login prompt when we have a valid cookie:
    shiny::observe({
        shinyjs::toggle(
                     id = "panel_login",
                     condition = (is.null(cookies) || cookies$settled()) &&
                         user$state()$logged_in == FALSE)
    })

    shiny::observeEvent(input$button_login, {
        checkPassword(input$user_name, input$password) %then% {
            user_id <- .
            if (is.null(user_id)) {
                ## Send “wrong password” UI events down the websocket:
                shinyjs::toggle(id = "error", anim = TRUE, time = 1, animType = "fade")
                shinyjs::delay(5000, shinyjs::toggle(id = "error", anim = TRUE, time = 1, animType = "fade"))
            } else {
                user$addLoginDetails(list(username = user_id))
                if (! is.null(cookies)) {
                    cookies$save(user$state()$info) %...>% { user$addLoginDetails(.) }
                }
            }
        }
    })

    if (! reload_on_logout) user$onLogout({
        shiny::updateTextInput(session, "password", value = "")
    })

    user
}

#' Shiny UI module for the “log in with password” functionality
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @param title header title for the login panel
#' @param user_title label for the user name text input
#' @param pass_title label for the password text input
#' @param login_title label for the login button
#' @param error_message message to display after failed login
#' @param additional_ui additional shiny UI element(s) to add below login button. Wrap multiple inside \code{shiny::tagList()}
#' @param cookie_js_ui The JavaScript UI for cookies, or NULL if not needed
#' @param additional_ui Any additional UI that the app wants to put next to the login form
#'
#' @return Shiny UI login panel with user name text input, password text input and login action button.
password_login_ui <- function(id,
                    title = "Please log in",
                    user_title = "User Name",
                    pass_title = "Password",
                    login_title = "Log in",
                    error_message = "Invalid username or password!",
                    cookie_js_ui = NULL,
                    additional_ui = NULL) {
    ns <- shiny::NS(id)

    id_password_field <- ns("password")
    id_login_button <- ns("button_login")

    ## Make it so that pressing Return in the password field does the
    ## same as clicking the login button:
    js_return_key_in_password_field <- shiny::tags$script(shiny::HTML(glue::glue(.open = "{{{", .close = "}}}", r'{
          $(document).keyup(function(event) {
            if ($("#{{{id_password_field}}}").is(":focus") && (event.keyCode == 13)) {
              $("#{{{id_login_button}}}").click();
            }
          });
        }')))

  shinyjs::hidden(
    shiny::div(
      id = ns("panel_login"),
      style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
      shiny::wellPanel(
        cookie_js_ui,
        shiny::tags$h2(title, class = "text-center", style = "padding-top: 0;"),
        shiny::textInput(ns("user_name"), shiny::tagList(shiny::icon("user"), user_title)),
        shiny::passwordInput(id_password_field, shiny::tagList(shiny::icon("unlock-alt"), pass_title)),
        js_return_key_in_password_field,
        shiny::div(
          style = "text-align: center;",
          shiny::actionButton(id_login_button, login_title, class = "btn-primary", style = "color: white;")
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
