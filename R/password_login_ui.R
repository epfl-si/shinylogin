# Shiny UI for when the user must type in their password directly into the R application

requireNamespace(c("glue", "shiny", "shinyjs"))

#' Shiny UI module for the “log in with password” functionality
#'
#' @param id An ID string that corresponds with the ID used to call the module's server function
#' @param title header title for the login panel
#' @param user_title label for the user name text input
#' @param pass_title label for the password text input
#' @param login_title label for the login button
#' @param error_message message to display after failed login
#' @param additional_ui additional shiny UI element(s) to add below login button. Wrap multiple inside \code{shiny::tagList()}
#' @param cookie_expire_days number of days to request browser to retain login cookie
#'
#' @return Shiny UI login panel with user name text input, password text input and login action button.
passwordLoginUI <- function(id,
                    title = "Please log in",
                    user_title = "User Name",
                    pass_title = "Password",
                    login_title = "Log in",
                    error_message = "Invalid username or password!",
                    additional_ui = NULL,
                    cookie_expire_days = 7) {
    ns <- shiny::NS(id)

    ## Arrange for the cookie to flow over the websocket into the R code:
    js_cookie_to_r <- {
        id <- ns("jscookie")
        cookie_name <- "shinylogin"

        shinyjs::extendShinyjs(
                     functions = c("shinylogin_getcookie", "shinylogin_setcookie", "shinylogin_rmcookie"),
                     text = glue::glue(.open = "{{{", .close = "}}}", r'{
                shinyjs.shinylogin_getcookie = function(params) {
                  var cookie = Cookies.get("{{{cookie_name}}}");
                  if (typeof cookie === "undefined") {
                    cookie = "";
                  }
                  Shiny.setInputValue("{{{id}}}", cookie);
                }
                shinyjs.shinylogin_setcookie = function(params) {
                  Cookies.set("{{{cookie_name}}}", escape(params), { expires: {{{cookie_expire_days}}} });
                  Shiny.setInputValue("{{{id}}}", params);
                }
                shinyjs.shinylogin_rmcookie = function(params) {
                  Cookies.remove("{{{cookie_name}}}");
                  Shiny.setInputValue("{{{id}}}", "");
                }
              }'))
    }

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
        shinyjs::useShinyjs(),
        ## Only password-based authentication schemes require cookies:
        shiny::includeScript(system.file("js-cookie/js-cookie.js", package = "shinylogin")),
        js_cookie_to_r,
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
