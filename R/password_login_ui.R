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
#' @param cookie_js_ui The JavaScript UI for cookies, or NULL if not needed
#' @param additional_ui Any additional UI that the app wants to put next to the login form
#'
#' @return Shiny UI login panel with user name text input, password text input and login action button.
passwordLoginUI <- function(id,
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
