#' UI module for when the user must type in their password directly into the R application

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
#' @param cookie_expiry number of days to request browser to retain login cookie
#'
#' @return Shiny UI login panel with user name text input, password text input and login action button.
passwordLoginUI <- function(id,
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
      id = ns("panel_login"),
      style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
      shiny::wellPanel(
        shinyjs::useShinyjs(),
        passwordLoginUI__jscookie_script(),
        shinyjs::extendShinyjs(text = passwordLoginUI__js_cookie_to_r_code(ns("jscookie"), expire_days = cookie_expiry), functions = c("getcookie", "setcookie", "rmcookie")),
        shinyjs::extendShinyjs(text = passwordLoginUI__js_return_click(ns("password"), ns("button_login")), functions = c()),
        shiny::tags$h2(title, class = "text-center", style = "padding-top: 0;"),
        shiny::textInput(ns("user_name"), shiny::tagList(shiny::icon("user"), user_title)),
        shiny::passwordInput(ns("password"), shiny::tagList(shiny::icon("unlock-alt"), pass_title)),
        shiny::div(
          style = "text-align: center;",
          shiny::actionButton(ns("button_login"), login_title, class = "btn-primary", style = "color: white;")
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


#' Javascript code for managing cookies
#'
#' Manipulating cookies directly is kind of the job of the UI in Shiny
#' terms, even though nothing visible happens. Doing so is mostly
#' useful when dealing with password verification and persistence by
#' ourselves, i.e. not for SSO schemes (which will presumably
#' manipulate their own cookies).
#'
#' The underlying JS code is minified from the GitHub project js-cookie/js-cookie.  We include it here
#' to make the shinylogin package more self-contained
#' @noRd
passwordLoginUI__jscookie_script <- function() {
  shiny::includeScript(system.file("js-cookie/js-cookie.js", package = "shinylogin"))
}

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
#'   text = passwordLoginUI__js_cookie_to_r_code(ns("jscookie")),
#'   functions = c("getcookie", "setcookie", "rmcookie")
#' )
#' }
passwordLoginUI__js_cookie_to_r_code <- function(id, expire_days = 7) {
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


#' Javascript code to trigger login button with return key
#'
#' @param idpassword name of password input, with correct namespace
#' @param idbutton name of action button, with correct namespace
#' @return Character string of Javascript code.
#' @noRd
passwordLoginUI__js_return_click <- function(idpassword, idbutton) {
  glue::glue(.open = "{{{", .close = "}}}", '
	  $(document).keyup(function(event) {
    if ($("#{{{idpassword}}}").is(":focus") && (event.keyCode == 13)) {
        $("#{{{idbutton}}}").click();
    }
    });')
}
