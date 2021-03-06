#' Shiny server behavior for cookie-based authentication
#'
#' For “stand-alone” authentication use cases (i.e. when the R
#' application is in charge of validating passwords, instead of
#' relying on some kind of Web single-sign-on system), it can be
#' beneficial to the user experience to make it so that users don't
#' have to re-type their password every time they visit the shiny
#' application in their browser again.
serve_cookie_login <- function(input, cookie_store, user) {
    settled <- shiny::reactiveVal(FALSE)

    shinyjs::js$shinylogin_getcookie()
    ## This just tells JS to send the cookie to R. As per
    ## https://stackoverflow.com/a/34728125 here is how we get the
    ## response:
    shiny::observeEvent(
        input$jscookie,
        {
            if (user$state()$logged_in) {
                ## E.g. receiving a cookie we just set
                settled(TRUE)
            } else if (! shiny::isTruthy(input$jscookie)) {
                ## E.g. no cookie
                settled(TRUE)
            } else {
                promises::finally(
                    async_load_cookie_data(cookie_store, input$jscookie, user),
                    ~{ settled(TRUE) })
            }
        })

    user$onLogout({ shinyjs::js$shinylogin_rmcookie() })

    list(
        settled = settled,
        save = function(user_info) {
            cookie_store$create(user_info) %then% {
                shinyjs::js$shinylogin_setcookie(.$sessionid)
                .$info
            }
        },
        clear = shinyjs::js$shinylogin_rmcookie)
}

#' Asynchronously load the data for `jscookie` out of `cookie_store` into `user`
#'
#' If the fetch fails, delete the (therefore invalid) cookie in the client
async_load_cookie_data <- function(cookie_store, jscookie, user) {
    cookie_store$retrieve(jscookie) %then% {
        stored_auth_data <- .
        if (shiny::isTruthy(stored_auth_data)) {
            user$addLoginDetails(stored_auth_data)
        } else {
            shinyjs::js$shinylogin_rmcookie()
        }
    }
}

#' A very simple, non-persistent cookie store.
#'
#' This store is tailored for applications that use
#' \link{passwordLogin} and manage the actual user permissions outside
#' of shinylogin (perhaps using some kind of user database or LDAP
#' directory; or perhaps there are none i.e. permissions are uniform
#' for all users). The store only persists the username, and provides
#' `$sessionid` (the cookie value) and `$login_time` (the time when
#' the cookie was created) as additional fields to `$info`.
#'
#' The implementation is based upon an in-memory RSQLite database.
#'
#' @export
inMemoryCookieStore <- function(expire_days = 7) {
    requireNamespace(c("DBI", "RSQLite", "lubridate", "tibble", "dplyr"))

    db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    DBI::dbCreateTable(db, "sessions", c(username = "TEXT", sessionid = "TEXT", login_time = "TEXT"))

    randomString <- function(n = 64) {
        paste(
            sample(x = c(letters, LETTERS, 0:9), size = n, replace = TRUE),
            collapse = "")
    }

    cookie_getter = function() {
        all_cookies <- DBI::dbReadTable(db, "sessions")
        all_cookies <- tibble::as_tibble(dplyr::mutate(all_cookies, login_time = lubridate::ymd_hms(login_time)))
        dplyr::filter(all_cookies, login_time > lubridate::now() - lubridate::days(expire_days))
    }

    cookie_setter = function(user_info, sessionid) {
        new_cookie <- tibble::tibble(username = user_info$username, sessionid = sessionid, login_time = as.character(lubridate::now()))
        DBI::dbWriteTable(db, "sessions", new_cookie, append = TRUE)
    }

    list(
        expire_days = expire_days,

        create = function(user_info) {
            sessionid_ <- randomString()
            cookie_setter(user_info, sessionid_)
            info <- dplyr::filter(cookie_getter(), sessionid == sessionid_)
            if (nrow(info) == 1) {
                list(
                    sessionid = sessionid_,
                    info = dplyr::select(info, -username))
            }
        },

        retrieve = function(session_id) {
            cookie_tibble <- dplyr::filter(cookie_getter(), sessionid == session_id)
            if (nrow(cookie_tibble) == 1) {
                cookie_tibble
            } else {
                NULL
            }
        })
}

#' The “UI” (just some JavaScript actually) for Shiny apps that use session cookies.
#'
#' The `shinylogin_getcookie` JavaScript function is invoked by the
#' server immediately after the Shiny app is done loading, and results
#' in the browser pushing a `jscookie` input, whose value is either
#' the session cookie (if set) or the empty string (thereby avoiding
#' the special-case handling of NULL in \link{shiny::observeEvent}).
#'
#' The `shinylogin_setcookie` JavaScript function is invoked by the
#' server upon succesful authentication.
#' 
#' Finally, the `shinylogin_rmcookie` JavaScript function is invoked by the
#' server upon logout or when detecting an invalid or expired cookie.
#'
#' These three functions are in turn implemented in terms of
#' [js-cookie](https://github.com/js-cookie/js-cookie), a small (<2k),
#' portable “shim” API for accessing cookies on any browser; a copy of
#' which is bundled with shinylogin.
cookie_js_ui <- function(id, expire_days) {
    ns <- shiny::NS(id)
    id <- ns("jscookie")
    cookie_name <- "shinylogin"

    shinyjs::hidden(
        shinyjs::useShinyjs(),
        shiny::includeScript(system.file("js-cookie/js-cookie.js", package = "shinylogin")),
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
                  Cookies.set("{{{cookie_name}}}", escape(params), { expires: {{{expire_days}}} });
                  Shiny.setInputValue("{{{id}}}", params);
                }
                shinyjs.shinylogin_rmcookie = function(params) {
                  Cookies.remove("{{{cookie_name}}}");
                  Shiny.setInputValue("{{{id}}}", "");
                }
              }')))
}
