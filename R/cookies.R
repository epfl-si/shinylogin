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
            cookie_store$create(user_info$user) %then% {
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

#' @export
inMemoryCookieStore <- function(expire_days = 7) {
    requireNamespace(c("DBI", "RSQLite", "lubridate", "tibble", "dplyr"))

    db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    DBI::dbCreateTable(db, "sessions", c(user = "TEXT", sessionid = "TEXT", login_time = "TEXT"))

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

    cookie_setter = function(user, sessionid) {
        new_cookie <- tibble::tibble(user = user, sessionid = sessionid, login_time = as.character(lubridate::now()))
        DBI::dbWriteTable(db, "sessions", new_cookie, append = TRUE)
    }

    list(
        expire_days = expire_days,

        create = function(user_id) {
            sessionid_ <- randomString()
            cookie_setter(user_id, sessionid_)
            info <- dplyr::filter(cookie_getter(), sessionid == sessionid_)
            if (nrow(info) == 1) {
                list(
                    sessionid = sessionid_,
                    info = dplyr::select(info, -user))
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

cookie_js_ui <- function(id, expire_days) {
    ns <- shiny::NS(id)
    id <- ns("jscookie")
    cookie_name <- "shinylogin"

    shinyjs::hidden(
        shinyjs::useShinyjs(),
        shiny::includeScript(system.file("js-cookie/js-cookie.js", package = "shinylogin")),
        ## Arrange for the cookie to flow over the websocket into the R code:
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
