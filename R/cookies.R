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
    shiny::observeEvent(input$jscookie, {
        ## Regardless of the outcome below, by the end of this “game
        ## turn” of the async event loop, it will be time to show
        ## either the login or the logout UI:
        settled(TRUE)

        ## Stop now if cookie is a dud for any reason:...
        shiny::req(
                   user$logged_in == FALSE,     ## ... if already logged in,
                   is.null(input$jscookie) == FALSE,   ## ... if no cookie,
                   nchar(input$jscookie) > 0           ## ... if cookie is empty
               )

        cookie_store$retrieve(input$jscookie) %then% {
            cookie <- .
            if (is.null(cookie)) {
                shinyjs::js$shinylogin_rmcookie()
            } else {
                user$logged_in <- TRUE
                user$info <- cookie
            }
        }
    })

    list(
        settled = settled,
        save = function(user_info) {
            cookie_store$create(user_info$user) %then% {
                shinyjs::js$shinylogin_setcookie(.$sessionid)
                .$info
            }
        },
        clear = function() {
            shinyjs::js$shinylogin_rmcookie()
        })
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
