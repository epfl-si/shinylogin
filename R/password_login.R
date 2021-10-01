#' Password-based login UI and server-side code, with cookies as a side serving
#'
#' The UI for the login form shows traditional login and password
#' fields. The shiny server is responsible for manipulating the
#' password, checking it, and computing user information like
#' permission levels. The shinylogin package does provide help for the
#' first two tasks, by means of `auth` helpers; it also
#' has the capability to persistently remember who is who using HTTP cookies.
#' However, computing permissions is on you, the app author.

requireNamespace(c("promises", "shiny", "shinyjs"))

#' Combined UI and server for password-based login
#'
#' @param auth     An object returned by e.g. \link{htpasswdAuth} with a `checkPassword` method
#' @param cookies  An object returned by e.g. \link{inMemoryCookieStore}, or NULL if no persistent session mechanism is to be used
#' @param reload_on_logout should app force a session reload on logout?
#'
#' @export
passwordLogin <- function(auth, cookies = NULL, reload_on_logout = FALSE) {
    id <- .ids$nextId()

    list(
        id = id,
        loginUI = function(title = "Please log in",
                           user_title = "User Name",
                           pass_title = "Password",
                           login_title = "Log in",
                           error_message = "Invalid username or password!",
                           additional_ui = NULL) {
            passwordLoginUI(id, title, user_title, pass_title,
                           login_title, error_message, additional_ui,
                           cookie_expire_days = cookies$expire_days)
        },

        logoutUI = function(label = "Log out", icon = NULL, class = "btn-danger",
                            style = "color: white;") {
            logoutUI(id, label, icon, class, style)
        },

        loginServer = function() {
            shiny::moduleServer(
                id,
                function(input, output, session) {
                    user <- serve_password_login(
                        input, output, session,
                        auth$checkPassword, cookies, reload_on_logout)

                    ## return object w/ reactive list
                    list(user = shiny::reactive({
                        shiny::reactiveValuesToList(user)
                    }))
                })
        })
}

newIDSequence <- function(stem) {
    uniqueID <- 0
    list(nextId = function() {
        uniqueID <<- uniqueID + 1
        sprintf("%s_%d", stem, uniqueID)
    })
}

.ids <- newIDSequence("passwordLogin")

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

#' Body of the `$loginServer` returned by \link{passwordLogin}
#'
#' This module uses shiny's new \link[shiny]{moduleServer} method as opposed to the \link[shiny]{callModule}
#' method used by the now deprecated \link{login} function and must be called differently in your app.
#' For details on how to migrate see the 'Migrating from callModule to moduleServer' section of
#' \href{https://shiny.rstudio.com/articles/modules.html}{Modularizing Shiny app code}.
#'
#' @param id 	An ID string that corresponds with the ID used to call the module's UI function
#' @param checkPassword  A function that takes login and password, and returns either NULL for login failure, or user information as a list, or a promise to either of the above
#' @param cookies  An object returned by e.g. \link{inMemoryCookieStore}, or NULL if no persistent session mechanism is to be used
#' @param reload_on_logout should app force a session reload on logout?
#'
#' @return A `shiny::ReactivValues` object `user` having
#'     `user$logged_in`, a boolean indicating whether there has been a
#'     successful login or not (or NULL to signify that login checks
#'     are still in progress); and `user$info`, which will be either
#'     the return value of `checkPassword` (if the `cookie` parameter
#'     is missing or NULL), the return value of `cookie$retrieve` (if
#'     `cookie` is non-NULL and the browser presented a valid cookie),
#'     or the union (performed with `c()` of the return values of
#'     `checkPassword` and `cookie$create` (if `cookie` is non-NULL
#'     and the browser did not present a valid cookie).
#'
#' @importFrom promises %...>%
#'
serve_password_login <- function(input, output, session, checkPassword, cookies = NULL, reload_on_logout = FALSE) {
    user <- serve_shinylogin()

    ## We want to avoid flashing a useless login prompt if we have a
    ## valid cookie, but at session initialization time we can't know
    ## immediately whether that will the case. Note that this
    ## reactiveValue is kept on the server side and not transmitted
    ## over the websocket:
    .auth_state <- shiny::reactiveValues(settled = is.null(cookies))

    ## Synchronize visibility of the login panel
    shiny::observe({
        shinyjs::toggle(id = "panel_login", condition = .auth_state$settled && !user$logged_in)
    })

    ## possibility 1: login through a cookie
    if (! is.null(cookies)) {
        passwordLoginServer__observeCookie(input, cookies, user, .auth_state)
    }

    ## possibility 2: login through login button
    shiny::observeEvent(input$button_login, {
        promises::future_promise(checkPassword(input$user_name, input$password)) %...>% {
            user_id <- .
            if (is.null(user_id)) {
                ## Send “wrong password” UI events down the websocket:
                shinyjs::toggle(id = "error", anim = TRUE, time = 1, animType = "fade")
                shinyjs::delay(5000, shinyjs::toggle(id = "error", anim = TRUE, time = 1, animType = "fade"))
            } else {
                user$logged_in <- TRUE
                user$info <- list(user = user_id)
                if (! is.null(cookies)) {
                    promises::future_promise(cookies$create(user_id)) %...>% {
                        cookie <- .
                        user$info <- c(user$info, cookie$info)
                        shinyjs::js$shinylogin_setcookie(cookie$sessionid)
                    }
                }
            }
        }
    })

    ## Logout button
    shiny::observeEvent(input$button_logout, {
        if (! is.null(cookies)) shinyjs::js$shinylogin_rmcookie()

        if (reload_on_logout) {
            session$reload()
        } else {
            shiny::updateTextInput(session, "password", value = "")
            user$logged_in <- FALSE
            user$info <- NULL
        }
    })

    user
}

passwordLoginServer__observeCookie <- function(input, cookies, user, auth_state) {
    shinyjs::js$shinylogin_getcookie()
    ## This just tells JS to send the cookie to R. As per
    ## https://stackoverflow.com/a/34728125 here is how we get the
    ## response:
    shiny::observeEvent(input$jscookie, {
        ## Regardless of the outcome below, by the end of this “game
        ## turn” of the async event loop, it will be time to show
        ## either the login or the logout UI:
        auth_state$settled <- TRUE

        ## Stop now if cookie is a dud for any reason:...
        shiny::req(
                   user$logged_in == FALSE,     ## ... if already logged in,
                   is.null(input$jscookie) == FALSE,   ## ... if no cookie,
                   nchar(input$jscookie) > 0           ## ... if cookie is empty
               )

        promises::future_promise(cookies$retrieve(input$jscookie)) %...>% {
            cookie_data <- .
            if (is.null(cookie_data)) {
                shinyjs::js$shinylogin_rmcookie()
            } else {
                user$logged_in <- TRUE
                user$info <- cookie_data
            }
        }
    })
}
