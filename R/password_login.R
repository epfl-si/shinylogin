#' Password-based login UI and server-side code, with cookies as a side serving
#'
#' The UI for the login form shows traditional login and password
#' fields. The shiny server is responsible for manipulating the
#' password, checking it, and computing user information like
#' permission levels. The shinylogin package does provide help for the
#' first two tasks, by means of `auth` helpers; it also
#' has the capability to persistently remember who is who using HTTP cookies.
#' However, computing permissions is on you, the app author.

#' @export
passwordLogin <- function(auth, cookies = NULL) {
    id <- .ids$nextId()

    list(
        loginUI = function(title = "Please log in",
                           user_title = "User Name",
                           pass_title = "Password",
                           login_title = "Log in",
                           error_message = "Invalid username or password!",
                           additional_ui = NULL) {
            passwordLoginUI(id, title, user_title, pass_title,
                           login_title, error_message, additional_ui,
                           cookie_expiry = cookies$expiry_days)
        },

        logoutUI = function(label = "Log out", icon = NULL, class = "btn-danger",
                            style = "color: white;") {
            legacy_logoutUI(id, label, icon, class, style)
        },

        loginServer = function() {
            legacy_loginServer(
                id = id,
                auth = auth,
                cookies = cookies)
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

#' @export
htpasswdAuth <- function(path) {
    ## XXX OUCH
    ## Needs eliminatin'
    requireNamespace("sodium")
    user_base <- tibble(
        user = c("user1", "user2"),
        password_hash = sapply(c("pass1", "pass2"), sodium::password_store))
    user_base <- dplyr::mutate_if(user_base, is.factor, as.character)

    ## TODO: make this an R6Class() or something
    list(checkPassword = function(username, password) {
        # check for match of input username to username column in data
        row_username <- which(dplyr::pull(user_base, user) == username)

        if (length(row_username)) {
          row_password <- dplyr::filter(user_base, dplyr::row_number() == row_username)
          password_match <- sodium::password_verify(row_password$password_hash, password)
        } else {
          password_match <- FALSE
        }

        if (password_match) {
            username
        } else {
            NULL
        }
    })
}

#' @export
inMemoryCookieStore <- function(expiry_days = 7) {
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
        dplyr::filter(all_cookies, login_time > lubridate::now() - lubridate::days(expiry_days))
    }

    cookie_setter = function(user, sessionid) {
        new_cookie <- tibble::tibble(user = user, sessionid = sessionid, login_time = as.character(lubridate::now()))
        DBI::dbWriteTable(db, "sessions", new_cookie, append = TRUE)
    }

    ## TODO: make this an R6Class() or something
    list(
        expiry_days = expiry_days,

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
