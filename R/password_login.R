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
            passwordLogoutUI(id, label, icon, class, style)
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

    ## TODO: make this an R6Class() or something
    list(checkPassword = function(username, password) {
        htpasswd_line <- read.csv(path, sep=":", header = FALSE,
                                  col.names = c("user", "hashed_password")
                                  )[which(htpasswd$user == username),]
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

    ## TODO: make this an R6Class() or something
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
