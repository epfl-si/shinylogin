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
    id_stem <- .ids$nextId()
    login_id <- sprintf("%s_login", id_stem)
    logout_id <- sprintf("%s_logout", id_stem)

    ## XXX OUCH
    ## Must be refactored inside `auth`, prior to being eliminated
    ## in favor of an htpasswd file
    requireNamespace("sodium")
    user_base <- tibble(
        user = c("user1", "user2"),
        password_hash = sapply(c("pass1", "pass2"), sodium::password_store))

    list(
        loginUI = function(title = "Please log in",
                           user_title = "User Name",
                           pass_title = "Password",
                           login_title = "Log in",
                           error_message = "Invalid username or password!",
                           additional_ui = NULL) {
            passwordLoginUI(id = login_id, title, user_title, pass_title,
                           login_title, error_message, additional_ui,
                           cookie_expiry = cookies$expiry_days)
        },

        logoutUI = function(label = "Log out", icon = NULL, class = "btn-danger",
                            style = "color: white;") {
            legacy_logoutUI(id = logout_id, label, icon, class, style)
        },

        loginServer = function() {
            loginServer <- legacy_loginServer(
                id = login_id,

                ## XXX BEGINNING OF OUCH
                ## Must be refactored inside `auth`, prior to being eliminated
                data = user_base,
                user_col = user,
                pwd_col = password_hash,
                sodium_hashed = TRUE,
                ## XXX END OF OUCH

                ## TODO: refactor so that loginServer doesn't need to care about this.
                sessionid_col = sessionid,
                cookie_logins = TRUE,
                cookie_getter = cookies$cookie_getter,
                cookie_setter = cookies$cookie_setter,

                log_out = reactive(logoutServer()))

            logoutServer <- legacy_logoutServer(
                id = logout_id,
                active = reactive(loginServer()$user_auth))

            loginServer
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
    # TODO
}

#' @export
inMemoryCookieStore <- function(expiry_days = 7) {
    requireNamespace(c("DBI", "RSQLite", "lubridate", "tibble", "dplyr"))

    db <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    DBI::dbCreateTable(db, "sessions", c(user = "TEXT", sessionid = "TEXT", login_time = "TEXT"))

    list(
        expiry_days = expiry_days,

        cookie_getter = function() {
            all_cookies <- DBI::dbReadTable(db, "sessions")
            all_cookies <- tibble::as_tibble(dplyr::mutate(all_cookies, login_time = lubridate::ymd_hms(login_time)))
            dplyr::filter(all_cookies, login_time > lubridate::now() - lubridate::days(expiry_days))
        },

        cookie_setter = function(user, sessionid) {
            new_cookie <- tibble::tibble(user = user, sessionid = sessionid, login_time = as.character(lubridate::now()))
            DBI::dbWriteTable(db, "sessions", new_cookie, append = TRUE)
        })
}
