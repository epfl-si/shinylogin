#' @export
login <- function() {
    id <- .ids$nextId()
    ns <- shiny::NS(id)

    list(
        id = id,
        loginUI  = function(title = "Log in like Magic") {
            shiny::actionButton(ns("button_login"), title, class = "btn-primary", style = "color: white;")
        },

        logoutUI = function(label = "Log out", icon = NULL, class = "btn-danger",
                            style = "color: white;") {
            shinylogin::core.logoutUI(id, label, icon, class, style)
        },

        loginServer = function(reload_on_logout = FALSE) {
            shiny::moduleServer(
                id,
                function(input, output, session) {
                    user <- serve(input, output, session, reload_on_logout)
                    ## Grant app code a low-privilege facet:
                    list(user = user$state, logout = user$logout)
                })
        })
}

.ids <- shinylogin::core.newIDSequence("custom")

serve <- function(input, output, session, reload_on_logout = FALSE) {
    user <- shinylogin::core.serve(input, output, session, reload_on_logout)

    shiny::observeEvent(input$button_login, {
        ## Wow, that was an easy login!
        user$addLoginDetails(list(username = "joe", permissions = "standard"))
    })

    user
}
