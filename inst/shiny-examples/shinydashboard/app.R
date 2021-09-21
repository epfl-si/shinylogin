library(shiny)
library(shinydashboard)
library(tibble)
library(glue)

login <- shinylogin::passwordLogin(
  auth = shinylogin::htpasswdAuth(system.file("shiny-examples/htpasswd", package = "shinylogin")),
  cookies = shinylogin::inMemoryCookieStore(expire_days = 7))

user_base <- tibble(
  user = c("user1", "user2"),
  name = c("User One", "User Two"),
  permissions = c("admin", "standard"),
  password = c("pass1", "pass2"),    # Just to help the user “cheat.” Obviously, your app shouldn't have this!
)

ui <- dashboardPage(
  dashboardHeader(
    title = "shinylogin",
    tags$li(
      class = "dropdown",
      style = "padding: 8px;",
      login$logoutUI()
    ),
    tags$li(
      class = "dropdown",
      tags$a(
        icon("github"),
        href = "https://github.com/paulc91/shinylogin",
        title = "See the code on github"
      )
    )
  ),
  dashboardSidebar(
    collapsed = TRUE,
    div(textOutput("welcome"), style = "padding: 20px")
  ),
  dashboardBody(
      login$loginUI(
      additional_ui = tagList(
        tags$p("test the different outputs from the sample logins below
             as well as an invalid login attempt.", class = "text-center"),
        HTML(knitr::kable(user_base, format = "html", table.attr = "style='width:100%;'"))
      )
    ),
    uiOutput("testUI")
  )
)

server <- function(input, output, session) {
  credentials <- login$loginServer()

  observe({
    if (credentials()$user_auth) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    }
  })

  user_info <- reactive({
      creds <- credentials()
      req(creds$user_auth)

      user_line <- dplyr::filter(user_base, user == creds$info$user)
      req(nrow(user_line) > 0)

      dplyr::left_join(
          by = c("user"),
          creds$info,
          ## Obviously in a real app, you wouldn't have a `password` colum and
          ## you wouldn't have to do this:
          dplyr::select(user_line, -password))
  })

  user_data <- reactive({
    req(info <- user_info())

    if (info$permissions == "admin") {
      dplyr::starwars[, 1:10]
    } else if (info$permissions == "standard") {
      dplyr::storms[, 1:11]
    }
  })

  output$welcome <- renderText({
    req(info <- user_info())

    glue("Welcome {info$name}")
  })

  output$testUI <- renderUI({
    req(info <- user_info())

    fluidRow(
      column(
        width = 12,
        tags$h2(glue("Your permission level is: {info$permissions}.
                     You logged in at: {info$login_time}.
                     Your data is: {ifelse(info$permissions == 'admin', 'Starwars', 'Storms')}.")),
        box(
          width = NULL,
          status = "primary",
          title = ifelse(info$permissions == "admin", "Starwars Data", "Storms Data"),
          DT::renderDT(user_data(), options = list(scrollX = TRUE))
        )
      )
    )
  })
}

shiny::shinyApp(ui, server)
