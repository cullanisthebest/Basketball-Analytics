# install.packages("shiny")
# install.packages("shinydashboard")
# install.packages("shinyBS")
# install.packages("plotly")

library(shiny)
library(shinydashboard)
library(shinyBS)
library(plotly)
options(scipen = 999)

shinyUI(dashboardPage(
  dashboardHeader(title = "Basketball App"),
  dashboardSidebar(disable = FALSE,
                   sidebarMenu(
                     menuItem(
                       text = "Database", tabName = "database", icon = icon("file-text")
                     )
                     #menuItem(text = "Data Integrity", tabName = "dataintegrity", icon = icon("table"))
                   )),
  dashboardBody(tags$head(tags$style(
    HTML(
      '
      @font-face {
      font-family:"frutiger-light";
      src:url("../fonts/722129/fbd7c0fb-f004-4e83-81d2-1d400413a873.eot?#iefix");
      src:url("../fonts/722129/fbd7c0fb-f004-4e83-81d2-1d400413a873.eot?#iefix") format("eot"), url("../fonts/722129/9aa32a81-1124-4c43-b3db-15bfb1f7aed2.woff") format("woff"), url("../fonts/722129/6faffbf4-f8e8-4817-b24b-a390e166be7e.ttf") format("truetype"), url("../fonts/722129/fc09de64-de25-425e-90dc-a0cae29b02c4.svg#fc09de64-de25-425e-90dc-a0cae29b02c4") format("svg");
      font-weight: bold;
      font-style: normal;
      }
      .main-header .logo {
      font-weight: bold;
      font-size:20px;
      }
      .box {
      border-radius: 0 !important;
      border: 0px solid #3c8dbc !important;
      }
      .selectize-input {
      border-radius: 2px !important;
      border-color: rgb(225, 229, 234);
      }
      .body {
      font-family: "frutiger-light";
      }
      .navbar {
      background-color: #222d32 !important;
      }
      '
    )
    )),
    
    tabItems(# First tab content
      tabItem(
        tabName = "database",
        fluidRow(
          column(
            width = 4,
            box(
              title = "Select Functions",
              solidHeader = TRUE,
              width = NULL,
              status = "danger",
              selectInput(
                'myfunction', label = "Select Function:", choices = c("All Statistics",
                                                                      "Top 10 Players", 
                                                                      "Most Similar Players", 
                                                                      "Statistics Prediction", 
                                                                      "Team Player Stat Counter",
                                                                      "Account and Mock Teams")
              ),
              conditionalPanel(
                condition = "input.myfunction == 'All Statistics'",
                tags$h4("Compare Statistics:"),
                selectInput("compStat1", "Statistic One: ", choices = c("PPG", "assists", "steals", "blocks", "rebounds")),
                selectInput("compStat2", "Statistic Two: ", choices = c("PPG", "assists", "steals", "blocks", "rebounds"))
              ),
              conditionalPanel(
                condition = "input.myfunction == 'Top 10 Players'",
                selectInput("top10statistic", "Select the statistic: ", choices = c("PPG", "assists", "steals", "blocks")),
                selectInput("top10year", "Select the year: ", choices = 2006:2015, selected = 2015)
              ),
              conditionalPanel(
                condition = "input.myfunction == 'Most Similar Players' || input.myfunction == 'Statistics Prediction'",
                textInput("firstName", label = "Player First Name:", value = "Alan"),
                textInput("lastName", label = "Player Last Name:", value = "Butler")
              ),
              conditionalPanel(
                condition = "input.myfunction == 'Statistics Prediction'",
                selectInput("graphStat", label = "Select Graph Statistic:", choices = c("PPG", "pointsScored", "assists", "steals", "blocks", "rebounds", "turnovers"), selected="PPG")
              ),
              conditionalPanel(
                condition = "input.myfunction == 'Team Player Stat Counter'",
                selectInput("teamPlayerStatStatistic", "Select the statistic: ", choices = c("pointsScored", "assists", "steals", "blocks")),
                sliderInput("teamPlayerStatCounter", "Greater than: ", min = 1, max = 50, value = 30, step = 1),
                selectInput("teamPlayerStatYear", "Select the year: ", choices = 2006:2015, selected = 2015)
                ),
              actionButton("resultsUpdateButton", "Update Results", icon = icon("refresh") )
              #actionButton("plotUpdateButton", "Update Plot", icon = icon("refresh") )
            ),
            conditionalPanel(condition = "input.myfunction == 'Account and Mock Teams'",
            box(
              title = "Account Functions",
              solidHeader = TRUE,
              width = NULL,
              status = "danger",
              selectInput("userFunction", "Select the function: ", choices = c("Register Account", "Make Mock Team", "Insert Player", "Delete Player", "Show Players of User")),
              conditionalPanel(
                condition = "input.userFunction == 'Register Account'",
                textInput("registerUsername", label = "Username:"),
                passwordInput("registerPassword", label = "Password:"),
                textInput("registerEmail", label = "Email:")
              ),
              conditionalPanel(
                condition = "input.userFunction == 'Make Mock Team' || input.userFunction == 'Insert Player' || input.userFunction == 'Delete Player'",
                textInput("functionUsername", label = "Username:"),
                passwordInput("functionPassword", label = "Password:"),
                textInput("mockTeamName", label = "Mock Team Name:")
              ),
              conditionalPanel(
                condition = "input.userFunction == 'Insert Player'",
                textInput("insertPlayerFirstName", label = "Player First Name:"),
                textInput("insertPlayerLastName", label = "Player Last Name:")
              ),
              conditionalPanel(
                condition = "input.userFunction == 'Delete Player'",
                textInput("deletePlayerFirstName", label = "Player First Name:"),
                textInput("deletePlayerLastName", label = "Player Last Name:")
              ),
              conditionalPanel(
                condition = "input.userFunction == 'Show Players of User'",
                textInput("mockTeamUsername", label = "Username:")
              ),
              conditionalPanel(
                condition = "input.userFunction == 'Register Account'",
                actionButton("registerAccountButton", "Register Account", icon = icon("refresh"))
              ),
              conditionalPanel(
                condition = "input.userFunction == 'Make Mock Team'",
                actionButton("makeMockTeamButton", "Make Mock Team", icon = icon("refresh"))
              ),
              conditionalPanel(
                condition = "input.userFunction == 'Insert Player'",
                actionButton("insertPlayerButton", "Insert Player", icon = icon("refresh"))
              ),
              conditionalPanel(
                condition = "input.userFunction == 'Delete Player'",
                actionButton("deletePlayerButton", "Delete Player", icon = icon("refresh"))
              )
#               conditionalPanel(
#                 condition = "input.userFunction == 'Delete Mock Team'",
#                 textInput("functionUsername", label = "Username:"),
#                 textInput("functionPassword", label = "Password:"),
#                 textInput("deleteMockTeamName", label = "Mock Team Name:"),
#                 actionButton("deleteMockTeamButton", "Delete Mock Team", icon = icon("refresh"))
#               )
            )
            ),
            box(
              title = "Message Console",
              solidHeader = TRUE,
              width = NULL,
              status = "danger",
              textOutput("message")
            )
          ),
          column(
            width = 8,
            tabBox(
              title = "Results",
              id = "results",
              #solidHeader = TRUE,
              width = NULL,
              #tatus = "primary",
              tabPanel(
                "Data Table",
                dataTableOutput("generateResults")
              ),
              tabPanel(
                "Data Plot",
                plotlyOutput("generatePlot")
              )
            )
          )
        )
      )))
  ))
