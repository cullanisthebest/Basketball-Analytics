# install.packages("DBI")
# install.packages("RMySQL")
# install.packages("shiny")
# install.packages("ggplot2")
# install.packages("plotly")

library(DBI)
library(shiny)
library(ggplot2)
library(plotly)
library(RMySQL) # Load the library

m <- dbDriver("MySQL") # Load the driver
shinyServer(function(input, output) {
  outputMessage <- "All errors will be shown here!"
  
  #Query One-----------------------------------------------------------------------
  #Look at all statistics
  allstatistics <- function() {
    con <-
      dbConnect(
        m,user = 'root',password = 'tonyhawks',host = '127.0.0.1',port =
          3306, dbname = "basketball"
      )
    query <- paste(
      'SELECT p.firstName, p.lastName, se.year, pointsScored, pointsScored/games AS PPG, offensiveRebounds + defensiveRebounds AS rebounds,
      assists, steals, blocks, turnovers, personalFouls, games, gamesStarted, threePointers,
      threePointers/threePointAttempts AS threePointPct, twoPointers, twoPointers/twoPointAttempts AS twoPointPct,
      freethrowsMade, freethrowsMade/freeThrowAttempts AS freeThrowPct
      FROM statistics s
      NATURAL JOIN player p
      NATURAL JOIN season se'
    )
    result <- dbGetQuery(conn = con, statement = query)
    all_cons <- dbListConnections(MySQL())
    for (con in all_cons)
      dbDisconnect(con)
    return(result)
  }
  
  #Query Two-----------------------------------------------------------------------
  #Find the top 10 players in the current year with the highest PPG
  top10players <- function() {
    con <-
      dbConnect(
        m,user = 'root',password = 'tonyhawks',host = '127.0.0.1',port =
          3306, dbname = "basketball"
      )
    query <- paste(
      'SELECT se.year, positionname, firstname, lastname, pointsScored/games AS PPG, assists, steals, blocks
      FROM player p
      JOIN statistics s ON p.playerID=s.playerID
      JOIN playerinseason ps ON p.playerID=ps.playerID AND s.seasonID=ps.seasonID
      JOIN position po ON po.positionID=ps.positionID
      JOIN season se ON ps.seasonID=se.seasonID
      WHERE se.year = ', input$top10year,
      ' ORDER BY ', input$top10statistic, ' DESC
      LIMIT 10;'
    )
    result <- dbGetQuery(conn = con, statement = query)
    all_cons <- dbListConnections(MySQL())
    for (con in all_cons)
      dbDisconnect(con)
    return(result)
    #dbClearResult(dbListResults(con)[[1]]) # Use this to free the connection
  }
  
  
  #Query Three-----------------------------------------------------------------------
  #Find the top 10 most similar players to a player
  similarPlayers <- function() {
    con <-
      dbConnect(
        m,user = 'root',password = 'tonyhawks',host = '127.0.0.1',port =
          3306, dbname = "basketball"
      )
    query <- paste0(
      'SELECT p2.firstName, p2.lastName, s.similarityScore
      FROM player p
      JOIN similarities s ON p.playerID=s.playerID1
      JOIN player p2 ON p2.playerID=s.playerID2
      WHERE s.playerID1=(SELECT playerID
      FROM player
      WHERE firstName = "', input$firstName,
      '" AND lastName="', input$lastName, '")
      ORDER BY s.similarityScore DESC LIMIT 10;'
      )
    result <- dbGetQuery(conn = con, statement = query)
    all_cons <- dbListConnections(MySQL())
    for (con in all_cons)
      dbDisconnect(con)
    return(result)
  }
  
  #Query Four-----------------------------------------------------------------------
  #Display the past and predicted stats for a player
  statisticsPrediction <- function() {
    con <-
      dbConnect(
        m,user = 'root',password = 'tonyhawks',host = '127.0.0.1',port =
          3306, dbname = "basketball"
      )
    query <-
      paste0(
        'SELECT s.year, firstName, lastName, pointsScored, steals, assists, blocks,
        pointsScored/games AS PPG, offensiveRebounds + defensiveRebounds AS rebounds,
        turnovers
        FROM(
        (SELECT *
        FROM statistics)
        UNION
        (SELECT *
        FROM statisticsprediction)) allstats
        JOIN player p ON p.playerID=allstats.playerID
        JOIN season s ON s.seasonID=allstats.seasonID
        WHERE firstName = "', input$firstName,
        '" AND lastName="', input$lastName, '"
        ORDER BY year DESC;'
      )
    result <- dbGetQuery(conn = con, statement = query)
    all_cons <- dbListConnections(MySQL())
    for (con in all_cons)
      dbDisconnect(con)
    return(result)
  }

  #Query Five-----------------------------------------------------------------------
  #Show the number of players on a team with a chosen stat
  teamPlayerStatCount <- function() {
    con <-
      dbConnect(
        m,user = 'root',password = 'tonyhawks',host = '127.0.0.1',port =
          3306, dbname = "basketball"
      )
    query <-
      paste0(
        'SELECT t.teamName, COUNT(1)
        FROM statistics sp
        JOIN season se ON sp.seasonID=se.seasonID
        JOIN playerinseason ps ON ps.playerID=sp.playerID AND ps.seasonID=se.seasonID
        JOIN team t ON t.teamID=ps.teamID
        WHERE se.year="', input$teamPlayerStatYear, '" AND  sp.', input$teamPlayerStatStatistic, '/sp.games>', input$teamPlayerStatCounter,
        ' GROUP BY teamName;'
      )
    result <- dbGetQuery(conn = con, statement = query)
    all_cons <- dbListConnections(MySQL())
    for (con in all_cons)
      dbDisconnect(con)
    return(result)
  }
  
  #Query Six-----------------------------------------------------------------------
  #Register a player
  observeEvent(input$registerAccountButton, {
    con <-
      dbConnect(
        m,user = 'root',password = 'tonyhawks',host = '127.0.0.1',port =
          3306, dbname = "basketball"
      )
    query <-
      paste0(
        'INSERT INTO user(username, password, email)
        VALUES ("', input$registerUsername, '", "', input$registerPassword, '", "',
        input$registerEmail, '");'
        )
    tryCatch({
      dbGetQuery(conn = con, statement = query)
      output$message <- renderText({
        paste(
          "Congratulations! Your account", input$registerUsername, "is now registered!"
        )
      })
    },
    warning = function(w) {
      print(w)
      observeEvent(input$registerAccountButton, {
        output$message <- renderText({
          paste(
            "Sorry, the username", input$registerUsername, "or email", input$registerEmail,"is already in use."
          )
        })
      })
    },
    error = function(e) {
      print(e)
      observeEvent(input$registerAccountButton, {
        output$message <- renderText({
          paste(
            "Sorry, the username", input$registerUsername, "or email", input$registerEmail,"is already in use!"
          )
        })
      })
    },
    stop = function(s) {
      print(s)
      observeEvent(input$registerAccountButton, {
        output$message <- renderText({
          paste(
            "Sorry, the username", input$registerUsername, "or email", input$registerEmail,"is already in use!"
          )
        })
      })
    },
    finally = function() {
      print("finished")
    })
    all_cons <- dbListConnections(MySQL())
    for (con in all_cons)
      dbDisconnect(con)
  })
  
  #Query Seven-----------------------------------------------------------------------
  #Make a mock team
  observeEvent(input$makeMockTeamButton, {
    con <-
      dbConnect(
        m,user = 'root',password = 'tonyhawks',host = '127.0.0.1',port =
          3306, dbname = "basketball"
      )
    query <-
      paste0(
        'INSERT INTO mockteam(mockTeamName, userID)
        VALUES ("', input$mockTeamName, '",
        (SELECT userID
        FROM user
        WHERE username="', input$functionUsername, '"
        AND password="', input$functionPassword, '"));'
      )
    tryCatch({
      dbGetQuery(conn = con, statement = query)
      output$message <- renderText({
        paste("Congratulations! Your team", input$mockTeamName, "is now created!")
      })
    },
    warning = function(w) {
      print(w)
      observeEvent(input$makeMockTeamButton, {
        output$message <- renderText({
          paste0(
            "Your credentials are incorrect or somebody already took ",input$mockTeamName ,"!"
          )
        })
      })
    },
    error = function(e) {
      print(e)
      observeEvent(input$makeMockTeamButton, {
        output$message <- renderText({
          paste0(
            "Your credentials are incorrect or somebody already took ",input$mockTeamName ,"!"
          )
        })
      })
    },
    stop = function(s) {
      print(s)
      observeEvent(input$makeMockTeamButton, {
        output$message <- renderText({
          paste0(
            "Your credentials are incorrect or somebody already took ",input$mockTeamName ,"!"
          )
        })
      })
    },
    finally = function() {
      "finished"
    })
    all_cons <- dbListConnections(MySQL())
    for (con in all_cons)
      dbDisconnect(con)
  })
  
  output$message <- renderText({
    return(outputMessage)
  })
  
  #Query Eight-----------------------------------------------------------------------
  #Insert player into mock team
  observeEvent(input$insertPlayerButton, {
    con <-
      dbConnect(
        m,user = 'root',password = 'tonyhawks',host = '127.0.0.1',port =
          3306, dbname = "basketball"
      )
    query <-
      paste0(
        'INSERT INTO playerinmockteam VALUES ((SELECT mockTeamID
        FROM mockteam
        WHERE userID=(SELECT userID
        FROM user
        WHERE username="', input$functionUsername, '"
        AND password="', input$functionPassword, '")
        AND mockTeamName="', input$mockTeamName, '"),
        (SELECT playerID
        FROM player
        WHERE firstname="', input$insertPlayerFirstName, '"
        AND lastname="', input$insertPlayerLastName, '"),
        15,
        CURDATE());'
)
    tryCatch({
      dbGetQuery(conn = con, statement = query)
      output$message <- renderText({
        paste(
          "Congratulations! You have added", input$insertPlayerFirstName, input$insertPlayerLastName,
          "into your team ", input$mockTeamName
        )
      })
    },
    warning = function(w) {
      print(w)
      observeEvent(input$insertPlayerButton, {
        output$message <- renderText({
          "Your credentials are incorrect or your Mock Team already has that player or the player doesn't exist!"
        })
      })
    },
    error = function(e) {
      print(e)
      observeEvent(input$insertPlayerButton, {
        output$message <- renderText({
          "Your credentials are incorrect or your Mock Team already has that player or the player doesn't exist!"
        })
      })
    },
    stop = function(s) {
      print(s)
      observeEvent(input$insertPlayerButton, {
        output$message <- renderText({
          "Your credentials are incorrect or your Mock Team already has that player or the player doesn't exist!"
        })
      })
    },
    finally = function() {
      "finished"
    })
    all_cons <- dbListConnections(MySQL())
    for (con in all_cons)
      dbDisconnect(con)
  })
  
  #Query Nine-----------------------------------------------------------------------
  #Delete player from a mock team
  observeEvent(input$deletePlayerButton, {
    con <-
      dbConnect(
        m,user = 'root',password = 'tonyhawks',host = '127.0.0.1',port =
          3306, dbname = "basketball"
      )
    query <-
      paste0(
        'DELETE FROM playerinmockteam
        WHERE mockTeamID=(SELECT mockTeamID
        FROM mockteam
        WHERE userID=(SELECT userID
        FROM user
        WHERE username="', input$functionUsername, '"
        AND password="', input$functionPassword, '")
        AND mockTeamName="', input$mockTeamName, '")
        AND playerID = (SELECT playerID
        FROM player
        WHERE firstname="', input$deletePlayerFirstName, '"
        AND lastname="', input$deletePlayerLastName, '")
        AND seasonID = 15;'
      )
    tryCatch({
      dbGetQuery(conn = con, statement = query)
      output$message <- renderText({
        paste(
          "You have deleted", input$deletePlayerFirstName, input$deletePlayerLastName,
          "from your team", input$mockTeamname
        )
      })
    },
    warning = function(w) {
      print(w)
      observeEvent(input$deletePlayerButton, {
        output$message <- renderText({
          "Your credentials are incorrect or your Mock Team doesn't have that player!"
        })
      })
    },
    error = function(e) {
      print(e)
      observeEvent(input$deletePlayerButton, {
        output$message <- renderText({
          "Your credentials are incorrect or your Mock Team doesn't have that player!"
        })
      })
    },
    stop = function(s) {
      print(s)
      observeEvent(input$deletePlayerButton, {
        output$message <- renderText({
          "Your credentials are incorrect or your Mock Team doesn't have that player!"
        })
      })
    },
    finally = function() {
      "finished"
    })
    all_cons <- dbListConnections(MySQL())
    for (con in all_cons)
      dbDisconnect(con)
  })
  
  #DID NOT USE THIS QUERY-----------------------------------------------------------------------
  observeEvent(input$deleteMockTeamButton, {
    con <-
      dbConnect(
        m,user = 'root',password = 'tonyhawks',host = '127.0.0.1',port =
          3306, dbname = "basketball"
      )
    query <-
      paste0('')
    tryCatch(
      dbGetQuery(conn = con, statement = query),
      warning = function(w) {
        print(w)
        observeEvent(input$deleteMockTeamButton, {
          output$message <- renderText({
            "Your credentials are incorrect or you don't have that Mock Team!"
          })
        })
      },
      error = function(e) {
        observeEvent(input$deleteMockTeamButton, {
          output$message <- renderText({
            "Your credentials are incorrect or you don't have that Mock Team!"
          })
        })
      },
      stop = function(s) {
        observeEvent(input$deleteMockTeamButton, {
          output$message <- renderText({
            "Your credentials are incorrect or you don't have that Mock Team!"
          })
        })
      },
      finally = function() {
        "finished"
      }
    )
    all_cons <- dbListConnections(MySQL())
    for (con in all_cons)
      dbDisconnect(con)
  })
  
  #Query Ten-----------------------------------------------------------------------
  #Show all the players of a user and which mock teams they belong
  showMockTeamPlayer <- function() {
    con <-
      dbConnect(
        m,user = 'root',password = 'tonyhawks',host = '127.0.0.1',port =
          3306, dbname = "basketball"
      )
    query <-
      paste0(
        'SELECT mockTeamName, firstName, lastName, dateAdded
        FROM(SELECT *
        FROM mockteam
        WHERE userID=(SELECT userID
        FROM user
        WHERE username="', input$mockTeamUsername, '")) userMockTeams
        JOIN playerinmockteam pm ON userMockTeams.mockTeamID=pm.mockTeamID
        JOIN player p ON p.playerid=pm.playerid;'
      )
    result <- dbGetQuery(conn = con, statement = query)
    output$message <- renderText({
      paste("All the players in your mock teams are shown!")
    })
    all_cons <- dbListConnections(MySQL())
    for (con in all_cons)
      dbDisconnect(con)
    return(result)
  }
  
  #Query Eleven-----------------------------------------------------------------------
  #Compare two statistics in the year 2015
  #Graph the statistics predictions for a player
  output$generatePlot <- renderPlotly({
    input$resultsUpdateButton
    isolate(if (input$myfunction == "Statistics Prediction") {
      updateFlag <- input$resultsUpdateButton
      data <- statisticsPrediction()
      data <- data[order(data[, 1]),]
      title <-
        paste("Statistics and Predictions for", input$firstName, input$lastName)
      myggplot <-
        ggplot(data = data, aes_string(
          x = "year", y = input$graphStat, group = 1
        )) +
        geom_point() + geom_smooth(method = "lm", formula = y ~ poly(x, 3), size =
                                     1) +
        ggtitle(title) +
        theme(plot.title = element_text(lineheight = .8, face = "normal"))
      myggplotly <- ggplotly(myggplot)
      myggplotly
    } else if (input$myfunction == "All Statistics") {
      con <-
        dbConnect(
          m,user = 'root',password = 'tonyhawks',host = '127.0.0.1',port =
            3306, dbname = "basketball"
        )
      query <- paste(
        'SELECT p.firstName, p.lastName, se.year, pointsScored, pointsScored/games AS PPG, offensiveRebounds + defensiveRebounds AS rebounds,
        assists, steals, blocks, turnovers, personalFouls, games, gamesStarted, threePointers,
        threePointers/threePointAttempts AS threePointPct, twoPointers, twoPointers/twoPointAttempts AS twoPointPct,
        freethrowsMade, freethrowsMade/freeThrowAttempts AS freeThrowPct
        FROM statistics s
        NATURAL JOIN player p
        NATURAL JOIN season se
        WHERE year=2015'
      )
      result <- dbGetQuery(conn = con, statement = query)
      all_cons <- dbListConnections(MySQL())
      for (con in all_cons)
        dbDisconnect(con)
      
      updateFlag <- input$resultsUpdateButton
      
      title <-
        paste("Statistics Comparison for", input$compStat1, "and", input$compStat2)
      
      if (input$compStat1 == "PPG" && input$compStat2 == "PPG")
        plot_ly(
          result, x = PPG, y = PPG, mode = "markers", color = PPG, size = PPG
        ) %>% layout(title = title)
      
      else if (input$compStat1 == "PPG" &&
               input$compStat2 == "assists")
        plot_ly(
          result, x = PPG, y = assists, mode = "markers", color = PPG, size = assists
        ) %>% layout(title = title)
      
      else if (input$compStat1 == "PPG" &&
               input$compStat2 == "steals")
        plot_ly(
          result, x = PPG, y = steals, mode = "markers", color = PPG, size = steals
        ) %>% layout(title = title)
      
      else if (input$compStat1 == "PPG" &&
               input$compStat2 == "blocks")
        plot_ly(
          result, x = PPG, y = blocks, mode = "markers", color = PPG, size = blocks
        ) %>% layout(title = title)
      
      else if (input$compStat1 == "PPG" &&
               input$compStat2 == "rebounds")
        plot_ly(
          result, x = PPG, y = rebounds, mode = "markers", color = PPG, size = rebounds
        ) %>% layout(title = title)
      
      else if (input$compStat1 == "assists" &&
               input$compStat2 == "PPG")
        plot_ly(
          result, x = assists, y = PPG, mode = "markers", color = assists, size = PPG
        ) %>% layout(title = title)
      
      else if (input$compStat1 == "assists" &&
               input$compStat2 == "assists")
        plot_ly(
          result, x = assists, y = assists, mode = "markers", color = assists, size = assists
        ) %>% layout(title = title)
      
      else if (input$compStat1 == "assists" &&
               input$compStat2 == "steals")
        plot_ly(
          result, x = assists, y = steals, mode = "markers", color = assists, size = steals
        ) %>% layout(title = title)
      
      else if (input$compStat1 == "assists" &&
               input$compStat2 == "blocks")
        plot_ly(
          result, x = assists, y = blocks, mode = "markers", color = assists, size = blocks
        ) %>% layout(title = title)
      
      else if (input$compStat1 == "assists" &&
               input$compStat2 == "rebounds")
        plot_ly(
          result, x = assists, y = rebounds, mode = "markers", color = assists, size = rebounds
        ) %>% layout(title = title)
      
      else if (input$compStat1 == "steals" &&
               input$compStat2 == "PPG")
        plot_ly(
          result, x = steals, y = PPG, mode = "markers", color = steals, size = PPG
        ) %>% layout(title = title)
      
      else if (input$compStat1 == "steals" &&
               input$compStat2 == "assists")
        plot_ly(
          result, x = steals, y = assists, mode = "markers", color = steals, size = assists
        ) %>% layout(title = title)
      
      else if (input$compStat1 == "steals" &&
               input$compStat2 == "steals")
        plot_ly(
          result, x = steals, y = steals, mode = "markers", color = steals, size = steals
        ) %>% layout(title = title)
      
      else if (input$compStat1 == "steals" &&
               input$compStat2 == "blocks")
        plot_ly(
          result, x = steals, y = blocks, mode = "markers", color = steals, size = blocks
        ) %>% layout(title = title)
      
      else if (input$compStat1 == "steals" &&
               input$compStat2 == "rebounds")
        plot_ly(
          result, x = steals, y = rebounds, mode = "markers", color = steals, size = rebounds
        ) %>% layout(title = title)
      
      else if (input$compStat1 == "blocks" &&
               input$compStat2 == "PPG")
        plot_ly(
          result, x = blocks, y = PPG, mode = "markers", color = blocks, size = PPG
        ) %>% layout(title = title)
      
      else if (input$compStat1 == "blocks" &&
               input$compStat2 == "assists")
        plot_ly(
          result, x = blocks, y = assists, mode = "markers", color = blocks, size = assists
        ) %>% layout(title = title)
      
      else if (input$compStat1 == "blocks" &&
               input$compStat2 == "steals")
        plot_ly(
          result, x = blocks, y = steals, mode = "markers", color = blocks, size = steals
        ) %>% layout(title = title)
      
      else if (input$compStat1 == "blocks" &&
               input$compStat2 == "blocks")
        plot_ly(
          result, x = blocks, y = blocks, mode = "markers", color = blocks, size = blocks
        ) %>% layout(title = title)
      
      else if (input$compStat1 == "blocks" &&
               input$compStat2 == "rebounds")
        plot_ly(
          result, x = blocks, y = rebounds, mode = "markers", color = blocks, size = rebounds
        ) %>% layout(title = title)
      
      else if (input$compStat1 == "rebounds" &&
               input$compStat2 == "PPG")
        plot_ly(
          result, x = rebounds, y = PPG, mode = "markers", color = rebounds, size = PPG
        ) %>% layout(title = title)
      
      else if (input$compStat1 == "rebounds" &&
               input$compStat2 == "assists")
        plot_ly(
          result, x = rebounds, y = assists, mode = "markers", color = rebounds, size = assists
        ) %>% layout(title = title)
      
      else if (input$compStat1 == "rebounds" &&
               input$compStat2 == "steals")
        plot_ly(
          result, x = rebounds, y = steals, mode = "markers", color = rebounds, size = steals
        ) %>% layout(title = title)
      
      else if (input$compStat1 == "rebounds" &&
               input$compStat2 == "blocks")
        plot_ly(
          result, x = rebounds, y = blocks, mode = "markers", color = rebounds, size = blocks
        ) %>% layout(title = title)
      
      else if (input$compStat1 == "rebounds" &&
               input$compStat2 == "rebounds")
        plot_ly(
          result, x = rebounds, y = rebounds, mode = "markers", color = rebounds, size = rebounds
        ) %>% layout(title = title)
    })
  })
  
  output$generateResults <- renderDataTable({
    input$resultsUpdateButton
    isolate(if (input$myfunction == "All Statistics") {
      output$message <- renderText({
        paste("Function: All statistics are shown!")
      })
      allstatistics()
    }
    else if (input$myfunction == "Top 10 Players") {
      output$message <- renderText({
        paste("Function: Top 10 players are shown!")
      })
      top10players()
    }
    else if (input$myfunction == "Most Similar Players") {
      output$message <- renderText({
        paste("Function: Most similar players are shown!")
      })
      similarPlayers()
    }
    else if (input$myfunction == "Statistics Prediction") {
      output$message <- renderText({
        paste("Function: Statistics prediction are shown!")
      })
      statisticsPrediction()
    }
    else if (input$myfunction == "Account and Mock Teams") {
      if (input$userFunction == "Show Players of User") {
        output$message <- renderText({
          paste("Function: User's players are shown!")
        })
        showMockTeamPlayer()
      }
    }
    else if (input$myfunction == "Team Player Stat Counter") {
      output$message <- renderText({
        paste("Function: Count of teams are shown!")
      })
      teamPlayerStatCount()
    })
  }, options = list(scrollX = TRUE, pageLength = 10))
  
  })
