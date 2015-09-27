#-----------------------------------------------------------------------------#
# Cousera - Data Science Specialization
# Developing Data Products - Course Project
# Manuel Fontenla Cadavid
#-----------------------------------------------------------------------------#

library(shiny)

# Load data
load("data/E0/updated.rda")
load("data/E0/ranking.rda")
load("data/E0/teams.rda")
source("R/ELOgame.R")
source("R/ELOpredict.R")
source("R/ELOranking.R")
source("R/ELOrating.R")

shinyServer(function(session, input, output) {
  
  output$updatedDate <- renderText({
    paste0("Last update: ", updated)
  })
  
  team.names <- c("Arsenal", "Aston Villa", "Bournemouth", "Chelsea",
                  "Crystal Palace", "Everton", "Leicester", "Liverpool",
                  "Man City", "Man United", "Newcastle", "Norwich",
                  "Southampton", "Stoke", "Sunderland", "Swansea",
                  "Tottenham", "Watford", "West Brom", "West Ham")
  updateSelectInput(session, inputId = "homeTeam", choices = team.names)
  observe({
    team.names <- team.names[-which(team.names %in% input$homeTeam)]
    updateSelectInput(session, inputId = "awayTeam", choices = team.names)
  })
  
  matchPred <- reactive({
    ELOpredict(ranking, input$homeTeam, input$awayTeam, draw.prob = 0.2)
  })
  
  output$homeELO <- renderText({
    home.team <- input$homeTeam
    home.team.ELO <- tail(teams[[home.team]], 1)$Rating
    paste("<h3 style='text-align: center'>ELO Rating:", home.team.ELO, "</h3><br>")
  })
  
  output$awayELO <- renderText({
    away.team <- input$awayTeam
    away.team.ELO <- tail(teams[[away.team]], 1)$Rating
    paste("<h3 style='text-align: center'>ELO Rating:", away.team.ELO, "</h3><br>")
  })
  
  output$plotWe <- renderPlot({
    
    match.We <- matchPred()
    
    # Check if both teams are selected
    if (!"Team" %in% names(match.We)) {
    
      p <- c(match.We[[1]], match.We[[3]], match.We[[2]])
      
      # Base plot
      plot(c(0, 100), c(0, 3), type = "n", xlab = "", ylab = "", cex.main = 2,
           xaxt = "n", yaxt = "n", bty = "n", main = "Result probabilities")
      # Left rect
      rect(0, 0.1, p[1]*100, 0.9, col = "blue")
      # Left legend
      legend(0, 3, paste("H:", round(p[1]*100, 2), "%"), cex = 2, xjust = 0,
             bty = "n", x.intersp = 0)
      # Center rect
      rect(p[1]*100, 0.1, 100 - p[3]*100, 0.9, col = "green")
      # Right legend
      legend(65, 3, paste("D:", round(p[2]*100, 2), "%"), cex = 2, xjust = 1,
             bty = "n", x.intersp = 0)
      # Right rect
      rect((p[1] + p[2])*100, 0.1, 100, 0.9, col = "red")
      # Right legend
      legend(100, 3, paste("A:", round(p[3]*100, 2), "%"), cex = 2, xjust = 1,
             bty = "n", x.intersp = 0)
      
    }
  
  })
  
  # Teams History
  output$plotRatings <- renderPlot({
    
    home.team <- input$homeTeam
    home.team.history <- teams[[home.team]]
    away.team <- input$awayTeam
    away.team.history <- teams[[away.team]]
    
    # Base plot
    plot(x = as.Date(c("2000-08-19", "2016-06-30")), y = c(1200, 2000), type = "n",
         ylab = "ELO Rating", xlab = "Date", main = "ELO Rating History", cex.main = 2)
    lines(x = home.team.history$Date, y = home.team.history$Rating, col = "blue", lwd = 2)
    lines(x = away.team.history$Date, y = away.team.history$Rating, col = "red", lwd = 2)
    legend("bottom", legend = c(home.team, away.team), col = c("blue", "red"), lty = 1, bty = "n")
    
  })

})
