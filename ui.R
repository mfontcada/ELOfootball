
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("ELO Football Rating - Premier League 2015/2016"),

  sidebarLayout(
    
    # Help text
    sidebarPanel(
      htmlOutput("updatedDate"),
      HTML('<br/><p>This application uses a simple implementation of the World
                Football Elo Rating System to rank the teams of the Premier
                League, from season 2000/2001 to 2015/2016, and compute the
                expectancies for two selected teams in a given match.</p>

                <p>The user could select a home team and an away team and see
                its ELO ratings compared and the computed probabilities for each
                possible result of a match between them:</p>

                <li>
                  <ul>"H": Home Win</ul>
                </li>
                <li>
                  <ul>"D": Draw</ul>
                </li>
                <li>
                  <ul>"H": Away Win</ul>
                </li>

                <p>You could learn more about the World Football Elo Rating System
                in <a href="http://www.eloratings.net/">ELOratings.net</a> or in its
                <a href="https://en.wikipedia.org/wiki/World_Football_Elo_Ratings">Wikipedia page</a>.</p>
               
                <p>This is a course project for the Coursera Data Science Specialization
                course: <a href="https://www.coursera.org/course/devdataprod">Developing Data Products</a>.
                <br>Do not expect high accuracy in the predictions.</p>
           
                <p>Results and data from previous matchs was obtained from
                <a href="http://football-data.co.uk">football-data.co.uk</a></p>')
    ),
    
    mainPanel(
      # Select teams
      fluidRow(
        column(4,
          selectInput("homeTeam", label = "Home Team", choices = list("Team" = "Team")),
          htmlOutput("homeELO")
        ),
        column(4,
          selectInput("awayTeam", label = "Away Team", choices = list("Team" = "Team")),
          htmlOutput("awayELO")
        ),
        
        # Plot
        column(8, offset = 0,
               
          plotOutput("plotWe", height = 250),
          plotOutput("plotRatings")
               
        )
      
      )
    )
  
  )
  
))
