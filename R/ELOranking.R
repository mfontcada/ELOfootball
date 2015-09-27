#-----------------------------------------------------------------------------#
# Football Rankings
# The World Football Elo Rating System
# Source: http://www.eloratings.net/system.html
#-----------------------------------------------------------------------------#

# ELOranking ------------------------------------------------------------------
#' Team ranking
#' 
#' Given a list of results, the \function{ELOranking} function computes the
#' team ranking.
#' 
#' @details The data.frame with the results needs to have the following
#' columns:
#' Season
#' Date
#' HomeTeam
#' AwayTeam
#' FTHG Full Time Home Goals
#' FTAG Full Time Away Goals
#' FTR Full Time Result
#' 
#' @param results data.frame with results.
#' @param ranking data.frame with previous ranking, is NULL by default
#' 
#' @result ranking data.frame with the team ranking
#' 
#' @export

ELOranking <- function(ranking = NULL, results) {
  
  # Check if there is previous ranking
  if (is.null(ranking)) {
    ranking <- data.frame()
  } else {
    prev.ranking <- ranking
  }
  
  # Team ratings
  team.ratings <- list()
  
  for (i in 1:nrow(results)) {
    
    # Extract team names
    teams <- c(results[i, "HomeTeam"], results[i, "AwayTeam"]); teams
    
    # Check if both teams are in the ranking, if not add it with default rating
    if (!teams[1] %in% ranking$Team) {
      ranking <- rbind(ranking, data.frame(Team = teams[1], Rating = 1500, Date = results[i, "Date"]))
    }
    if (!teams[2] %in% ranking$Team) {
      ranking <- rbind(ranking, data.frame(Team = teams[2], Rating = 1500, Date = results[i, "Date"]))
    }
    
    # Compute new ratings for game i
    new.ratings <- ELOgame(ranking, results[i, ]); new.ratings
    
    # Update team ratings and dates
    ranking[ranking$Team %in% teams[1], "Rating"] <- round(new.ratings[[teams[1]]]$Rn)
    ranking[ranking$Team %in% teams[1], "Date"] <- results[i, ]$Date
    ranking[ranking$Team %in% teams[2], "Rating"] <- round(new.ratings[[teams[2]]]$Rn)
    ranking[ranking$Team %in% teams[2], "Date"] <- results[i, ]$Date
    
    # Update team ratings
    team1.rating <- data.frame("Rating" = round(new.ratings[[teams[1]]]$Rn), "Date" = results[i, ]$Date)
    team.ratings[[teams[1]]] <- rbind(team.ratings[[teams[1]]], team1.rating)
    team.ratings
    team2.rating <- data.frame("Rating" = round(new.ratings[[teams[2]]]$Rn), "Date" = results[i, ]$Date)
    team.ratings[[teams[2]]] <- rbind(team.ratings[[teams[2]]], team2.rating)
    team.ratings
    
    # Save update date
    updated <- results[i, ]$Date
    
  }
  
  return(list(ranking = ranking, updated = updated, teams = team.ratings))
  
}