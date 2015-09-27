#-----------------------------------------------------------------------------#
# Football Rankings
# The World Football Elo Rating System
# Source: http://www.eloratings.net/system.html
#-----------------------------------------------------------------------------#

# ELOpredict ------------------------------------------------------------------
#' Predict match result
#' 
#' Given a previous ranking and two teams, the \function{ELOpredict} function 
#' tries to predict the result of a match.
#' 
#' @details The data frame with the ranking needs to have the followin columns:
#' Team
#' Rating
#' Date
#' 
#' @param ranking data.frame with previous ranking.
#' @param home.team character with the home team.
#' @param home.team character with the away team.
#' @param draw.prob the range of probabilities to consider for draws.
#' 
#' @result A list with the winning expectancy for both teams and the result
#' prediction.
#' 
#' @export

ELOpredict <- function(ranking, home.team, away.team, draw.prob) {
  
  # Check if both teams are in the ranking, if not apply default rating (1500)
  # Home Team
  if (home.team %in% ranking$Team) {
    # Previous ranking
    home.Ro <- ranking$Rating[ranking$Team %in% home.team]
  } else {
    # Default ranking
    home.Ro <- 1000
  }
  # Away Team
  if (away.team %in% ranking$Team) {
    # Previous ranking
    away.Ro <- ranking$Rating[ranking$Team %in% away.team]
  } else {
    # Default ranking
    away.Ro <- 1000
  }

  # Difference in ratings
  home.dr <- home.Ro - away.Ro
  away.dr <- away.Ro - home.Ro
  # Home advantage
  H <- 100
  # Augment dr
  home.dr <- home.dr + H
  away.dr <- away.dr - H
  
  # Compute We (win expectancy)
  home.We <- 1 / (10^(-home.dr/400) + 1)
  away.We <- 1 / (10^(-away.dr/400) + 1)
  
  # Predicted result
  if (home.We - away.We > draw.prob) {
    game.pred <- "H"
  } else if (abs(home.We - away.We) < draw.prob) {
    game.pred <- "D"
  } else if (home.We - away.We < -draw.prob) {
    game.pred <- "A"
  }
  
  # Draw prediction
  if (max(home.We, away.We) == home.We) {
    draw <- away.We
    away.We <- away.We / 2
    home.We <- 1 - draw - away.We
  } else {
    draw <- home.We
    home.We <- home.We / 2
    away.We <- 1 - draw - home.We
  }
  
  ELOprediction <- list(); ELOprediction
  ELOprediction[[home.team]] <- home.We; ELOprediction
  ELOprediction[[away.team]] <- away.We; ELOprediction
  ELOprediction$draw <- draw; ELOprediction
  ELOprediction[["Result"]] <- game.pred
  
  return(ELOprediction)
  
}