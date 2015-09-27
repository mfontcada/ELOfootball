#-----------------------------------------------------------------------------#
# Football Rankings
# The World Football Elo Rating System
# Source: http://www.eloratings.net/system.html
#-----------------------------------------------------------------------------#

# ELOgame ---------------------------------------------------------------------
#' Team ratings
#' 
#' Given a previous ranking and a game information, the \function{ELOgame}
#' function computes the resultant ELO ranking for both teams.
#' 
#' @details The data frame with the ranking needs to have the followin columns:
#' Team
#' Rating
#' Date
#' 
#' The data.frame with the game information needs to have the
#' following columns:
#' Season
#' Date
#' HomeTeam
#' AwayTeam
#' FTHG Full Time Home Goals
#' FTAG Full Time Away Goals
#' FTR Full Time Result
#' 
#' @param ranking data.frame with previous ranking.
#' @param game data.frame with the game information.
#' 
#' @result A list with the resultant ELO information for both teams.
#' 
#' @export

ELOgame <- function(ranking, game) {
  
  # Home and Away Team
  home.team <- game$HomeTeam; home.team
  away.team <- game$AwayTeam; away.team
  # Previous ranking
  home.Ro <- ranking$Rating[ranking$Team %in% home.team]
  away.Ro <- ranking$Rating[ranking$Team %in% away.team]
  # Importance of competition
  K <- 20
  # Result
  if (game$FTR == "H") {
    home.W <- 1
    away.W <- 0
  } else if (game$FTR == "D") {
    home.W <- 0.5
    away.W <- 0.5
  } else {
    home.W <- 0
    away.W <- 1
  }
  # Goal difference
  N <- abs(game$FTHG - game$FTAG)
  # Difference in ratings
  home.dr <- home.Ro - away.Ro
  away.dr <- away.Ro - home.Ro
  # Home advantage
  H <- 100
  
  # Compute ELO
  home.ELO <- ELOrating(home.Ro, K, W = home.W, N, dr = home.dr, H); home.ELO
  away.ELO <- ELOrating(away.Ro, K, W = away.W, N, dr = away.dr, -H); away.ELO
  
  ELOresult <- list(); ELOresult
  ELOresult[[home.team]] <- home.ELO; ELOresult
  ELOresult[[away.team]] <- away.ELO; ELOresult
  
  return(ELOresult)
  
}