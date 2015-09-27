#-----------------------------------------------------------------------------#
# Football Rankings
# The World Football Elo Rating System
# Source: http://www.eloratings.net/system.html
#-----------------------------------------------------------------------------#

# ELOrating -------------------------------------------------------------------
#' Team rating
#' 
#' Computes the rating of one team in a match with the World Football Elo
#' Rating System.
#' 
#' @details The rating is based on the following formulas:
#' 
#' \deqn{R_n = R_o + K \times (W - W_e)}
#' 
#' \eqn{R_n} is the new rating.
#' \eqn{R_o} is the old (pre-match) rating.
#' \eqn{K} is the weight constant for the tournament played (national leagues 
#' = 20). K is adjusted for the goal difference in the game. It is increased by
#' \eqn{1/2} if a game is won by two goals, by \eqn{3/4} if a game is won by 
#' three goals, and by \eqn{3/4 + (N-3)/8} if the game is won by four or more
#' goals, where \eqn{N} is the goal difference.
#' \eqn{W} is the result of the game (1 for a win, 0.5 for a draw, and 0 for a
#' loss).
#' \eqn{W_e} is the expected result (win expectancy), either from the chart or
#' the following formula: \eqn{W_e = 1 / (10^{-dr/400} + 1)}
#' \eqn{dr} equals the difference in ratings plus 100 points for a team playing
#'  at home.
#'  
#'  @param Ro numeric indicating the old (pre-match) rating.
#'  @param K numeric indicating the weight constant for the tournament played.
#'  @param N numeric indicating the goal difference in the game.
#'  @param W numeric indicating the result of the game (1 = win, 0.5 = draw,
#'  0 = loss).
#'  @param dr numeric indicating the difference in ratings.
#'  @param H numeric indicating the plus points for play Home or Away.
#'  
#'  @result We (win expectancy), P (ELO points earned), Rn (the new rating).
#'  
#'  @references The World Football Elo Rating System: 
#'  http://www.eloratings.net/system.html
#'  
#'  @export

ELOrating <- function(Ro, K, W, N, dr, H) {
 
  # Adjust K in function of the goal difference
  if (N == 2) {
    K <- K + K * 1/2
  } else if (N == 3) {
    K <- K + K * 3/4
  } else if (N >= 4) {
    K <- K + K * (3/4 + (N - 3)/8)
  }
  
  # Augment dr
  dr <- dr + H
  
  # Compute We (win expectancy)
  We <- 1 / (10^(-dr/400) + 1)
  
  # Compute new ELO rating
  Rn <- Ro + K * (W - We)
  
  # Points earned
  P <- Rn - Ro
  
  # Return new ELO rating and aditional information
  return(list(We = We, P = P, Rn = Rn))

}