ELOteams <- function (results) {
  
  teams.ratings <- list()
  
  i <- 1
  
  # Extract team names
  teams <- c(results[i, "HomeTeam"], results[i, "AwayTeam"]); teams
  
  team <- 'Man City'
  results <- sTable
  
  # Select only team results
  team.results <- results[results$HomeTeam %in% team | results$AwayTeam %in% team, ]; team.results
  
  # Init ranking
  team.ranking <- data.frame("Team" = team, "Rating" = 1500, "Date" = "2000-08-01"); team.ranking
  
  # New ranking
  new.ratings <- ELOgame(team.ranking, team.results[i, ]); new.ratings
  
}