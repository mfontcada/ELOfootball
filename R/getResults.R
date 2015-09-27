#-----------------------------------------------------------------------------#
# Football Rankings
# Data web scrapper
# BD: http://www.football-data.co.uk
#-----------------------------------------------------------------------------#

# getResults ------------------------------------------------------------------
getResults <- function(league, seasons) {

  # Get data ----
  # Prepare data folder
  if (!file.exists("data")) {
    dir.create("data")
  }
  # Prepare league folder
  if (!file.exists(paste0("data/", league))) {
    dir.create(paste0("data/", league))
  }
  # Download csv for each season
  for (i in seq_along(seasons)) {
    if (!file.exists(paste0("data/", league, "/s", seasons[i], ".csv"))) {
      sURL <- paste0("http://www.football-data.co.uk/mmz4281/", seasons[i], "/", league, ".csv"); sURL
      download.file(sURL, destfile = paste0("data/", league, "/s", seasons[i], ".csv"))
    }
  }

  # Read data ----
  # Create one list with all the seasons
  sList <- list(); sList
  for (i in seq_along(seasons)) {
    sActual <- read.csv(paste0("data/", league, "/s", seasons[i], ".csv"))
    sList[[seasons[i]]] <- sActual
  }
  rm(sActual)
  
  # Clean data ----
  # Columns to extract
    # Date = Match Date (dd/mm/yy)
    # HomeTeam = Home Team
    # AwayTeam = Away Team
    # FTHG = Full Time Home Team Goals
    # FTAG = Full Time Away Team Goals
    # FTR = Full Time Result (H=Home Win, D=Draw, A=Away Win)
    sCol <- c("Date", "HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR"); sCol
  # Reduced tables with only selected columns
  for (i in seq_along(sList)) {
    sList[[seasons[i]]] <- sList[[seasons[i]]][, which(names(sList[[seasons[i]]]) %in% sCol)]
  }

  # Remove empty or NA rows
  for (i in seq_along(sList)) {
    sList[[seasons[i]]] <- sList[[seasons[i]]][!(rowSums(is.na(sList[[seasons[i]]])) > 0), ]
  }

  # Merge tables and add season column
  sTable <- c()
  for (i in seq_along(sList)) {
    sTable <- rbind(sTable, cbind(Season = seasons[i], sList[[seasons[i]]]))
  }

  # Format columns
  sTable$Season <- as.character(sTable$Season)
  sTable$Date <- as.Date(sTable$Date, "%d/%m/%y")
  sTable$HomeTeam <- as.character(sTable$HomeTeam)
  sTable$AwayTeam <- as.character(sTable$AwayTeam)
  sTable$FTR <- as.character(sTable$FTR)

  # Reset rownames
  rownames(sTable) <- NULL
  
  # Return table results
  return(sTable)

}
#-----------------------------------------------------------------------------#
