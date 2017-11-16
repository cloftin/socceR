
#' @export
get_historical_data <- function() {
  base <- "http://www.football-data.co.uk/mmz4281/"

  countries <- c("England", "Scotland", "Germany", "Italy", "Spain", "France", "Netherlands")
  suffixes <- list(c("E0", "E1", "E2", "E3"),  c("SC0"), c("D1"), c("I1"), c("SP1"), c("F1"), c("E1"))
  leagueNames <- list(c("Premier League", "Championship", "League One", "League Two"),
                      c("Premier League"), c("Bundesliga One"), c("Serie A", "Serie B"),
                      c("La Liga Primera"), c("Le Championnat"), c("Eredivisie"))

  for (j in 1:length(countries)) {
    dat <- data.frame()

    for(i in 2017:2008) {
      print(i)
      leagues <- list()

      for(k in 1:length(suffixes[[j]])) {
        leagues[[k]] <- read.csv(file = paste0(base, substr(i - 1, 3, 4), substr(i, 3, 4), "/", suffixes[[j]][k], ".csv"))
      }

      leagues <- lapply(leagues, function(x) {
        x$Date <- as.Date(x$Date, "%d/%m/%y")
        for(a in 1:length(suffixes[[j]])) {
          x$Div <- gsub(suffixes[[j]][a], leagueNames[[j]][a], x$Div)
        }
        return(x)
      })

      homecols <- c("GID", "Div", "Date", "HomeTeam", "FTHG", "FTR", "HTHG", "HTR", "HS", "HST",
                    "HC", "HF", "HY", "HR")
      awaycols <- c("GID", "Div", "Date", "AwayTeam", "FTAG", "FTR", "HTAG", "HTR", "AS", "AST",
                    "AC", "AF", "AY", "AR")
      columns <- c("GID", "Div", "Date", "Team", "FTGoals", "FTResult", "HTGoals", "HTResult",
                   "Shots", "ShotsOnT", "Corners", "FoulsCom",
                   "Yellows", "Reds", "Home", "SecondGoals")

      dat <- rbind(dat, plyr::ldply(leagues, function(x) {
        x$GID <- c(1:nrow(x))
        # print(head(x))
        # print(homecols[!(homecols %in% colnames(x))])
        home <- x[, homecols]
        home$Home <- 1
        home$SHG <- home$FTHG - home$HTHG
        home$FTR <- gsub("H", 1, home$FTR)
        home$FTR <- gsub("A", 0, home$FTR)
        home$FTR <- gsub("D", -1, home$FTR)
        home$HTR <- gsub("H", 1, home$HTR)
        home$HTR <- gsub("A", 0, home$HTR)
        home$HTR <- gsub("D", -1, home$HTR)

        away <- x[, awaycols]
        away$Home <- 0
        away$SHG <- away$FTAG - away$HTAG
        away$FTR <- gsub("H", 0, away$FTR)
        away$FTR <- gsub("A", 1, away$FTR)
        away$FTR <- gsub("D", -1, away$FTR)
        away$HTR <- gsub("H", 0, away$HTR)
        away$HTR <- gsub("A", 1, away$HTR)
        away$HTR <- gsub("D", -1, away$HTR)

        colnames(home) <- columns
        colnames(away) <- columns
        return <- rbind(home, away)
        return$Season <- paste((i-1), i, sep = "-")
        return <- return %>% .[order(.$GID, .$Home),] %>% select("GID", "Div", "Season", "Date", "Team", "Home",
                                                                 "FTGoals", "FTResult", "HTGoals", "HTResult",
                                                                 "SecondGoals", "Shots", "ShotsOnT", "Corners",
                                                                 "FoulsCom", "Yellows", "Reds")
        rownames(return) <- c(1:nrow(return))
        return(return)
      }))

    }

    dat$Date <- as.character(dat$Date)
    cn <- DBI::dbConnect(RSQLite::SQLite(), "data/Soccer.sqlite3")

    tryCatch({dbGetQuery(cn, paste0("Drop Table ", countries[j], "Games"))}, error = function(e) {e})
    DBI::dbWriteTable(cn, paste0(countries[j], "Games"), dat)
  }
}
