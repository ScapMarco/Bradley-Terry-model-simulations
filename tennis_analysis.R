# Remove previous sessions
rm(list=ls())

# Load libraries
usePackage <- function(p){
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

# Load the required libraries
Packages <- c("xml2", "rvest","data.table","lubridate", "plyr", "BradleyTerry2", "readxl", "DescTools", "gtools", "Matrix", "qvcalc", "stringr")
for (i in 1:length(Packages)){
  usePackage(Packages[i])
}
rm(i,Packages,usePackage)

# Import all CSV files from path/dir and create a single dataframe
take_data <- function(path, dir) {
  final_path <- paste(path, dir, sep = "/")
  final_path <- paste(final_path, "/", sep = "")
  temp <- list.files(path = final_path, pattern = c("*.xls", "*.xlsx")) 
  myfiles <- lapply(paste(final_path, temp, sep = "/"), read_excel)
  df <- rbind.fill(myfiles)
  rm(temp, myfiles)
  return(df)
}

# Return a contingency matrix from a dataframe with columns: winner, loser
make_cont_matrix <- function(data) {

  winner <- data$Winner
  loser <- data$Loser
  players <- sort(union(winner, loser))
  nplayers <- length(players)
  winnum <- match(winner, players)
  losernum <- match(loser, players)

  matches <- mapply(c, winner, loser, SIMPLIFY = FALSE)
  score <- laply(.data = matches, .fun = function(x) {sum( (data$Winner == as.character(unlist(x)[1]))  & (data$Loser == as.character(unlist(x)[2]) )) })

  mat <- Matrix(0, nrow = nplayers, ncol = nplayers)
  mat <- replace(mat, cbind(winnum, losernum), score)

  cont_matrix <- data.frame(as.matrix(mat))
  colnames(cont_matrix) <- players
  row.names(cont_matrix) <- players

 return(cont_matrix)
}

# The same as countsToBinomial (Bradley Terry package) but faster and works with larger matrices
my_countsToBinomial <- function (xtab)  {
  players <- rownames(xtab)
  options(expressions=1e5)
  comb <- CombSet(nrow(xtab), 2)
  options(expressions=1e5)
  won <- xtab[comb]
  lost <- t(xtab)[comb]
  res <- !(won == 0 & lost == 0)
  player1 <- factor(players[comb[, 1]], levels = players)[res]
  player2 <- factor(players[comb[, 2]], levels = players)[res]
  data.frame(player1, player2, win1 = won[res], win2 = lost[res])
}

### Centipede plot for quasi-variance intervals
centipedePlot <- function(names, BradT_Model.qv, vcov_abilities, year) {

  # Plot
  estimated <- BradT_Model.qv$qvframe$estimate
  quasiSE <- BradT_Model.qv$qvframe$quasiSE
  
  players_interval <- data.frame(cbind(estimated, quasiSE), row.names = names)
  players_interval <- players_interval[order(players_interval$estimated), ]
  
  x <- 1:length(players_interval$estimated)
  
  # Selected years 
  title_year <- year
  if (length(year) > 1) {
    title_year <- paste(year[1], year[length(year)], sep = "-")
  }
  
  title <- paste("Quasi-confidence intervals", title_year, sep=" ")
  mini <- min(estimated)
  maxi <- max(estimated)
  plot(x = x, y = players_interval$estimated, ylim = c(-4, 6.5), xaxt="n", xlab = '', ylab = 'Estimated ability', main= title)
  xtick <- 1:nrow(players_interval)
  axis(side = 1, at = xtick, labels = FALSE)
  text(x=xtick, y = -6, labels=row.names(players_interval), srt=45, xpd=TRUE)
  
  segments(x, players_interval$estimated-qnorm(p = 1-0.05/2)*players_interval$quasiSE, x, players_interval$estimated+qnorm(p = 1-0.05/2)*players_interval$quasiSE)
  epsilon <- 0.05
  segments(x-epsilon, players_interval$estimated-qnorm(p = 1-0.05/2)*players_interval$quasiSE, x+epsilon, players_interval$estimated-qnorm(p = 1-0.05/2)*players_interval$quasiSE)
  segments(x-epsilon, players_interval$estimated+qnorm(p = 1-0.05/2)*players_interval$quasiSE, x+epsilon, players_interval$estimated+qnorm(p = 1-0.05/2)*players_interval$quasiSE)
}


quasiVEstimate <- function(tennis_data) {
  # Creation of contingency matrix
  cont_matrix <- make_cont_matrix(tennis_data)
  
  # Creation of contingency table
  match_bin_freq <- my_countsToBinomial(cont_matrix)
  
  # Model estimation
  BradT_Model <- BTm(outcome = cbind(match_bin_freq$win1, match_bin_freq$win2), 
                     player1 = player1, player2 = player2, 
                     formula = ~ player_, id = "player_", 
                     data = match_bin_freq,  br = TRUE)
  
  # Remove player with the lowest ability
  abilities <- data.frame(BTabilities(BradT_Model))
  last_player <- row.names(abilities[order(abilities$ability, decreasing = FALSE),][1,])
  BradT_Model <- update(BradT_Model, refcat = last_player)
    
  # Top 20 players in the model
  abilities <- data.frame(BTabilities(BradT_Model))
  interesting_players <- abilities[order(abilities$ability, decreasing = TRUE),][1:20,]
  year <- unique(tennis_data$year)
  
  # Set of players considered
  names <- row.names(interesting_players)
  # Calculate quasi-variances
  vcov_abilities <- attr(BTabilities(BradT_Model), 'vcov')[names, names]
  BradT_Model.qv <- qvcalc(vcov_abilities, labels = names, estimates = interesting_players[,1])
  #saveRDS(BradT_Model.qv, file = paste(path, "BradT_Modelqv.rds", sep = "/"))
  print(names)
  centipedePlot(names, BradT_Model.qv, vcov_abilities, year)
}

####################### 
# Setting parameters #
#######################

path <- "" # Insert here the Path for this project
data_dir <- "Data" # Path for Tennis Data

####################### 
# Analysis #
#######################

# Take all data
tennis_data <- take_data(path, data_dir)

# Subset data using only 4 Grand Slam tournaments
tennis_data <- tennis_data[tennis_data$Series == "Grand Slam", ]
tennis_data$Series <- NULL

# The 4 Grand Slam tournaments are: 
unique(tennis_data$Tournament)
unique(tennis_data$Location)

# Check anomalous data
sum(is.na(tennis_data$LRank))
sum(tennis_data$Comment != 'Completed')
unique(tennis_data$Comment)

# Clean dataset
tennis_data <- tennis_data[!is.na(tennis_data$LRank) & !is.na(tennis_data$WRank), ]
tennis_data <- tennis_data[tennis_data$Comment == "Completed", ] 
tennis_data$Comment <- NULL

# Remove Betting columns
tennis_data <- tennis_data[,1:24]

# Remove Number of games won by match winner and by match loser 
tennis_data <- tennis_data[, 1:12]

# Split by year
year <- str_split_fixed(tennis_data$Date, "-", 2)[,1]
tennis_data$year <- year
tennis_data$Date <- NULL


############### END Cleaning Dataset ##################

#######################################################
#### Analysis by year without additional weights
  
# Estimate by year
estimate <- function(y) {
  yearTennisData <- tennis_data[tennis_data$year %in% y,]
  players <- union(yearTennisData$Winner, yearTennisData$Loser)
  for (i in players) {
    if (sum(yearTennisData$Winner == i) + sum(yearTennisData$Loser == i) < 10) {
      yearTennisData <- yearTennisData[!yearTennisData$Winner == i,]
      yearTennisData <- yearTennisData[!yearTennisData$Loser == i,]
    }
  }
  quasiVEstimate(yearTennisData)
}

# Estimation from year to year
estimate(paste(2000:2010))
estimate(paste(2010:2020))
estimate(paste(2000:2020))

# Read the saved estimation
BradT_Model <- readRDS(file = paste(path, "BTModel.rds", sep = "/"))
BradT_Model.qv <- readRDS(file = paste(path, "BradT_Modelqv.rds", sep = "/"))

# Subset interesting players
players <- c("Djokovic N.", "Nadal R.", "Federer R.")
# Abilities of the players
BTabilities(BradT_Model)[players,]
# Quasi-variance of the players
attr(BTabilities(BradT_Model), 'vcov')[players, players]


##################################
#### Analysis with additional weights

# All types of rounds
unique(tennis_data$Round)

# Weights associated with the matches
tennis_data$weight <-  with(tennis_data, ifelse(Round == "Quarterfinals", 1/4 , 1))
tennis_data$weight <-  with(tennis_data, ifelse(Round == "Semifinals", 1/2, weight))
tennis_data$weight <-  with(tennis_data, ifelse(Round == "The Final", 1, weight))
tennis_data$weight <-  with(tennis_data, ifelse(Round == "1st Round", 1/64, weight))
tennis_data$weight <-  with(tennis_data, ifelse(Round == "2nd Round", 1/32, weight))
tennis_data$weight <-  with(tennis_data, ifelse(Round == "3rd Round", 1/16, weight))
tennis_data$weight <-  with(tennis_data, ifelse(Round == "4th Round", 1/8, weight))

quasiVEstimate <- function(tennis_data) {
  # Creation of contingency matrix
  cont_matrix <- make_cont_matrix(tennis_data)
  # Creation of contingency table
  match_bin_freq <- my_countsToBinomial(cont_matrix)
  
  get_weight <- function(row) {
    p1 <- row[1]
    p2 <- row[2] 
    tot_weights <- sum(tennis_data[tennis_data$Winner == p1 & tennis_data$Loser == p2,]$weight)
    tot_weights <- tot_weights + sum(tennis_data[tennis_data$Winner == p2 & tennis_data$Loser == p1,]$weight)
    return(tot_weights)
  }
  
  match_bin_freq$weights <- apply(match_bin_freq, 1, FUN = get_weight)
  
  # Model estimation
  BradT_Model <- BTm(outcome = cbind(match_bin_freq$win1, match_bin_freq$win2), 
                     player1 = player1, player2 = player2, weights = match_bin_freq$weights,
                     formula = ~ player_, id = "player_", 
                     data = match_bin_freq,  br = TRUE)
  
  # Remove player with the lowest ability
  abilities <- data.frame(BTabilities(BradT_Model))
  last_player <- row.names(abilities[order(abilities$ability, decreasing = FALSE),][1,])
  BradT_Model <- update(BradT_Model, refcat = last_player)
  
  # Save the model
  saveRDS(BradT_Model, file = paste(path, "BTModel.rds", sep = "/"))
  
  # Top 20 players in the model
  abilities <- data.frame(BTabilities(BradT_Model))
  interesting_players <- abilities[order(abilities$ability, decreasing = TRUE),][1:20,]
  year <- unique(tennis_data$year)
  
  # Set of players considered
  names <- row.names(interesting_players)
  # Calculate quasi-variances
  vcov_abilities <- attr(BTabilities(BradT_Model), 'vcov')[names, names]
  BradT_Model.qv <- qvcalc(vcov_abilities, labels = names, estimates = interesting_players[,1])
  print(names)
  
  # Save the quasi-variance
  saveRDS(BradT_Model.qv, file = paste(path, "BradT_Modelqv.rds", sep = "/"))
  
  centipedePlot(names, BradT_Model.qv, vcov_abilities, year)
}

# Estimate by year
estimate <- function(y) {
  yearTennisData <- tennis_data[tennis_data$year %in% y,]
  players <- union(yearTennisData$Winner, yearTennisData$Loser)
  for (i in players) {
    if (sum(yearTennisData$Winner == i) + sum(yearTennisData$Loser == i) < 10) {
      yearTennisData <- yearTennisData[!yearTennisData$Winner == i,]
      yearTennisData <- yearTennisData[!yearTennisData$Loser == i,]
    }
  }
  quasiVEstimate(yearTennisData)
}

# Estimation from year to year
estimate(paste(2000:2010))
estimate(paste(2010:2020))
estimate(paste(2000:2020))

# Read the saved estimation
BradT_Model <- readRDS(file = paste(path, "BTModel.rds", sep = "/"))
BradT_Model.qv <- readRDS(file = paste(path, "BradT_Modelqv.rds", sep = "/"))

# Subset interesting players
players <- c("Ferrer D.", "Federer R.", "Ivanisevic G.")
# Abilities of the players
BTabilities(BradT_Model)[players,]
# Quasi-variance of the players
attr(BTabilities(BradT_Model), 'vcov')[players, players]


