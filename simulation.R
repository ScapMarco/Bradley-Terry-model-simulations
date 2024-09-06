library('BradleyTerry2')
library('plyr')

rm(list = ls())

# Create ranking matrix for the tournament
rankingD <- function(n) {
  m <- choose(n, 2)  # Number of matches (combinations of players)
  D <- matrix(0, m, n - 1)  # Initialize matrix
  indices <- cbind(1:m, t(combn(1:n, 2)) - 1)  # Matrix indices
  D <- replace(D, indices[indices[, 2] > 0, c(1, 2)], 1)  # Replace with 1s
  replace(D, indices[, c(1, 3)], -1)  # Replace with -1s for other player
}

# Resize the tournament: Remove matches
matchResize <- function(mat, n) {
  # Randomly select matches to remove
  row <- sample(1:nrow(mat), n, replace = FALSE)
  mat[row, ] <- 0
  return(mat)
}

# Simulate a single tournament
simOne <- function(ability, rankingMatrix, nplayers, biasReduced) {
  # Calculate the ability differences for the matches
  ability_diff <- rankingMatrix %*% ability
  # Compute the win probability vector
  prob_vector <- as.vector(exp(ability_diff) / (1 + exp(ability_diff)))
  # Simulate the match results based on the probabilities
  match_res <- rbinom(n = length(prob_vector), size = 1, prob = prob_vector)
  
  # Create a contingency matrix to use with countsToBinomial function
  cont_matrix <- matrix(0, nplayers, nplayers)
  z <- 1
  for (i in 1:nrow(rankingMatrix)) {
    first_player <- which(rankingMatrix[i,] == 1)
    second_player <- which(rankingMatrix[i,] == -1)
    if (length(first_player) == 0) {
      # The last player may not be in the tournament matrix
      first_player <- 20
    }
    
    if (match_res[z] == 1) {
      # If player1 wins, increment the win count in the matrix
      cont_matrix[first_player, second_player] <- cont_matrix[first_player, second_player] + 1
    } else {
      # If player2 wins, increment their win count
      cont_matrix[second_player, first_player] <- cont_matrix[second_player, first_player] + 1
    }
    z <- z + 1
  }
  
  # Set row and column names
  colnames(cont_matrix) <- 1:nplayers
  row.names(cont_matrix) <- 1:nplayers
  
  # Fit the Bradley-Terry model
  counts_bin <- countsToBinomial(cont_matrix)
  model <- try(BTm(outcome = cbind(win1, win2), 
                   player1 = player1, player2 = player2, 
                   formula = ~ player, id = "player", 
                   data = counts_bin, br = biasReduced))
  
  # Update the model by referencing the last player
  model <- update(model, refcat = '20')
  
  # Return NA if there was an error, otherwise return the model coefficients
  if (inherits(model, "try-error")) {
    return(rep(NA, nplayers - 1))
  } else {
    return(model$coefficient)
  }
}

# Set a seed for reproducibility
set.seed(123)
nplayers <- 20
# Simulate the initial ability vector
ability <- rnorm(n = nplayers - 1)
# Create the ranking matrix for the tournament
rankingMatrix <- rankingD(nplayers)
# Number of simulations to run
sim <- 500


########################
#### Simulation 1: Full Tournament Matrix without Bias Reduced ####
########################

# Run the simulation and obtain coefficients
coefficients <- replicate(sim, simOne(ability, rankingMatrix, nplayers, FALSE))

# Count the number of estimated differences
tot_estimate1 <- length(coefficients[!is.na(coefficients)])

# Replace NA values with 0
coefficients[is.na(coefficients)] <- 0

# Estimate the differences between abilities
hat_deltaij <- rankingMatrix %*% coefficients

# Calculate the true differences between abilities
deltaij <- rankingMatrix %*% ability

# Compute Mean Squared Error (MSE)
se <- apply(hat_deltaij, 2, function(x) { (x - deltaij)^2 })
mse1 <- apply(se, 2, mean)


########################
#### Simulation 2: 20% of Matches Removed without Bias Reduced ####
########################

# Determine the number of matches to remove (20%)
less_twenty <- (nrow(rankingMatrix) * 20) / 100

# Reduce the tournament matrix by removing matches
rankingMatrix <- matchResize(rankingMatrix, less_twenty)

# Run the simulation with the reduced matrix
coefficients <- replicate(sim, simOne(ability, rankingMatrix, nplayers, FALSE))

# Count the number of estimated differences
tot_estimate2 <- length(coefficients[!is.na(coefficients)])

# Replace NA values with 0
coefficients[is.na(coefficients)] <- 0

# Estimate the differences between abilities
hat_deltaij <- rankingMatrix %*% coefficients

# Calculate the true differences between abilities
deltaij <- rankingMatrix %*% ability

# Compute Mean Squared Error (MSE)
se <- apply(hat_deltaij, 2, function(x) { (x - deltaij)^2 })
mse2 <- apply(se, 2, mean)


########################
#### Simulation 3: 40% of Matches Removed without Bias Reduced ####
########################

# Calculate the number of matches to remove (40%)
less_forty <- (nrow(rankingMatrix) * 40) / 100

# Reduce the tournament matrix by removing matches
rankingMatrix <- matchResize(rankingMatrix, less_forty)

# Run the simulation with the reduced matrix
coefficients <- replicate(sim, simOne(ability, rankingMatrix, nplayers, FALSE))

# Count the number of estimated differences
tot_estimate3 <- length(coefficients[!is.na(coefficients)])

# Replace NA values with 0
coefficients[is.na(coefficients)] <- 0

# Estimate the differences between abilities
hat_deltaij <- rankingMatrix %*% coefficients

# Calculate the true differences between abilities
deltaij <- rankingMatrix %*% ability

# Compute Mean Squared Error (MSE)
se <- apply(hat_deltaij, 2, function(x) { (x - deltaij)^2 })
mse3 <- apply(se, 2, mean)


########################
#### Simulation 4: 60% of Matches Removed without Bias Reduced ####
########################

# Calculate the number of matches to remove (60%)
less_sixty <- (nrow(rankingMatrix) * 60) / 100

# Reduce the tournament matrix by removing matches
rankingMatrix <- matchResize(rankingMatrix, less_sixty)

# Run the simulation with the reduced matrix
coefficients <- replicate(sim, simOne(ability, rankingMatrix, nplayers, FALSE))

# Count the number of estimated differences
tot_estimate4 <- length(coefficients[!is.na(coefficients)])

# Replace NA values with 0
coefficients[is.na(coefficients)] <- 0

# Estimate the differences between abilities
hat_deltaij <- rankingMatrix %*% coefficients

# Calculate the true differences between abilities
deltaij <- rankingMatrix %*% ability

# Compute Mean Squared Error (MSE)
se <- apply(hat_deltaij, 2, function(x) { (x - deltaij)^2 })
mse4 <- apply(se, 2, mean)


########################
#### Simulation 5: 80% of Matches Removed without Bias Reduced ####
########################

# Calculate the number of matches to remove (80%)
less_eighty <- (nrow(rankingMatrix) * 80) / 100

# Reduce the tournament matrix by removing matches
rankingMatrix <- matchResize(rankingMatrix, less_eighty)

# Run the simulation with the reduced matrix
coefficients <- replicate(sim, simOne(ability, rankingMatrix, nplayers, FALSE))

# Count the number of estimated differences
tot_estimate5 <- length(coefficients[!is.na(coefficients)])

# Replace NA values with 0
coefficients[is.na(coefficients)] <- 0

# Estimate the differences between abilities
hat_deltaij <- rankingMatrix %*% coefficients

# Calculate the true differences between abilities
deltaij <- rankingMatrix %*% ability

# Compute Mean Squared Error (MSE)
se <- apply(hat_deltaij, 2, function(x) { (x - deltaij)^2 })
mse5 <- apply(se, 2, mean)

# Set up plotting area to show 5 plots in one row
par(mfrow = c(1, 5))

# Plot Mean Squared Error (MSE) for full tournament
boxplot(mse1, main = "Complete Tournament", 
        ylab = "MSE", col = 'steelblue', border = "brown", 
        xlab = paste("Total Estimated Differences:", toString(tot_estimate1)))

# Plot MSE for 20% of matches removed
boxplot(mse2, main = "Tournament with 20% Matches Removed", 
        ylab = "MSE", col = 'steelblue', border = "brown", 
        xlab = paste("Total Estimated Differences:", toString(tot_estimate2)))

# Plot MSE for 40% of matches removed
boxplot(mse3, main = "Tournament with 40% Matches Removed", 
        ylab = "MSE", col = 'steelblue', border = "brown", 
        xlab = paste("Total Estimated Differences:", toString(tot_estimate3)))

# Plot MSE for 60% of matches removed
boxplot(mse4, main = "Tournament with 60% Matches Removed", 
        ylab = "MSE", col = 'steelblue', border = "brown", 
        xlab = paste("Total Estimated Differences:", toString(tot_estimate4)))

# Plot MSE for 80% of matches removed
boxplot(mse5, main = "Tournament with 80% Matches Removed", 
        ylab = "MSE", col = 'steelblue', border = "brown", 
        xlab = paste("Total Estimated Differences:", toString(tot_estimate5)))

# Reset plotting area to default (one plot per page)
par(mfrow = c(1, 1))




####################
# Analysis: Empirical Coverage of (Quasi-)Confidence Intervals
####################

# Clear the workspace
rm(list = ls())

# Create tournament matrix
rankingD <- function(n) {
  m <- choose(n, 2)  # Number of matchups
  D <- matrix(0, m, n - 1)  # Initialize the matrix
  indices <- cbind(1:m, t(combn(1:n, 2)) - 1)  # Generate indices for the matrix
  D <- replace(D, indices[indices[, 2] > 0, c(1, 2)], 1)  # Set positive entries
  replace(D, indices[, c(1, 3)], -1)  # Set negative entries
}

# Simulate matches and return the contingency matrix
simOne <- function(ability, rankingMatrix, nplayers) {
  # Vector of skill differences in matches
  ability_diff <- rankingMatrix %*% ability
  
  # Probability vector
  prob_vector <- as.vector(exp(ability_diff) / (1 + exp(ability_diff)))
  
  # Simulate matches using the probability vector
  match_res <- rbinom(n = length(prob_vector), size = 1, prob = prob_vector)
  
  # Create contingency matrix to use with countsToBinomial function
  cont_matrix <- matrix(0, nplayers, nplayers)
  z <- 1
  for (i in 1:nrow(rankingMatrix)) {
    first_player <- which(rankingMatrix[i,] == 1)
    second_player <- which(rankingMatrix[i,] == -1)
    
    if (length(first_player) == 0) {
      # Handle the case where the first player is not present in the tournament matrix
      first_player <- 20
    }
    
    if (match_res[z] == 1) {
      # Player 1 wins
      cont_matrix[first_player, second_player] <- cont_matrix[first_player, second_player] + 1
    } else {
      # Player 2 wins
      cont_matrix[second_player, first_player] <- cont_matrix[second_player, first_player] + 1
    }
    z <- z + 1
  }
  
  # Rename rows/columns
  colnames(cont_matrix) <- 1:nplayers
  row.names(cont_matrix) <- 1:nplayers
  
  return(cont_matrix)
}

# Fit the model 
fitModel <- function(cont_matrix, biasReduced) {
  counts_bin <- countsToBinomial(cont_matrix)
  model <- try(BTm(outcome = cbind(win1, win2), 
                   player1 = player1, player2 = player2, 
                   formula = ~ player, id = "player", 
                   data = counts_bin, br = biasReduced))
  model <- update(model, refcat = '20')
  
  if (inherits(model, "try-error")) {
    return(NA)
  } else {
    return(model)
  }
}


# Confidence interval levels: 0.90, 0.95, 0.99
int_levels <- c(qnorm(p = 1-0.1/2), qnorm(p = 1-0.05/2), qnorm(p = 1-0.01/2))

# Standard deviations for abilities
sd <- c(0.25, 0.5, 0.75, 1, 2)

# Total number of correct intervals for each confidence level
tot_count_levels <- vector("list", length(sd))

# Total number of correct intervals
tot_count <- vector(mode = "integer", length = length(int_levels))

# Loop through different standard deviations
for (i in 1:length(sd)) {
  # Loop through different confidence interval levels
  for (j in 1:length(int_levels)) {
    set.seed(123)
    nplayers <- 20
    
    # Simulate initial ability vector
    ability <- rnorm(n = nplayers - 1, sd = sd[i])
    
    # Create tournament matrix
    rankingMatrix <- rankingD(nplayers)
    sim <- 500  # Number of simulations
    
    # Initialize count of correct intervals
    count <- vector(mode = "integer", length = sim)
    
    # Perform simulations
    for (z in 1:sim) {
      # Simulate the tournament
      contMatrix <- simOne(ability, rankingMatrix, nplayers)
      
      # Fit the model
      model <- fitModel(contMatrix, "TRUE") # TRUE = estimate with bias reduced
      
      # Calculate quasi-variances
      data.qv <- qvcalc(BTabilities(model))
      
      # Confidence intervals
      estimate <- data.qv$qvframe$estimate[1:19]
      quasiVar <- data.qv$qvframe$quasiVar[1:19]
      
      # Calculate confidence intervals
      estimate_ability_diff <- rankingMatrix %*% estimate
      quasiInterval <- sqrt(replace(rankingMatrix, rankingMatrix < 0, values = 1) %*% quasiVar)
      
      upper_limit <- estimate_ability_diff + int_levels[j] * quasiInterval
      lower_limit <- estimate_ability_diff - int_levels[j] * quasiInterval
      
      # Real ability differences
      real_ability_diff <- rankingMatrix %*% ability
      
      # Count exact matches
      d <- data.frame(real_ability_diff, lower_limit, upper_limit)
      countCorrect <- function(row) {
        estimate <- row[1]
        lower_limit <- row[2]
        upper_limit <- row[3]
        if (estimate <= upper_limit & estimate >= lower_limit) {
          return(1)
        }
        return(0)
      }
      
      # Total number of correct intervals in simulation z
      tot <- sum(apply(d, 1, countCorrect)) / length(real_ability_diff)
      count[z] <- tot
    }
    
    # Average proportion of correct intervals after sim simulations
    tot_count[j] <- mean(count)
  }
  
  # Vector of average correct intervals for int_levels
  tot_count_levels[i] <- list(append(tot_count_levels[i], tot_count))
}

# Plotting results
pdf("test.pdf", height = 5.9, width = 5.9)
par(mfrow = c(3, 2))

# Plot for each standard deviation
for (i in 1:length(sd)) {
  plot(unlist(tot_count_levels[i]), type = 'b', ylab = "", xlab = "", main = "", xaxt = "n", cex.axis = 0.9)
  axis(side = 1, labels = c("90%", "95%", "99%"), at = c(1, 2, 3), cex.axis = 0.9)
  text(c(1 + 0.2, 2 - 0.2, 3 - 0.2), unlist(tot_count_levels[i]), round(unlist(tot_count_levels[i]), digits = 3), cex = 0.7)
  mtext("Empirical Coverage", side = 2, line = 2.2, cex = 0.8)
  mtext("Nominal Level", side = 1, line = 2.2, cex = 0.8)
}

dev.off()



#################################################### Brier Score Analysis

rm(list = ls())

# Brier score calculation
brierScore <- function(pa, ia) {
  return(2 * (pa - ia)^2)
}

# Create tournament matrix
rankingD <- function(n) {
  m <- choose(n, 2)
  D <- matrix(0, m, n - 1)
  indices <- cbind(1:m, t(combn(1:n, 2)) - 1)
  D <- replace(D, indices[indices[, 2] > 0, c(1, 2)], 1)
  replace(D, indices[, c(1, 3)], -1)
}

# Simulate matches and return the contingency matrix
simOne <- function(ability, rankingMatrix, nplayers) {
  # Vector of ability differences in matches
  ability_diff <- rankingMatrix %*% ability
  # Vector of probabilities
  prob_vector <- as.vector(exp(ability_diff) / (1 + exp(ability_diff)))
  # Simulate matches using the probability vector
  match_res <- rbinom(n = length(prob_vector), size = 1, prob = prob_vector)
  # Create a contingency matrix for use with countsToBinomial function
  cont_matrix <- matrix(0, nplayers, nplayers)
  z <- 1
  for (i in 1:nrow(rankingMatrix)) {
    first_player <- which(rankingMatrix[i,] == 1)
    second_player <- which(rankingMatrix[i,] == -1)
    if (length(first_player) == 0) {
      # Handle case where the first player is not present in the tournament matrix
      first_player <- 20
    }
    
    if (match_res[z] == 1) {
      # Player 1 is the winner
      cont_matrix[first_player, second_player] <- cont_matrix[first_player, second_player] + 1
    } else {
      # Player 2 is the winner
      cont_matrix[second_player, first_player] <- cont_matrix[second_player, first_player] + 1
    }
    z <- z + 1
  }
  # Rename rows/columns
  colnames(cont_matrix) <- 1:nplayers
  row.names(cont_matrix) <- 1:nplayers

  return(cont_matrix)
}

# Remove a specified number of matches from the contingency matrix and return the updated matrix and removed matches
removeMatch <- function(contMatrix, reduction) {
  # Rows and columns representing the matches
  m <- which(contMatrix > 0, arr.ind = TRUE)
  # Randomly select rows to remove based on the reduction percentage
  row <- sample(1:nrow(m), reduction, replace = FALSE)
  # Remove matches between the row player and column player
  contMatrix[m[row,]] <- 0
  # Return both removed matches and the updated matrix
  return(list(m[row,], contMatrix))
}

# Fit the model
fitModel <- function(cont_matrix, biasReduced) {
  counts_bin <- countsToBinomial(cont_matrix)
  model <- try(BTm(outcome = cbind(win1, win2), 
                   player1 = player1, player2 = player2, 
                   formula = ~ player, id = "player", 
                   data = counts_bin, br = biasReduced))
  model <- update(model, refcat = '20')
  if (inherits(model, "try-error")) {
    return(NA)
  } else {
    return(model)
  }
}

# Calculate the probability of a player winning
probability_winner <- function(winner_r, loser_r) {
  p <- exp(winner_r - loser_r) / (1 + exp(winner_r - loser_r))
  return(p)
}

# Returns the probability of the first player winning in the model without bias reduction
findProb <- function(match) {
  winner <- match[1]
  loser <- match[2]
  
  winnerName <- paste("player", winner, sep = "")
  loserName <- paste("player", loser, sep = "")

  wrank <- model$coefficients[winnerName]
  lrank <- model$coefficients[loserName]
  
  # Handle missing ability values
  if (is.na(wrank)) {
    wrank <- 0
  }
  
  if (is.na(lrank)) {
    lrank <- 0
  }

  return(probability_winner(wrank, lrank))
}

# Returns the probability of the first player winning in the model with bias reduction
findProb_reduction <- function(match) {
  winner <- match[1]
  loser <- match[2]
  
  winnerName <- paste("player", winner, sep = "")
  loserName <- paste("player", loser, sep = "")
  
  wrank <- model_reduction$coefficients[winnerName]
  lrank <- model_reduction$coefficients[loserName]
  
  # Handle missing ability values
  if (is.na(wrank)) {
    wrank <- 0
  }
  
  if (is.na(lrank)) {
    lrank <- 0
  }
  
  return(probability_winner(wrank, lrank))
}

set.seed(123)
nplayers <- 20 # Number of players

# Simulate initial ability vector
ability <- rnorm(n = nplayers - 1)

# Create tournament matrix
rankingMatrix <- rankingD(nplayers)

# Simulate tournament
simContMatrix <- simOne(ability, rankingMatrix, nplayers) 

# Total number of matches
tot <- nrow(rankingMatrix)

# Reduction percentages: 40%, 60%, 80%
reductions <- c(30, 20, 10)

# Reduce the tournament by 40%
splitData <- removeMatch(simContMatrix, tot * 40 / 100) 
# 20% of data to predict
data20 <- data.frame(splitData[1])
row.names(data20) <- NULL
# 80% of data used for estimation
data80 <- data.frame(splitData[2])
colnames(data80) <- sub("^X", "", colnames(data80))

# Fit model without bias reduction
model <- fitModel(data80, "FALSE") # FALSE = without bias reduction 
# Fit model with bias reduction
model_reduction <- fitModel(data80, "TRUE") # TRUE = with bias reduction 

# Predicted probabilities without bias reduction
probvector <- apply(data20, 1, findProb)
# Predicted probabilities with bias reduction
probvector_reduction <- apply(data20, 1, findProb_reduction)

# Calculate Brier score for predictions without bias reduction
brscore <- brierScore(probvector, 1)
# Calculate Brier score for predictions with bias reduction
brscore_reduction <- brierScore(probvector_reduction, 1)

# Brier scores without bias reduction
z <- 1
brscores <- vector("list", 4)
brscores[1] <- list(append(brscores[1], brscore))

# Brier scores with bias reduction
brscores_reduced <- vector("list", 4)
brscores_reduced[1] <- list(append(brscores_reduced[1], brscore_reduction))

for (red in reductions) {
  z <- z + 1
  # Remove 'red' matches from the tournament
  splitData <- removeMatch(data80, red)
  # New data for estimation
  data80 <- data.frame(splitData[2])
  colnames(data80) <- sub("^X", "", colnames(data80))
  
  # Fit model without bias reduction
  model <- fitModel(data80, "FALSE")
  # Fit model with bias reduction
  model_reduction <- fitModel(data80, "TRUE") 
  
  # Predicted probabilities without bias reduction
  probvector <- apply(data20, 1, findProb)
  # Predicted probabilities with bias reduction
  probvector_reduction <- apply(data20, 1, findProb_reduction)
  
  # Calculate Brier score for predictions without bias reduction
  brscore <- brierScore(probvector, 1)
  brscores[z] <- list(append(brscores[z], brscore))
  
  # Calculate Brier score for predictions with bias reduction
  brscore_reduction <- brierScore(probvector_reduction, 1)
  brscores_reduced[z] <- list(append(brscores_reduced[z], brscore_reduction))
}


# Names for the boxplot groups
names <- c("Without reduction", "With reduction")

# Create PDF file for the plots
pdf("test3.pdf", height = 9, width = 5.9)

# Set up plotting area: 3 rows, 2 columns
par(mfrow = c(3, 2))

# Boxplot for Brier scores without and with bias reduction (first reduction level)
boxplot(unlist(brscores[1]), unlist(brscores_reduced[1]), names = names, 
        main = "", ylab = "Brier Score", col = 'steelblue', border = "brown", 
        cex.axis = 1.2, boxwex = 0.3, cex.lab = 1.4, cex = 1.4)

# Boxplot for Brier scores without and with bias reduction (second reduction level)
boxplot(unlist(brscores[2]), unlist(brscores_reduced[2]), names = names, 
        main = "", ylab = "Brier Score", col = 'steelblue', border = "brown", 
        cex.axis = 1.2, boxwex = 0.3, cex.lab = 1.4, cex = 1.4)

# Boxplot for Brier scores without and with bias reduction (third reduction level)
boxplot(unlist(brscores[3]), unlist(brscores_reduced[3]), names = names, 
        main = "", ylab = "Brier Score", col = 'steelblue', border = "brown", 
        cex.axis = 1.2, boxwex = 0.3, cex.lab = 1.4, cex = 1.4)

# Boxplot for Brier scores without and with bias reduction (fourth reduction level)
boxplot(unlist(brscores[4]), unlist(brscores_reduced[4]), names = names, 
        main = "", ylab = "Brier Score", col = 'steelblue', border = "brown", 
        cex.axis = 1.2, boxwex = 0.3, cex.lab = 1.4, cex = 1.4)

# Close the PDF device
dev.off()








