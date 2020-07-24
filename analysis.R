###############################################################################
#                                                                             #
#                 BlackJack Data Analysis                                    #
#                                                                             #
###############################################################################

# Working environment

setwd("~/blackjack_simulation/")

# Libraries

library(data.table)
library(ggplot2)
library(viridis)

# Load Data

games <- fread("blackjack_games.csv")

###############################################################################

# First processing

# Check if dealer bust
games[, bust_dealer := score_fin_dealer > 21]

# Check if the player bust
games[, bust := ifelse(is.na(score_if_hit), T, score_if_hit > 21)]

# Check if win at current level
games[, win := (score > score_fin_dealer) | bust_dealer]

# Check if draw at current level
games[, draw := (score == score_fin_dealer)]

# Check if win if hit
games[, win_if_hit := (score_if_hit > score_fin_dealer | bust_dealer) & !bust]
games[is.na(win_if_hit), win_if_hit := FALSE]

# Check if draw if hit
games[, draw_if_hit := score_if_hit == score_fin_dealer & !bust]
games[is.na(draw_if_hit), draw_if_hit := FALSE]

# Check if the hand stay hard if hit
games[, hard_if_hit := shift(hard, 1, type = "lead")]
games[shift(game_id, 1, type = "lead") != game_id, hard_if_hit := TRUE]

# Outcome if stand
games[, stand := ifelse(win, 1, ifelse(draw, 0, -1))]

# Outcome if double
games[, double:= 2 * ifelse(win_if_hit, 1, ifelse(draw_if_hit, 0, -1))]

# Outcome if hit
games[, hit := ifelse(win_if_hit, 1, ifelse(draw_if_hit, 0, -1))] 

# Clean the table
games <- games[, c("score", "score_dealer", "hard", "score_if_hit",
                   "score_fin_dealer", "game_id", "hit", "stand", "double", 
                   "hard_if_hit")]

###############################################################################

# Maximal possible earnings

# Best possible outcome as best possible earning at each game
best_possible_outcome = games[, .(outcome = max(max(hit), 
                                                max(stand), 
                                                first(double))), 
                              by = game_id]


# Print the average earnings for a truly perfect game
cat("Best possible win ratio:", 
    mean(best_possible_outcome$outcome) * 100,
    "% \n")

# Outcome repartition
best_possible_outcome[, .(rep = .N / nrow(best_possible_outcome) * 100),
                      by = outcome]

###############################################################################

# Static strategy: hit or stand at a threshold without doubling

# Earn function depending on the strategy
earns <- function(i, games){
  # i: the thresholds at which we stand
  # games: the games database
  
  # copy the data.table (works with pointers)
  g <- copy(games)
  # Compute the last score at which we stand
  g[, strat := last(score[score < i]), by = game_id] # if we do not exceed the threshold
  g[is.na(strat), strat := first(score[score >= i]), by = game_id] # if we exceed it
  # Aggregate in order to get the value at which we stop
  b <- g[, lapply(.SD, function(x) first(x[score == strat])), 
         .SDcols = c("score", "hit", "stand")  , 
         by = game_id]
  # Create outcome depending on score at which we arrived
  b[, outcome := ifelse(score < i, hit, stand)]
  # Return the average earning
  return(data.table(threshold = i,
                    exp = mean(b$outcome),
                    draw = mean(b$outcome == 0),
                    lose = mean(b$outcome < 0),
                    win = mean(b$outcome > 0)))
}

# Compute earning for each stopping threshold from 2 to 21
simple_strat <- lapply(2:21, function(x) earns(x, games))

# Convert simple_strat from list to data.table
simple_strat <- rbindlist(simple_strat)

# Prepare for plotting
simple_strat_long <- melt(simple_strat, 
                          id.vars = c("threshold", "exp"),
                          measure.vars = c("draw", "lose", "win"))

# Parameters to rescale the axis
M = max(simple_strat_long$exp)
m = min(simple_strat_long$exp)

# Best strat
cat("Best possible threshold:", 
    simple_strat[which.max(exp), threshold],
    "\n")

# Plotting
p <- ggplot() +
  geom_bar(data = simple_strat_long, 
           aes(fill=variable, 
               x = threshold,
               y = value), 
           position="stack", 
           stat="identity", 
           alpha = 0.5) +
  geom_line(data = simple_strat_long,
            aes(x = threshold, 
                y = (exp - min(exp))/ (max(exp) - min(exp))),
            color = rgb(72/255,61/255,139/255)) +
  theme_minimal() +
  ggtitle("") +
  scale_fill_viridis(discrete = TRUE, name = "") +
  scale_y_continuous(labels = function(x) paste0(round((x * (M - m) + m) * 100, 0), "%")) +
  xlab("Hit threshold") +
  ylab("Expected earning")

# Save graph
p
ggsave("simple_strat.png", p)

###############################################################################

# More complex strategy: hit, stand, double and split based both score and
# dealer score

# Need to aggregate by score, score_dealer and hard is logical since those are
# the inputs we want to get. We also need to aggregate by score_if_hit and
# hard if hit, because the strategy we will need to build is recursive. The 
# best move depends on the next best move and the associated probability to
# win. So the computations will also depends on the future state.
g <- games[, .(N = .N, hit = mean(hit),
               double = mean(double),
               stand = mean(stand)), 
           by = c("score", "score_dealer", "hard", "score_if_hit", "hard_if_hit")]

# Creation of the recursive strategy. We need to go from highest score to lower,
# first with score higher than 10 and hard, because whatever we hit, we go on
# a higher score that stay hard. Then its all the soft hands (always with higher
# scores to treat first) in order, because whatever we draw, its either a soft
# hand with a higher value or a hard hand. Then we go with the rest
strat <- unique(games[order(-(score > 9 & hard), hard, -score, -score_dealer), 
                      c("score", "score_dealer", "hard")])
strat[, ":="(decision = NA_character_, earn = NA_real_)]

# Recursive treatment of all the cases
for(i in 1:nrow(strat)){
  # get the score, the dealer score, and if the hand is soft for the step
  sc = strat[i, score]
  sc_deal = strat[i, score_dealer]
  hd = strat[i, hard]
  # Get all possibilities associated with next drawn card
  h = g[score == sc & score_dealer == sc_deal & hard == hd, ]
  # Merge those future possibilities with the already computed cases for
  # the strategy
  t =  strat[h, on = c(score = "score_if_hit", 
                       hard = "hard_if_hit", 
                       score_dealer = "score_dealer")]
  # Compute the earnings associated with each decision: stand, double, hit
  a_stand = weighted.mean(t$stand, t$N)
  a_double = weighted.mean(t$double, t$N)
  a_hit = weighted.mean(ifelse(is.na(t$earn), t$hit, t$earn), t$N)
  # Update the strategy table with the best decision
  strat[i, ":="(decision = c("double", "hit", "stand")[which.max(c(a_double, a_hit, a_stand))],
                earn = max(a_double, a_hit, a_stand),
                earn_if_stand = a_stand,
                earn_if_double = a_double,
                earn_if_hit = a_hit)]
}

# Plot the hard cases
p <- ggplot(strat[hard == TRUE], 
             aes(score_dealer, score, fill= decision)) +
  geom_tile(alpha = 0.8, colour = "white") +
  scale_fill_viridis(discrete = T) +
  scale_x_continuous("Dealer score", 
                     labels = as.character(2:11), 
                     breaks = 2:11)+
  scale_y_continuous("Score", 
                     labels = as.character(2:20), 
                     breaks = 2:20)+
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# Save graph
p
ggsave("hard_strat.png", p)


# Plot the soft cases
p <- ggplot(strat[hard == FALSE & !(score %in% c(21))], 
            aes(score_dealer, score, fill= decision)) +
  geom_tile(alpha = 0.8, colour = "white") +
  scale_fill_viridis(discrete = T) +
  scale_x_continuous("Dealer score", 
                     labels = as.character(2:11), 
                     breaks = 2:11)+
  scale_y_continuous("Score", 
                     labels = as.character(11:20), 
                     breaks = 11:20)+
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# Save graph
p
ggsave("soft_strat.png", p)

# Determine case when we need to split. It's just when the earning expectation
# is at least twice higher when dividing the score by 2.
for(i in 1:nrow(strat)){
  strat[i, should_split := ifelse(strat[i, score] %% 2 == 0, 
                                  strat[i, earn] < 2 * strat[score == strat[i, score]/2 & 
                                                             score_dealer == strat[i, score_dealer] & 
                                                             hard == strat[i, hard], earn],
                                  NA)]
}

# Handle the case when there are two aces, when you should always split
strat[score == 12 & hard == F, should_split := TRUE]

# Create a data.table to plot the strategy 
gs <- strat[!is.na(should_split) & hard]
gs[should_split == TRUE, decision := "split"]

# Plot the split cases
p <- ggplot(gs, 
            aes(score_dealer, score, fill= decision)) +
  geom_tile(alpha = 0.8, colour = "white") +
  scale_fill_viridis(discrete = T) +
  scale_x_continuous("Dealer score", 
                     labels = as.character(2:11), 
                     breaks = 2:11)+
  scale_y_continuous("Score", 
                     labels = as.character(seq(2, 20, 2)), 
                     breaks = seq(2, 20, 2))+
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# Save graph
p
ggsave("split.png", p)

# Save srategy
saveRDS(strat, "strategy.rds")
fwrite(strat, "strategy.csv", row.names = F)
