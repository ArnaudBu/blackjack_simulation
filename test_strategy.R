###############################################################################
#                                                                             #
#                 Strategy for blackjack simulation                           #
#                                                                             #
###############################################################################

# Working environment

setwd("~/blackjack_simulation/")

# Source helpers

source("func.R")

# Strategy

strategy <- list(readRDS("strategy.rds"), readRDS("strategy_bench.rds"))

###############################################################################

# Global variables

ndecks = 8
soft17hit = TRUE
length_session = 1000
n_session = 1000
nb_hands = 2

###############################################################################

# Initializations

dt <- array(NA_integer_, dim = c(nb_hands, length_session, n_session))

###############################################################################

# Utilitaries functions


# Get card for a hand
get_card <- function(){
  card <- deck$draw()
  # Set the redeck variable if the stop token is hit
  if(card == "Stop"){
    redeck <<- TRUE
    card <- deck$draw()
    # Redraw a deck if there is no more card
  }
  # Register the card drawn
  score_card = as.numeric(gsub("([0-9]+).*$", "\\1",gsub("J|Q|K", "10", gsub("A", 1, card))))
  if(length(deck$cards) == 0){
    deck <<- deck.bj(ndecks)
    burncard <- deck$draw() # Burn one card for a new deck
    redeck <<- F
  }
  return(card)
}

# Draw 2 cards for each hand
init_hands <- function(){
  for(hand in append(hands, dealer)){
    hand$clear()
    for(i in 1:2){
      card <- get_card()
      # Add the card to the hand
      hand$get(card)
    }
  }
}

# decision to make
decision <- function(hand, strat, splitdone = F){
  r <- strat[score == hand$score() & 
               score_dealer == dealer$score(T) &
               hard == hand$ishard()]
  if(hand$cansplit() & !splitdone){
    if(!is.na(r$should_split)){
      if(r$should_split){
        return("split")
      }
    }
  }
  return(ifelse(length(r$decision) > 0, r$decision, "stand"))
}

# Play for one hand
play <- function(hand, strat){
  score <- hand$score()
  dec <- decision(hand, strat)
  if(dec == "split"){
    sp <- hand$split()
    res <- c()
    for(s in sp){
      score <- s$score()
      dec <- decision(s, strat, T)
      while(score < 21 & dec != "stand"){
        card <- get_card()
        s$get(card)
        score <- s$score()
        dec <- decision(s, strat, T)
      }
      res <- c(res, score)
    }
    return(res)
  } else if(dec == "double"){
    card <- get_card()
    hand$get(card)
    score <- hand$score()
    return(c(score, score))
  } else{
    while(score < 21 & dec != "stand"){
      card <- get_card()
      hand$get(card)
      score <- hand$score()
      dec <- decision(hand, strat)
    }
    return(c(score))
  }
}

# Play for the dealer
play_dealer <- function(){
  score_dealer <- dealer$score()
  
  # Play while not bust
  while(score_dealer < 17 | (score_dealer == 17 & !dealer$ishard() & soft17hit)){
    
    # Get one card
    card <- get_card()
    
    # Add card to hand
    dealer$get(card)
    
    # Compute score
    score_dealer <- dealer$score()
  }
  if(score_dealer == 21 & grepl("A", dealer$cards[1])){
    return(0)
  }
  return(score_dealer)
}

# Function to calculate earnings
earnings <- function(score, score_deal){
  if(score > 21){
    return(-1)
  } else if(score == 0 & score_deal != 0){
    return(1.5)
  }  else if(score != 0 & score_deal == 0){
    return(-1)
  } 
  else if(score > score_deal | score_deal > 21){
    return(1)
  } else if(score == score_deal){
    return(0)
  } else if(score < score_deal){
    return(-1)
  }
}

###############################################################################

# Run

pb <- txtProgressBar(min = 0, max = n_session, style = 3)

for(i in 1:n_session){
  # Declare new deck
  deck <- deck.bj(ndecks)
  burncard <- deck$draw() # Burn one card for a new deck
  
  # Declare dealer
  dealer <- hand.bj()
  # Declare hands
  hands <- list()
  for(k in 1:nb_hands){
    hands[[k]] <- hand.bj()
  }
  
  # Variable to handle if the deck must be redrawn
  redeck = FALSE
  for(j in 1:length_session){
    init_hands()
    c = list()
    for(h in 1:nb_hands){
      if(hands[[h]]$score() == 21){
        c[[h]] <- 0
      } else{
        c[[h]] <- play(hands[[h]], strategy[[h]]) 
      }
    }
    sd <- play_dealer()
    dt[, j, i] <- sapply(c, function(x) sum(sapply(as.list(x), function(y) earnings(y, sd))))
    if(redeck){
      deck <<- deck.bj(ndecks)
      burncard <- deck$draw() # Burn one card for a new deck
      nb_cards <<- rep(0, 10) # Reinitialize number of cards
      redeck <<- F
    }
  }
  setTxtProgressBar(pb, i)
  saveRDS(dt, "blackjack_strat.rds")
}

close(pb)

saveRDS(dt, "blackjack_strat.rds")

###############################################################################

# Analysis of the results

dt = readRDS("blackjack_strat.rds")[1,,]

# Cumulate by column
dt = apply(dt, 1, cumsum)

# Convert to long format for plot
r <- data.table(melt(dt))
colnames(r) <- c("game", "scenario", "earnings")

# Get mean and sd on earnings

# Transform into aggregated values
r <- r[, .(e_mean = mean(earnings),
           e_1 = quantile(earnings, 0.01),
           e_5 = quantile(earnings, 0.05),
           e_10 = quantile(earnings, 0.1),
           e_90 = quantile(earnings, 0.9),
           e_95 = quantile(earnings, 0.95),
           e_99 = quantile(earnings, 0.99),
           e_min = min(earnings),
           e_max = max(earnings),
           e_rnd = first(earnings)), by = .(game)]

# Define colors for visualization
rouge <- rgb(255,53,37, maxColorValue = 255)
viol2 <- rgb(134,85,183, maxColorValue = 255)
viol1 <- rgb(193,70,112, maxColorValue = 255)
bleu <- rgb(82,105,254, maxColorValue = 255)
jaune <- rgb(251,247,7, maxColorValue = 255)

# Plot a visualisation
p <- ggplot(r, aes(x = game)) +
  geom_ribbon(aes(ymin = e_min, ymax = e_max, fill = "min | max"), color = NA) +
  geom_ribbon(aes(ymin = e_1, ymax = e_99, fill = "1% | 99%"), color = NA) +
  geom_ribbon(aes(ymin = e_5, ymax = e_95, fill = "5% | 95%"), color = NA) +
  geom_ribbon(aes(ymin = e_10, ymax = e_90, fill = "10% | 90%"), color = NA) +
  geom_line(aes(y = e_mean, linetype = "Average"), color = jaune) +
  geom_line(aes(y = e_rnd, linetype = "Random"), color = jaune) +
  scale_y_continuous(name = "Earnings") +
  scale_x_continuous(name = "") +
  scale_fill_manual(name="",values=c("min | max" = rouge,
                                     "1% | 99%" = viol1,
                                     "5% | 95%" = viol2,
                                     "10% | 90%" = bleu)) +
  scale_linetype_manual(name = "", values = c("Average" = 1,
                                              "Random" = 2)) +
  theme_minimal()

# Save graph
p
ggsave("strategy.png", p)

# Values
r[nrow(r)]
