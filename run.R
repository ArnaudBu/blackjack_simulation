###############################################################################
#                                                                             #
#                 BlackJack Data Generator                                    #
#                                                                             #
###############################################################################

# Working environment

setwd("~/blackjack_simulation/")

# Source helpers

source("func.R")

###############################################################################

# Global variables

ndecks = 8
soft17hit = TRUE
max_tries = 10000000
nb_hands = 3

###############################################################################

# Initializations

dt <- data.table()

# Declare new deck
deck <- deck.bj(ndecks)
burncard <- deck$draw() # Burn one card for a new deck

# Declare dealer
dealer <- hand.bj()
# Declare hands
hands <- list()
for(i in 1:nb_hands){
  hands[[i]] <- hand.bj()
}

# Variable to handle if the deck must be redrawn
redeck = FALSE

# Get number of cards
nb_cards <- rep(0, 10)

# Games
game = 0

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
  nb_cards[score_card] <<- nb_cards[score_card] + 1
  if(length(deck$cards) == 0){
    deck <<- deck.bj(ndecks)
    burncard <- deck$draw() # Burn one card for a new deck
    nb_cards <<- rep(0, 10) # Reinitialize number of cards
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

# Play for one hand
play <- function(hand){
  score <- hand$score()
  
  # Initialize matrix
  tmp <- data.table()
  res <- data.table(score = score,
                    score_dealer =dealer$score(T) ,
                    nbas = sum(grepl("A", hand$cards)),
                    hard = hand$ishard(),
                    score_if_hit = NA_integer_,
                    score_fin_dealer = NA_integer_,
                    nb_as_out = nb_cards[1],
                    nb_2_out = nb_cards[2],
                    nb_3_out = nb_cards[3],
                    nb_4_out = nb_cards[4],
                    nb_5_out = nb_cards[5],
                    nb_6_out = nb_cards[6],
                    nb_7_out = nb_cards[7],
                    nb_8_out = nb_cards[8],
                    nb_9_out = nb_cards[9],
                    nb_10_out = nb_cards[10])
  
  # Play while not bust
  while(score < 21){
    
    # Get one card
    card <- get_card()
    
    # Add card to hand
    hand$get(card)
    
    # Compute score
    score <- hand$score()
    res$score_if_hit = score
    tmp <- rbindlist(list(tmp, res))
    # Add to matrix
    res <- data.table(score = score,
                      score_dealer =dealer$score(T) ,
                      nbas = sum(grepl("A", hand$cards)),
                      hard = hand$ishard(),
                      score_if_hit = NA_integer_,
                      score_fin_dealer = NA_integer_,
                      nb_as_out = nb_cards[1],
                      nb_2_out = nb_cards[2],
                      nb_3_out = nb_cards[3],
                      nb_4_out = nb_cards[4],
                      nb_5_out = nb_cards[5],
                      nb_6_out = nb_cards[6],
                      nb_7_out = nb_cards[7],
                      nb_8_out = nb_cards[8],
                      nb_9_out = nb_cards[9],
                      nb_10_out = nb_cards[10])
  }
  if(nrow(tmp) == 0){
    return(res)
  } else{
    return(tmp)
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
  return(score_dealer)
}

###############################################################################

# Run

pb <- txtProgressBar(min = 0, max = max_tries, style = 3)

while(nrow(dt) < max_tries){
  init_hands()
  g <- data.table()
  for(hand in hands){
    # Test splits
    if(hand$cansplit()){
      hsp <- hand$split()
      for(h in hsp){
        game <- game + 1
        d <-play(h)
        d$game_id <- game
        g <- rbindlist(list(g, d), fill = T)
      }
    }
    game <- game + 1
    d <-play(hand)
    d$game_id <- game
    g <- rbindlist(list(g, d), fill = T)
  }
  g$score_fin_dealer <- play_dealer()
  if(redeck){
    deck <<- deck.bj(ndecks)
    burncard <- deck$draw() # Burn one card for a new deck
    nb_cards <<- rep(0, 10) # Reinitialize number of cards
    redeck <<- F
  }
  dt <- rbindlist(list(dt, g), fill = T)
  setTxtProgressBar(pb, nrow(dt))
  if(nrow(dt) %% 10000 < 5){
    fwrite(dt, "blackjack_games.csv", sep = ",", dec = ".", row.names = F)
  }
}

close(pb)

fwrite(dt, "blackjack_games.csv", sep = ",", dec = ".", row.names = F)