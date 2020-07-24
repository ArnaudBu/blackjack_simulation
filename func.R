###############################################################################
#                                                                             #
#                 Helpers for blackjack simulation                            #
#                                                                             #
###############################################################################


# libraries

library(R6) # object oriented programming with R
library(data.table) # data.table for data management

# utils

cross <- function(l, m) sapply(l, function(x) paste(x, m))

###############################################################################

# Deck class

Deck <- R6Class("Deck",
                private = list(
                  .rank = c("A", 2:10, "J", "Q", "K"),
                  .colors = c("♠", "♥", "♦", "♣"),
                  .cards = c()
                ),
                active = list(
                  rank = function(value){
                    if(missing(value)){
                      private$.rank
                    } else {
                      stop("`$rank` is read only", call. = FALSE)
                    }
                  },
                  colors = function(value){
                    if(missing(value)){
                      private$.colors
                    } else {
                      stop("`$colors` is read only", call. = FALSE)
                    }
                  },
                  cards = function(value){
                    if(missing(value)){
                      private$.cards
                    } else {
                      stop("`$cards` is read only", call. = FALSE)
                    }
                  }
                ),
                public = list(
                  initialize = function(ndecks) {
                    private$.cards = rep(cross(private$.rank, private$.colors), ndecks)
                  },
                  shuffle = function(withStopToken = T) {
                    c = sample(private$.cards[private$.cards != "Stop"])
                    stop_pos = min(max(2,round(rnorm(1, length(c) /2, length(c) /8), 0)),length(c) - 1)
                    if(withStopToken){
                      private$.cards <- c(c[1:stop_pos-1], "Stop", c[stop_pos:length(c)])
                    } else {
                      private$.cards <- c
                    }
                  },
                  draw = function(ncards = 1) {
                    ncards <- min(ncards, length(private$.cards))
                    if(ncards == 0){return(NA)}
                    c <- private$.cards[1:ncards]
                    private$.cards <- private$.cards[-(1:ncards)]
                    return(c)
                  }
                )
)

# Deck initialization

deck.bj <- function(ndecks = 1, withStopToken = T){
  d <- Deck$new(ndecks)
  d$shuffle(withStopToken)
  return(d)
}

###############################################################################

# Hand class

Hand <- R6Class("Hand",
                private = list(.cards = c()
                ),
                active = list(
                  cards = function(value){
                    if(missing(value)){
                      private$.cards
                    } else {
                      stop("`$cards` is read only", call. = FALSE)
                    }
                  }
                ),
                public = list(
                  initialize = function() {
                    private$.cards = c()
                  },
                  get = function(cards) {
                    private$.cards <- c(private$.cards, cards)
                  },
                  clear = function(){
                    private$.cards <- c()
                  },
                  cansplit = function(){
                    if(length(private$.cards) != 2){
                      return(FALSE)
                    }
                    scores <- gsub("♠|♥|♦|♣", "", private$.cards)
                    if (scores[1] != scores[2]){
                      return(FALSE)
                    }
                    return(TRUE)
                  },
                  split = function(){
                    if(length(private$.cards) != 2){
                      stop("Unable to split")
                    }
                    scores <- gsub("♠|♥|♦|♣", "", private$.cards)
                    if (scores[1] != scores[2]){
                      stop("Unable to split")
                    }
                    hand1 <- hand.bj()
                    hand2 <- hand.bj()
                    hand1$get(private$.cards[1])
                    hand2$get(private$.cards[2])
                    return(list(hand1, hand2))
                  },
                  ishard = function(){
                    h = T
                    s <- 0
                    cards <- gsub("J|Q|K", "10", private$.cards)
                    isas <- grepl("A", cards)
                    for(c in cards[!isas]){
                      s <- s + as.numeric(gsub("([0-9]+).*$", "\\1", c))
                    }
                    s <- s + sum(isas)
                    if(s <= 11 & sum(isas) > 0){
                      h <- F
                    }
                    return(h)
                  },
                  score = function(first_card = F){
                    s <- 0
                    cards <- gsub("J|Q|K", "10", private$.cards)
                    if(first_card){cards <- cards[1]}
                    isas <- grepl("A", cards)
                    for(c in cards[!isas]){
                      s <- s + as.numeric(gsub("([0-9]+).*$", "\\1", c))
                    }
                    s <- s + sum(isas)
                      if(s <= 11 & sum(isas) > 0){
                        s <- s + 10
                      }
                    return(s)
                  }
                )
)

# Hand initialization

hand.bj <- function(){
  Hand$new()
}