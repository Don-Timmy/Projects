#Use the adjusted score function in your code for the lost.to.slot() function...
lost.to.slot <- function(n, ppp) {
    n <- n
    #this function calculates your loss if you play the slot machine
    #n consecutive times
    #n is the number of consecutive times you spun the wheel
    #ppp is the price per spin in the casino
    
    #spin the wheel
    symbol.gen <- function() {
        #this function randomly generates three symbols from the wheel
        wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0") #the slot machine symbols
        sample(wheel, size =  3, replace = TRUE,
               prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52)) #randomly generate three num
        #bers with the weights 'prob'
    }
    
    #Build the score function to calculate payouts
    score <- function(symbols) {
        diamonds <- sum(symbols == "DD")
        cherries <- sum(symbols == "C")
        # identify case
        # since diamonds are wild, only nondiamonds
        # matter for three of a kind and all bars
        slots <- symbols[symbols != "DD"]
        same <- length(unique(slots)) == 1
        bars <- slots %in% c("B", "BB", "BBB")
        # assign prize
        if (diamonds == 3) {
            prize <- 100
        } else if (same) {
            payouts <- c("7" = 80, "BBB" = 40, "BB" = 25,
                         "B" = 10, "C" = 10, "0" = 0)
            prize <- unname(payouts[slots[1]])
        } else if (all(bars)) {
            prize <- 5
        } else if (cherries > 0) {
            # diamonds count as cherries
            # so long as there is one real cherry
            prize <- c(0, 2, 5)[cherries + diamonds + 1]
        } else {
            prize <- 0
        }
        # double for each diamond
        prize * 2^diamonds
    } #this treats DD as the wild card that it is
    
    #Build the play command
    play <- function() {
        symbols <- symbol.gen()
        structure(score(symbols), symbols = symbols, class = "slots") #structure returns an object with its
        #defined attributes
    }
    
    #how to display objects of class 'slots'
    slot_display <- function(prize) {
        
        #extract symbols
        symbols <- attr(prize, "symbols")
        
        #collapse symbols into single string
        symbols <- paste(symbols, collapse = " ")
        
        #combine symbol with prize as a regular expression
        #\n is regular expression for new line (i.e enter)\
        string <- paste(symbols, prize, sep = "\n$")
        
        #display regular expression without quotes
        cat(string)
    }
    
    print.slots <- function(x, ...) {
        slot_display(x) #we have already written this code
    } 
    
    #play the slot machine n consecutive times
    n.plays <- replicate(n, play())
    
    prize.tally <-  data.frame(n.plays)
    
    #name the column in prize.tally
    colnames(prize.tally) <- "Prize"
    
    #amount spent is the number of time you spun multiplied by the 
    #price per spin
    amount.spent <- n *ppp
    
    #total prize
    total.won <- sum(prize.tally$Prize)
    
    #amount lost to the game. Will be negative if money is gained
    amount.lost <- (amount.spent - total.won)
    
    #amount gained in the game. Will be negative if money is lost
    amount.gained <- (total.won - amount.spent)
    
    maximum.winning <- max(prize.tally$Prize)
    
    a <- c("You played the slot machine")
    b <- c("times, consecutively")
    c <- c("and spent --")
    d <- c("--. You lost --")
    e <- c("--. You gained --")
    f <- c("Your maximum winning was")
    #print.slots(n.plays)
    if (total.won < amount.spent) {
        string <- paste(a, n, b, c, "$", amount.spent, ".", f, "$", maximum.winning, ".", d, "$", amount.lost, "--", sep = " ")
        cat(string)
    } else if (total.won > amount.spent) {
        string <- paste(a, n, b, c, "$", amount.spent, ".", f, "$", maximum.winning, ".", e, "$", amount.gained, "--", sep = " ")
        cat(string)
    }
}

lost.to.slot(1000000, 1)


#VECTORISED VERSION OF SCORE
score_many <- function(symbols) {
    # Step 1: Assign base prize based on cherries and diamonds ---------
    ## Count the number of cherries and diamonds in each combination
    cherries <- rowSums(symbols == "C")
    diamonds <- rowSums(symbols == "DD")
    ## Wild diamonds count as cherries
    prize <- c(0, 2, 5)[cherries + diamonds + 1]
    ## ...but not if there are zero real cherries
    ### (cherries is coerced to FALSE where cherries == 0)
    prize[!cherries] <- 0
    # Step 2: Change prize for combinations that contain three of a kind
    same <- symbols[, 1] == symbols[, 2] &
        symbols[, 2] == symbols[, 3]
    payoffs <- c("DD" = 100, "7" = 80, "BBB" = 40,
                 "BB" = 25, "B" = 10, "C" = 10, "0" = 0)
    prize[same] <- payoffs[symbols[same, 1]]
    # Step 3: Change prize for combinations that contain all bars ------
    bars <- symbols == "B" | symbols == "BB" | symbols == "BBB"
    all_bars <- bars[, 1] & bars[, 2] & bars[, 3] & !same
    prize[all_bars] <- 5
    # Step 4: Handle wilds ---------------------------------------------
    ## combos with two diamonds
    two_wilds <- diamonds == 2
    ### Identify the nonwild symbol
    one <- two_wilds & symbols[, 1] != symbols[, 2] &
        symbols[, 2] == symbols[, 3]
    two <- two_wilds & symbols[, 1] != symbols[, 2] &
        symbols[, 1] == symbols[, 3]
    three <- two_wilds & symbols[, 1] == symbols[, 2] &
        symbols[, 2] != symbols[, 3]
    ### Treat as three of a kind
    prize[one] <- payoffs[symbols[one, 1]]
    prize[two] <- payoffs[symbols[two, 2]]
    prize[three] <- payoffs[symbols[three, 3]]
    ## combos with one wild
    one_wild <- diamonds == 1
    ### Treat as all bars (if appropriate)
    wild_bars <- one_wild & (rowSums(bars) == 2)
    prize[wild_bars] <- 5
    ### Treat as three of a kind (if appropriate)
    one <- one_wild & symbols[, 1] == symbols[, 2]
    two <- one_wild & symbols[, 2] == symbols[, 3]
    three <- one_wild & symbols[, 3] == symbols[, 1]
    prize[one] <- payoffs[symbols[one, 1]]
    prize[two] <- payoffs[symbols[two, 2]]
    prize[three] <- payoffs[symbols[three, 3]]
    # Step 5: Double prize for every diamond in combo ------------------
    unname(prize * 2^diamonds)
}

get_many_symbols <- function(n) {
    wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
    vec <- sample(wheel, size = 3 * n, replace = TRUE,
                  prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))
    matrix(vec, ncol = 3)
}

play_many <- function(n) {
    symb_mat <- get_many_symbols(n = n)
    data.frame(w1 = symb_mat[,1], w2 = symb_mat[,2],
               w3 = symb_mat[,3], prize = score_many(symb_mat))
} #the play function takes in an argument that represents the number of plays. the get
#many symbols function returns a matrix of n rows and 3 columns showing the symbols
#that were generated in the plays... function score_many takes in this matrix and returns
#the prize associated with the symbol combinations...

#Test play_many
PlayMany1 <- play_many(20)
summary(PlayMany1)

system.time(play_many(10000000)) #10000000 simulations in about 15 seconds...
