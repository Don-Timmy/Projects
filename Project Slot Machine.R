#Brief... 
#We are going to be simulating a slot machine gambling game. You press the lever by running
#a function and it will generate three random sysmbols and calculate your payout

#First Task: Build the Play Function
#Capabilities of the Play function: 1) Randomly generate three casino symbols from a casino
                                      #symbols group
                                   #2) Calculate a prize based on the symbols
#Building a function for the first task:
symbol.gen <- function() {
    #this function randomly generates three symbols from the wheel
    wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0") #the slot machine symbols
    sample(wheel, size =  3, replace = TRUE,
           prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52)) #randomly generate three num
    #bers with the weights 'prob'
}
#test symbol.gen
symbol.gen()



score <- function (symbols) {
    # identify case
    same <- symbols[1] == symbols[2] && symbols[2] == symbols[3]
    bars <- symbols %in% c("B", "BB", "BBB")
    # get prize
    if (same) {
        payouts <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25,
                     "B" = 10, "C" = 10, "0" = 0)
        prize <- unname(payouts[symbols[1]])
    } else if (all(bars)) {
        prize <- 5
    } else {
        cherries <- sum(symbols == "C")
        prize <- c(0, 2, 5)[cherries + 1]
    }
    # adjust for diamonds
    diamonds <- sum(symbols == "DD")
    prize * 2 ^ diamonds
}

play <- function() {
    symbols <- symbol.gen()
    print(symbols)
    score(symbols)
    #list(Output = symbols, Payouts = score)
}
play()

#playsimul <- replicate(50000, play()$Payouts)
#hist(playsimul)
#table(playsimul) #if you play this thing 20,000 times, you are likely to have $80 payout
#only once... Wow... No wonder casinos are so profitable


#THE S3 System
#We would like for the results of the play function to be pretty... To at least look some
#thing like:

## 0 0 DD
## $0
#We wouldn't be able to achieve this though without a thorough understanding of R's S3 
#system...

#R's S3 system, governs how objects of different classes are handled

#The components of the S3 system include: Attributes, Methods, and Generic Functions
#Every object in R has attributes that you can set to it...
#Helper functions help one access and set attributes of objects
carddeck <- read.csv("C:/Users/user/Desktop/Don Timmy/Don Timmy's Work/Career Growth/Data Science Career/MY R Projects/RScripts/Hands-On Programming/deck.csv",
                     stringsAsFactors = FALSE)

#check attributes of the 'carddeck' object
attributes(carddeck) #names, class, row.names

#you can reset the 'row.names' attribute of the carddeck object just by writing this simple
#code
row.names(carddeck) <- 101:152

#you can even give it an attribute that it didn't have before
levels(carddeck) <- c("hearts", "minds", "souls")

attributes(carddeck) #row.names have changed from 1 to 52 to 101 to 152... and another
#attribute 'levels' have been added to the object. the only time R will complain about
#the an attribute is when it needs it in an object and can't find it, otherwise, R will
#not object to adding as many attributes to an object

one.play <- play() #save the result of playing the slot machine once to one.play

one.play #one.play isn't displaying the symbolsof the slot machine

#does one.play have any attributes
attributes(one.play) #no

#give one.play an attribute called 'symbols' consisting of character strings
attr(one.play, "symbols") <- c("BB", "0", "0")
#check again
attributes(one.play) #yay

attr(one.play, "symbols") #if you know the name of any attribute, you can use attr to 
#check it's content

one.play #so far the assigned attribute doesn't change the class of an atomic vector object
#R will display the values of the attributes of the vector beneath its values

#you can manipulate an object without affecting it's attributes safe for the names, class
#and some other attributes
one.play + 1 #affected the values without changing the attributes

#if we change the class of the object... we will not be able to manipulate it arithmetically
#like before because R will need it's class to do the operation
class(one.play) <- "timmy" 
one.play + 1

#We'll modify this play function to return the symbols associated with the scores as 
#an attribute of the output rather than a stand alone
play <- function() {
    symbols <- symbol.gen()
    prize <-  score(symbols)
    attr(prize, "symbols") <- symbols #add the symbols object as an attribute
    #of the prize object
    prize #return prize
}

two.play <- play()
two.play #the slot plays now have the symbols attached


#Another way to do that is to use the 'structure' function which creates objects and 
#adds attributes to them.
play <- function() {
    symbols <- symbol.gen()
    structure(score(symbols), symbols = symbols) #structure returns an object with its
    #defined attributes
}

three.play <- play()
three.play

#we'll write a function that will look up the 'symbols' attribute of the play function
#output and display the results in a pretty manner
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

#test the slot display function
slot_display(two.play) #the output of play is now being displayed fantastically

slot_display(play())

#ABOUT the Generic Function
#A generic function is what I call the do everything function. It does it's job based on
#the class of the input you give to it. Take the 'print' function for instance
print(2) #Will print the numeric vector 2

print("Tope") #will print the character vector

num <- 1000000
print(num) #print num with the numeric class

class(num) <- c("POSIXct", "PosiXt")

print(num) #print num with the date time class

#How it's able to do this is that the generic print function calls the 'UseMethod' function
#which in turn looks for the print function associated with the class of that object. 
#To check this check the 'print' function code
print

#Methods
#Generic functions have different methods for different classes of objects
methods(print) #we can also write a print method for our slot_display

class(two.play) <- "slots" #create the class attribute for the play output object

#the print method must be named print.slots else R will not be able to find print and 
#associate it with the slots class
print.slots <- function(x, ...) {
    slot_display(x) #we have already written this code
} 

#we can modify the play code such that every of it's output would be of the slots class
play <- function() {
    symbols <- symbol.gen()
    structure(score(symbols), symbols = symbols, class = "slots") #structure returns an object with its
    #defined attributes
}
play()

prize.tally <- data.frame(replicate(10, play()))

head(prize.tally)

colnames(prize.tally) <- c("Prize")

prize.tally$Times <- 1:length(prize.tally$Prize)

tail(prize.tally)

amount.lost <- (max(prize.tally$Times) - sum(prize.tally$Prize))
amount.lost

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
    score <- function (symbols) {
        # identify case
        same <- symbols[1] == symbols[2] && symbols[2] == symbols[3]
        bars <- symbols %in% c("B", "BB", "BBB")
        # get prize
        if (same) {
            payouts <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25,
                         "B" = 10, "C" = 10, "0" = 0)
            prize <- unname(payouts[symbols[1]])
        } else if (all(bars)) {
            prize <- 5
        } else {
            cherries <- sum(symbols == "C")
            prize <- c(0, 2, 5)[cherries + 1]
        }
        # adjust for diamonds
        diamonds <- sum(symbols == "DD")
        prize * 2 ^ diamonds
    }
    
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
rm("print.slots")

#EXPECTED VALUE
#Use the Expected Value Method to find out if a die is fair or not...

#create the die
die <- c(1, 2, 3, 4, 5, 6)

#Use the expand.grid function to get all the possible combinations of rolling two dice
rolls <- expand.grid(die, die)
rolls #expand.grid outputs a dataframe

#Next, calculate the sum of the random numbers to get a single value

rolls$value <- rolls$Var1 + rolls$Var2

#Now, calculate the probability of rolling each combination by using the basic probability
#rule: P(A & B & C...) = P(A) * P(B) * P(C) *...

#Let's use a lookup table to store the probabilities associated with each random number

prob <- c("1" = 1/8, "2" =1/8, "3" = 1/8, "4" = 1/8, "5" = 1/8, "6" = 3/8)
prob
#Now to know which probability associated with each numner in rolls, subset with prob
#like this...
rolls$prob1 <- prob[rolls$Var1]
rolls$prob2 <- prob[rolls$Var2]
rolls

#We can now calculate the probability of rolling the different combinations by multiply
#ing prob1 with prob2
rolls$prob <- rolls$prob1 * rolls$prob2
rolls

#We can now calculate expected value since we have the outcome, the value of the outcome
#and the probability of each outcome now...

expectedvalue <- sum(rolls$value * rolls$prob)
expectedvalue #the expected value of rolling the two dice is 8.25
#By contrast, the expected value of a fair die is 7... Meaning the other dice are not fair
#they are loaded

#EXPECTED VALUE OF THE SLOT MACHINE: The payout rate of the machine assuming infinite
#spins

#First Step--- List all the possible combinations of three symbols from the wheel
wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")

symbol.combine <- expand.grid(wheel, wheel, wheel, stringsAsFactors = FALSE) #There are
#about 343 combination possibilities...

prob <- c("DD" = 0.03, "7" = 0.03, "BBB" = 0.06,
          "BB" = 0.1, "B" = 0.25, "C" = 0.01, "0" = 0.52) #probabilities of each

#Second Step--- Look up the probabilites for each of those outcomes

symbol.combine$prob1 = prob[symbol.combine$Var1]
symbol.combine$prob2 = prob[symbol.combine$Var2]
symbol.combine$prob3 = prob[symbol.combine$Var3]

#Step 3--- Calculate the probabilities of the combination... Since they are independent
#events, we can use P(A $ B & C...) = P(A) * P(B) * P(C)...
symbol.combine$prob <- symbol.combine$prob1 * symbol.combine$prob2 * symbol.combine$prob3
sum(symbol.combine$prob) #the combination probabilities add up to 1 so it must be correct
head(symbol.combine)

#Step 4--- Calculate the associated value of the outcome. Here, the prize of each of 
#the combinations
#We can use:
symbols <- c(symbol.combine[1,1], symbol.combine[1, 2], symbol.combine[1, 3])
score(symbols)
#Running the two lines of code above for all 343 observation is tedious. We employ a 
#for loop to help automate that task

#First, create the prize column and fill it up with NAs
symbol.combine$prize <- NA

for (i in 1:nrow(symbol.combine)) {
    #fantastic way to run a for-loop
    symbols <- c(symbol.combine[i, 1], symbol.combine[i, 2], symbol.combine[i, 3])
    symbol.combine$prize[i] <- score(symbols)
}

head(symbol.combine)

#Now, that we have the possible outcomes and the value (the prize) as well as the proba-
#bility of each outcome we can calculate expected value:
sum(symbol.combine$prize * symbol.combine$prob) #payout rate of this machine appears
#to be about 54% on the dollar....

#This is confirmed by my lost.to.slot simulation which reveals that if someone spins
#the slot machine for about 1,000,000 consecutive times with $1 per spin, he's going to
# get about $538,372 and lose $461628... Of course in reality, one person cannot play
# that number of times consecutively...What it does mean is that if the house collects
# about a $100 from players in the long run, it gets to keep about $46... Wow...
lost.to.slot(1000000, 1)

#The Casino that has this slot machine says their payout rate is 92% on the dollar so 
#let's investigate

#The problem here is that the 'diamond' symbol is actually a wild card and that complica
#tes the picture. We need to rewrite the score function to treat DD as a wild card.

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
} #this treats DD as the wild card that it is...

#Recalculate the expected value now... All the steps can be reviewed above (where
#we first calculated it)
symbol.combine2 <- expand.grid(wheel, wheel, wheel, stringsAsFactors = FALSE)

#individual probabilities
symbol.combine2$prob1 <- prob[symbol.combine2$Var1]
symbol.combine2$prob2 <- prob[symbol.combine2$Var2]
symbol.combine2$prob3 <- prob[symbol.combine2$Var3]

#joint probability
symbol.combine2$prob <- symbol.combine2$prob1 * symbol.combine2$prob2 * symbol.combine2$prob3

#calculate prize using the score function in a for loop
symbol.combine2$prize <- NA

for (i in 1:nrow(symbol.combine2)) {
    symbol.combine2$prize[i] <- score(c(symbol.combine2[i, 1], symbol.combine2[i, 2], symbol.combine2[i, 3]))
}

head(symbol.combine2)
tail(symbol.combine)
#Expected Value
sum(symbol.combine2$prob * symbol.combine2$prize) #payout rate is now about 93.4% on 
#the dollar considering the diamond is a wildcard... #recheck that wildcard score code
