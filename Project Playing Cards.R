#Let's create an atomic vector of the face names of the cards in a royal flush. 'ace', 'king'l
#'queen', 'jack' and 'ten'
face.names <- c("ace", "king", "queen", "jack", "ten")

#check the datatype of the face.names atomic vector
typeof(face.names) #character

#attributes() #use function attributes() to check info (metadata) about an R object

#Some of the attributes you might want to give an R Object include names, dimensions and 
#class... 
#Assign the names attribute like this: names(object) <- c("character vector")
#Assign the dim attribute like this: dim(object) <- c(n)... length n is the number of dimen
#sions to assign to the object
#For instance: Create a 6-number sequence
die <- c(11:16)
#Does 'die' have a 'names' attribute?
names(die) #No... will give a NULL
#Assign the 'names' attribute to 'die' then...
names(die) <- c("eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen")
#check the attributes of 'die' again
attributes(die) #die now has a 'names' attribute. Check it using the names() function
names(die)

die + 12 #the attributes doesn't affect the values of the vector
#To edit the names attribute of any object. All you need do is just assign the attribute
#using names(), like we did earlier

#Does 'die' have a dimensions attribute as of yet?
dim(die) #No

#Assign a dimensions attribute then...
dim(die) <- c(2, 3) #Turns 'die' into a 2-D object (rows, columns)
dim(die)
die #die's now a matrix of two dimensions

#Assign the class attribute to an object by putting the assigned class in a string and 
#as below
classtest <- c(1 + 2i, 2 + 5i)
class(classtest)
class(classtest) <- "character"
#R has implicit classes assigned to objects whose class attribute is not explicitly stated
#the classes are: 'function', 'matrix', 'array', 'numeric', 'character'...

#CREATING A MATRIX of cards
matrix(c( "ace", "king", "queen", "jack", "ten", rep("spades", 5)), nrow = 5, ncol = 2)


#DATE AND TIME
#R handles dates and times with a special classes called teh POSIXct and POSIXt classes
#Its calculates any time as the number of seconds after 12:00 AM on Jan. 1, 1970
#You can check the time of your system by using:
now <- Sys.time()
class(now) #the class is POSIXct and POSIXt
typeof(now) #Its also of the 'double' datatype
#If you want to know how many seconds 'now' is from 12:00 AM on Jan. 1, 1970
unclass(now) #1, 561, 824, 816 seconds from that time
#Ever wondered what time and date 1, 000, 000 seconds was from that time as well?
mill <- 1000000
#assign the POSIXct and POSIXt class to mill
class(mill) <- c("POSIXct", "POSIXt") #coerced into the date time class
mill #1970-01-12 14:46:40 WAT... Turns out a million seconds was just about 11 days added



class((c("a", "b", 1, 2)))


a <- NULL #create a NULL (empty) object by assigning NULL to the object...


#ABOUT LISTS
mylist <- list(name = c("Tope", "Segun"), mylov = c(1, 2, 3)) #lists are 1-D R objects
#that are convenient for keeping anything
mylist$name
mylist$mylov

card <- list("ace", "hearts", 1) #if you wanted to use a vector to keep this, it will
#coerce '1' into the character type

#MY Deck of Cards
carddeck <- read.csv("C:/Users/user/Desktop/Don Timmy/Don Timmy's Work/Career Growth/Data Science Career/MY R Projects/RScripts/Hands-On Programming/deck.csv",
                     stringsAsFactors = FALSE)
View(carddeck)

#check the head of the dataframe
head(carddeck, 10)

#Save the cards in the my present working directory
write.csv(carddeck, "C:/Users/user/Desktop/Don Timmy/Don Timmy's Work/Career Growth/Data Science Career/MY R Projects/RScripts/Hands-On Programming/newdeck.csv",
          row.names = FALSE)

#INDEXING with positive integers
#subsetting in R involves the use of [] with the indices in between those brackets
#Indexes can be written for R in six ways that are slightly different to each other
#Positive Integers
carddeck[1, 1] #data on 1st row of the 1st column
carddeck[1, ] #data on the 1st row
carddeck[c(1, 1), ] #if you repeat the same data, R will produce as many copies has you
#want
#You can index any R object, just know the dimension structure
carddeck$face[1:3] #this is a vector (1-D) and R returns the first 3 entries in it

#if you subset just one column,  R will return a vector, to avoid that set the 'drop'
#argument to FALSE
carddeck[1:2, 1, drop = FALSE]

#Negative Integers Indexing
#Setting an index to negative tells R to exclude that index and return everything outs
#ide it
carddeck[-(2:52), ] #will return every row in all the columns but the 2nd to 52nd rows
#Only one row of course

carddeck[-1, 1:3] #don't include row 1 in the results

#carddeck[c(-1, 1), ] #will throw an error. you can't index with +ve and -ve integers
#at the same time

#Logical Values Indexing
carddeck[1, c(TRUE, TRUE, FALSE)] #first row, and the first and second columns alone
#For it to work for the rows for instance, you have to assign logical values to every
#element
rows <- c(T, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F, F,
          F, F, F, F, F, F, F, F, F, F, F, F, F, F)
carddeck[rows, ] #returns only the first row because it's the only row with a logical
#value of T

#Column Name Indexing
carddeck[1, "face"] #return the  data in the 'face' column of the first row
#write a function to deal the card on top each time
deal <- function(card) {
    card[1, ]
}

#write a function to shuffle the cards each time
shuffle <- function(cards) {
    #function to shuffle the card each time
    random <- sample(1:52, 52) #generate random indices
    cards[random, ] #rearrange the cards using the random indices
}

#YOU can now deal and shuffle
deal(carddeck)
carddeckex <- shuffle(carddeck)
deal(carddeckex)

carddeckmod <- carddeck

#MODIFYING VALUES in R
#You can modify or add a value to any object in R by addressing where you want to put a 
#new value and adding the value to it using the assignment operator
#Add a new variable to the carddeckmod object:
carddeckmod$new <- 1:52

#You can delete any column in a dataframe or any element in a list by assigning NULL to it
carddeckmod$new <- NULL
head(carddeckmod) #Awesome
View(carddeckmod)

#In the card game called 'war', the aces have the highest value, 14. We are going to 
#change their values from 1. Before the card is shuffled, the aces appear every 13
#cards
carddeckmod[c(13, 26, 39, 52), ] #all the four aces cards are addressed

carddeckmod[c(13, 26, 39, 52), 3] #will give us only the values of the aces

carddeckmod$value[c(13, 26, 39, 52)]
#Now, assign 14 to these values. You can either use c(14, 14, 14, 14) or just use a
#single 14 and leave the rest in R's hands
carddeckmod$value[c(13, 26, 39, 52)] <- 14
#Now recheck the values. They've been changed in place
carddeckmod[c(13, 26, 39, 52), ] #14, 14, 14, 14
#You can use this for any R object... Just make sure you get the address correctly

#what if the card deck had been shuffled and the ace cards were not in every 13 cards
#You'd have to use 'logical subsetting'...
carddeckmod2 <- shuffle(carddeckmod)
carddeckmod2$value[carddeckmod2$face == "ace"] <- 14
carddeckmod2[carddeckmod2$face == "ace", ] #Awesome, Fixed!

#Now, we change the card game we are playing... Let's switch to the HEARTS game
#In the game of hearts, every card except the suit of hearts and the queen of spades 
#has a 0 value. The suit of hearts have a value of 1: Assign 1 to them:
carddeckmod3 <- shuffle(carddeckmod2)
#First, change all the values to zero for all the cards
carddeckmod3$value <- 0
head(carddeckmod3)

#Now, change the values for the  suit of hearts
carddeckmod3$value[carddeckmod3$suit == "hearts"] <- 1
#THe queen of spades has a value of 13 in the hearts game
carddeckmod3$value[carddeckmod3$face == "queen" & carddeckmod3$suit == "spades"] <- 13

#this is testing your understanding of logical tests...

w <- c(-1, 0, 1)
x <- c(5, 15)
y <- "February"
z <- c("Monday", "Tuesday", "Friday")

w > 0
x > 10 & x < 20
y == "February"
all(z %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))


#BLACKJACK Card Game
#The value of each number card in blackjack is it's face value. But the face cards have
#a value of ten and the ace cards's value changes depending on the point in the game
#Create another pristine card deck
carddeckmod4 <- carddeck

facecard <- carddeckmod4$face %in% c("king", "queen", "jack") #store the indices of the 
#facecards in 'facecard'
#change the values to 10
carddeckmod4$value[facecard] <- 10
carddeckmod4[facecard, ] #Awesome...
#Treat the ace cards as missing info since it's value depends on the other cards in the 
#player's hands
carddeckmod4$value[carddeckmod4$face == "ace"] <- NA
sum(is.na(carddeckmod4$value)) #Four (4) of the cards now have missing values

#carddeckmod4 is now ready for blackjack, carddeckmod3 is for the hearts game and card
#deckmod2 is for the war game


deal(carddeck)

#ENVIRONMENTS
as.environment("package:stats") #about the package:stats environment

globalenv() #the accessor function for the global environment
baseenv() #accessor function for the base environment
emptyenv() #the highest level parent environment

parent.env(globalenv()) #the parent environment of the global environment
parent.env(baseenv()) #the parent environment of the base environment is the 'EmptyEnv'

ls() #you can list the objects in any environment using ls()
ls.str(globalenv()) #ls.str gives more info about those objects
ls.str(baseenv()) #the base environment contains a lot of objects

globalenv()$carddeck #you can access any objects in any environment by using the dollar
#operator $. It follows that environments are also another type of R objects... Akin
#to lists

#you can also save objects into any environment using the assign() function
assign("awesome", 5, envir = globalenv()) #this saves an object 'awesome" with value 5
#into the global environment akin to:
awesome <- 5
globalenv()$awesome #this will access the value of the 'awesome' object

#ACTIVE ENVIRONMENT
#The environment that R is working most closely with at any given moment...
#Check the current 'active environment' using environment()
environment() #the global environment is most always the active environment for any
#command issued at the command line...

show_env <- function(){
    #function to show what runtime environments and objects were created during
    #the function call
    list(ran.in = environment(),
         parent = parent.env(environment()),
         objects = ls.str(environment()))
}
show_env() #running show_env lets us see the runtime environment that was created...

#the environment in which a function was created will always serve as the 'parent'
#environment to the created 'runtime environment' on creation... 
#check the environment in which a function was created by using 'environment()'
#environment(amelia) #function was created in the 'namespace:Amelia' environment

#objects inside of functions will be stored in the temporary active run enviroments
#create objects in show_env and see the objects
show_env <- function(){
    #function to show what runtime environments and objects were created during
    #the function call
    a <- 1
    b <- 2
    list(ran.in = environment(),
         parent = parent.env(environment()),
         objects = ls.str(environment()))
}
show_env() #the objects were stored in that runtime environment
#the fact that the objects are shielded from the global environment explains why objects
#with the same name as those in the function will not be affected when the function is run

#R will also copy arguments from the global environment into the runtime environment
#as objects with the given values
foo <- "take me to your runtime" #saved in the global environment

show_env <- function(x = foo){
    #function to show what runtime environments and objects were created during
    #the function call
    a <- 1
    b <- 2
    list(ran.in = environment(),
         parent = parent.env(environment()),
         objects = ls.str(environment()))
}
show_env(x = foo) #x copies the value of the argument and assigns it to the object in 
#the runtime environment


#The 'origin environment' will always be the parent of the 'runtime environment'of a function even if
#its not the 'calling/active environment' at the time of call

#Fixing the deal function with and understanding of R's environment tree
#Because the function 'deal' was created at the command line, it has the 'global environment'
#has a parent of it's runtime environment. Meaning? We don't need to put the carddeck
#as an argument before we can use it in the function... If R cannot find the deck in 
#the 'runtime environment', it will look it up in the 'parent environment' i.e. the 
#global environment... and use it in the evaluation of the function... Rewrite the 'deal'
#function
deal <- function() {
    #this function returns the first card in the carddeck
    #Note: we don't have to put the carddeck as an argument because
    #it is present in the 'origin/parent environment' of the function
    carddeck[1, ]
}
deal() #the function call will always return the same cards

#Now, we will use our understanding of R's environment to make sure that each 'deal' call
#takes the top card then immediately puts it away. As such there will be different top
#cards each time the function is called
#A look at what will not work first:
deal <- function() {
    card <- carddeck[1, ] #pick top card
    carddeck <- carddeck[-1, ] #put the top card away
    card #return the top card
}

deal() #the function will continue to return the same top cards because that new 'cardeck'
#object that was created was only created in the temporary runtime environment. but the 
#carddeck in the global environment which is the parent environment was not changed.

#To avoid that, we need to create that modified carddeck object in the 'global environment'
#so that it will update the value of the carddeck object in the global environment and 
#give us a new top card anytime we call the deal function
deal <- function() {
    card <- carddeck[1, ] #pick top card
    assign("carddeck", carddeck[-1, ], envir = globalenv()) #create new carddeck object
    #in the global environment to overwrite the former carddeck
    card #return the first top card
}
deal() #returns the next top card in the next call while putting aside the former top
#card. the deal function has been fixed!
carddeck #keeps reducing until the game finishes


#FIXING the 'shuffle' function now
#create a pristine copy of the carddeck and call it CARDDECK
CARDDECK <- carddeck

#The problem with the previous shuffle function:
shuffle <- function(cards) {
    #function to shuffle the card each time
    random <- sample(1:52, 52) #generate random indices
    cards[random, ] #rearrange the cards using the random indices
}
#instead of really shuffling the carddeck, 'shuffle' creates a 'shuffled copy' of the 
#carddeck...
shuf <- shuffle(cards = carddeck)
shuf #shuffle creates a shuffled copy
carddeck #doesn't at all affect the original carddeck

#THE MISSION now:
#the function should 1) replace the copy of carddeck in the global environment with
#a shuffled CARDDECK... no argument, no output
shuffle <- function() {
    #this function shuffles the CARDDECK and then overwrites the
    #carddeck object
    assign("CARDDECK", CARDDECK[sample(1:52, 52), ], envir = globalenv()) #shuffle
    #the CARDDECK object in the global environment
    assign("carddeck", CARDDECK, envir = globalenv()) #replace the carddeck object
    #with the already shuffled CARDDECK
}

shuffle() #this indeed shuffles the CARDDECK object 
head(carddeck)
deal()

#we actually don't need as much line of code to fix shuffle
shuffle <- function() {
    #this function shuffles the CARDDECK and then overwrites the
    #carddeck object
    assign("carddeck", CARDDECK[sample(1:52, 52), ], envir = globalenv()) #replace the carddeck object
    #with the already shuffled CARDDECK
} #this works as well... the other line was unnecessary

#Since CARDDECK lives in the global environment, shuffle's environment of origin, shuffle
#will be able to find CARDECK at runtime. R will search for CARDDECK first in shuffle's runtime
#environment, and then in shuffle's origin environment-the global environment-
#which is where CARDDECK is stored.
#The second line of shuffle will create a reordered copy of CARDDECK and save it as carddeck in
#the global environment. This will overwrite the previous, nonshuffled version of carddeck.


#We would like the deal() and the shuffle() functions to run independent of the global
#environment which is the parent of the runtime environment as of now. We want to achieve
#what we call CLOSURE. such that no matter what happens to the carddeck in the global
#environment, our functions will not be affected!

#we achieve this by storing the carddeck and the CARDDECK objects in a function that we'll
#call 'setup' and then we are going to store the deal and shuffle functions there as well
#so that the runtime environment of the setup function becomes the parent environment of
#the deal and shuffle functions' runtime environment

setup <- function(carddeck) {
    CARDDECK <- carddeck #create a CARDDECK object
    
    DEAL <- function() { #store the deal function in the setup function
        card <- carddeck[1, ] #take top card
        assign("carddeck", carddeck[-1, ], envir = parent.env(environment())) #update
        #the carddeck object by putting the top card aside. the parent environment is
        #now the setup function runtime environment and not the global environment
        card #return chosen top card
    }
    
    SHUFFLE <- function() {
        assign("carddeck", CARDDECK[sample(1:52, 52), ], envir = parent.env(environment())) #update
        #the carddeck object in the parent environment which is the runtime environment of the 
        #setup function with the shuffled CARDDECK
    }
    
    list(deal = DEAL, shuffle = SHUFFLE) #return the 'deal' and 'shuffle' functions in
    #a list
}

cards <- setup(carddeck) #save the list in the cards object
deal <- cards$deal
shuffle <- cards$shuffle

environment(deal) #the origin environment (parent environment at runtime) 
#of these two functions is no longer the global
#environment. it is the runtime environment of the setup function. nothing that happens
#to the deck in the global environment will affect it. 
environment(shuffle)

rm(carddeck) #remove carddeck from the global environment and deal and shuffle are still
#working because they are actually modifying objects in the environment of the setup
#function

deal()
shuffle()
#MISSION COMPLETED!!!
