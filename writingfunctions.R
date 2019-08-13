#Let's write a function that rolls a pair of die and returns the sum of their output
roll <- function(die = 1:6) { #die is a default argument
    dice <- sample(die, size = 2, replace = TRUE) #2 draws from the roll of the die
    sum(dice) #this is what the function returns
}

roll() #test the roll function
#you can define the argument as another thing, but if you don't put anything it will use
#the default value

roll #if you don't put parentheses in front of the function, it will display the content
#of the function to you
#
add <- function(n, m) {
    #input two numbers n, m
    #output sum of n and m
    n + m
}

add(2, 3)
?add

greet <- function() {
    "Welcome Dear"
}

greet()

#want to see if this changes are made
