#Non-Vectorized Function
abs_loop <- function(vec){
    for (i in 1:length(vec)) {
        if (vec[i] < 0) {
            vec[i] <- -vec[i]
        }
    }
    vec
}

#Vectorized Function
abs_sets <- function(vec){
    negs <- vec < 0
    vec[negs] <- vec[negs] * -1
    vec
}

long <- rep(c(-1, 1), 5000000)

system.time(abs_sets(long))
system.time(abs_loop(long)) #
system.time(abs(long)) #R's base function for absolute numbers is the fastest

mean(numeric(replicate(5, system.time(abs_sets(long)))))
abs

#this is a non vectorised function that changes the value of the cards
change_symbols <- function(vec){
    for (i in 1:length(vec)){
        if (vec[i] == "DD") {
            vec[i] <- "joker"
        } else if (vec[i] == "C") {
            vec[i] <- "ace"
        } else if (vec[i] == "7") {
            vec[i] <- "king"
        }else if (vec[i] == "B") {
            vec[i] <- "queen"
        } else if (vec[i] == "BB") {
            vec[i] <- "jack"
        } else if (vec[i] == "BBB") {
            vec[i] <- "ten"
        } else {
            vec[i] <- "nine"
        }
    }
    vec
}
vec <- c("DD", "C", "7", "B", "BB", "BBB", "0")
change_symbols(vec)
## "joker" "ace" "king" "queen" "jack" "ten" "nine"
many <- rep(vec, 1000000)
system.time(change_symbols(many)) #time it took the system to evaluate non vectorised
#change.symbols

#vectorised version
change_vec <- function (vec) {
    vec[vec == "DD"] <- "joker"
    vec[vec == "C"] <- "ace"
    vec[vec == "7"] <- "king"
    vec[vec == "B"] <- "queen"
    vec[vec == "BB"] <- "jack"
    vec[vec == "BBB"] <- "ten"
    vec[vec == "0"] <- "nine"
    vec
}

system.time(change_vec(many)) #time it took the system to evaluate vectorised version of 
#change symbols... It's like lightning ... Why? R didn't need to go over a loop, examining
#each case one after the other then moving to the next.... It evaluated everything at the 
#same time.

#Lookup tables will do much better in fact
change_vec2 <- function(vec){
    tb <- c("DD" = "joker", "C" = "ace", "7" = "king", "B" = "queen",
            "BB" = "jack", "BBB" = "ten", "0" = "nine")
    unname(tb[vec])
}

system.time(change_vec2(many)) #And they did
