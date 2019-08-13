library(ggplot2)

#qplot makes quick plots with loads of default settings
x <- c(-1, -0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1)
x
## -1.0 -0.8 -0.6 -0.4 -0.2 0.0 0.2 0.4 0.6 0.8 1.0
y <- x^3
y
## -1.000 -0.512 -0.216 -0.064 -0.008 0.000 0.008
## 0.064 0.216 0.512 1.000
qplot(x, y) #if you give qplot two sets of vectors, it will plot a scatter plot

x <- c(1, 2, 2, 2, 3, 3)
qplot(x, binwidth = 1) #one argument and it will plot an histogram

#We want to be sure that the die we created is a fair die. So, we roll it 10,000 times and
#plot the sums

rolls <- replicate(10000, roll())
qplot(rolls, binwidth = 1) #result is a normal distribution, our die is a fair die. Not we
#ighted. Each sum occurs in proportion to the number of combinations that form them


#We want to weight the dice now such that we increase the probability for a 6 to 3/8
roll2 <- function(die = 1:6) {
    dice <- sample(die, size = 2, replace = TRUE, prob = c(1/8, 1/8, 1/8, 1/8, 1/8, 3/8))
    sum(dice)
}

roll2()
#roll the dice 10000 times and plot the distribution
rolls2 <- replicate(10000, roll2())
qplot(rolls2, binwidth = 1) #This is a left-skewed distribution, favouring higher sums

