# Sydney Honyouti

#Problem 1: Page 325 #3
# Sketch the p.d.f. of the gamma distribution for each of
# the following pairs of values of the parameters α and β:
# α = 1/2 and β = 1, (b) α = 1 and β = 1, (c) α = 2 and β = 1.

x <- seq(0,10,.01)
y <- dgamma(x,1/2,1)  #(a)
y2 <- dgamma(x,1,1)   #(b)
y3 <- dgamma(x,2,1)   #(c)

plot(x,y,col="blue",type='l',ylim=c(0,1),main="PDF's of Three Gamma Distributions",
     sub="Alpha varying 1/2 to 2 and Beta = 1")
lines(x,y2,col="darkgreen")
lines(x,y3,col="orange")
legend(7, 1, legend=c("G(1/2,1)", "G(1,1)", "G(2,1)"),
       col=c("blue", "darkgreen","orange"), text.font=4, bg='gray90',lty=c(1,1,1))

#Problem 2: Page 325 #4
# Determine the mode of the gamma distribution with parameters α and β.
# You can find the mode of gamma if you distribute
# Starting with:
# (1/Gamma(Alpha)*Beta^Alpha)*Beta^Alpha*x^Alpha-1*e^-Betax
# = (1/Gamma(Alpha))*Beta^Alpha*d(x^Alpha-1*e^Betax)/dx
# = (1/Gamma(Alpha)*Beta^Alpha)*e^-Betax-x^Alpha(Betax)^-Alpha*Gamma(Alpha+1Betax)
# = (Betax/Gamma(Alpha)*e^-Betax)*(Alpha-1-Betax)x^Alpha-2

# Using only (Alpha-1-Betax) and we get x by itself, we get,
# The mode of the Gamma distribution is (alpha - 1) / beta
alpha <- 3
beta <- 24

gamma_mode <- (alpha - 1)/beta
gamma_mode

#Problem 3: Page 325 #5
# Sketch the p.d.f. of the exponential distribution for each
# of the following values of the parameter β: (a) β = 1/2, (b)β = 1, and (c) β = 2.

x <- seq(0,100,.01)
y <- dexp(x,1/2)  #(a)
y2 <- dexp(x,1)   #(b)
y3 <- dexp(x,2)   #(c)

plot(x,y,col="red",type='l',ylim=c(0,1),main="PDF's of Three Exponential Distribution",
     sub="Beta varying between 1/2 to 2")
lines(x,y2,col="blue")
lines(x,y3,col="orange")
legend(7, 1, legend=c("β(1/2)", "β(1)", "β(2)"),
       col=c("red", "blue","orange"), text.font=4, bg='gray90',lty=c(1,1,1))


#Problem 4: Page 325 #9; explain reasoning and use R to obtain the result.

# Suppose that the length of life of the ﬁrst component
# measured in hours, has the exponential distribution with 
# parameter β = 0.001, the length of life of the second
# component has the exponential distribution with parameter β = 0.003, and 
# the length of life of the third component has the exponential distribution 
# with parameterβ = 0.006. Determine the probability that the system will not fail before 100 hours

#Referred to 5.7.9
#Seeing that exponential distribution is Beta*exp(-Beta*x) = exp(-Beta*t)
#Using this formula I added each Beta parameter inside the exponential and multiplied with 100 hours
#We find the probability as such:
Beta <- -(.001 + .006 + .003)
Beta <- Beta * 100
exp(Beta)

#Problem 5: Page 348 #3 
# This problem requires a computer program because the calculation is too 
# tedious to do by hand. Extend the calculation in Example 6.1.1 to the case 
# of n = 200 ﬂips. That is, let W be the number of heads in 200 ﬂips of a 
# fair coin, and compute

# Pr(.4 <= n/200 <= .6)
# = Pr(80 <= n <= 120)

pnorm(120, mean = 100, sd = 15) 
pnorm(80, mean = 100, sd = 15)


#Problem 6: Page 358 #3

#Referred to Theorem 6.2.2 Chebyshev Inequality
#Seeing that Pr(|X-E(x)| >= t) <= Var(x)/2
# We can use this to solve for t
E <- 10
t = 13 - E

#Now we can substitute t with the rest of the equation
# t^2Pr(|X-E(x)| >= t) <= Var(x)
# 9*Pr(|X-E(x) >= t) <= Var(x)/2
t <- t^2
# = Pr(-t <= (X-E(x)) >= t)
# Since Pr(X ≤ 7) = 0.2, and Pr(X ≥ 13) = 0.3
# We can add up both Pr(X ≤ 7) = 0.2 + Pr(X ≥ 13) = 0.3
Pr_Sum <- .2 + .3

#This means that Pr(-t <= (X-E(x)) >= t) = .5
#Now we can substitute it and multiply by 9
t * Pr_Sum
#This gives us 4.5 or 9/2 

#Proving this we can see if we divide 9/2
Var <- 9/2
Var

#Problem 7: Page 359 #5; use R to demonstrate the result with an example.

# Using Pr(|X-E(x)| >= t) <= Var(x)^2/2t
# We can substitute sigma with t and mu with E(x) and Var(x)
# Using Pr(|X-mu| >= sigma) <= mu^2/2sigma
#We can substitute sigma with 2Sigma giving us: 1 - 1/4n >= .99 (at least a probability of .99)
# Getting n by itself we can get 1/.04
n <- 1/.04
n

#This means that the sample size needs to be at least 25

#Problem 8: Page 425 #7
# B = 2/Xn


#Problem 9: Page 426 #11
# theta 1 of min = {x1, x2, ... xn}
# theta 2 of max = {x1, x2, ..., xn}

#Problem 10:
#In the urdata.csv file, the y variable represents lifetimes of an electronic 
#component in hours.  Use R to propose a distribution that fits this data and 
#explain your reasoning.  Make a graph that shows the data along with your proposed distribution.

# I believe that all the y values in the data set will come to a exponential distribution.
# I say this because when graphing the data in a histogram format it starts from 300 and
# the data significantly declines. I also referred to Problem #3  of the hw
# to see that it is a pattern of how a exponential distribution graph looks like

UD <- read.csv("urdata.csv")

str(UD)
str(UD$y)

hist(UD$y,breaks=20)
xbar <- apply(UD,1,mean)

hist(xbar)


#Problem 11:
gamma(1/2)
sqrt(pi)
