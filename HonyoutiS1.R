#Sydney Honyouti

#Problem 1: p. 107 #4; Use R to sketch the pdf
#Solving for c, c = 3/7
c <- 3/7
x <- seq(1,2,.01)
pdfx <- c*x^2
plot(x,pdfx)

#--------------------------------------------------------------------------------------------------#
#Problem 2: p. 116 #15; Use R to sketch the cdf
#Plotting the c.d.f of x
x<-seq(-2,8,length=100)
y<-dunif(x,min=0,max=5)
plot(x,y,type='l')

#--------------------------------------------------------------------------------------------------#
#Problem 3: p.216 #1; Use the definition of the mean and demonstrate with random numbers using R.
#The mean of the problem: (a+b)/2
a<-5                            #Used 5 as a random number
b<-10
mean_definition <- ((a+b)/2)
mean_definition

a<-5
b<-10
x<-runif(1000,a,b)
mean(x)

#--------------------------------------------------------------------------------------------------#
#Problem 4: p. 233 #1; Use the definition of the variance and demonstrate with random numbers using R.
#Solving the problem in the text book: E(x^2) = 1/3 and E(x) = 1/2 based on equation:
#Var(x) = E(x^2) -E(x)^2
#Var(x) = 1/3 - (1/2)^2 = 1/12 or .0833
a<-0
b<-1
x<-runif(1000,a,b)
var(x) #this prints out the complete variance of x, which is .08325 or .0833

#--------------------------------------------------------------------------------------------------#
#Problem 5: p. 315 #1; use R functions; Find the .5,.25,.75.,.1 and .9 quantiles of the
#standard normal distribution
qnorm(c(.5,.25,.75,.1,.9)) #using qnorm because we want quantiles of the std

#--------------------------------------------------------------------------------------------------#
#Problem 6: P.315 #2; Suppose that X has the normal distribution for which
#the mean is 1 and the variance is 4. Find the value of each of the following probabilities:

#a) Pr(X <= 3)
pnorm(4,1,3) #Answer is .8413 when rounded to the nearest ten thousandth place

#b) Pr(X > 1.5)

#We first need to subtract 1.5 - 1 and divide it by 2; Pr(1.5 - 1 / 2)
#This gives us Pr(.25) but we still need to minus by one
#Referenced to Example 5.6.6 in the textbook
1 - pnorm(.25) #We minus 1 because it 1.5 was over 1
#Answer is .4013 when rounded

#c) Pr(X = 1)

#Doing the calculations, it should be zero
q_norm <- 0 #Answer is 0
q_norm

#d) Pr(2 < x < 5)
x <- pnorm(2) #we get 2 from Pr(5-1/2) = Pr(4/2) = Pr(2)
y <- pnorm(.5) #we get .5 from Pr(2-1/2) = Pr(1/2) = Pr(.5)
x-y #Answer should be .2857 when rounded

#e) Pr(x >= 0)

#We first need to subtract 0 with 1 and divide it by 2; Pr(0 - 1 / 2)
#This gives us Pr(Z >= -.5) and we switch the signs and make .5 positive
#Then this gives us Pr(Z <= .5), now we can use the functions in R
pnorm(.5) #Answer should be .6915 when rounded

#f) Pr(-1 < x <.5)

#We first need to separate Pr(-1) and Pr(.5) by subtracting 1 and dividing by 2
#Pr(-1 - 1 / 2) and Pr(.5 - 1 / 2); It gives us Pr(-1 < x < -.25)
#Using symmetric property, you can get Pr(.25 < x < 1)
#Now we can use the R functions
x <- pnorm(1)
y <- pnorm(.25)
x-y #Answer should be .2426 when rounded

#g) Pr(|x| <= 2)

#Solving for both Pr's. We know that |x| will have -2 and 2
#It gives us Pr(-2-1/2) and Pr(2-1/2); Pr(-1.50 <= x <= .50)
x <- pnorm(.5) #Here we can do Pr(.5)
y <- (1 - pnorm(1.50)) #We have to minus 1 because it is greater than 1
x-y #Answer should be .6247 when rounded

#h) Pr(1 <= -2x + 3 <= 8)

#Before we can find the Pr's we first need to add 3 to both sides. Then we must divide 2 but this
#is not going to find the Pr yet.
#Pr(1-3/2 <= -x <= 8-3/2), then we get Pr(-1 <= -x <= 2.5)
#Now we can find the Pr's; Pr(-1.75 <= x <= 0)
x <- pnorm(0)
y <- (1 - pnorm(1.75)) # We minus because it is over 1
x-y #Answer should be .4599 when rounded

#--------------------------------------------------------------------------------------------------#
#Problem 7: Let X1,X2, and X3 be independent lifetimes of memory chips. Suppose that each Xi has 
# the normal distribution with mean 300 hours and standard deviation 10 hours.
# Compute the probability that at least one of the three chips lasts at least 290 hours

#I referenced Example 5.6.6 to help me solve this problem
#Using z = x - Mu / sigma. Here x = 290 hours, Mu = 300 hours, and sigma = 10 hours
#Since there is x1, x2, x3, we cube the equation because there are 3 independent x's:
x <- (290-300)/10
x <- pnorm(x)
1-(x^3) #Answer should be .9960 when it is rounded

#--------------------------------------------------------------------------------------------------#
#Problem 8:Suppose that the measured voltage in a certain electric circuit has the normal 
# distribution with mean 120 and standard deviation 2. If three independent measurements
# of the voltage are made, what is the probability that all three measurements will lie between 116 and 118?
#Referring to the problem above; we have 3 independent measurements (so we have to cube)
#Using the normal distribution equation we can use it in the standard normal distribution

#x is 116V and 118V, Mu = 120 and sigma is 2
#We can visualize it by seeing it like Pr(116 <= x <= 118)^3
#Then we can break it up like (Pr(x <= 118) - Pr(x <= 116))^3
#Answer I got was: .0025

x <-pnorm((118-120)/2)
y <-pnorm((116-120)/2)
(x-y)^3

#--------------------------------------------------------------------------------------------------#
#Problem 9: Referring to the the last problem above
#First found the total sigma values: 20 + 14 + 26 - 4 = 56 <- Minus because I added the boxes with 2
#Then I found the total sigma values: .04 + .01 + .04 = .09
#Both of these values we will use in the equation: z = x - mu / signma
#I also separated the values into different Pr() functions:
#Pr(55.7 <= x <= 56.3) <- we use these values because it is between those two points
#Pr(x <= 55.7) - Pr(x <= 56.3)

#Answer I got was .6827

x <- pnorm((56.3-56)/sqrt(.09))
y <- pnorm((55.7-56)/sqrt(.09))
x-y

#--------------------------------------------------------------------------------------------------#
#Problem 10:Make histograms for the variables in the urdata.csv file.  
#Make these histograms as nice as possible using optional parameters that you have seen or learned about
getwd()
file_data <- read.csv("urdata.csv")
str(file_data)

#histogram of data X
hist(file_data$x, main="Histogram of file_data from value X",xlab="X Data", col="darkmagenta")
#Histogram of data Y
hist(file_data$y,main="Histogram of file_data from value Y", xlab= "Y Data", col="darkmagenta")
#Histogram of data Z
hist(file_data$z,main="Histogram of file_data from value Y", xlab= "Z Data", col="darkmagenta")

#Still needed practice with the lines for the graphs, wouldn't work for me 
