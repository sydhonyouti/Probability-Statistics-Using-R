# Sydney Honyouti

#Problem 1: Sketch the p.d.f. ofthe x^2 distribution with m-degrees of
# freedom for each of the following values of m. Locate the
# mean, the median, and themodeoneach sketch. (a) m = 1, (b) m = 2, (c) m = 3
# (d) m = 4

# Mode Function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
# (a) m = 1
mDeg_data <- rchisq(1000,1)
hist(mDeg_data,breaks = 30)
# Mean
data_mean = mean(mDeg_data)
data_mean
# Median
data_median = median(mDeg_data)
data_median
# Mode
data_mode = Mode(mDeg_data)
data_mode

#(b) m = 2
mDeg_data <- rchisq(1000,2)
hist(mDeg_data,breaks = 30)
# Mean
data_mean = mean(mDeg_data)
data_mean
# Median
data_median = median(mDeg_data)
data_median
# Mode
data_mode = Mode(mDeg_data)
data_mode

#(c) m = 3
mDeg_data <- rchisq(1000,3)
hist(mDeg_data,breaks = 30)

data_mean = mean(mDeg_data)
data_mean

data_median = median(mDeg_data)
data_median

data_mode = Mode(mDeg_data)
data_mode

#(d)
mDeg_data <- rchisq(1000,4)
hist(mDeg_data,breaks = 30)

data_mean = mean(mDeg_data)
data_mean

data_median = median(mDeg_data)
data_median

data_mode = Mode(mDeg_data)
data_mode
# -----------------------------------------------------------------------------#
#Problem 5:
# First we can generate 1000 random numbers from a t-distribution with 20 degrees
# of freedom.
td <- rt(1000,20)

#Calculating the variance should be fairly close to m/m-2
var(td)

m = 20
m/(m-2)

# -----------------------------------------------------------------------------#
#Problem 6:
# 20 Degrees of Freedom
td <- rt(100000,20)

hist(td,breaks=150,prob=TRUE,col='lightblue')
lines(density(td),col='firebrick',lwd='3') 

#25 Degrees of Freedom
td <- rt(100000,25)

hist(td,breaks=150,prob=TRUE,col='lightblue')
lines(density(td),col='firebrick',lwd='3') 

#30 Degrees of Freedom
td <- rt(100000,30)

hist(td,breaks=150,prob=TRUE,col='lightblue')
lines(density(td),col='firebrick',lwd='3') 
# -----------------------------------------------------------------------------#
#Problem 7:

#Using 7 degrees of free some because level of significance is 8 - 1 = 7
x <- c(3.1,3.5,2.6,3.4,3.8,3.0,2.9,2.2)

mean(x)
sd(x)
# (a) .90
qt(.90,7)

left = mean(x)-qt(.90,7)*sd(x)/sqrt(10)
right = mean(x)+qt(.90,7)*sd(x)/sqrt(10)
left
right

#(b) .95
qt(.95,7)

left = mean(x)-qt(.95,7)*sd(x)/sqrt(10)
right = mean(x)+qt(.95,7)*sd(x)/sqrt(10)
left
right

#(c) .99
qt(.99,7)

left = mean(x)-qt(.99,7)*sd(x)/sqrt(10)
right = mean(x)+qt(.99,7)*sd(x)/sqrt(10)
left
right
# -----------------------------------------------------------------------------#
#Problem 8:

x <- c(186,181,176,149,184,190,158,139,175,148,152,111,141,
       153,190,157,131,149,135,132)

mean(x)
sd(x)

temp = sd(x)/sqrt(20)
temp

#Finding the 90% confidence for the mean
qt(.90,19) #19 degrees of freedom

left = mean(x)-qt(.90,19)*sd(x)/sqrt(10)
right = mean(x)+qt(.90,19)*sd(x)/sqrt(10)
left
right
# -----------------------------------------------------------------------------#
#Problem 9: Generate 25 random numbers coming from a normal distribution 
#with mean = 100 and standard deviation = 4.  Find a 95% confidence interval 
#using the t-distribution and compare it to a 95% confidence using the normal 
#distribution. Repeat the experiment for a sample size of 400.

ran_num <- rnorm(25,100,4)

qt(.975,9)
# The 95% confidence interval starts at 
left = mean(ran_num)-qt(.975,9)*sd(ran_num)/sqrt(10) # and ends at 
right = mean(ran_num)+qt(.975,9)*sd(ran_num)/sqrt(10)
left
right

# Sample size of 400
ran_num <- rnorm(400,100,4)

qt(.975,9)
# The 95% confidence interval starts at 
left = mean(ran_num)-qt(.975,9)*sd(ran_num)/sqrt(10) # and ends at 
right = mean(ran_num)+qt(.975,9)*sd(ran_num)/sqrt(10)
left
right
# -----------------------------------------------------------------------------#
#Problem 10: Use R to design the following experiment.  Generate 25 random 
#numbers coming from a normal distribution with mean = 10 and standard 
#deviation = 2.  Use the numbers to create a 90% confidence interval for 
#the mean.  Did the confidence interval capture the mean?  Repeat this 100 
#times to check to see if you capture the mean about 90% of the time (this is 
#what a 90% confidence interval means). 

ran_num <- rnorm(25,10,2)
qt(.95,9) #
left = mean(x)-qt(.95,9)*sd(x)/sqrt(10) 
right = mean(x)+qt(.95,9)*sd(x)/sqrt(10)
left
right
