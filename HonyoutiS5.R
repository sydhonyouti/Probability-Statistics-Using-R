# Sydney Honyouti

# Problem 1: Investigate the “randomness” of your favorite pseudo-
# random number generator as follows. Simulate 200
# pseudo-random numbers between 0 and 1 and divide the
# unit interval into k = 10 intervals of length 0.1 each. Apply
# the χ2 test of the hypothesis that each of the 10 intervals
# has the same probability of containing a pseudo-random number.

# Generate 200 random numbers ranging from 0 to 1
x <- runif(200,0,1)

# Need to make the the random numbers into a table while also categorizing
# it into 10 sections by .1
y = table(cut(x,c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1.0)))

# Since we have the y categorized how we need to, we can use the chi square test
chisq.test(y)

# Concludes that if the chi-squared distribution with 9 degrees of freedom
# then the probability is greater than the significance level, .2
# So this means that each of the 10 intervals has the same probability of
# containing a pseudo-random number

#------------------------------------------------------------------------------#
# Problem 2: According to a simple genetic principle, if both the
# mother and the father of a child have genotype Aa, then
# there is probability 1/4 that the child will have genotype
# AA, probability 1/2 that she will have genotype Aa, and
# probability 1/4 that she will have genotype aa. In a random
# sample of 24 children having both parents with genotype
# Aa, it is found that 10 have genotype AA, 10 have genotype
# Aa, and four have genotype aa. Investigate whether the
# simple genetic principle is correct by carrying out a χ2 test
# of goodness-of-ﬁt.

numberOfChildren = 24
AA = 1/4
Aa = 1/2
aa = 1/4

# Multiply the number of children with the genotypes so it can be used inside
# the chi-square test
AA = AA * numberOfChildren
Aa = Aa * numberOfChildren
aa = aa * numberOfChildren

#Create the vector
genotypeVect <- c(AA,Aa,aa)

chisq.test(genotypeVect)

# Concludes that if the chi-squared distribution with 2 degrees of freedom
# hen the probability is greater than the significance level, .05, so we fail
# to reject the null hypothesis. Because we do not have enough evidence that
# at least one of the genotype is different

#------------------------------------------------------------------------------#
# Problem 3: It is known that 30 percent of small steel rods produced
# by a standard process will break when subjected to a load
#of 3000 pounds. In a random sample of 50 similar rods produced by a new 
# process, it was found that 21 of them broke when subjected to a load of 
# 3000 pounds. Investigate the hypothesis that the breakage rate for the 
# new process is the same as the rate for the old process by carrying out a
#chi-squared test of goodness-of-ﬁt.

p = .3 # represents the small steel rods
n = 50 # represents the number of steel rods produced by new process
Xn = 21/50

top = n*(Xn - p)^2
bottom = p * (1 - p)

result = top/bottom

# If we subtract 1 from the chi-squared distribution with one degree of freedom
# we can conclude that the breakage rate for the new process is different at
# least one rate for the old process

#------------------------------------------------------------------------------#
# Problem 4: Chase and Dummer (1992) studied the attitudes of school-aged 
# children in Michigan. The children were asked which of the following was most 
# important to them: good grades, athletic ability, or popularity. Additional
# information about each child was also collected, and Table 10.16 shows the 
# results for 478 children classiﬁed by sex and their response to the survey 
# question. Test the null hypothesis that a child’s answer to the survey 
# question is independent of his or her sex.

boys = c(117,60,50)
girls = c(130,30,91)

gender = rbind(boys,girls)
colnames(gender) <- c("Good Grades", "Athletic Ability", "Popularity")
gender

chisq.test(gender, correct = FALSE)

# Since  p-value is smaller than the significance level we can reject the Ho
# So it concludes that the proportion of child's answer to the survey
# question is not independent of his or her sex

#------------------------------------------------------------------------------#
# Problem 5: Suppose that an experiment is carried out to see if there is any 
# relation between a man’s age and whether he wears a mustache. 
# Suppose that 100 men, 18 years of age or older, are selected at random, 
# and each man is classiﬁed according to whether or not he is between
# 18 and 30 years of age and also according to whether or not he wears a 
# mustache. The observed numbers are given in Table 10.17. Test the 
# hypothesis that there is no relationship between a man’s age and 
#whether he wears a mustache.

between18and30 = c(12,28)
over30 = c(8,52)

age = rbind(between18and30,over30)
colnames(age) <- c("Wears a mustache", "Does not have a mustache")
age

chisq.test(age, correct = FALSE)

# Conclude that since the p-value is smaller than .05, we can reject Ho. So this
# means that there is a relationship between a man's age and whether he wears a
# mustache

#------------------------------------------------------------------------------#
# Problem 6: Suppose that 300 persons are selected at random from
# a large population, and each person in the sample is classiﬁed according to 
# blood type, O, A, B,or AB, and also according to Rh, positive or negative. 
# The observed numbers are given in Table 10.18. Test the hypothesis that the
# two classiﬁcations of blood types are independent.

rhPostive <- c(82,89,54,19)
rhNegative <- c(13,27,7,9)

bloodType = rbind(rhPostive, rhNegative)

chisq.test(bloodType, correct = FALSE)

# Concludes that the proportion of the two classifications of the two
# blood types are not independent

#------------------------------------------------------------------------------#
# Problem 7: Compute the MLE’s 𝑋 and 𝜎 and use them to test whether the
# hypothesis that the test scores in the file TestScoresD1.csv come from
# a 𝑁(𝑋,𝜎) distribution using the technique developed in class.

UD <- read.csv("TestScoresD1.csv")
UD

# This is not finished

#------------------------------------------------------------------------------#
# Problem 8: Suppose that the pair (X, Y) is uniformly distributed on
# the interior of a circle of radius 1. Compute ρ(X, Y).

# Not sure how to demonstrate this in R
# But if you compute the covariance of X & Y you will find that
# X & Y should not be correlated
x <- c(1, 3, 5, 10)
y <- c(2, 4, 6, 20)

cor(x, y)

#------------------------------------------------------------------------------#
# Problem 9: For all random variables X and Y and all constants a, b, c, and d, 
# show that Cov(aX + b, cY + d) = ac Cov(X, Y).


#------------------------------------------------------------------------------#
# Problem 10: Let X and Y be random variables such that 0 <σ2
# X <∞ and 0 <σ2 Y <∞. Suppose that U = aX + b and V = cY + d, 
# where a 	= 0 and c 	= 0. Show that ρ(U, V) = ρ(X, Y) if
# ac > 0, and ρ(U, V) =−ρ(X, Y) if ac < 0.
