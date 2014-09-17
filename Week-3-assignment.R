# Matthew Miesle
# CUNY SPS
# IS 607 SECT 01
# Week 3 Assignment

#### Prob 1 ####
# 1. Write a function that takes a vector as input and returns the
# number of missing values in the vector.
func1 <- function(v1)
{
    length(which(is.na(v1)))
}


#### Prob 2 ####
# 2. Write a function that takes a data frame as input and returns a 
# named vector with the number of missing values in each column of the
# data frame. (The names of the entries should be the corresponding 
# column names of the data frame.) You may use the function from the 
# previous question as part of your solution.
func2 <- function(df2)
{
    (rv2 <- apply(df2, 2, func1))
}


#### Prob 3 ####
# 3. Write a function that takes a numeric vector as input and uses
# it to determine the minimum, the maximum, the mean, the median, 
# the first quartile, the third quartile, the standard deviation of 
# the vector, and the number of missing values. Do not use any built-in
# functions to do this. Return a named list with the eight desired values
# in an order you deem best. (You may, if you like, use the function 
# you wrote for question 1.)

# interpretation of problem statement is that functions that provide the
# desired values immediately are not allowed to be used
# This specifically means the following shouldn't be used:
# summary(), min(), max(), median(), mean(), max(), sd()

# sort() is being considered acceptable to use

func3 <- function(nv3)
{
    sortnv3 <- sort(nv3)
# built-in sort() removes NA
    lensnv3 <- length(sortnv3)
    minnv3 <- sortnv3[1]
    maxnv3 <- sortnv3[lensnv3]
    meannv3 <- sum(sortnv3)/lensnv3
    if(lensnv3 %% 2 == 0)
    {
        mediannv3 <- (sortnv3[lensnv3 %/% 2] + sortnv3[lensnv3 %/% 2 + 1])/2
#         firstqrt <- (sortnv3[lensnv3 %/% 4] + sortnv3[lensnv3 %/% 4 + 1])/2
#         thirdqrt <- (sortnv3[3 * lensnv3 %/% 4] + sortnv3[3 * lensnv3 %/% 4 + 1])/2
    }else
    {
        mediannv3 <- sortnv3[lensnv3 %/% 2 + 1]
#         firstqrt <- sortnv3[lensnv3 %/% 4 + 1]
#         thirdqrt <- sortnv3[3 * lensnv3 %/% 4 + 1]
    }
# quartiles were not implemented
# beginning of assignments are commented out

    stdev <- sqrt (sum((sortnv3 - meannv3) ^ 2) / lensnv3)
# the value calculated here seems to deviate from the built-in sd()
# but I couldn't figure out why

    list(Min = minnv3, Median = mediannv3, Mean = meannv3, Max = maxnv3, 
         SD = stdev, NAs = func1(nv3))
# quartiles are not included because they were not completely implemented

}
# https://stat.ethz.ch/pipermail/r-help/2005-June/074179.html
# used to find quotient function (%/%)
# http://en.wikipedia.org/wiki/Standard_deviation
# used to confirm standard deviation formula

# http://www.amstat.org/publications/jse/v14n3/langford.html
# provided by Daniel Dittenhafer on 9/11/14 in 
# Discussion Board > Forum: Week 3 Assignments > 
#     Thread: R bug for Summary - incorrect 1st and 3rd quartile
# explains some different accepted methods for calculating quartiles
# could use for reference but didn't get to completely implement


#### Prob 4 ####
# 4. Write a function that takes a character or factor vector and 
# determines the number of distinct elements in the vector, the most
# commonly occurring element, the number of times the most commonly 
# occurring element occurs, and the number of missing values.
# (Be sure to handle ties gracefully.)
# Have the function return a named list with the desired information 
# in a logical order.
func4 <- function(v4)
{
    fv4 <- as.factor(v4)
    levs <- levels(fv4)
    distinctelem <- length(levs)
    levfreq <- c()
    for(iter in 1:distinctelem)
    {
        levfreq[iter] <- length(which(fv4 == levs[iter]))
    }
# seems like an apply could be used here instead of the loop    
    fmc <- max(levfreq)
    mc <- levs[levfreq == max(levfreq)]
    list(NumElem = distinctelem, MostCommon = mc, FreqMostCommon = fmc, NAs = func1(v4))
}
# learned that must be careful using which() because it really counts NAs as equivalent
# to the test item when the vector is character type
# not exactly sure how this function works to eliminate the NAs from the counts


#### Prob 5 ####
# 5. Write a function that takes a logical vector and determines the 
# number of true values, the number of false values, the proportion 
# of true values, and the number of missing values. Have the function 
# return a named list with the desired information in a logical order.
func5 <- function(lv5)
{
    numtrue <- length(which(lv5 == TRUE))
    numfalse <- length(which(lv5 == FALSE))
    proportiontrue <- numtrue / (numtrue + numfalse)
    list(TrueVals = numtrue, FalseVals = numfalse, TrueProportion = proportiontrue, 
         NAs = func1(lv5))
}


#### Prob 6 ####
# 6. Write a function that takes as its input a data frame and returns 
# a summary of its columns using the functions you write for questions 3-5. 
# You may assume that all columns will be of the three types in those questions. 
# You are expected to use the functions you have written in the previous 
# questions, so you do not have to write them again by scratch. Return the 
# desired information in a format that you deem best. (One suggestion would 
# be a named list of lists, but I leave it to your judgment.)
func6 <- function(df6)
{
    colsummary <- list()
    for(iter in 1:ncol(df6))
    {
        if(class(df6[, iter]) == "logical")
        {
            colsummary[[iter]] <- func5(df6[, iter])
        }else if(class(df6[, iter]) == "numeric")
        {
            colsummary[[iter]] <- func3(df6[, iter])
        }else
        {
            colsummary[[iter]] <- func4(df6[, iter])
        }
    }
    names(colsummary) <- names(df6)
    print(colsummary)
}