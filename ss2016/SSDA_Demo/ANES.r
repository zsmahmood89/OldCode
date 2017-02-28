### William Isaac & Zuhaib Mahmood | Feb. 12, 2016
###################################################

###################################################
### Code Chunk one | Setting File Dir & Libraries
###################################################

getwd()
setwd("Write your directory here")
library(foreign)
library(MASS)

#Run this quick function
vfreq <-function(var){
  a <- table(var)
  return(a)
}

###################################################
### Code Chunk Two | Reading in Stata File
###################################################

anes <- read.dta("Write Data file name here")
ls(anes)

###################################################
### Code Chunk Three | Get Summary Stats
###################################################

#Write a new lines of code here