#----------------------------------------------#
# Mathmatical Statistics With R : Chapter 1    #
# Authors: Hamid Jahani, Vahid Rezaei Tabar    #
# Introduction to R Program                    #
#----------------------------------------------#

# q() is commented out so you don't accidently exit

#Header 9-1
data()

data(CO2)
CO2
#q()

#Header 10-1
mydata = data.frame(age=numeric(0),
                    gender = character(0), weight=numeric(0))
mydata = edit(mydata)


mydata <-"
age gender weight
25 m 166
30 f 115
18 f 120
"
mydata <-read.table(header= TRUE, text=mydata)

data= read.table("c:/data.txt")

Install.packages("Hmisc")
library(Hmisc)
Mydataframe<-spss.get("mydata.sav", use.value.labels=TRUE)

library(Hmisc)
Datadir<- "C:/mydata"
Sasexe<-"C:/Program Files/SASHome/SASFoundation/9.4/sas.exe"
Mydata<-sas.get(libraryName=datadir,member="clients", sasprog=sasexe)

library(foreign)
Mydataframe<- read.dta("mydata.dta")
#q()

#Header 12-1
source("file name")
library("RWinEdt")
#q()

#Header 13-1
x <- c(7, 8, 10, 45)
x
is.vector(x)

x<- vector(length=5) 
x[5] <- 8
#q()

#Header 14-1
#define a function
my.first.function = function(x){
  #return the square of x
  return(x^2)
}	
#call the function
my.first.function(2)

#return true if a value is even
is.even = function(x){
  #if even, return true
  if(x%%2 == 0){
    return(TRUE)
  }
  #if odd, return false
  if(x%%2 != 0){
    return(FALSE)
  }
}
#q()

#Header 15-1
x <- c(7, 8, 10, 45)
y <- c(-7, -8, -10, -45)
places <- which(x > 9) 
y[places]
#q()

#Header 16-1
#check if x is positive or not
if (x >= 0) {
  x 
} else { 
  -x
}

#see if 1 equals 2 OR 2 equals 2
if(1 == 2 || 2 == 2){
  x = 10
}
#see if 1 equals 2 AND 2 equals 2
if(1 == 2 && 2 == 2){
  x = 20
}
#print x
x
#q()

#Header 17-1
#initialize a vector; empty path of length 10
x = rep(NA, 10)
#loop 10 times, set x equal to the increment
for(i in 1:10){
  x[i] = i
}
#print x
x


#replicate
set.seed(110)
#create a path for S
S = rep(NA, 100)
#define the first value
S[1] = rnorm(1, 0, 1)
#run the loop
for(i in 2:100){
  #generate
  S[i] = S[i - 1] + rnorm(1, 0, 1)
}
#plot S
plot(S, main = "S", type = "l", xlab = "i", col = "darkred", lwd = 3)
#line at 0
abline(h = 0, col = "black", lty = 3, lwd = 2)
#q()

#Header 18-1

#initialize i
i = 0
#the 'condition' is that i < 10
while(i < 10){
  #increment i
  i = i + 1
}
#print i
i
## [1] 10

#replicate
set.seed(110)
#iterate until we break
while(TRUE){
  #draw X and Y
  X = runif(1)
  Y = runif(1)
  #see if we got the sample we wanted
  if(X + Y < 1){
    break
  }
}
#print X and Y
X + Y
## [1] 0.8326754


#Header 19-1
#generate a vector for X
X = rnorm(100)
#calculate the square of X
Y = sapply(X, function(x) x^2)
#q()

#Header 20-1
#define vectors
x = 1:10
y = x^2
#create the plot
plot(x, y,
     xlab = "x",
     ylab = "y",
     xlim = c(0, 10),
     ylim = c(0, 100),
     type = "l",
     lwd = 5,
     col = "darkred")


#plot on a 3x3 grid
par(mfrow = c(3,3))
#run the loop for the plots
for(i in 1:9){
  plot(rnorm(i*5), 
       main = "", 
       xlab = "x",
       ylab = "y", 
       type = "p",
       pch = 16,
       col = "dodgerblue4")
}
#q()

#Example 1-1
x = 1:18 
dim(x) = c(2,3,3) 
x
#, , 1

#[,1] [,2] [,3]
#[1,]    1    3    5
#[2,]    2    4    6

#, , 2

#[,1] [,2] [,3]
#[1,]    7    9   11
#[2,]    8   10   12

#, , 3

#[,1] [,2] [,3]
#[1,]   13   15   17
#[2,]   14   16   18

#q()

#Example 2-1
x = 1:20 
y = array(x,c(5,4)) 
y
#[,1] [,2] [,3] [,4]
#[1,]    1    6   11   16
#[2,]    2    7   12   17
#[3,]    3    8   13   18
#[4,]    4    9   14   19
#[5,]    5   10   15   20

#q()

#Header 1-22
x <- matrix(c(40,1,60,3),nrow=2)
x
#[,1] [,2]
#[1,]   40   60
#[2,]    1    3
is.array(x) 
#[1] TRUE
is.matrix(x)
#[1] TRUE

s<- matrix(rep(7,6),ncol=3) 
s
#[,1] [,2] [,3]
#[1,]    7    7    7
#[2,]    7    7    7

x%*% s
#[,1] [,2] [,3]
#[1,]  700  700  700
#[2,]   28   28   28

det(x)
#[1] 60
diag(x)
#[1] 40  3
diag(x) <- c(35,4)
x
#[,1] [,2]
#[1,]   35   60
#[2,]    1    4
diag(x)== c(40,3)
#[1] FALSE FALSE
solve(x)
#[,1]    [,2]
#[1,]  0.0500 -0.7500
#[2,] -0.0125  0.4375
x %*% solve(x)
#[,1] [,2]
#[1,]    1    0
#[2,]    0    1

#q()

#Header 23-1
#Normal
?rnorm()

#Binomial/Bernoulli
?rbinom()

#Geometric
?rgeom()

#Exponential
?rexp()

#Beta
?rbeta()

#Gamma
?rgamma()

#Poisson
?rpois()

#Uniform
?runif()
#q()

#Example 3-1
n <- 1000 
u <- runif(n)
x <- u^(1/3) 
hist(x,prob =TRUE)
y <- seq(0, 1,0.1)
lines(y, 3*y^2)
#q()

#Example 4-1
n=1000
u=runif(n)
x=as.integer(u>0.6)

#Example 5-1
n=1000
u=runif(n)
p=0.25
x=ceiling(log(1-u)/log(1-p))-1


#http://library.lol/main/7AEAD73047B0807179C0A505C10173E2
#https://b-ok.asia/book/2150187/ea6d43