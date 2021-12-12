#----------------------------------------------#
# Mathmatical Statistics With R : Chapter 2    #
# Authors: Hamid Jahani, Vahid Rezaei Tabar    #
# Basic concepts and definitions               #
#----------------------------------------------#

# q() is commented out so you don't accidently exit

#Example 1-2
#replicate
set.seed(110)
sims = 1000
#create vectors to track if it is sunny/if Ali goes to work
sun = rep(0, sims)
work = rep(0, sims)

#run the loop
for(i in 1:sims){
  
  #flip to see what the weather is
  weather = runif(1)
  #flip to see if Ali goes to work
  go = runif(1)
  
  #the case where it is sunny
  if(weather <= .6){
    #mark that it was sunny
    sun[i] = 1
    
    #Ali goes to work with probability .95 in this case
    if(go <= .95){
      work[i] = 1
    }    
  }
  
  #the case where it is rainy
  else{
    
    #Ali goes to work with probability .3 in this case
    if(go <= .3){
      work[i] = 1
    }
  }
}
#we should get .6 for sun and .705 for work
mean(work)
#q()

#Example 2-2
#replicate
set.seed(110)
sims = 1000

#set paths for Sam coming/Ali making it
Sam = rep(0, sims)
Ali = rep(0, sims)

#run the loop
for(i in 1:sims){ 
  #flip for each Sam and Ali
  Sam.flip = runif(1)
  Ali.flip = runif(1)
  
  #the case where Sam comes
  if(Sam.flip <= .8){
    
    #mark that Sam came
    Sam[i] = 1
    
    #Ali makes it with .9 probability
    if(Ali.flip <= .9){
      Ali[i] = 1
    }
  }
  
  #the case where Sam didn't come
  if(Sam.flip > .8){
    
    #Ali makes it with .1 probability
    if(Ali.flip <= .1){
      Ali[i] = 1
    }
  }
}

#should get .8 overall for Sam
mean(Sam)
## [1] 0.829
#find the mean of Sam conditioned on Ali making it; should get .97
mean(Sam[Ali == 1])
#q()

#Example 3-2
#replicate
set.seed(110)
sims = 1000
#keep track if these events occurred or not
H = rep(0, sims)
E = rep(0, sims)
O = rep(0, sims)
#run the loop
for(i in 1:sims){   
  #flip to see if we get heads
  flip = runif(1)
  
  #roll the die
  roll = sample(1:6, 1)
  
  #see if we got heads
  if(flip <= 1/2){
    H[i] = 1
  }
  
  #see if we got an even number
  if(roll%%2 == 0){
    E[i] = 1
  }
  
  #see if we got an odd number
  else{
    O[i] = 1
  }
}
#should get 1/2 for all
mean(E); mean(O); mean(H)
#the probability of heads doesn't change if we condition on O and E,
#   and vice versa: they are independent!
mean(H[O == 1]); mean(H[E == 1])

mean(O[H == 1]); mean(E[H == 1])
#however, the mean of E changes when O is 1; they are dependent!
mean(E[O == 1])
#q()

#Example 4-2
#replicate
set.seed(110)
sims = 1000

#generate the random variables
white = sample(1:6, sims, replace = TRUE)
red = sample(1:6, sims, replace = TRUE)

#calculate the sum of the rolls
S = white + red
#now compare when we also condition on the sum being 6
#   now, we only have one option for the white roll

#set graphics
par(mfrow = c(1,2))

#graphics
hist(white, main = "White", xlab = "Roll",
     freq = FALSE)
hist(white[red == 3 & S == 6], main = "White | Red = 3, Sum = 6", xlab = "Roll",
     freq = FALSE, breaks = 1:6)
#q()

#Example 6-2
#plot the CDF
plot(seq(from = -3, to = 3, length.out = 100), pnorm(seq(from = -3, to = 3, length.out = 100)),
     xlab = "x", ylab = "P(X <= x)", main = "CDF of X where X ~ N(0, 1)",
     type = "p", pch = 16)
#q()

#Example 7-2
#replicate
set.seed(110)

#define mean matrix; means of 2 and 1
mean.matrix = matrix(c(2, 1), nrow = 2, ncol = 1)

#define Covariance matrix (variances of 1, Covariance of 1/2)
cov.matrix =  matrix(c(1, 1/2, 1/2, 1), nrow = 2, ncol = 2)

#generate 4 data points for each of the two Normal r.v.'s
library(mvtnorm)
rmvnorm(4, mean = mean.matrix, sigma = cov.matrix)
#find the density at (1,1)
dmvnorm(c(1, 1), mean = mean.matrix, sigma = cov.matrix)
#q()

#Example 8-2
#replicate
set.seed(110)
sims = 1000
#generate winnings
winnings = sample(c(10, 0, -5), sims, replace = TRUE, prob = c(2/6, 3/6, 1/6))
#the mean and variance should match above (2.5 and 31.25)
mean(winnings); var(winnings)
#q()


#Example 9-2
#replicate
set.seed(110)
sims = 1000
#generating
U1 = runif(sims)
U2 = runif(sims)
#combine into a product (call it X)
X = U1*U2
#should get 1/4
mean(X)
#q()

#Example 10-2
#replicate
set.seed(110)
sims = 1000
#draw the data
data = rmultinom(sims, 5, prob = c(1/3, 1/3, 1/3))
#call the rows X, Y and Z (different buckets)
X = data[1, ]
Y = data[2, ]
Z = data[3, ]
#histograms should match
#set 1x2 graphing grid
par(mfrow = c(1,2))
#plot X
hist(X[Y == 0], main = "X|Y = 0", xlab = "", breaks = 0:5)
hist(rbinom(sims, 5, 1/2), main = "Bin(5, 1/2)", xlab = "", breaks = 0:5)
#q()

#Header 22-2
#define x and y = x^2
x = -10:10
y = x^2
#plot x and y; clearly a relationship
plot(x, y, pch = 16)
#should get a Covariance of 0
cov(x, y)
#q()

#Example 11-2
#replicate
set.seed(110)
sims = 1000
#define a simple parameter
lambda = 5
#generate the r.v.
X = rexp(sims, lambda)
#define t (in an interval near 0)
t = seq(from = 0, to = 2, length.out = 10)
#calculate the empirical and analytical MGFs
MGF.a = sapply(t, function(t) lambda/(lambda  - t))
MGF.e = sapply(t, function(t) mean(exp(t*X)))
#plot the MGFs, should match
plot(t, MGF.e, main = "Exponential MGF", xlab = "t",
     ylab = "", type = "l", col = "black", lwd = 3)
lines(t, MGF.a, type = "p", pch = 16,
      lwd = 3, col = "red")
legend("topleft", legend = c("Analytical MGF", "Empirical MGF"),
       lty=c(1,1), lwd=c(2.5,2.5),
       col=c("red", "black"))
#q()

#Example 12-2
#replicate
set.seed(110)
sims = 1000
#define simple parameters
j = 5
n = 10
#keep track of Xj
Xj = rep(NA, sims)
#run the loop
for(i in 1:sims){
  #generate the data; use standard normals for simplicity
  data = rnorm(n)
  #order the data
  data = sort(data)
  #find the j^th smallest
  Xj[i] = data[j]
}
#show that the CDFs are the same
#calculate the analytical CDF; recall that we use the standard normal CDF
x = seq(from = min(Xj), to = max(Xj), 
        length.out = 100)
k = j:n
#path for the CDF
CDF = rep(NA, length(x))
#calculate the CDF
for(i in 1:length(x)){
  
  CDF[i] = sum(sapply(k, function(k)
    choose(n, k)*pnorm(x[i])^k*(1 - pnorm(x[i]))^(n - k)))
}
#plots should line up
#empirical
plot(ecdf(Xj), col = "black", main = "CDF of j^th Order Statistic",
     xlab = "x", ylab = "P(X < x)", 
     ylim = c(0, 1), lwd = 3)
#analytical
lines(x, CDF, main = "Analytical CDF", 
      ylab = "P(X = x)", xlab = "x", 
      col = "red", pch = 20, ylim = c(0, 1), 
      type = "p", lwd = 1/2)
#create a legend
legend("topleft", legend = c("Empirical CDF", "Analytical CDF"),
       lty=c(1,20), lwd=c(2.5,2.5),
       col=c("black", "red"))

#q()

#Example 13-2
#replicate
set.seed(110)
sims = 1000
#define simple parameters
j = 7
n = 10
#keep track of Xj
Xj = rep(NA, sims)
#run the loop
for(i in 1:sims){
  #generate the data; use standard uniforms
  data = runif(n)
  #order the data
  data = sort(data)
  #find the j^th smallest
  Xj[i] = data[j]
}
#show that the j^th order statistic is Beta
#1x2 graphing grid
par(mfrow = c(1,2))
#Beta
hist(rbeta(sims, j, n + 1 - j), main = "Beta(j, n - j + 1)", xlab = "")
#Order Statistics
hist(Xj, main = "X_(j) for Unif(0, 1)", xlab = "")
#q()

#Header 31-2
sims=1000 
m=0
for(i in 1:sims){
  B=rbinom(sims,10,0.4) 
  m[i]=mean(B) 
}
hist(m,prob=T)
f=function(x){
  dnorm(x,mean = mean(m),sd=sd(m))
} 
curve(f,col="red",add=T,lwd=3)
#q()

#Header 32-2-1
sims=1000
n=10
p=0.2
binomial_distribution=rbinom(sims,n,p)
mean(binomial_distribution)
var(binomial_distribution)
hist(binomial_distribution)
#q()

#Example 14-2
n=20
p=0.35
par(mfrow=c(1,2))
plot(0:20,dbinom(0:n,n,p),xlab = "x",
     ylab = "p(X=x)",
     main="Binomial probability mass fanction")
plot(seq(0,1,0.1),qbinom(seq(0,1,0.1),n,p),
     xlab = "quantiles",
     ylab = "x",main = "quantiles of binomial")
#q()

#Example 15-2
library(scatterplot3d)
par(mfrow=c(1,3))
n=10
p=0.35
##plotting the CDF
plot(0:10,pbinom(0:10,n,p),xlab = "x-values",
     ylab = expression(p(X<=x)),
     main="Binomial CDF")
t=seq(-1,1,0.1)
#######################the MGF
mgf_binomial=function(t,n,p){(1-p+p*exp(t))^{n}}
plot(t,mgf_binomial(t,n,p),xlab="t",
     ylab = expression(M(t)),
     main="The binomial MGF")
t=seq(-10,10,0.01)
library(scatterplot3d)
par(mfrow=c(1,3))
n=10
p=0.35
##plotting the CDF
plot(0:10,pbinom(0:10,n,p),xlab = "x-values",
     ylab = expression(p(X<=x)),
     main="Binomial CDF")
t=seq(-1,1,0.1)
#######################the MGF
mgf_binomial=function(t,n,p){(1-p+p*exp(t))^{n}}
plot(t,mgf_binomial(t,n,p),xlab="t",
     ylab = expression(M(t)),
     main="The binomial MGF")
t=seq(-10,10,0.01)
############the characteristic function
cf_binomial=function(t,n,p){(1-p+p*exp(1i*t))^{n}}
scatterplot3d(t,Re(cf_binomial(t,n,p)),
              Im(cf_binomial(t,n,p)),
              xlim=c(-11,11),ylim=c(-1,1),
              zlim=c(-1,1),xlab="t",
              ylab=expression(paste("Real part of ",
                                    phi(t))),
              zlab=expression(paste("Complex part of",
                                    phi(t))),
              highlight.3d=T,col.axis="blue",
              col.grid="lightblue",
              pch=20,type="l",
              main="character(t) function")


#
p=ecdf(X)
plot(p,main="Binomial CDF")
par(mfrow=c(1,2))
sims = 1000
n = 25
p = 0.3
X = rbinom(sims, n,p)
t = seq(from = 0, to = 2, length.out = 10)
MGF.a = sapply(t, function(t) 1-p+p*exp(t)^{n})
MGF.e = sapply(t, function(t) mean(exp(t*X)))
plot(t, MGF.e, main = "Binomial MGF", xlab = "t",
     ylab = "", type = "l", lwd = 3,lty=1)
lines(t, MGF.a, type = "p", pch = 16,
      lwd = 3,lty=2)
legend("topleft", legend = c("Analytical MGF",
                             "Empirical MGF"),
       lty=c(1,3), lwd=c(2.5,2.5))
######################ecdf
p=ecdf(X)
plot(p,main="Binomial CDF")
#q()


#Header 32-2-2
sims=20
p=0.6
geometric_distribution=rgeom(sims,p)
mean(geometric_distribution)
var(geometric_distribution)
hist(geometric_distribution)
#
##the probability distribution of geometric random variables
par(mfrow=c(2,2))
plot(0:20,dgeom(0:20,prob = 0.2),xlab="x",
     ylab="probability",type="h")
plot(0:50,dgeom(0:50,prob = 0.5),xlab="x",
     ylab="probability",type="h")
plot(0:100,dgeom(0:100,prob = 0.7),xlab="x",
     ylab="probability",type="h")
plot(0:150,dgeom(0:150,prob = 0.9),xlab="x",
     ylab="probability",type="h")
#

sims = 1000
p = 0.6
X = rgeom(sims,p)
t = seq(from = 0, to = 0.9, length.out = 10)
MGF.a = sapply(t, function(t) p/(1-(1-p)*exp(t)))
MGF.e = sapply(t, function(t) mean(exp(t*X)))
plot(t, MGF.e, main = "Geometric MGF", xlab = "t",
     ylab = "", type = "l", lwd = 3)
lines(t, MGF.a, type = "p", pch = 16,
      lwd = 3)
legend("topleft", legend = c("Analytical MGF",
                             "Empirical MGF"),
       lty=c(1,3), lwd=c(2.5,2.5))
####ecdf
p=ecdf(X)
plot(0:10,pgeom(0:10,prob = 0.6),xlab="x",
     ylab="probability",type="h",main="geometric CDF")
lines(p,xlim=c(0,10),type="l",lwd=3)
legend("topleft", legend = c("Analytical CDF",
                             "Empirical CDF"),
       lty=c(1,1), lwd=c(1,3))
#q()

#Example 16-2
p=seq(0,1,0.02)
mu=(1-p)/p
var=(1-p)/p^(2)
par(mfrow=c(1,2))
plot(p,mu,xlab ="probability of success",
     ylab = "mean","l",
     main = "A:mean of geometric distribution")
plot(p,var,xlab = "probability of success",
     ylab = "variance","l",
     main = "B:variance of geometric distribution")
#q()

#Example 17-2
n=0:50
i=0
p=seq(0.05,1,0.05)
plot(n,p[1]^{n},xlab = "x",
     ylab = "tail probabilities","l",
     col=1,xlim = c(0,20),
     main = "c:tail probabilities of geometric")
for(i in 2:20){
  lines(n,p[i]^{n},"l",col=i)
}
#q()

#Header 32-2-3
par(mfrow=c(1,2))
sims=100
m=10
n=7
k=8
hayper_distribution=rhyper(sims,m,n,k)
mean(hayper_distribution)
var(hayper_distribution)
hist(hayper_distribution)
################ecdf
sims = 1000
p = 0.6
X = rhyper(sims,m,n,k)
p=ecdf(X)
plot(0:10,phyper(0:10,m=10,n=7,k=8),xlab="x",
     ylab="probability",type="h", 
     main = "hyper geometric CDF")
lines(p,xlim=c(0,10),type="l",lwd=3)
legend("topleft", legend = c("Analytical CDF",
                             "Empirical CDF"),
       lty=c(1,1), lwd=c(1,3))
#
par(mfrow=c(2,2))
plot(0:20,dhyper(0:20,m=10,n=7,k=8),xlab="x",
     ylab="probability",type="h")
plot(0:50,dhyper(0:50,m=10,n=7,k=8),xlab="x",
     ylab="probability",type="h")
plot(0:100,dhyper(0:100,m=10,n=7,k=8),xlab="x",
     ylab="probability",type="h")
plot(0:150,dhyper(0:150,m=10,n=7,k=8),xlab="x",
     ylab="probability",type="h")
#q()

#Header 32-2-4
######negative binomial distribution
sims=1000
n=30
p=0.7
N_binomial_distribution=rnbinom(sims,n,p)
mean(N_binomial_distribution)
var(N_binomial_distribution)
hist(N_binomial_distribution)
#

##the probability distribution of negative binomial random variables
par(mfrow=c(2,2))
plot(0:20,dnbinom(0:20,size = 20,prob = 0.2),xlab="x",
     ylab="probability",type="h")
plot(0:50,dnbinom(0:50,size = 50,prob = 0.5),xlab="x",
     ylab="probability",type="h")
plot(0:100,dnbinom(0:100,size = 100,prob = 0.7),xlab="x",
     ylab="probability",type="h")
plot(0:150,dnbinom(0:150,size = 150,prob = 0.9),xlab="x",
     ylab="probability",type="h")

#

par(mfrow=c(1,2))
sims = 100
n=20
p = 0.4
X = rnbinom(sims,n,p)
t = seq(from = 0, to = 0.5, length.out = 10)
MGF.a = sapply(t, function(t) (p^n)/((1-(1-p)*exp(t)))^n)
MGF.e = sapply(t, function(t) mean(exp(t*X)))
plot(t, MGF.e, main = "negative binomial MGF", xlab = "t",ylab = "", type = "l", lwd = 3)
lines(t, MGF.a, type = "p", pch = 16,lwd = 3)
legend("topleft", legend = c("Analytical MGF","Empirical MGF"),lty=c(1,3), lwd=c(2.5,2.5))
######################ecdf
p=ecdf(X)
plot(0:100,pnbinom(0:100,size = 20,prob = 0.4),xlab="x",
     ylab="probability",type="h",
     main = "Negative binomial CDF")
lines(p,xlim=c(0,100),type="l",lwd=3)
legend("topleft", legend = c("Analytical CDF",
                             "Empirical CDF"),
       lty=c(1,1), lwd=c(1,3))
#q()

#Example 18-2
m=1:50
r=5
p=seq(0.05,1,0.05)
plot(m,pbeta(p[1],m,r),xlab = "x",ylab = "tail probabilities","l",
     col=1,xlim = c(0,50),ylim = c(0,1),
     main = "tail probabilities of negative binomial")
for(i in 2:20){
  lines(m,pbeta(p[i],m,r),col=i)
}
#q()

#Header 32-2-5
#########poisson distribution
sims=1000
lambda=5
poisson_distribution=rpois(sims,lambda)
mean(poisson_distribution)
var(poisson_distribution)
hist(poisson_distribution)
#

##the probability distribution of poisson random variables
par(mfrow=c(2,2))
plot(0:20,dpois(0:20,lambda = 2),xlab="x",
     ylab="probability",type="h")
plot(0:50,dpois(0:50,lambda = 5),xlab="x",
     ylab="probability",type="h")
plot(0:100,dpois(0:100,lambda = 20),xlab="x",
     ylab="probability",type="h")
plot(0:150,dpois(0:150,lambda = 110),xlab="x",
     ylab="probability",type="h")
#

par(mfrow=c(1,2))
sims = 1000
lambda = 5
X = rpois(sims, lambda)
t = seq(from = 0, to = 2, length.out = 10)
MGF.a = sapply(t, function(t) exp(lambda*(exp(t)-1)))
MGF.e = sapply(t, function(t) mean(exp(t*X)))
plot(t, MGF.e, main = "poisson MGF", xlab = "t",
     ylab = "", type = "l", lwd = 3)
lines(t, MGF.a, type = "p", pch = 16,
      lwd = 3)
legend("topleft", legend = c("Analytical MGF",
                             "Empirical MGF"),
       lty=c(1,3), lwd=c(2.5,2.5))
####ecdf
p=ecdf(X)
plot(0:10,ppois(0:10,lambda=5),xlab="x",
     type="h",main = "poisson CDF")
lines(p,xlim=c(0,10),type="l",lwd=3)
legend("topleft", legend = c("Analytical CDF",
                             "Empirical CDF"),
       lty=c(1,1), lwd=c(1,3))
#q()

#Example 19-2
n=seq(20,41,3)
p=1/n
approxdiff=n*0
par(mfrow=c(2,4))
for(i in 1:length(n)){
  binomprob=dbinom(c(0:n[i]),n[i],p[i])
  poisprob=dpois(c(0:n[i]),n[i]*p[i])
  plot(c(0:n[i]),binomprob,xlab = "x",
       ylab = "binomial and poisson probability",
       ylim = c(0,0.5),"l",lwd=1)
  lines(c(0:n[i]),poisprob,ylim=c(0,0.5),"p",lwd=3)
  approxdiff[i]=sum(binomprob-poisprob)
  legend("topleft", legend = c("binomprob"," poisprob"),
         lty=c(1,3), lwd=c(1,3))
}
title(main = "poisson approximation of binomial RV",
      outer = T,line = -2)
#q()

#Header 33-2-1
##the uniform distribution
sims=1000
uniform_distribution=runif(sims)
mean(uniform_distribution)
var(uniform_distribution)
hist(uniform_distribution)
#

##the probability distribution of uniform random variables
par(mfrow=c(2,2))
plot(0:20,dunif(0:20,min =  1,max = 4),xlab="x",
     ylab="probability",type="h")
plot(0:50,dunif(0:50,min = 3,max = 6),xlab="x",
     ylab="probability",type="h")
plot(0:100,dunif(0:100,min = 5,max = 8),xlab="x",
     ylab="probability",type="h")
plot(0:150,dunif(0:150,min = 6,max = 10),xlab="x",
     ylab="probability",type="h")
#

par(mfrow=c(1,2))
sims = 1000
min=2
max=5
X = runif(sims,min,max)
t = seq(from = 1, to = 2, length.out = 10)
MGF.a = sapply(t, function(t) (exp(max*t)-exp(min*t))/(max-min))
MGF.e = sapply(t, function(t) mean(exp(t*X)))
plot(t, MGF.e, main = "uniform MGF", xlab = "t",
     ylab = "", type = "l", lwd = 3)
lines(t, MGF.a, type = "p", pch = 16,
      lwd = 3)
legend("topleft", legend = c("Analytical MGF","Empirical MGF"),lty=c(1,3), lwd=c(2.5,2.5))
####ecdf
p=ecdf(X)
plot(0:10,punif(0:10,min = 2,max = 5),xlab="x",
     ylab="probability",type="h",main = "uniform CDF")
lines(p,xlim=c(0,10),type="l",lwd=3)
legend("topleft", legend = c("Analytical CDF",
                             "Empirical CDF"),lty=c(1,1), lwd=c(1,3))
#q()

#Header 33-2-2
sims=1000
beta_distribution=rbeta(sims,0.6,5)
mean(beta_distribution)
var(beta_distribution)
hist(beta_distribution)
########################################CDF
x=seq(0,1,0.05)
plot(x,pbeta(x,0.5,0.5),"l",
     ylab = "beta density plot",xlab = "x",
     ylim = c(0,1),main = "beta CDF")
lines(x,pbeta(x,5,1),"l");lines(x,pbeta(x,1,3),"l")
lines(x,pbeta(x,2,2),"l");lines(x,pbeta(x,2,5),"l")
lines(x,pbeta(x,1,5),"l");lines(x,pbeta(x,5,3),"l")

#
##different density shapes of beta distribution
x=seq(0,1,0.05)
plot(x,dbeta(x,0.05,0.05),"l",
     ylab = "beta density plot",xlab = "x",
     ylim = c(0,1))
lines(x,dbeta(x,0.05,0.5),"l");lines(x,dbeta(x,0.05,1),"l")
lines(x,dbeta(x,0.05,5),"l");lines(x,dbeta(x,0.5,0.05),"l")
lines(x,dbeta(x,1,0.05),"l");lines(x,dbeta(x,5,0.05),"l")
#q()

#Header 33-2-3
#####the exponential distirbution
sims=1000
rate=5
exponential_distribution=rexp(sims,rate)
mean(exponential_distribution)
var(exponential_distribution)
hist(exponential_distribution)
#

##the density of exponential random variables
par(mfrow=c(2,2))
plot(0:20,dexp(0:20,rate = 0.2),xlab="x",
     ylab="probability",type="h")
plot(0:50,dexp(0:50,rate = 0.4),xlab="x",
     ylab="probability",type="h")
plot(0:100,dexp(0:100,rate = 0.7),xlab="x",
     ylab="probability",type="h")
plot(0:150,dexp(0:150,rate = 1),xlab="x",
     ylab="probability",type="h")

#

par(mfrow=c(1,2))
sims= 1000
lambda = 5
X = rexp(sims, lambda)
t = seq(from = 0, to = 2, length.out = 10)
MGF.a = sapply(t, function(t) lambda/(lambda  - t))
MGF.e = sapply(t, function(t) mean(exp(t*X)))
plot(t, MGF.e, main = "Exponential MGF", xlab = "t",
     ylab = "", type = "l", lwd = 3)
lines(t, MGF.a, type = "p", pch = 16, lwd = 3)
legend("topleft", legend = c("Analytical MGF","Empirical MGF"),
       lty=c(1,3), lwd=c(2.5,2.5))
####ecdf
p=ecdf(X)
plot(0:10,pexp(0:10,rate = 5),xlab="x",
     ylab="probability",type="h",main = "Exponential CDF")
lines(p,xlim=c(0,10),type="l",lwd=3)
legend("bottomleft", legend = c("Analytical CDF",
                                "Empirical CDF"),
       lty=c(1,1), lwd=c(1,3))
#q()

#Header 33-2-4
######the gamma distiribution
sims=1000
gamma_distribution=rgamma(sims,0.6)
mean(gamma_distribution)
var(gamma_distribution)
hist(gamma_distribution)
#

par(mfrow=c(2,2))
x=seq(0,2,0.1)
plot(x,dgamma(x,shape=0.5,scale = 0.5),xlab = "x",
     ylab = "gamma density plot","l")
x=seq(0,8,0.1)
plot(x,dgamma(x,shape=2,scale = 0.5),xlab = "x",
     ylab = "gamma density plot","l")
lines(x,dgamma(x,shape = 2,scale = 1),"l")
lines(x,dgamma(x,shape = 2,scale = 2),"l")
x=seq(0,20,0.1)
plot(x,dgamma(x,shape=4,scale = 2),xlab = "x",
     ylab = "gamma density plot","l")
lines(x,dgamma(x,shape = 4,scale = 4),"l")
x=seq(0,35,0.1)
plot(x,dgamma(x,shape=8,scale = 2),xlab = "x",
     ylab = "gamma density plot","l")
#

sims = 1000
shape=3
rate=5
X = rgamma(sims,shape,rate)
t = seq(from = 0, to = 2, length.out = 10)
MGF.a = sapply(t, function(t) (rate/(rate-t))^shape)
MGF.e = sapply(t, function(t) mean(exp(t*X)))
plot(t, MGF.e, main = "gamma MGF", xlab = "t",
     ylab = "", type = "l", lwd = 3)
lines(t, MGF.a, type = "p", pch = 16,lwd = 3)
legend("topleft", legend = c("Analytical MGF","Empirical MGF"),lty=c(1,3), lwd=c(2.5,2.5))
####ecdf
p=ecdf(X)
x=seq(0,8,0.1)
plot(x,pgamma(x,shape=2,scale = 0.5),xlab = "x",
     ylab = "gamma CDF","l")
lines(x,pgamma(x,shape = 2,scale = 1),"l")
lines(x,pgamma(x,shape = 2,scale = 2),"l")
lines(p,xlim=c(0,8),type="l",lwd=3)
legend("bottomright", legend = c("Analytical CDF","Empirical CDF"),lty=c(1,1), lwd=c(1,3))
#q()

#Header 33-2-5
######the normal distribution
sims=1000
normal_distribution=rnorm(sims)
mean(normal_distribution)
var(normal_distribution)
hist(normal_distribution)
#

##some shady normal curves
par(mfrow=c(1,3))
#probability z greater than 0
curve(dnorm(x,0,1),-4,4,xlab = "z",ylab = "f(z)")
z=seq(0,4,0.02)
lines(z,dnorm(z),type = "h")
#95% coverage
curve(dnorm(x,0,1),-4,4,xlab = "z",ylab = "f(z)")
z=seq(-1.96,1.96,0.001)
lines(z,dnorm(z),type = "h")
#95% coverage
curve(dnorm(x,0,1),-4,4,xlab = "z",ylab = "f(z)")
z=seq(-2.58,2.58,0.001)
lines(z,dnorm(z),type = "h")
#

par(mfrow=c(1,2))
sims = 1000
mean=5
sd=2
X = rnorm(sims,mean,sd)
t = seq(from = 0, to = 2, length.out = 10)
MGF.a = sapply(t, function(t) exp((mean*t)+((1/2)*sd*(t^2))))
MGF.e = sapply(t, function(t) mean(exp(t*X)))
plot(t, MGF.e, main = "simsormal MGF", xlab = "t",
     ylab = "", type = "l", lwd = 3)
lines(t, MGF.a, type = "p", pch = 16,lwd = 3)
legend("topleft", legend = c("Analytical MGF","Empirical MGF"),lty=c(1,3), lwd=c(2.5,2.5))
###ecdf
p=ecdf(X)
plot(0:10,pnorm(0:10,mean,sd),xlab="x",
     ylab="probability",type="h",main = "simsormal CDF")
lines(p,xlim=c(0,10),type="l",lwd=3)
legend("topleft", legend = c("Analytical MGF","Empirical MGF"),lty=c(1,1), lwd=c(1,3))
#q()

#Header 33-2-6
par(mfrow=c(1,2))
sims=100
shape=5
scale=1
weibull_distribution=rweibull(sims,shape,scale)
mean(weibull_distribution)
var(weibull_distribution)
hist(weibull_distribution)
#################CDF
X = rweibull(sims,shape,scale)
p=ecdf(X)
plot(0:10,pweibull(0:10,shape = 5,scale = 1),xlab="x",
     ylab="probability",type="h", 
     main = "Weibull CDF")
lines(p,xlim=c(0,10),type="l",lwd=3)
legend("bottomright", legend = c("Analytical CDF","Empirical CDF"),lty=c(1,1), lwd=c(1,3))
#

par(mfrow=c(2,2))
plot(0:20,dweibull(0:20,shape=2,scale = 1),xlab="x",
     ylab="probability",type="h")
plot(0:50,dweibull(0:50,shape=5,scale = 2),xlab="x",
     ylab="probability",type="h")
plot(0:100,dweibull(0:100,shape=6,scale = 3),xlab="x",
     ylab="probability",type="h")
plot(0:150,dweibull(0:150,shape=9,scale = 5),xlab="x",
     ylab="probability",type="h")
#q()

#Header 33-2-7
par(mfrow=c(2,2))
plot(0:20,dweibull(0:20,shape=2,scale = 1),xlab="x",
     ylab="probability",type="h")
plot(0:50,dweibull(0:50,shape=5,scale = 2),xlab="x",
     ylab="probability",type="h")
plot(0:100,dweibull(0:100,shape=6,scale = 3),xlab="x",
     ylab="probability",type="h")
plot(0:150,dweibull(0:150,shape=9,scale = 5),xlab="x",
     ylab="probability",type="h")
par(mfrow=c(1,2))
sims=100
location=5
scale=1
Cauchy_distribution=rcauchy(sims,location,scale)
mean(Cauchy_distribution)
var(Cauchy_distribution)
hist(Cauchy_distribution)
#################CDF
X = rcauchy(sims,location,scale)
p=ecdf(X)
plot(0:10,pcauchy(0:10,location = 5,scale = 1),xlab="x",
     ylab="probability",type="h", 
     main = "cauchy CDF")
lines(p,xlim=c(0,10),type="l",lwd=3)
legend("bottomright", legend = c("Analytical CDF","Empirical CDF"),lty=c(1,1), lwd=c(1,3))
#


par(mfrow=c(2,2))
plot(0:20,dcauchy(0:20,location=3,scale = 0.1),xlab="x",
     ylab="probability",type="h")
plot(0:50,dcauchy(0:50,location=5,scale = 0.5),xlab="x",
     ylab="probability",type="h")
plot(0:100,dcauchy(0:100,location=8,scale = 1),xlab="x",
     ylab="probability",type="h")
plot(0:150,dcauchy(0:150,location=9,scale = 3),xlab="x",
     ylab="probability",type="h")

#q()


#Header 33-2-8
par(mfrow=c(1,2))
sims=100
df=3
chisquare_distribution=rchisq(sims,df)
mean(chisquare_distribution)
var(chisquare_distribution)
hist(chisquare_distribution)
#################CDF
X = rchisq(sims,df)
p=ecdf(X)
plot(0:10,pchisq(0:10,df=3),xlab="x",
     ylab="probability",type="h", 
     main = "Chisquare CDF")
lines(p,xlim=c(0,10),type="l",lwd=3)
legend("bottomright", legend = c("Analytical CDF","Empirical CDF"),lty=c(1,1), lwd=c(1,3))
#

par(mfrow=c(2,2))
plot(0:20,dchisq(0:20,df=2),xlab="x",
     ylab="probability",type="h")
plot(0:50,dchisq(0:50,df=5),xlab="x",
     ylab="probability",type="h")
plot(0:100,dchisq(0:100,df=6),xlab="x",
     ylab="probability",type="h")
plot(0:150,dchisq(0:150,df=9),xlab="x",
     ylab="probability",type="h")
#q()

#Header 33-2-9
par(mfrow=c(1,2))
sims=100
df=5
T_distribution=rt(sims,df)
mean(T_distribution)
var(T_distribution)
hist(T_distribution)
#################CDF
X = rt(sims,df)
p=ecdf(X)
plot(0:10,pt(0:10,df=5),xlab="x", ylab="probability",type="h", 
     main = "T CDF")
lines(p,xlim=c(0,10),type="l",lwd=3)
legend("bottomright", legend = c("Analytical CDF","Empirical CDF"),lty=c(1,1), lwd=c(1,3))
#

par(mfrow=c(2,2))
plot(0:20,dt(0:20,df=3),xlab="x",ylab="probability",type="h")
plot(0:50,dt(0:50,df=4),xlab="x",ylab="probability",type="h")
plot(0:100,dt(0:100,df=7),xlab="x",ylab="probability",type="h")
plot(0:150,dt(0:150,df=10),xlab="x",ylab="probability",type="h")
#q()

#Header 33-2-10

plot(0:20,dcauchy(0:20,location=3,scale = 0.1),xlab="x",
     ylab="probability",type="h")
plot(0:50,dcauchy(0:50,location=5,scale = 0.5),xlab="x",
     ylab="probability",type="h")
plot(0:100,dcauchy(0:100,location=8,scale = 1),xlab="x",
     ylab="probability",type="h")
plot(0:150,dcauchy(0:150,location=9,scale = 3),xlab="x",    ylab="probability",type="h")
par(mfrow=c(1,2))
sims=100
df1=3
df2=5
F_distribution=rf(sims,df1,df2)
mean(F_distribution)
var(F_distribution)
hist(F_distribution)
#################CDF
X = rf(sims,df1,df2)
p=ecdf(X)
plot(0:10,pf(0:10,df1=3,df2=5),xlab="x",ylab="probability",type="h", main = "F CDF")
lines(p,xlim=c(0,10),type="l",lwd=3)
legend("bottomright", legend = c("Analytical CDF","Empirical CDF"),lty=c(1,1), lwd=c(1,3))
#

par(mfrow=c(2,2))
plot(0:20,df(0:20,df1=2,df2=4),xlab="x",
     ylab="probability",type="h")
plot(0:50,df(0:50,df1=5,df2=3),xlab="x",
     ylab="probability",type="h")
plot(0:100,df(0:100,df1=6,df2=5),xlab="x",
     ylab="probability",type="h")
plot(0:150,df(0:150,df1=9,df2=12),xlab="x",
     ylab="probability",type="h")
#q()

#Header 33-2-11
library(mvtnorm)
### correlation for the bivariate normal distribution
rho <- 0.4
### number of points for x and y at which to evaluate the density of the bivariate normal distribution
n <- 501
sigma <- matrix(c(1,rho,rho,1), nrow=2)
x <- seq(-3, 3, length=n)
y <- seq(-3, 3, length=n)
z <- matrix(NA, nrow=n, ncol=n)
for (i in seq_along(x)) {
  for (j in seq_along(y)) {
    z[i,j] <- dmvnorm(c(x[i],y[j]), sigma=sigma)
  }
}
### used width and height equal to twice the resolution, which looks nice (but adapt as needed)
jpeg("bivariate_normal.jpg", width=2732, height=1536, quality=95, bg="gray10", type="cairo")
### contour plot
par(mar=c(0,0,15,0))
lvls <- c(seq(.02, .20, by=.01))
cols <- colorRampPalette(c("gray25", "gray95"))(length(lvls))
contour(z=z, axes=F, drawlabels=FALSE, levels=lvls, col=cols)
par(new=TRUE)
### bivariate normal surface
par(mar=c(13,0,9,0))
nrz <- nrow(z)
ncz <- ncol(z)
jet.colors <- colorRampPalette(c(rgb(36,36,36,maxColorValue=255), "gray80"))
nbcol <- 1000
color <- jet.colors(nbcol)
zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
facetcol <- cut(zfacet, nbcol)
persp(x, y, z, theta=45, phi=30, r=10, box=FALSE, shade=0.3, col=color[facetcol], border=NA, expand=0.6, ltheta=-180, lphi=-10)
### add pdf text
text(0, 0.07, col="gray70", pos=1, cex=2.6,expression(frac(1, 2 ~ pi ~ sigma[x] ~ sigma[y] ~ sqrt(1 - rho^2)) ~exp ~ bgroup("[", -frac(1,2*(1-rho^2)) ~ bgroup("(", frac((x-mu[x])^2, sigma[x]^2) ~ + ~frac((y-mu[y])^2, sigma[y]^2) ~ - ~                                       frac(2 ~ rho ~ (x-mu[x]) ~ (y-mu[y]), sigma[x] ~ sigma[y]), ")"),"]")))                                                               
dev.off()
#


# plot a multinormal distribution
# SIMULATING MULTIVARIATE DATA
# lets first simulate a bivariate normal sample.
#The following block of code generates 5,000 draws from a bivariate normal distribution
#with mean (0,0) and covariance matrix Sigma printed in code. The function kde2d(),
#also from the Mass package generates a two-dimensional kernel density 
#estimation of the distribution's probability density function. 
library(MASS)
# Simulate bivariate normal data
mu <- c(0,0)                         # Mean
Sigma <- matrix(c(1, .5, .5, 1), 2)  # Covariance matrix
# > Sigma
#       [,1] [,2]
# [1,]  1.0  0.5
# [2,]  0.5  1.0
# Generate sample from N(mu, Sigma)
bivn <- mvrnorm(5000, mu = mu, Sigma = Sigma )  # from Mass package
# Calculate kernel density estimate
bivn.kde <- kde2d(bivn[,1], bivn[,2], n = 50)   # from MASS package
#R offers several ways of visualizing the distribution. These next two lines
#of code overlay a contour plot on a "heat Map" that maps the density of points to a gradient of colors.
# Contour plot overlayed on heat map image of results
image(bivn.kde)       # from base graphics package
contour(bivn.kde, add = TRUE)     # from base graphics package
#This plots the irregular contours of the simulated data. The code below which 
#uses the ellipse() function from the ellipse package generates the 
#classical bivariate normal distribution plot that graces many a textbook.
# Classic Bivariate Normal Diagram
library(ellipse)
rho <- cor(bivn)
y_on_x <- lm(bivn[,2] ~ bivn[,1])    # Regressiion Y ~ X
x_on_y <- lm(bivn[,1] ~ bivn[,2])    # Regression X ~ Y
plot_legend <- c("99% CI green", "95% CI red","90% CI blue","Y on X black", "X on Y brown")
plot(bivn, xlab = "X", ylab = "Y",col = "dark blue",main = "Bivariate Normal with Confidence Intervals")
lines(ellipse(rho), col="red")# ellipse() fromellipse package
lines(ellipse(rho, level = .99), col="green")
lines(ellipse(rho, level = .90), col="blue")
abline(y_on_x)
abline(x_on_y, col="brown")
legend(3,1,legend=plot_legend,cex = .5, bty = "n")
#The next bit of code generates a couple of three dimensional surface plots. 
#The second of which is an rgl plot that you will be able to rotate and 
#view from different perspectives on your screen.
# Three dimensional surface
# Basic perspective plot
persp(bivn.kde, phi = 45, theta = 30, shade = .1, border = NA) # from base graphics package
# RGL interactive plot
library(rgl)
col2 <- heat.colors(length(bivn.kde$z))[rank(bivn.kde$z)]
persp3d(x=bivn.kde, col = col2)
# threejs Javascript plot
library(threejs)
# Unpack data from kde grid format
x <- bivn.kde$x; y <- bivn.kde$y; z <- bivn.kde$z
# Construct x,y,z coordinates
xx <- rep(x,times=length(y))
yy <- rep(y,each=length(x))
zz <- z; dim(zz) <- NULL
# Set up color range
ra <- ceiling(16 * zz/max(zz))
col <- rainbow(16, 2/3)
# 3D interactive scatter plot
scatterplot3js(x=xx,y=yy,z=zz,size=0.4,color = col[ra],bg="black")
#q()


#Header 34-2-1
#a

binom=function(n,p,m){
  for(i in 1:m){
    X=rbinom(n,1,p)
    Y[i]=sum(X)
  }
  return(Y)
}
n=50
p=0.6
m=20
Y=0
binom(n,p,m)
q=seq(20,40,1)
hist(binom(n,p,m),prob=T,xlab = "y",main = "binomial")
points(q,dbinom(q,n,p),col="red")
#b

par(mfrow=c(2,2))
library(car)
p=0.01
n=c(50,100,500,10000)
for(i in 1:4){
  y=rbinom(n[i],n[i],p)
  qqPlot(y,"pois",lambda=n[i]*p)
}
#c

x1=rbinom(10,10,1/2)
x2=rbinom(100,10,1/2)
x3=rbinom(1000,10,1/2)
x4=rbinom(10000,10,1/2)
par(mfrow=c(2,2))
hist(x1,probability = T,main = "sample size: 10")
curve(dnorm(x,mean=5,sd=sqrt(2.5)),add=T)
hist(x2,probability = T,breaks = 9,main = "sample size: 100")
curve(dnorm(x,mean=5,sd=sqrt(2.5)),add=T)
hist(x3,probability = T,breaks = 9,main = "sample size: 1000")
curve(dnorm(x,mean=5,sd=sqrt(2.5)),add=T)
hist(x4,probability = T,breaks = 9,main = "sample size: 10000")
curve(dnorm(x,mean=5,sd=sqrt(2.5)),add=T)
#q()

#Header 34-2-2
binom1=function(a,n,m,p,z){
  X=rbinom(a,n,p)
  Y=rbinom(a,m,p)
  t=X+Y
  u=rep(NA,a)
  for(i in 1:a){
    if(X[i]+Y[i]==z){
      u[i]=X[i]
    }
  }
  u=u[!is.na(u)]
  outputs=list(t,u)
  names(outputs)=c("binomial","hypergeometric")
  return(outputs)
} 
par(mfrow=c(1,2))
a=500
n=10
m=15
p=0.5
z=10
u=0
binom1(a,n,m,p,z)
q=seq(0,25,1)
hist(binom1(a,n,m,p,z)$binomial,prob=T,xlab = "t",main = "binomial")
points(q,dbinom(q,n+m,p),col="red")
hist(binom1(a,n,m,p,z)$hypergeometric,prob=T,xlab = "u",main = "hypergeometric")
points(q,dhyper(q,n,m,z),col="red")
#q()

#Header 34-2-3
#b
geom1=function(n,p,m){
  for(i in 1:m){
    X=rgeom(n,p)
    Y[i]=sum(X)
  }
  return(Y)
}
n=10
p=0.4
m=30
Y=0
geom1(n,p,m)
q=seq(1,30,1)
hist(geom1(n,p,m),prob=T,xlab = "Y",main = "negative binomial")
points(q,dnbinom(q,n,p),col="red")
#q()

#Header 34-2-4
#5
par(mfrow=c(2,2))
library(car)
p=0.01
n=c(50,100,500,10000)
for(i in 1:4){
  y=rnbinom(n[i],n[i],p)
  qqPlot(y,"pois",lambda=n[i]*(1-p))
}
#q()

#Header 34-2-7
#a
pois=function(n,lambda,m){
  for(i in 1:m){
    X=rpois(n,lambda)
    Y[i]=sum(X)
  }
  return(Y)
}
n=10
lambda=1
m=30
Y=0
pois(n,lambda,m)
q=seq(1,20,1)
hist(pois(n,lambda,m),prob=T,xlab = "Y",main = "poisson")
points(q,dpois(q,n*lambda),col="red")
#q()

#Header 34-2-8
#b
x=runif(1000)
y=-10*log(x)
hist(y,probability = T)
z=seq(min(y),max(y),by=0.01)
lines(z,dexp(z,1/10))
qqPlot(y,"exp",rate=1/10)
#c

x=runif(1000)
y=-2*log(x)
hist(y,probability = T,main = "exp")
z=seq(0,15,by=0.1)
lines(z,dchisq(z,2))
#q()

#Header 34-2-9
#a
par(mfrow=c(1,2))
N=1000
n=20
lambda=5
y=vector(mode = "numeric",length = N)
for (i in 1:N) {
  x=rexp(n,lambda)
  y[i]=min(x)
}
hist(y,probability = T)
z=seq(min(y),max(y),by=0.01)
lines(z,dexp(z,lambda*n))
qqPlot(y,"exp",rate=lambda*n)
#e

chisq=function(n,lambda){
  x=rexp(n,lambda)
  y=2*lambda*x
  hist(y,probability = T)
  z=seq(min(y),max(y),by=0.01)
  lines(z,dchisq(z,2))
  qqPlot(y,"chisq",df=2)
  return(y)
}
n=1000
lambda=5
y=0
chisq(n,lambda)
#f

chisq1=function(n,lambda){
  x1=rexp(n,lambda)
  x2=rexp(n,lambda)
  y=x1/(x1+x2)
  hist(y,probability = T)
  z=seq(min(y),max(y),by=0.01)
  lines(z,dunif(z,0,1))
  qqPlot(y,"unif",min=0,max=1)
  return(y)
}
n=1000
lambda=5
y=0
chisq1(n,lambda)
#g

chisq2=function(n,lambda){
  x1=rexp(n,lambda)
  x2=rexp(n,lambda)
  y=(x1-x2)/(x1+x2)
  hist(y,probability = T)
  z=seq(min(y),max(y),by=0.01)
  lines(z,dunif(z,-1,1))
  qqPlot(y,"unif",min=-1,max=1)
  return(y)
}
n=1000
lambda=5
y=0
chisq2(n,lambda)
#h

#for fifth quantile
library(car)
chisq3=function(N,n,lambda,r){
  y=vector(mode = "numeric",length = N)
  for (i in 1:N) {
    x=rexp(n,lambda)
    y[i]=sum(sort(x)[1:r])+(n-r)*quantile(x,probs = 1/r)
  }
  hist(y,probability = T)
  z=seq(min(y),max(y),by=0.01)
  lines(z,dgamma(z,r,lambda))
  qqPlot(y,"gamma",shape=r,rate=lambda)
  return(y)
}
N=1000
n=20
lambda=6
r=5
chisq3(N,n,lambda,r)
#q()

#Header 34-2-10
#a
par(mfrow=c(1,2))
ddexp=function(x,mu,sigma=1){
  f=(1/sigma)*exp(-(x-mu)/sigma)
  return(f)
}
pdexp=function(q,mu,sigma=1){
  p=1-exp((mu-q)/sigma)
  return(p)
}
rdexp=function(n,mu,sigma=1){
  x=runif(n)
  y=mu-(sigma*log(1-x))
  return(y)
}
qdexp=function(p,mu,sigma=1){
  q=mu-(sigma*log(1-p))
  return(q)
}
qqplot_dexp=function(x,mu,sigma=1){
  sample_probs=seq(1:length(x))/(length(x)+1)
  theorical_quantile=qdexp(sample_probs,mu,sigma)
  plot(theorical_quantile,sort(x),xlab = "Theorical quantile",ylab = "Sample quantile",main = "Double exponential Q-Q plot",cex.main=1.5)
  qG<-function(p) qdexp(p,mu,sigma)
  qqline(y=sort(x),distribution = qG)
}
y=vector(mode = "numeric",length = 1000)
for (i in 1:1000) {
  x=rdexp(30,5,3)
  y[i]=min(x)
}
hist(y,probability = T)
z=seq(min(y),max(y),by=0.01)
lines(z,ddexp(z,5,3/30))
qqplot_dexp(y,5,3/30)
#b

n=15
i=0
sigma=0.5
mu=1
N=100
j=0
y=0
z=0
x=0
u=0
for(j in 1:N){
  for(i in 1:n){
    y[i]=x-mu
    y[i]=rexp(1,sigma)
    z[i]=sort(y)
  }
  u[j]=sum(y-z)
}
hist(u,prob=T)
m=seq(1,60,0.5)
lines(m,dchisq(m,(2*n)-2),col="red")
#q()

#Header 34-2-11
#a
par(mfrow=c(1,2))
library(car)
gamma=function(N,alpha1,alpha2,lambda){
  x=rgamma(N,alpha1,lambda)
  y=rgamma(N,alpha2,lambda)
  z=x/(x+y)
  hist(z,probability = T)
  h=seq(min(z),max(z),by=0.01)
  lines(h,dbeta(h,alpha1,alpha2))
  qqPlot(z,"beta",shape1=alpha1,shape2=alpha2)
  return(z)
}
N=100
alpha1=4
alpha2=6
lambda=3
gamma(N,alpha1,alpha2,lambda)
#b

par(mfrow=c(1,2))
library(car)
gamma1=function(N,alpha1,alpha2,lambda){
  x=rgamma(N,alpha1,lambda)
  y=rgamma(N,alpha2,lambda)
  z=x+y
  hist(z,probability = T)
  h=seq(min(z),max(z),by=0.01)
  lines(h,dgamma(h,alpha1+alpha2,lambda))
  qqPlot(z,"gamma",shape1=alpha1+alpha2,shape2=lambda)
  return(z)
}
N=100
alpha1=4
alpha2=6
lambda=3
gamma(N,alpha1,alpha2,lambda)
#c

par(mfrow=c(1,2))
gamma2=function(n,lambda){
  x=rgamma(n,n,scale=lambda)
  sigma=sqrt(n*(lambda^2))
  mu=n*lambda
  y=(x-mu)/sigma
  hist(y,probability = T)
  h=seq(min(y),max(y),by=0.01)
  lines(h,dnorm(h))
  qqnorm(y)
  return(y)
}
n=1000
lambda=3
gamma2(n,lambda)
#q()

#Header 34-2-12
#a
par(mfrow=c(1,2))
Chi_Squared=function(n,r){ #n=degrees of freedom  r=sample size
  x=matrix(0,nrow = r,ncol = length(n))
  for (i in 1:length(n)) {
    x[,i]=rchisq(r,n[i]) 
  }
  y=apply(x,1,sum)
  hist(y,probability = T)
  h=seq(min(y),max(y),by=0.01)
  lines(h,dchisq(h,sum(n)))
  library(car)
  qqPlot(y,"chisq",df=sum(n))
  return(y)
}
n=20
r=10
Chi_Squared(n,r)
#b

Chi_Squared1=function(n,df1,df2){
  x=rchisq(n,df1)
  y=rchisq(n,df2)
  z=(x/df1)/(y/df2)
  hist(z,probability = T)
  t=seq(0,15,by=0.1)
  lines(t,df(t,df1,df2))
  return(z)
}
n=100
df1=5
df2=8
Chi_Squared1(n,df1,df2)
#c

Chi_Squared2=function(n,df1,df2){
  x=rchisq(n,df1)
  y=rchisq(n,df2)
  z=x/(x+y)
  hist(z,probability = T)
  t=seq(min(z),max(z),by=0.1)
  lines(t,dbeta(t,df1/2,df2/2))
  return(z)
}
n=1000
df1=2
df2=1
Chi_Squared2(n,df1,df2)
#q()

#Header 34-2-13
#b

par(mfrow=c(1,2))
Normal=function(n,r){ #n=sample size  r=Number of variables
  x=matrix(rnorm(r*n)^2,nrow = n,ncol = r)
  y=apply(x,1,sum)
  hist(y,probability = T)
  h=seq(min(y),max(y),by=0.01)
  lines(h,dchisq(h,r))
  library(car)
  qqPlot(y,"chisq",df=r)
  return(y)
}
n=100
r=10
Normal(n,r)
#c

Normal2=function(n,u){ 
  x=rnorm(n)
  y=rnorm(n)
  z=x/y
  hist(z,probability = T,breaks = 50)
  u=seq(min(z),max(z),by=0.1)
  lines(u,dcauchy(u))
  return(z)
}
n=100
u=1
Normal2(n,u)
#q()

#Header 34-2-14
T_dist<-function(n,df){
  Z<-rnorm(n,0,1)
  V<-rchisq(n,df)
  T=Z/sqrt(V/df)
  return(T)
}
T_dist(100,20)
p<-seq(-5,5,0.01)
hist(T_dist(100,10),prob=T)
lines(p,dt(p,10),col="red")
#q()

#Header 34-2-15
L_normal=function(n,mu,sigma,m){
  for(i in 1:m){
    x=rlnorm(n,mu,sigma)
    y[i]=prod(x)
  }
  hist(y,probability = T)
  z=seq(min(y),max(y),by=0.1)
  lines(z,dlnorm(z,n*mu,n*sigma),col="red")
  return(y)
}
n=100
mu=0
sigma=0.5
m=300
y=0
#q()

#Header 34-2-18
#a
T1<-function(n,df){
  X<-rt(n,df)
  Y<-(X^2)
  hist(Y,probability = T)
  t=seq(0,15,by=0.1)
  lines(t,df(t,1,n),col="red")
  return(Y)
}
n=100
df=5
T1(n,df)
#q()

#Header 34-2-19
#a
n=100
alpha=2
x=rbeta(n,alpha,1)
y=-log(x)
hist(y,prob=T)
p=seq(0,10,0.1)
d=dexp(p,alpha)
lines(p,d,col="red")
#b

n=100
alpha=2
x=rbeta(n,alpha,1)
y=-2*alpha*log(x)
hist(y,prob=T)
p=seq(0,10,0.1)
d=dchisq(p,2)
lines(p,d,col="red")
#q()

#Header 34-2-20
beta=function(n,alpha1,alpha2){
  x=rbeta(n,alpha1,alpha2)
  y=(2*alpha1*x)/(2*alpha2*(1-x))
  hist(y,probability = T)
  z=seq(0,max(y),by=0.01)
  lines(z,df(z,2*alpha1,2*alpha2),col="red")
  return(y)
}
n=100
alpha1=5
alpha2=5
beta(n,alpha1,alpha2)
#q()