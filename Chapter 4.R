#--------------------------------------------------------#
# Mathmatical Statistics With R : Chapter 4              #
# Authors: Hamid Jahani, Vahid Rezaei Tabar              #
# Point estimators                                       #
#--------------------------------------------------------#

# q() is commented out so you don't accidently exit

#Example 7-4
n=10
x=0
likelihoodfunction=function(n,x,p){
  choose(n,x)*p^{x}*(1-p)^{n-x}
}
pseq=seq(0,1,by=0.02)
likelihoodfunction_binom=
  sapply(pseq,n=10,x=0,likelihoodfunction)
likelihoodfunction_binom=
  likelihoodfunction_binom/max(likelihoodfunction_binom)
plot(pseq,likelihoodfunction_binom,"l",xlab = "p"
     ,ylab = "likelihood",col="red")
legend(x=0,y=0.95,legend = "L(p|x=0)",col="red",
       box.col = "white",box.lwd = 0)
likelihoodfunction_binom=
  sapply(pseq,n=10,x=2,likelihoodfunction)
likelihoodfunction_binom=
  likelihoodfunction_binom/max(likelihoodfunction_binom)
lines(pseq,likelihoodfunction_binom,col="green")
legend(x=0.15,y=0.8,legend = "L(p|x=2)",col="green",
       box.col = "white",box.lwd = 0)
likelihoodfunction_binom=
  sapply(pseq,n=10,x=5,likelihoodfunction)
likelihoodfunction_binom=
  likelihoodfunction_binom/max(likelihoodfunction_binom)
lines(pseq,likelihoodfunction_binom,col="brown")
legend(x=0.42,y=0.8,legend = "L(p|x=5)",col="brown",
       box.col = "white",box.lwd = 0)
likelihoodfunction_binom=
  sapply(pseq,n=10,x=8,likelihoodfunction)
likelihoodfunction_binom=
  likelihoodfunction_binom/max(likelihoodfunction_binom)
lines(pseq,likelihoodfunction_binom,col="gray")
 legend(x=0.6,y=0.95,legend = "L(p|x=8)",col="gray",
         box.col = "white",box.lwd = 0)
likelihoodfunction_binom=
  sapply(pseq,n=10,x=10,likelihoodfunction)
likelihoodfunction_binom=
  likelihoodfunction_binom/max(likelihoodfunction_binom)
lines(pseq,likelihoodfunction_binom,col="blue")
legend(x=0.8,y=0.95,legend = "L(p|x=10)",col="blue",
       box.col = "white",box.lwd = 0)
#q()

#Example 8-4
likelihood=function(x){
  
  L=function(par)  dbinom(x,par[1],par[2])
  possibles=matrix(c(2,1/2,2,1/3,3,1/2,3,1/3),ncol=2,byrow=T)
  
  index=which.max(apply(possibles,1,L))
  possibles[index,]
}
likelihood(1)
#q()

#Example 9-4
x=2.45
sigma=1

likelihood_norm=function(mu) dnorm(x,mu,sigma)
curve(likelihood_norm,x-3*sigma,x+3*sigma,xlab = expression(mu),
      ylab = expression(L(mu,'|',x)))
#q()

#Example 10-4
#normalized likelihood function for the 
#maximum of the random sample
n=5
xmax=3.5
sigma=1
likelihood_norm=function(mu){(n*(pnorm(xmax-mu)^{n-1})*dnorm(xmax-mu))}
curve(likelihood_norm,-6,6,xlab = expression(mu),ylab = expression(L(mu|x)))
#q()

#Example 11-4
Likelihood_function=function(teta,x,n)
  exp((n*log(teta))-(teta*x))
MLE=function(y){
  n=length(y)
  mle=1/mean(y)
  curve(Likelihood_function(x,sum(y),n),0,2*mle,xlab = expression(theta),
        ylab = expression(paste(L,"(",theta,")")))
  cat("MLE =",mle)
  abline(v=mle,lty=2)
  legend("topright",expression(paste(hat(theta))),lty=2)
}
y=rexp(20,5)
MLE(y)
#q()

#Example 12-4
MSE_Comparison=function(n,m,teta){
  mle=mme=vector(mode = "numeric",length = m)
  for (i in 1:m) {
    x=runif(n,0,teta)
    mle[i]=max(x)
    mme[i]=2*mean(x)
  }
  MSE_mle=var(mle)
  MSE_mme=var(mme)
  cat("MSE of MLE =",MSE_mle,"\n","MSE of MME=",MSE_mme)
}
MSE_Comparison(20,100000,7)
#q()

#Example 13-4
n=200
x=rpois(n,2)
library(stats4)
##MLE of Poisson distribution
pois_ln=function(lambda){
  -sum(dpois(x,lambda,log = T))
}
pois_mle=mle(pois_ln,start = list(lambda=1),nobs=length(x))
summary(pois_mle)
#q()

#Example 14-4
f=function(teta){# tata must be in [-1/4,1/4]
  y=sample(c(-2,-1,1,2),1,
           prob = c((1/4)-teta,(1/4)-(teta/2),(1/4)+
                      (teta/2),(1/4)+teta))
  return(y)
}
x=f(1/8)
if(x==-1 | x==-2) print("MLE= -1/4")  else  print("MLE= 1/4")
#q()

#Example 16-4
x=rnorm(1000,4,1)
library(stats4)
nlogl=function(mean,sd){-sum(dnorm(x,mean = mean,sd=sd,
                                   log = TRUE))}
norm_mle=mle(nlogl,start = list(mean=median(x),sd=IQR(x)),
             nobs = length(x))
summary(norm_mle)
#q()

#Example 18-4
MSE_Comparison=function(n,m,teta){
  mle=mme=vector(mode = "numeric",length = m)
  library(nimble)
  for (i in 1:m) {
    x=rdexp(n,location=teta)
    mle[i]=median(x)
    mme[i]=mean(x)
  }
  MSE_mle=var(mle)
  MSE_mme=var(mme)
  cat("MSE of MLE =",MSE_mle,"\n","MSE of MME =",MSE_mme)
}
MSE_Comparison(20,1000000,5)
#q()

#Example 19-4
n=20
x=rgamma(n,5)
gamma_ln <- function(shape) 
  -sum(dgamma(x, shape, log = TRUE))
library(stats4)
gamma_mle <- mle(gamma_ln, start = list(shape = 3), nobs = length(x))
summary(gamma_mle)
#

MSE_Comparison=function(n,m,alpha){
  MLE=mme=vector(mode = "numeric",length = m)
  library(stats4)
  gamma_ln <- function(shape) 
    -sum(dgamma(x, shape, log = TRUE))
  for (i in 1:m) {
    x=rgamma(n,shape = alpha)
    MLE[i]=mle(gamma_ln, start = list(shape = 3), nobs = length(x))@coef
    mme[i]=mean(x)
  }
  MSE_mle=var(MLE)
  MSE_mme=var(mme)
  cat("MSE of MLE =",MSE_mle,"\n","MSE of MME =",MSE_mme)
}
MSE_Comparison(20,10000,6)
#q()

#Example 20-4
f=function(p){
  par(mfrow=c(2,2))
  n=c(10000,20000,30000,60000)
  m=c(10,20,30,60)
  VAR=vector(mode = "numeric",length = 4)
  for(i in 1:4){
    x=matrix(rbinom(n[i],1,prob = p),nrow=m[i],ncol=1000)
    z=apply(x,2,mean)
    s=sqrt(p*(1-p)/m[i])
    hist(z,probability = T)
    f=seq(min(z),max(z),by=0.01)
    lines(f,dnorm(f,p,s))
    VAR[i]=var(z)
  }
  cat("sample Variance =",VAR,"\n")
  cat("True Variance =",p*(1-p)/m[1:4],"\n",
      "Difference=",abs(VAR-(p*(1-p)/m[1:4])))
}
f(0.6)
#q()

#Example 21-4
par(mfrow=c(1,2))
x=matrix(rexp(100000,2),nrow = 100)
xbar=apply(x,2,mean)
xbar=1/xbar
hist(xbar,probability = T)
y=seq(min(xbar),max(xbar),length.out = 1000)
lines(y,dnorm(y,2,sqrt(4/100)))
# with for
xbar=vector(mode="numeric",length = 1000)
for(i in 1:1000){
  x=rexp(100,2)
  xbar[i]=1/mean(x)
}
hist(xbar,probability = T)
y=seq(min(xbar),max(xbar),length.out = 1000)
lines(y,dnorm(y,2,sqrt(4/100)))
#q()

#Example 22-4
x=matrix(runif(10000,0,5),nrow = 10,ncol = 1000)
y=apply(x, 2, max)/5
hist(y,probability =T)
h=seq(min(y),max(y),by=0.01)
lines(h,dbeta(h,10,1))
#q()

#Example 23-4
#undetstanding the sampling variation of the score function
par(mfrow=c(2,2))
#the normal model
ns=matrix(rnorm(1000,4,1),nrow = 100)
n=10
sample_means=colMeans(ns)
normal_score_fn=function(mu,xbar){n*(xbar-mu)}
mu=seq(from=2,to=8,by=0.2)
plot(mu,sapply(mu,normal_score_fn,xbar=sample_means[1]),
     "l",xlab=expression(mu),ylab=expression(s(mu)))
title(main = "A:score function plot of the normal model")
for(i in 2:20)lines(mu,sapply(mu,normal_score_fn,
                              xbar=sample_means[i]),"l")
abline(v=4)
abline(h=0)
#the poisson model
ps=matrix(rpois(1000,4),nrow = 100)
n=10
sample_means=colMeans(ps)
poisson_score_fn=function(theta,xbar){(n*(xbar-theta))/theta}
theta=seq(from=2,to=8,by=0.2)
plot(theta,sapply(theta,poisson_score_fn,xbar=sample_means[1]),
     "l",xlab=expression(lambda),ylab=expression(s(lambda)),
     ylim=c(-5,15))
title(main = "B:score function plot of the poisson model")
for(i in 2:20)lines(theta,sapply(theta,poisson_score_fn,
                                 xbar=sample_means[i]),"l")
abline(v=4)
abline(h=0)
#the binomial model
bs=matrix(rbinom(1000,7,0.6),nrow = 100)
n=10
sample_means=colMeans(bs)
binomial_score_fn=function(p,xbar){(n*(xbar-(10*p)))/(p*(1-p))}
p=seq(from=0,to=1,by=0.02)
plot(p,sapply(p,binomial_score_fn,xbar=sample_means[1]),
     "l",xlab=expression(p),ylab=expression(s(p)))
title(main = "C:score function plot of the binomial model")
for(i in 2:20)lines(p,sapply(p,binomial_score_fn,
                             xbar=sample_means[i]),"l")
abline(v=4)
abline(h=0)
#the cauchy model
cs=matrix(rcauchy(1000,4,1),nrow = 100)
n=10
cauchy_score_fn=function(mu,x){sum(2*(x-mu)/(1+(x-mu)^{2}))}
mu=seq(from=-15,to=20,by=0.5)
plot(mu,sapply(mu,cauchy_score_fn,x=cs[,1]),
     "l",xlab=expression(mu),ylab=expression(s(mu)),
     ylim=c(-50,50))
title(main = "D:score function plot of the cauchy model")
for(i in 2:10)lines(mu,sapply(mu,cauchy_score_fn,
                              x=cs[,i]),"l")
abline(v=4)
abline(h=0)
#q()

#Example 24-4
n=20
x=rlnorm(n,1,2)
Lognormal_ln <- function(meanlog,sdlog) 
  -sum(dlnorm(x,meanlog,sdlog,log = TRUE))
library(stats4)
Lognrmal_mle <- mle(Lognormal_ln, start = list(meanlog=1,sdlog=1), nobs = length(x))
summary(Lognrmal_mle)
y=log(x)
mu=Lognrmal_mle@coef[1]
sigma2=Lognrmal_mle@coef[2]^2
J=exp(mu+(0.5*sigma2))
J
exp(mean(y)+((1/(2*n))*(n-1)*var(y)))
#q()

#The Delta Method Theorem 2-4
x=matrix(rbinom(5000000,1,2/3),nrow = 500)
p_hat=apply(x, 2, mean)
g=p_hat/(1-p_hat)
par(mfrow=c(1,2))
hist(g,probability =T)
z=seq(min(g),max(g),length.out = 1000)
lines(z,dnorm(z,2,sqrt(0.036)))
library(car)
qqPlot(g)

