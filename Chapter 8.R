#--------------------------------------------------------#
# Mathmatical Statistics With R : Chapter 8              #
# Authors: Hamid Jahani, Vahid Rezaei Tabar              #
# Likelihood Ratio Test                                  #
#--------------------------------------------------------#

# q() is commented out so you don't accidently exit

#Example 1-8
Test=function(n,alpha){
  x=rnorm(n)
  c=qchisq(1-(alpha),n)
  t=sum(x^2)
  if(t>c)
    print("H0 was rejected")
  else
    print("H0 was accepted")
  cat("beta","= ",pchisq(c/2,n))
}
Test(10,0.05)
#q()

#Example 4-8
LRT_NormalMean=function(mu,sigma2,n,alpha){
  x=rnorm(n,mu,sqrt(sigma2))
  c=sqrt(sigma2/n)*qnorm(1-(alpha/2))
  if(abs(mean(x))>c)
    print("H0 was rejected")
  else
    print("H0 was accepted")
}
LRT_NormalMean(2,10,10,0.05)
#q()

#Example 5-8
L_kernel=function(beta,alpha,n,y)
  (beta^(-n*alpha))*exp(-y/beta)
curve(L_kernel(x,5,5,10),0,0.8,ylab = expression(paste("L(",beta,")")))
title(main = "Likelihood function")
#

g=function(alpha,n,y)
  ((alpha/y)^(-n*alpha))*exp(-n*(y-alpha))
curve(g(5,5,x),0,10,ylab = "g(y)")
abline(h=0.75,lty=3)
abline(v=5)
f=function(alpha,n,y,a)
  ((alpha/y)^(-n*alpha))*exp(-n*(y-alpha))-a
root=uniroot(f,c(5,7),a=0.75,alpha=5,n=5)$root
lines(c(root,root),c(-1,0.75),lty=2)
legend(x=8,y=0.6,expression(lambda[0],K),lty=c(3,2))
#

LRT_gamma=function(lambda,beta,beta0,alpha,n){
  x=rgamma(n,lambda,scale = beta)
  k=(1/(2*n))*qnorm(1-alpha,2*n*lambda)
  if((mean(x)/beta0)>=k)
    print("H0 was rejected")
  else
    print("H0 accsepts")
}
LRT_gamma(5,2,4,0.05,25)
#q()

#Example 7-8
LRT_4=function(teta){
  z=sample(1:4,1,prob=c(teta/3,(1-teta)/3,1/2,1/6))
  if(z==1 | z==2)
    print("H0 was rejected in Test 1")
  else
    print("H0 was accepted in Test 1")
  if(z==3)
    print("H0 was rejected in Test 2")
  else
    print("H0 accept in Test 2")
}
LRT_4(1/8)
#q()

#Example 8-8
LRT_NormalMean2=function(mu,sigma2,mu0,n,alpha){
  x=rnorm(n,mu,sqrt(sigma2))
  t=(sqrt(n)*(mean(x)-mu0))/sd(x)
  c=qt(1-(alpha/2),n-1)
  if(abs(t)>c)
    print("H0 was rejected")
  else
    ("H0 was accepted")
}
LRT_NormalMean2(15,10,14,20,0.05)
#q()

#Example 9-8
LRT_Uniform=function(teta,ro,ro_0,n,alpha){
  x=runif(n,teta-ro,teta+ro)
  equation=function(c,n1,alpha1)
    ((n1*(c^(n1-1)))-(n1-1)*(c^n1))-alpha1
  c=uniroot(equation,c(0,1),n1=n,alpha1=alpha)$root
  t=(max(x)-min(x))/(2*ro_0)
  if(t<c)
    print("H0 was rejected")
  else
    print("H0 accrpts")
}
LRT_Uniform(5,5,7,10,0.05)
#q()

#Example 10-8
Lambda=function(m1,n1,t2)
  1/(1+(t2/(m1+n1-2)))
LRT_TwoNormal=function(mu1,mu2,sigma2,m,n,alpha){
  x=rnorm(m,mu1,sqrt(sigma2))
  y=rnorm(n,mu2,sqrt(sigma2))
  k1=((m*n)/(m+n))*((mean(x)-mean(y))^2)
  k2=(1/(m+n-2))*(((m-1)*var(x))*((n-1)*(var(y))))
  t=sqrt(k1/k2)
  c=qt(1-(alpha/2),m+n-2)
  curve(Lambda(m,n,x),0,3*qt(1-(alpha/2),m+n-2),ylab=expression(paste(lambda,("x,y"))),xlab = expression(t^2))
  abline(v=c,lty=2)
  abline(v=abs(t),lty=3)
  legend("topright",c("C","|t|"),lty = c(2,3))
  if((abs(t))>c)
    print("H0 was rejected")
  else
    print("H0 accpets")
}
LRT_TwoNormal(10,14,6,30,40,0.05)
#q()

#Example 11-8
Lambda=function(t)
  exp(-2*t)
LRT_2Exponential=function(mu,n,alpha){
  k=length(mu)
  x=matrix(0,n,k)
  for(i in 1:k)
    x[,i]=rdexp(n,mu[i],1)
  x1=apply(x, 2, min)
  x11=min(x)
  t=2*n*sum(x1-x11)
  c=qchisq(1-alpha,(2*k)-2)
  curve(Lambda(x),0,qchisq(1-(alpha),(2*k)-2)/k,ylab=expression(paste(lambda,("x"))),xlab = expression(t))
  if(t>c)
    print("H0 was rejected")
  else
    print("H0 was accepted")
}
LRT_2Exponential(c(1,1.3,1.2,1.2),15,0.05)
#q()

#Example 12-8
LRT_NormalVar=function(mu,sigma2,n,alpha){
  data=list()
  for(i in 1:length(mu))
    data[[i]]=rnorm(n[i],mu[i],sqrt(sigma2[i]))
  Vars=sapply(data, var)
  lambda=prod((((n-1)/n)*Vars)^(n/2))/(((1/sum(n))*sum((n-1)*Vars))^(sum(n)/2))
  t=-2*log(lambda)
  c=qchisq(1-alpha,length(mu)-1)
  if(t>c)
    print("H0 was rejected")
  else
    print("H0 was accepted")
}
mu=c(15,14,10,2,4,8)
sigma2=c(8,8,9,12,13,15)
n=c(65,61,62,61,65,60)
LRT_NormalVar(mu,sigma2,n,0.05)
[1] "H0 was rejected"

Lambda=function(mu,sigma2,n,m){
  t=c()
  data=list()
  for(j in 1:m){
    for(i in 1:length(mu))
      data[[i]]=rnorm(n[i],mu[i],sqrt(sigma2[i]))
    Vars=sapply(data, var)
    lambda=prod((((n-1)/n)*Vars)^(n/2))/(((1/sum(n))*sum((n-1)*Vars))^(sum(n)/2))
    t[j]=-2*log(lambda)
  }
  library(car)
  qqPlot(t,"chisq",df=length(mu)-1)
}
Lambda(mu,sigma2,n,1000)
#q()

#Example 16-8
Lambda=function(t,n,m,a)
  (((n+m)^(n+m))/((n^n)*(m^m)))*((t^n)*((1-t)^m))-a
curve(Lambda(x,10,20,0),0,0.8,ylab=expression(paste(c[n],phi(t))),xlab = "t")
abline(h=0.7,lty=3)
a=uniroot(Lambda,c(0.2,0.3),a=0.7,n=10,m=20)$root      
b=uniroot(Lambda,c(0.3,0.5),a=0.7,n=10,m=20)$root 
lines(c(a,a),c(-1,0.7),lty=2,)
lines(c(b,b),c(-1,0.7),lty=2)
#

Equations<-function(x,parms){
  c(F1=pbeta(x[2],parms[1],parms[2])-pbeta(x[1],parms[1],parms[2])-(1-parms[3]),
    F2=(x[2]^parms[1])*((1-x[2])^parms[2])-(x[1]^parms[1])*((1-x[1])^parms[2]))
}
curve(dbeta(x,10,20),0,1,ylab = "f(x)",main="Beta Disrtibution")
#

library(rootSolve)
c=multiroot(Equations,start=c(0.15,0.55),parms = c(10,20,0.05))$root
LRT_Exponentials=function(c,n,m,teta,mu,alpha){
  x=rexp(n,1/teta)
  y=rexp(m,1/mu)
  t=sum(x)/(sum(x)+sum(y))
  if(t<c[1] | t>c[2])
    print("H0 was rejected")
  else
    print("H0 was accepted")
  curve(Lambda(x,10,20,0),0,0.8,ylab=expression(paste(c[n],phi(t))),xlab = "t")
  abline(v=c[1],lty=2)
  abline(v=c[2],lty=2)
  abline(v=t,lty=3)
  legend("topright",c("t","c"),lty = c(3,2))
}
LRT_Exponentials(c,10,20,5,2)
#q()

#Example 17-8
Lambda=function(t,n,a)
  ((exp(1)/n)^(n/2))*(t^(n/2)*exp(-t/2))-a
curve(Lambda(x,10,0),0,2*qchisq(0.95,10),ylab=expression(paste(c[n],phi(t))),xlab = "t")
abline(h=0.7,lty=3)
a=uniroot(Lambda,c(0,10),a=0.7,n=10)$root      
b=uniroot(Lambda,c(10,20),a=0.7,n=10)$root 
lines(c(a,a),c(-1,0.7),lty=2)
lines(c(b,b),c(-1,0.7),lty=2)
#

Equations<-function(x,parms){
  c(F1=(pchisq(x[2],parms[1]-1)-pchisq(x[1],parms[1]-1))-(1-parms[2]),
    F2=(x[2]^(3/2))*dchisq(x[2],parms[1]-1)-(x[1]^(3/2))*dchisq(x[1],parms[1]-1))
}
curve(dchisq(x,9),0,2*qchisq(0.95,9),ylab = "f(x)",main="Chi-Squared Distribution")
library(rootSolve)
c=multiroot(Equations,start=c(2,17),parms = c(10,0.05))$root
#

LRT_Normalvar2=function(c,n,mu,sigma02,sigma2,alpha){
  x=rnorm(n,mu,sqrt(sigma2))
  t=n*var(x)/sigma02
  if(t<c[1] | t>c[2])
    print("H0 was rejected")
  else
    print("H0 was accepted")
  curve(Lambda(x,10,0),0,2*qchisq(1-alpha,n-1),ylab=expression(paste(c[n],phi(t))),xlab = "t")
  abline(v=c[1],lty=2)
  abline(v=c[2],lty=2)
  abline(v=t,lty=3)
  legend("topright",c("t","c"),lty = c(3,2))
}
LRT_Normalvar2(c,10,15,10,13,0.05)
#

Equations<-function(x,parms){
  c(F1=(pchisq(x[2],parms[1]-1)-pchisq(x[1],parms[1]-1))-(1-parms[2]),
    F2=(x[2])*dchisq(x[2],parms[1]-1)-(x[1])*dchisq(x[1],parms[1]-1))
}
library(rootSolve)
multiroot(Equations,start=c(2,17),parms = c(10,0.05))$root
#q()

#Example 19-8
Lambda=function(t,n,a)
  ((exp(1)/(2*n))^n)*((t^n)*exp(-t/2))-a
curve(Lambda(x,10,0),0,2*qchisq(0.95,18),ylab=expression(paste(c[n],phi(t))),xlab = "t")
abline(h=0.7,lty=3)
a=uniroot(Lambda,c(10,20),a=0.7,n=10)$root      
b=uniroot(Lambda,c(20,30),a=0.7,n=10)$root 
lines(c(a,a),c(-1,0.7),lty=2)
lines(c(b,b),c(-1,0.7),lty=2)
legend(x=44,y=0.5,expression(paste(lambda[0]),"a,b"),lty = c(3,2))
#

Equations<-function(x,parms){
  c(F1=(pchisq(x[2],2*(parms[1]-1))-pchisq(x[1],2*(parms[1]-1)))-(1-parms[2]),
    F2=(x[2]^2)*dchisq(x[2],2*(parms[1]-1))-(x[1]^2)*dchisq(x[1],2*(parms[1]-1)))
}
curve(dchisq(x,18),0,2*qchisq(0.95,18),ylab = "f(x)",main="Chi-Squared Distribution")
library(rootSolve)
c=multiroot(Equations,start=c(8,30),parms = c(10,0.05))$root
#

LRT_2Exponential2=function(c,n,mu,sigma0,sigma,alpha){
  x=rdexp(n,mu,sigma)
  t=2*(sum(x-min(x)))/sigma0
  if(t<c[1] | t>c[2])
    print("H0 was rejected")
  else
    print("H0 was accepted")
  curve(Lambda(x,10,0),0,2*qchisq(1-alpha,2*n-2),ylab=expression(paste(c[n],phi(t))),xlab = "t")
  abline(v=c[1],lty=2)
  abline(v=c[2],lty=2)
  abline(v=t,lty=3)
  legend("topright",c("t","c"),lty = c(3,2))
}
LRT_2Exponential2(c,10,15,5,10,0.05)
#q()

#Example 20-8
Lambda=function(t,n,m,a){
  c_mn=(((m+n)/n)^((m+n)/2))*(((m-1)*n)/((n-1)*m))^(m/2)
  phi=(t^(m/2))/(1+((m-1)/(n-1))*t)^((m+n)/2)
  return((c_mn*phi)-a)
}
curve(Lambda(x,7,8,0),0,2*qf(0.95,6,8),ylab=expression(paste(c[n],phi(t))),xlab = "t")
abline(h=0.7,lty=3)
a=uniroot(Lambda,c(0,1),a=0.7,n=7,m=9)$root      
b=uniroot(Lambda,c(1,2),a=0.7,n=7,m=9)$root 
lines(c(a,a),c(-1,0.7),lty=2)
lines(c(b,b),c(-1,0.7),lty=2)
legend(x=5.7,y=0.6,expression(paste(lambda[0]),"a,b"),lty = c(3,2))
#

Equations<-function(x,parms){
  phi=function(m,n,t) (t^(m/2))/(1+((m-1)/(n-1))*t)^((m+n)/2)
  c(F1=(pf(x[2],parms[1]-1,parms[2]-1)-pf(x[1],parms[1]-1,parms[2]-1))-(1-parms[3]),
    F2=phi(parms[1],parms[2],x[2])-phi(parms[1],parms[2],x[1]))
}
curve(df(x,6,8),0,2*qf(0.95,6,8),ylab = "f(x)",main="F Distribution")
library(rootSolve)
c=multiroot(Equations,start=c(0.2,2.5),parms = c(7,9,0.05))$root
#

LRT_Normalvar3=function(c,m,n,mu1,mu2,sigma12,sigma22,delta,alpha){
  x=rnorm(m,mu1,sqrt(sigma12))
  y=rnorm(n,mu2,sqrt(sigma22))
  t=(((n-1)*m)*(((n-1)/n)*var(x)))/(((m-1)*n)*(delta*((n-1)/n)*var(y)))
  if(t<c[1] | t>c[2])
    print("H0 was rejected")
  else
    print("H0 was accepted")
  curve(Lambda(x,m-1,n-1,0),0,2*qf(1-alpha,m-1,n-1),ylab=expression(paste(c[n],phi(t))),xlab = "t")
  abline(v=c[1],lty=2)
  abline(v=c[2],lty=2)
  abline(v=t,lty=3)
  legend("topright",c("t","c"),lty = c(3,2))
}
LRT_Normalvar3(c,7,9,5,10,5,10,0.5,0.05)
#q()


#Example 22-8
Lambda=function(t,m,n,a){
  c_mn=(((m+n)^((m+n)/2))/((m^(n/2))*(n^(n/2))))*(((m-1)/(n-1))^(m/2))
  phi=(t^(m/2))/(1+((m-1)/(n-1))*t)^((m+n)/2)
  return((c_mn*phi)-a)
}
Equations<-function(x,parms){
  phi=function(m,n,t) (t^(m/2))/(1+((m-1)/(n-1))*t)^((m+n)/2)
  c(F1=(pf(x[2],parms[1]-1,parms[2]-1)-pf(x[1],parms[1]-1,parms[2]-1))-(1-parms[3]),
    F2=phi(parms[1],parms[2],x[2])-phi(parms[1],parms[2],x[1]))
}
curve(df(x,14,19),0,2*qf(0.95,14,19),ylab = "f(x)",main="F Distribution")
library(rootSolve)
c=multiroot(Equations,start=c(0.2,2.5),parms = c(15,20,0.05))$root
#

LRT_IG=function(c,m,n,mu1,mu2,lambda1,lambda2,alpha){
  library(statmod)
  x=rinvgauss(m,mu1,lambda1)
  y=rinvgauss(n,mu2,lambda2)
  S1=sum((1/x)-(1/mean(x)))
  S2=sum((1/y)-(1/mean(y)))
  t=((n-1)/(m-1))*(S1/S2)
  if(t<c[1] | t>c[2])
    print("H0 was rejected")
  else
    print("H0 was accepted")
  curve(Lambda(x,m-1,n-1,0),0,2*qf(1-alpha,m-1,n-1),ylab=expression(paste(c[n],phi(t))),xlab = "t")
  abline(v=c[1],lty=2)
  abline(v=c[2],lty=2)
  abline(v=t,lty=3)
  legend("topright",c("t","c"),lty = c(3,2))
}
LRT_IG(c,15,20,5,10,5,5,0.05)
#q()

#Example 23-8
mu=c(15,14,10,2,4,8)
sigma2=c(8,8,9,12,13,15)
n=c(5,7,10,4,10,4)

Lambda=function(mu,sigma2,n,m){
  lambda=c()
  data=list()
  for(j in 1:m){
    for(i in 1:length(mu))
      data[[i]]=rnorm(n[i],mu[i],sqrt(sigma2[i]))
    Vars=sapply(data, var)
    lambda[j]=prod((((n-1)/n)*Vars)^(n/2))/(((1/sum(n))*sum((n-1)*Vars))^(sum(n)/2))
  }
  return(lambda)
}
x=Lambda(mu,sigma2,n,1000000)
library(propagate)
fitDistr(x)
