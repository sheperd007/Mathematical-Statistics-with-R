#--------------------------------------------------------#
# Mathmatical Statistics With R : Chapter 5              #
# Authors: Hamid Jahani, Vahid Rezaei Tabar              #
# Confidence interval                                    #
#--------------------------------------------------------#

# q() is commented out so you don't accidently exit

#Example 1-5
Confidence=function(n,m,mu,alpha){ #n=sample size   
  #m=Number of simulations
  counter=0
  for(i in 1:m){
    x=rnorm(n,2,1)
    upper.bound=mean(x)+(qnorm(1-(alpha/2))*(1/sqrt(n)))
    lower.bound=mean(x)-(qnorm(1-(alpha/2))*(1/sqrt(n)))
    if(mu>lower.bound & mu<upper.bound)
      counter=counter+1
  }
  cat("%",(counter/m)*100)
}
Confidence(400,1000,2,0.05)
Confidence(40,1000,2,0.05)
#q()

#Example 2-5
Confidence=function(n,m,teta,alpha){ #n=sample size  
  #m=Number of simulations
  counter=0
  for(i in 1:m){
    x=runif(n,0,teta)
    upper.bound=exp((1/n)*((qchisq(1-(alpha/2),2*n)/2)+sum(log(x))))                                                      
    lower.bound=exp((1/n)*((qchisq(alpha/2,2*n)/2)+sum(log(x))))   
    if(teta>lower.bound & teta<upper.bound)
      counter=counter+1
  }
  cat("%",(counter/m)*100)
}
Confidence(100,1000,5,0.05)
Confidence(1000,1000,5,0.05)
#q()

#Example 4-5
Confidence=function(n,m,mu,sigma2,alpha){
  Diffrence=vector(mode = "numeric",length=m)
  counter=0
  for (i in 1:m) {
    x=rnorm(n,mu,sqrt(sigma2))
    L1=2*qnorm(1-(alpha/2))*(sqrt(sigma2)/sqrt(n))
    L2=2*qt(1-(alpha/2),n-1)*(sd(x)/sqrt(n))
    Diffrence[i]=abs(L1-L2)
    if(L1<L2)
      counter=counter+1
  }
  cat("%",(counter/m)*100,"\n")
  return(head(Diffrence,50))
}
Confidence(5,10000,20,10,0.05)
Confidence(10,10000,20,10,0.05)
Confidence(20,10000,20,10,0.05)
Confidence(50,10000,20,10,0.05)
Confidence(100,10000,20,10,0.05)
#q()

#Example 6-5
#functions related to chapter 1

Confidence=function(n,m,mu,sigma,alpha){
  Diffrence=vector(mode = "numeric",length=m)
  counter=0
  for (i in 1:m) {
    x=rdexp(n,mu,sigma)
    L1=2*sum(x-mu)*((1/qchisq(alpha/2,2*n))-(1/qchisq(1-      (alpha/2),2*n)))
    L2=2*sum(x-min(x))*((1/qchisq(alpha/2,(2*n)-2))-(1/qchisq(1-(alpha/2),(2*n)-2)))
    Diffrence[i]=abs(L1-L2)
    if(L1<L2)
      counter=counter+1
  }
  cat("%",(counter/m)*100,"\n")
  return(head(Diffrence,50))
}
Confidence(5,1000,10,5,0.05)
Confidence(20,1000,10,5,0.05)
Confidence(50,1000,10,5,0.05)
Confidence(1000,1000,10,5,0.05)
#q()

#Example 7-5
#a
Confidence=function(n1,n2,m,mu1,mu2,sigma2,alpha){
  counter=0
  teta=mu1-mu2
  for (i in 1:m){
    x1=rnorm(n1,mu1,sqrt(sigma2))
    x2=rnorm(n2,mu2,sqrt(sigma2))
    Sp_2=((n1-1)*var(x1)+(n2-1)*var(x2))/(n1+n2-2)
    variation=(qt(1-(alpha/2),n1+n2-2)*sqrt(Sp_2))*sqrt((1/n1)+(1/n2))
    upper.bound=mean(x1)-mean(x2)+variation
    lower.bound=mean(x1)-mean(x2)-variation
    if(teta>lower.bound & teta<upper.bound)
      counter=counter+1
  }
  cat("%",(counter/m)*100)
}
Confidence(50,50,10000,5,15,10,0.05)
#b

Confidence=function(n1,n2,m,mu1,mu2,sigma1,sigma2,alpha){
  counter=0
  teta=sigma2/sigma1
  for (i in 1:m){
    x1=rnorm(n1,mu1,sqrt(sigma1))
    x2=rnorm(n2,mu2,sqrt(sigma2))
    upper.bound=(var(x2)/var(x1))*qf(1-(alpha/2),n1-    1,n2-1)
    lower.bound=(var(x2)/var(x1))*qf(alpha/2,n1-1,n2-1)
    if(teta>lower.bound & teta<upper.bound)
      counter=counter+1
  }
  cat("%",(counter/m)*100)
}
Confidence(50,50,10000,5,15,10,15,0.05)
#q()

#Eample 8-5
rexample=function(n,teta,a){
  y=runif(n)
  x=teta*(y^(1/a))
  return(x)
}
dexample=function(x,teta,a)
  return((a/teta)*((x/teta)^(a-1)))
Confidence=function(n,m,teta,a,alpha){
  counter=0
  for (i in 1:m){
    x=rexample(n,teta,a)
    upper.bound=max(x)*exp(qchisq(1-(alpha/2),2)/(2*n*a))
    lower.bound=max(x)*exp(qchisq((alpha/2),2)/(2*n*a))
    if(teta>lower.bound & teta<upper.bound)
      counter=counter+1
  }
  cat("%",(counter/m)*100)
}

Confidence(20,1000,10,3,0.05)
#q()

#Example 11-5
Equations<-function(x,parms){
  c(F1=pchisq(x[2],parms[1]-1)-pchisq(x[1],parms[1]-1)-(1-parms[2]),
    F2=(x[1]^2)*dchisq(x[1],parms[1])-(x[2]^2)*dchisq(x[2],parms[1]))
}
#
plot(0:100,dchisq(0:100,49),type="l",xlab="x",ylab="f(x)")
#
library(rootSolve)
multiroot(Equations,parm=c(50,0.05),start=c(30,70))
#
Confidence=function(m,mu,sigma2,alpha){
  counter=0
  for (i in 1:m){
    x=rnorm(50,mu,sqrt(sigma2))
    upper.bound=49*var(x)/33.36423
    lower.bound=49*var(x)/76.54419
    if(sigma2>lower.bound & sigma2<upper.bound)
      counter=counter+1
  }
  cat("%",(counter/m)*100)
}
Confidence(1000,5,10,0.05)
#q()

#Example 12-5
Equations<-function(x,parms){
  c(F1=pchisq(x[2],2*parms[1])-pchisq(x[1],2*parms[1])-(1-parms[2]),
    F2=(x[1]^2)*dchisq(x[1],2*parms[1])-(x[2]^2)*dchisq(x[2],2*parms[1]))
}
plot(5:80,dchisq(5:80,40),type = "l",xlab ="x",ylab = "f(x)")
#
library(rootSolve)
multiroot(Equations,parms = c(20,0.05),start =c(22.5,68))
Confidence=function(m,teta,alpha){
  library(rmutil)
  counter=0
  for (i in 1:m){
    x=rlaplace(20,s=teta)
    upper.bound=2*sum(abs(x))/25.82274
    lower.bound=2*sum(abs(x))/63.83405
    if(teta>lower.bound & teta<upper.bound)
      counter=counter+1
  }
  cat("%",(counter/m)*100)
}
Confidence(1000,10,0.05)
#q()

#Example 13-5
Shortest_confidence=function(n,m,teta,alpha){
  counter=0
  lower.bound=0
  for (i in 1:m){
    x=rbeta(n,teta,1)
    upper.bound=qchisq(1-alpha,2)/(-2*n*log(max(x)))
    if(teta>lower.bound & teta<upper.bound)
      counter=counter+1
  }
  cat("%",(counter/m)*100)
}
Shortest_confidence(100,1000,5,0.05)
#

Confidence=function(n,m,teta,alpha){
  Diffrence=vector(mode = "numeric",length=m)
  counter=0
  for (i in 1:m) {
    x=rbeta(n,teta,1)
    L1=qchisq(1-alpha,2)/(-2*n*log(max(x))) #lenght of Shortest confidence Interval
    L2=(qchisq(alpha/2,2)-qchisq(1- (alpha/2),2))/(2*n*log(max(x)))
    Diffrence[i]=abs(L1-L2)
    if(L1<L2)
      counter=counter+1
  }
  cat("%",(counter/m)*100,"\n")
  return(head(Diffrence,50))
}
Confidence(10,1000,5,0.05)
Confidence(50,1000,5,0.05)
Confidence(1000,1000,5,0.05)
#q()

#Example 14-5
Shortest_confidence=function(n,m,teta,alpha){
  counter=0
  for (i in 1:m){
    x=runif(n,-teta,teta)
    upper.bound=max(abs(x))/(alpha^(1/n))
    lower.bound=max(abs(x))
    if(teta>lower.bound & teta<upper.bound)
      counter=counter+1
  }
  cat("%",(counter/m)*100)
}

Shortest_confidence(10,1000,10,0.05)
#

confidence=function(n,m,teta,alpha){
  counter=0
  for (i in 1:m){
    x=runif(n,-teta,teta)
    upper.bound=max(abs(x))/((alpha/2)^(1/n))
    lower.bound=max(abs(x))/((1-(alpha/2))^(1/n))
    if(teta>lower.bound & teta<upper.bound)
      counter=counter+1
  }
  cat("%",(counter/m)*100)
}
confidence(10,1000,10,0.05)
#

Confidence=function(n,m,teta,alpha){
  Diffrence=vector(mode = "numeric",length=m)
  counter=0
  for (i in 1:m) {
    x=runif(n,-teta,teta)
    L1=max(abs(x))*((1/(alpha^(1/n)))-1) #lenght of Shortest confidence Interval
    L2=max(abs(x))*((1/((alpha/2)^(1/n)))-(1/((1-(alpha/2))^(1/n))))
    Diffrence[i]=abs(L1-L2)
    if(L1<L2)
      counter=counter+1
  }
  cat("%",(counter/m)*100,"\n")
  return(head(Diffrence,50))
}
Confidence(100,1000,5,0.05)
#q()

#Example 17-5
Equations<-function(x,parms){
  c(F1=pchisq(x[2],2*parms[1])-pchisq(x[1],2*parms[1])-(1-parms[2]),
    F2=x[1]*dchisq(x[1],2*parms[1])-x[2]*dchisq(x[2],2*parms[1]))
}
plot(30:160,dchisq(30:160,100),type = "l",xlab ="x",ylab = "f(x)")
#
library(rootSolve)
multiroot(Equations,parms = c(50,0.05),start = c(66,122))
#
Confidence=function(m,teta,alpha){
  library(rmutil)
  counter=0
  for (i in 1:m){
    x=rlaplace(50,s=teta)
    upper.bound=2*sum(abs(x))/74.74361
    lower.bound=2*sum(abs(x))/130.39099
    if(teta>lower.bound & teta<upper.bound)
      counter=counter+1
  }
  cat("%",(counter/m)*100)
}
Confidence(1000,10,0.05)
#q()

#Example 19-5
Confidence=function(n,m,teta,alpha){ #n=sample size   #m=Number of simulations
  counter=0
  for(i in 1:m){
    x=rexp(n,1/teta)
    upper.bound=mean(x)/(1-(qnorm(1-(alpha/2))/sqrt(n)))
    lower.bound=mean(x)/(1+(qnorm(1-(alpha/2))/sqrt(n)))
    if(teta>lower.bound & teta<upper.bound)
      counter=counter+1
  }
  cat("%",(counter/m)*100)
}
Confidence(100,1000,5,0.05)
Confidence(10,1000,5,0.05)
#q()

#Example 20-5
Confidence=function(n,m,teta,alpha){ #n=sample size   #m=Number of simulations
  counter=0
  for(i in 1:m){
    x=runif(n,0,teta)
    upper.bound=(2*mean(x))/(1-(qnorm(1-(alpha/2))/sqrt(3*n)))
    lower.bound=(2*mean(x))/(1+(qnorm(1-(alpha/2))/sqrt(3*n)))
    if(teta>lower.bound & teta<upper.bound)
      counter=counter+1
  }
  cat("%",(counter/m)*100)
}
Confidence(100,1000,5,0.05)
#q()

#Example 21-5
Confidence=function(n,m,teta,alpha){ #n=sample size   #m=Number of simulations
  counter=0
  for(i in 1:m){
    x=rbinom(n,1,teta)
    upper.bound=mean(x)+(qnorm(1-   (alpha/2))*sqrt((mean(x)*(1-mean(x)))/n))
    lower.bound=mean(x)-(qnorm(1-(alpha/2))*sqrt((mean(x)*(1-mean(x)))/n))
    if(teta>lower.bound & teta<upper.bound)
      counter=counter+1
  }
  cat("%",(counter/m)*100)
}
Confidence(10,1000,0.2,0.05)
Confidence(100,1000,0.2,0.05)
Confidence(1000,1000,0.2,0.05)
Confidence(10000,1000,0.2,0.05)
#q()

#Example 22-5
Confidence=function(n,m,teta,alpha){ #n=samplesize   #m=Number of simulations
  counter=0
  for(i in 1:m){
    x=rpois(n,teta)
    upper.bound=mean(x)+(qnorm(1-(alpha/2))*sqrt(mean(x)/n))
    lower.bound=mean(x)-(qnorm(1-(alpha/2))*sqrt(mean(x)/n))
    if(teta>lower.bound & teta<upper.bound)
      counter=counter+1
  }
  cat("%",(counter/m)*100)
}
Confidence(10,1000,0.2,0.05)
Confidence(100,1000,0.2,0.05)
Confidence(1000,1000,0.2,0.05)
#q()

#Example 23-5
Confidence=function(n,m,teta,alpha){ #n=sample size   #m=Number of simulations
  counter=0
  for(i in 1:m){
    x=rgeom(n,teta)
    upper.bound=1/(1+mean(x))+(qnorm(1-(alpha/2))*sqrt(mean(x)/(((1+mean(x))^3)*n)))
    lower.bound=1/(1+mean(x))-(qnorm(1-(alpha/2))*sqrt(mean(x)/(((1+mean(x))^3)*n)))
    if(teta>lower.bound & teta<upper.bound)
      counter=counter+1
  }
  cat("%",(counter/m)*100)
}
Confidence(5,1000,0.2,0.05)
Confidence(20,1000,0.2,0.05)
Confidence(100,1000,0.2,0.05)
#q()

#Example 24-5
Confidence=function(n1,n2,m,mu1,mu2,sigma1,sigma2,alpha){
  counter=0
  teta=mu1-mu2
  for (i in 1:m){
    x1=rnorm(n1,mu1,sqrt(sigma1))
    x2=rnorm(n2,mu2,sqrt(sigma2))
    variation=qnorm(1-(alpha/2))*sqrt((var(x1)/n1)+(var(x2)/n2))
    upper.bound=mean(x1)-mean(x2)+variation
    lower.bound=mean(x1)-mean(x2)-variation
    if(teta>lower.bound & teta<upper.bound)
      counter=counter+1
  }
  cat("%",(counter/m)*100)
}
Confidence(5,5,1000,3,7,8,20,0.05)
Confidence(100,100,1000,3,7,8,20,0.05)
Confidence(2000,2000,1000,3,7,8,20,0.05)