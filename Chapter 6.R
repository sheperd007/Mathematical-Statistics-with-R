#--------------------------------------------------------#
# Mathmatical Statistics With R : Chapter 6              #
# Authors: Hamid Jahani, Vahid Rezaei Tabar              #
# statistical hypothesis testing                         #
#--------------------------------------------------------#

# q() is commented out so you don't accidently exit


#Example 5-6
theta_H0 <- 10; theta_H1 <- 20;
xbar <- 15; n <- 5
1-pgamma(xbar,n,n/theta_H0) # Type I Error of Test 1 [1] 0.1321
pgamma(xbar,n,n/theta_H1) #Type II Error of Test 1 [1] 0.3225
xbar <- 15; n <- 15
1-pgamma(xbar,n,n/theta_H0) #Type I Error of Test2 [1] 0.0386
pgamma(xbar,n,n/theta_H1) #Type II Error of Test 2 [1] 0.1648
xbar <- 13; n <- 25
1-pgamma(xbar,n,n/theta_H0) #Type I Error of Test3 [1] 0.07536
pgamma(xbar,n,n/theta_H1) #Type II Error of Test 3 [1] 0.02614
xbar <- 18; n <- 25
1-pgamma(xbar,n,n/theta_H0) #Type I Error of Test4 [1] 0.0004492
pgamma(xbar,n,n/theta_H1) #Type II Error of Test 4 [1] 0.3262
#q()

#Example 6-6
Q1 <- function(x) {1-pgamma(15,shape=5,rate=5/x)}
Q2 <- function(x) {1-pgamma(15,shape=15,rate=15/x)}
Q3 <- function(x) {1-pgamma(13,shape=25,rate=25/x)}
Q4 <- function(x) {1-pgamma(18,shape=25,rate=25/x)}
curve(Q1,from=0.1,to=40,n=400,xlab=expression(theta),ylab=expression(Q(theta))
      ,type="l",lty=1,add=FALSE,ylim=c(0,1))
curve(Q2,from=0.1,to=40,n=400,type="l",lty=2,add=TRUE)
curve(Q3,from=0.1,to=40,n=400,type="l",lty=3,add=TRUE)
curve(Q4,from=0.1,to=40,n=400,type="l",lty=4,add=TRUE)
title(main="Various Power Functions")
exp_legends <- expression(paste(Q[phi[1]],"(",theta,")"),
                          paste(Q[phi[2]], "(",theta,")"),paste(Q[phi[3]],
                                                                "(",theta,")"),paste(Q[phi[4]],"(",theta,")"))
legend(x=c(30,40),y=c(0.7,0.01),exp_legends,lty=1:4,lwd=rep(1.5,4))
abline(v=c(10,20))
#q()

#Example 7-6
curve(dnorm(x,0,3/4),-4,5,ylab = "Y",col="Grey25")
curve(dnorm(x,1,3/4),-4,5,ylab = "Y",add = T,col="Grey")
ld.lower=qnorm(0.2119,mean=0,sd=3/4)
ld.upper=5
x=seq(-8,8,length.out = 100)
Collide=function(x){ dnorm(x,0.3/4)-dnorm(x,1,3/4)}
z=uniroot(Collide,c(0,2))$root
n=x[x>z+0.1]    
polygon(c(z,n,ld.upper),c(0,dnorm(n,mean = 0,sd=3/4),0),col="Grey")
ld.lower=-5
ld.upper=qnorm(0.2981,mean=1,sd=3/4)
x=seq(-8,8,length.out = 100)
i=(x >= ld.lower)&(x <= ld.upper)     #i=subset(x,(x >= ld.lower)&(x <= ld.upper))
polygon(c(ld.lower,x[i],ld.upper),c(0,dnorm(x[i],mean = 1,sd=3/4),0),col="Grey25")
abline(h=0)
title(main = "Type I & Type II Error")
legend("topright",legend = c("N[1,9/16]","N[0,9/16]"),col=c("Grey","Grey25"),lwd = c(2,2))
legend("topleft",legend = c("Type II","Type I"),col=c("Grey25","Grey"),lwd = c(2,2),pch = c(15,15))
#q()

#Example 8-6
#a
Test1=function(n) { #number of runing the test
  x=sample(1:5,n,replace = T,prob = c(0.2,0.3,0.1,0.3,0.1))
  z=length(x[x==1 | x==3 | x==5])
  print(z/n)
}
Test1(100)
Test1(1000)
Test1(10000)
#b

Test2=function(n) { #number of runing the test
  x=sample(1:5,n,replace = T,prob = c(0.2,0.3,0.1,0.3,0.1))
  z=length(x[x==2 | x==3])
  print(z/n)
}
Test2(100)
Test2(1000)
Test2(10000)
#q()

#Example 9-6
Test=function(sigma2){
  x=rnorm(1,sd=sqrt(sigma2))
  if(x>(1.645*sqrt(sigma2)))
    print("H0 rejects")
  else
    print("H0 is correct")
}
Test(5)
#q()

#Example 10-6
Test=function(n,alpha){
  library(rmutil)
  c=qchisq(1-alpha,2*n)
  x=rlaplace(n,s=2)
  stat=sum(abs(x))
  beta=pchisq((2/3)*qchisq(1-alpha,2*n),2*n)
  if(stat>c)
    print("H0 rejects")
  else
    print("H0 is correct")
  return(beta)
}
Test(15,0.05)
#q()

#Example 11-6
Test=function(alpha){
  c=qnorm(1-(alpha/2))
  x=rnorm(1)
  if(abs(x)>c)
    print("H0 rejects")
  else
    print("H0 is correct")
}
Test(0.05)
#q()

#Example 12-6
Test=function(n,P,alpha,observation){ #P=probability in H0
  P_value=pbinom(observation,n,P)
  if(P_value<alpha)
    print("H0 rejects")
  else
    print("H0 is correct")
  return(P_value)
}
Test(10,1/2,0.05,2)
#q()

#Example 14-6
MPbinomial <- function(H0, H1, alpha,n){
  k <- min(which((1-pbinom(0:n,size=n,prob=H0))<alpha))-1
  gamma <- (alpha-1+pbinom(k,size=n,prob=H0))/dbinom(k,size=n,prob=H0)
  return(list=c(k,gamma))
}
MPbinomial(H0=0.3,H1=0.8,alpha=0.05,n=5)
#q()

#Example 13-6
Test=function(H0,H1,alpha,n){
  a=MPbinomial(H0,H1,alpha,n)
  x=rbinom(1,n,H0)
  if(x<a[[1]])
    print("H0 is correct")
  if(x>a[[1]])
    print("H0 rejects")
  if(x==a[[1]]){
    y=runif(1)
    if(y<a[[2]])
      print("H0 rejects")
    else
      print("H0 is correct")
  }
}
Test(0.3,0.8,0.05,5) 
#q()

#Example 16-6
n=5
c=seq(0,90,length.out = 80)
alpha=pchisq(c,2*n,lower.tail = F)
beta.star=pchisq((2/3)*c,2*n,lower.tail = F)
plot(alpha,beta.star,type = "l",ylab = expression(1-beta),xlab = expression(alpha))
lines(1-alpha,1-beta.star,type = "l")
points(1/2,1/2,pch=16)
n=15
alpha=pchisq(c,2*n,lower.tail = F)
beta.star=pchisq((2/3)*c,2*n,lower.tail = F)
lines(alpha,beta.star,type = "l",lty=4)
lines(1-alpha,1-beta.star,type = "l",lty=4)
legend(c("topleft"),c("n=5"),lty=1)
legend(c("bottomright"),c("n=15"),lty=4,text.width = 0.07)
#q()

#Example 17-6
n=4
c=seq(0,200,length.out = 90)
alpha=pchisq(c/4,n,lower.tail = F)
beta.star=pchisq(c/9,n,lower.tail = F)
plot(alpha,beta.star,type = "l",ylab = expression(1-beta),xlab = expression(alpha))
lines(1-alpha,1-beta.star,type = "l")
points(1/2,1/2,pch=16)
n=16
alpha=pchisq(c/4,n,lower.tail = F)
beta.star=pchisq(c/9,n,lower.tail = F)
lines(alpha,beta.star,type = "l",lty=4)
lines(1-alpha,1-beta.star,type = "l",lty=4)
legend(c("topleft"),c("n=4"),lty=1,cex=0.6)
legend(c("bottomright"),c("n=16"),lty=4,cex= 0.6)
#q()

#Example 18-6
n=6
c=0:20
alpha=pnbinom(c,n,prob = 0.5,lower.tail = F)
beta.star=pnbinom(c,n,prob = 0.4,lower.tail = F)
plot(alpha,beta.star,type = "b",ylab = expression(1-beta),xlab = expression(alpha))
lines(1-alpha,1-beta.star,type = "b")
points(1/2,1/2,pch=16)
#q()

#Example 19-6
alpha=c(0,0.05,0.35,0.65,1)
beta=c(1,0.7,0.1,0.05,0)
plot(alpha,beta,type = "b",ylab = expression(beta),xlab = expression(alpha))
lines(1-alpha,1-beta,type = "b")
points(1/2,1/2,pch=16)
#q()

#Example 20-6
c=seq(0,1,length.out = 50)
alpha=1-c
beta=ifelse(alpha<=(1/2),yes = 0.5-alpha,no=0)
plot(alpha,beta,type = "l",ylab = expression(beta),xlab = expression(alpha),xlim = c(0,1),ylim = c(0,1))
lines(1-alpha,1-beta,type = "l")
points(1/2,1/2,pch=16)
lines(c(1,1),c(0,0.5),type = "l")
lines(c(0,0),c(0.5,1),type = "l")
#q()