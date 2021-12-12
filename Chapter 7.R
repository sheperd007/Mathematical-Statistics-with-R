#--------------------------------------------------------#
# Mathmatical Statistics With R : Chapter 7              #
# Authors: Hamid Jahani, Vahid Rezaei Tabar              #
# uniformly most powerful Test                           #
#--------------------------------------------------------#

# q() is commented out so you don't accidently exit

#Example 1-7
Power_function_1=function(p)  p^3
Power_function_2=function(p)  (3*(p^2)-2*(p^3))
curve(Power_function_1(x),0,1,ylab = expression(paste(pi[phi],"(",p,")")),xlab = "p")
curve(Power_function_2(x),0,1,add = T,lty=3)
exp_legends <- expression(paste(pi[phi[1]],"(",p,")"),paste(pi[phi[2]], "(",p,")"))
legend("topleft",exp_legends,lty = c(1,3),text.width = 0.14,cex = 1.5)
#q()

#Example 2-7
Power_function=function(k,n,mu)  (1-pchisq(n*(k-mu),2))
curve(Power_function(1,10,x),0,1,ylab = expression(paste(pi[phi],"(",mu,")")),xlab = expression(mu))
curve(Power_function(1,50,x),0,1,ylab = expression(paste(pi[phi],"(",mu,")"))
      ,xlab = expression(mu),add = T,lty=3)
legend("topleft",c("n=10","n=50"),lty = c(1,3))
#q()

#Example 3-7
Power_function=function(c1,c2,teta,n) 
  pchisq(((2*c1)/teta),2*n)+pchisq(((2*c2)/teta),2*n,lower.tail = F)
curve(Power_function(0.5,3,x,5),0,2,ylab = expression(paste(pi[phi],"(",theta,")")),xlab = expression(theta))
curve(Power_function(0.5,3,x,10),0,2,add = T,lty=3)
legend("bottomright",c("n=5","n=10"),lty = c(1,3))
#q()

#Example 10-7
f=function(teta0,teta1,x) (1+((x-teta0)^2))/(1+((x-teta1)^2))
curve(f(0,1,x),-10,10,ylab = expression(paste(f[theta[1]],"(",x,")","/",f[theta[2]],"(",x,")")),las=1)
abline(h=1,lty=3)
#q()

#Example 11-7
Power_function=function(teta,sigma2,teta0,n,alpha)
  return(pnorm((sqrt(n)*((teta-teta0)/sqrt(sigma2)))-((qnorm(1-alpha))/sqrt(n))))
Test=function(teta,sigma2,teta0,n,alpha){
  x=rnorm(n,teta,sqrt(sigma2))
  c=teta0+((qnorm(1-alpha)*sqrt(sigma2))/sqrt(n))
  xbar=mean(x)
  if(xbar>c)
    print("H0 rejects")
  else
    print("H0 accepts")
  print(Power_function(teta,sigma2,teta0,n,alpha))
  curve(Power_function(x,sigma2,teta0,n,alpha),teta0-(3*sqrt(sigma2)),
        teta0+(3*sqrt(sigma2)),ylab = expression(paste(pi[phi],"(",theta,")")),xlab = expression(theta))
  title(main = expression(paste(H[0],":",theta <= theta[0]," vs ",H[1],":",theta>theta[0])))
  abline(h=Power_function(teta,sigma2,teta0,n,alpha),lty=3)
}
Test(5,4,4,5,0.05)
#q()

#Example 12-7
Power_function=function(teta,teta0,n,alpha)
  return(1-((1-alpha)*((teta0/teta)^n)))
Test=function(teta,teta0,n,alpha){
  x=runif(n,-teta,teta)
  y=max(abs(x))
  c=teta0*((1-alpha)^(1/n))
  if(y>c)
    print("H0 was rejected")
  else
    print("H0 was accepted")
  print(Power_function(teta,teta0,n,alpha))
}
Test(5,3,8,0.05)
#

find_n=function(teta,teta0,alpha,power){
  n=(log(1-power)-log(1-alpha))/(log(teta0)-log(teta))
  return(ceiling(n))
}
find_n(5,3,0.05,0.9)
Test(5,3,5,0.05)
Test(5,3,4,0.05)

#q()

#Example 13-7
Test=function(){
  n=3;teta=1/2
  x=as.vector(rmultinom(1,n,prob=c(teta^2,2*teta*(1-teta),(1-teta)^2)))
  t=2*x[1]+x[2]
  if(t>5)
    print("H0 was rejected in both test")
  if(t==5){
    y=runif(1)
    if(y<0.9)
      print("H0 was rejected in test 1")
    else
      print("H0 was accepted in test 1")
    print("H0 was rejected in test 2")
  }
  if(t==4){
    print("H0 was accepted in test 1")
    y=runif(1)
    if(y<0.6)
      print("H0 was rejected in test 2")
    else
      print("H0 was accepted in test 2")
  }
  if(t<4)
    print("H0 was accepted in both test")
  return(t)
}
Test()
#q()

#Example 14-7
UMPbinomial <- function(H0,alpha,n){
  c <- min(which((1-pbinom(0:n,size=n,prob=H0))<alpha))-1
  gamma <- (alpha-1+pbinom(c,size=n,prob=H0))/dbinom(c,size=n,prob=H0)
  return(list=c(c,gamma))
}
Test=function(p0,p,alpha,n){
  a=UMPbinomial(p0,alpha,n)
  x=rbinom(1,n,p)
  if(x>a[[1]])
    print("H0 was rejected")
  if(x==a[[1]]){
    y=runif(1)
    if(y<a[[2]])
      print("H0 was rejected")
    else
      print("H0 was accepted")
  }
  if(x<a[[1]])
    print("H0 was accepted")
}
Test(1/2,1/8,0.05,3)
#q()

#Example 15-7
UMPgeometric=function(H0,alpha,n){
  c=0
  while(pnbinom(c,n,H0)<alpha)
    c=c+1
  gamma <- (alpha-pnbinom(c-1,n,H0))/dnbinom(c,n,H0)
  return(list=c(c,gamma))
}
Test=function(p0,p,alpha,n){
  t=rnbinom(1,n,p)
  a=UMPgeometric(p0,alpha,n)
  if(t<a[[1]])
    print("H0 was rejected")
  if(t==a[[1]]){
    y=runif(1)
    if(y<a[[2]])
      print("H0 was rejected")
    else
      print("H0 was accepted")
  }
  if(t>a[[1]])
    print("H0 was accepted")
}
Test(1/2,1/3,0.05,15)
#q()

#Example 16-7
Power_function=function(teta,beta,beta0,n,alpha)
  return(pchisq((beta0/beta)*qchisq(alpha,2*n*teta),2*n*teta))
Test=function(teta,beta,beta0,n,alpha){
  x=sum(rgamma(n,scale = beta,shape = teta))
  c=beta0*qchisq(alpha,2*n*teta)/2
  if(x<c)
    print("H0 was rejected")
  else
    print("H0 was accepted")
  print(Power_function(teta,beta,beta0,n,alpha))
  curve(Power_function(teta,x,beta0,n,alpha),0,
        2*beta0,ylab = expression(paste(pi[phi],"(",beta,")")),xlab = expression(beta))
  title(main = expression(paste(H[0],":",beta >= beta[0]," vs ",H[1],":",beta>beta[0])))
  abline(h=Power_function(teta,beta,beta0,n,alpha),lty=3)
}
Test(5,4,6,12,0.05)
#q()

#Example 17-7
Power_function=function(n,m,delta0,delta,alpha)
  return(1-pf((delta0*qf(1-alpha,n,m)/delta),n,m))
Test=function(n,m,mu1,sigma1,mu2,sigma2,delta0,alpha){
  x=rnorm(m,mu1,sigma1)  #sigma1:sd of x
  y=rnorm(n,mu2,sigma2)  #sigma2:sd of y
  w=(sum((y-mu2)^2)/n)/(sum((x-mu1)^2)/m)
  k=delta0*qf(1-alpha,n,m)
  delta=(sigma2^2)/(sigma1^2)
  if(w>k)
    print("H0 was rejected")
  else
    print("H0 was accepted")
  print(Power_function(n,m,delta0,delta,alpha))
  curve(Power_function(n,m,delta0,x,alpha),0,
        2*qf(0.99,n,m),ylab = expression(paste(pi[phi],"(",Delta,")")),xlab = expression(Delta))
  title(main = expression(paste(H[0],":",Delta >= Delta[0]," vs ",H[1],":",Delta>Delta[0])))
  abline(h=Power_function(n,m,delta0,delta,alpha),lty=3)
}
Test(15,20,3,4,6,5,1,0.05)
#q()

#Example 20-7
powertestplot <- function(mu0,sigma,n,alpha){
  mu0seq <- seq(mu0-3*sigma, mu0+3*sigma,(6*sigma/100))
  betamu <- pnorm(sqrt(n)*(mu0-mu0seq)/sigma-qnorm(1-alpha))
  betamu2 <- pnorm(sqrt(n)*(mu0seq-mu0)/sigma-qnorm(1-alpha))
  plot(mu0seq,betamu,"l",xlab=expression(mu[0]),
       ylab = expression(paste(pi[phi],"(",mu,")")),main =       expression(paste("H:",mu,"=", mu[0]," vs K:",mu !=     mu[0])),col=,xaxt="n",lty=3)
  points(mu0seq,betamu2,"l")
  legend(2,0.6,c(expression(phi[1]),expression(phi[2])),lty=c(3,1))
  abline(v=mu0,lty=2)
}
powertestplot(mu0=0,sigma=1,n=10,alpha=0.05)
#q()

#https://powerapp.shinyapps.io/powerapp/