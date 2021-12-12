#--------------------------------------------------------#
# Mathmatical Statistics With R : Chapter 3              #
# Authors: Hamid Jahani, Vahid Rezaei Tabar              #
# Sufficiency,minimal sufficiency and Completeness       #
#--------------------------------------------------------#

# q() is commented out so you don't accidently exit

#Example 35-3
g=function(mu,sigma,n){ #sigma is Variance
  x1=integrate(function(x){x*n*dnorm(x)*(pnorm(x)^(n-1))},-  Inf,Inf)$value
  x2=integrate(function(x){x*n*dnorm(x)*((1-pnorm(x))^(n-      1))},-Inf,Inf)$value
  cat("E[X(n)]= ",mu+sqrt(sigma)*x1,"\n","E[X(1)]= ",mu+sqrt(sigma)*x2)
}
g(3,4,10)
#E[X(n)]=  6.077505 
#E[X(1)]=  -0.07750546
#

f=function(mu,sigma,m,n){ #sigma is Variance 
  y1=y2=vector(mode = "numeric",length = m)
  for(i in 1:m){
    x=rnorm(n,mu,sqrt(sigma))
    y1[i]=max(x)
    y2[i]=min(x)
  }
  cat("E[X(n)]= ",mean(y1),"\n","E[X(1)]= ",mean(y2))
}
f(3,4,10000,10)
#E[X(n)]=  6.08393 
#E[X(1)]=  -0.08505979