#----------------------------------------------#
# Mathmatical Statistics With R : Chapter 9    #
# Authors: Hamid Jahani, Vahid Rezaei Tabar    #
# Advanced Topics                              #
#----------------------------------------------#

#Example 9-5
# bootstrapping a single statistic (R2)
rsq <- function(formula, data, indices) {
  d <- data[indices,]
  fit <- lm(formula, data=d)
  return(summary(fit)$r.square)
} 

library(boot)
set.seed(1234)
results <- boot(data=mtcars, statistic=rsq, 
                R=1000, formula=mpg~wt+disp)
print(results)
plot(results)
boot.ci(results, type=c("perc", "bca"))
#q()


#Example 9-6
# bootstrapping several statistics (regression coefficients)
bs <- function(formula, data, indices) {                
  d <- data[indices,]                                    
  fit <- lm(formula, data=d)                                                                                                
  return(coef(fit))                                    
}
library(boot)
set.seed(1234)
results <- boot(data=mtcars, statistic=bs,             
                R=1000, formula=mpg~wt+disp) 

print(results)
plot(results, index=2)
boot.ci(results, type="bca", index=2)
boot.ci(results, type="bca", index=3)
#q()