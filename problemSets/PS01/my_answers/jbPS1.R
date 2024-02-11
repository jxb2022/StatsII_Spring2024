#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
getwd()
setwd("/Users/ierja/Documents/GitHub/StatsII_Spring2024/")


#####################
# Problem 1
#####################

set.seed(123)
# create empirical distribution of observed data
ECDF <- ecdf(data)
empiricalCDF <- ECDF(data)
# generate test statistic
D <- max(abs(empiricalCDF - pnorm(data)))

#Creating ks.test by hand.
# https://stats.stackexchange.com/questions/471732/intuitive-explanation-of-kolmogorov-smirnov-test
my_ksTest <- function(data) {
  l <- length(data) # l will equal the distribution given
  s_data <- sort(data) # l will need to be sorted 
  empirical_CDF <- ecdf(s_data) 
  theoretical_CDF <- pnorm(s_data) 
  D <- max(abs(empirical_CDF(s_data) - theoretical_CDF))
  p_value <- 1 - 2 * sum((-1)^(1:(l-1)) * exp(-(2 * (1:(l-1)) - 1)^2 * pi^2 / (8 * D^2))) #https://stats.stackexchange.com/questions/389034/kolmogorov-smirnov-test-calculating-the-p-value-manually
  return(list(D = D, p_value = p_value))
}

#testing function

cauchy_variables <- rcauchy(1000, location = 0, scale = 1)

test_ks <- my_ksTest(cauchy_variables)
print(test_ks)

#checking function

check <- ks.test(cauchy_variables, "pnorm")
print(check)




#####################
# Problem 2
#####################

set.seed (123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)


ols_reg <- function(beta, x, y) {
  y_hat <- beta[1] + beta[2] * x
  sum((y - y_hat)^2) # least squares regression
}

# need to have x and y values from main dataset
x <- data$x
y <- data$y


# Use optim function with BFGS mhttps://stackoverflow.com/questions/71268528/using-pgtol-in-rs-optim-function
result <- optim(par = c(0,0), fn = ols_reg, x = x, y = y, method = "BFGS")

coef_bfgs <- result$par
print(coef_bfgs)

# linear model 
lm_model <- lm(y ~ x, data = data)
lm_coef_model <- coef(lm_model)

print(lm_coef_model)




