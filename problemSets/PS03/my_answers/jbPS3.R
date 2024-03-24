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

lapply(c("nnet", "MASS"),  pkgTest)
lapply(c('stargazer'), pkgTest)
# set wd for current folder
getwd()
setwd("/Users/ierja/Documents/GitHub/StatsII_Spring2024")


#####################
# Problem 1
#####################

# load data
data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/gdpChange.csv", stringsAsFactors = F)

## 1



#creating factor variables
data[data$GDPWdiff == 0, "GDPfactor"] <- "no change"
data[data$GDPWdiff > 0, "GDPfactor"] <- "positive"
data[data$GDPWdiff < 0, "GDPfactor"] <- "negative"


# re-leveling data
data$GDPfactor<- relevel(as.factor(data$GDPfactor), ref = "no change")

# running the unordered regression

unor_mul_regr <- multinom(GDPfactor ~ REG + OIL, data = data)

summary(unor_mul_regr)
stargazer(unor_mul_regr, title = "Unordered Multinomial Regression", type = "latex")


## 2


# reordering

data$GDPfactor <- relevel(data$GDPfactor, ref = "negative")

# run the ordered regression

order_regression <- polr(GDPfactor ~ REG + OIL, data = data)
summary(order_regression)
# Intercepts negative --> no change aprox -0.7312....Intercepts no change --> positive aprox -0.7105
stargazer(order_regression, title = "Ordered Multinomial Regression", type = "latex")
#####################
# Problem 2
#####################

# load data
mexico_elections <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/MexicoMuniData.csv")



# 2a

poisson <- glm(PAN.visits.06 ~ 
              competitive.district + marginality.06 + PAN.governor.06,
               data = mexico_elections, family = poisson)

summary(poisson)




stargazer(poisson, title = "Mexican Poisson Regression", type = "latex")



# 2c

exp(coef(poisson)[1] + coef(poisson)[2] * 1 + coef(poisson)[3] * 0 + coef(poisson)[4] * 1)#from slides
# 0.01494818
# OR
prediction <- data.frame(competitive.district = 1,
                         marginality.06 = 0,
                         PAN.governor.06 = 1)
predict(poisson, newdata = prediction, type = "response")
# 0.0194818



(exp(-0.31158))
(exp(-2.08014))
