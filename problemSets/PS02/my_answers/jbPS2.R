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

lapply(c('stargazer'), pkgTest)

# set wd for current folder
getwd()
setwd("/Users/ierja/Documents/GitHub/StatsII_Spring2024")

#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2024/blob/main/datasets/climateSupport.RData?raw=true"))


# Part 1 

# Creating factor variables 
climateSupport$choice <- as.numeric(climateSupport$choice == "Supported")
climateSupport$countries <- relevel(factor(climateSupport$countries, 
                                           ordered = FALSE),
                                    ref = "20 of 192")
climateSupport$sanctions <- relevel(factor(climateSupport$sanctions, 
                                           ordered = FALSE),
                                    ref = "None")

add_model <- glm(choice~ countries + sanctions,
                 data = climateSupport,
                 family = binomial(link = "logit"))
summary(add_model)
stargazer(add_model, title = "Additive model of ClimateSupport", type = "latex")




# Part 2

## a

climateSupport$sanctions <- relevel(factor(climateSupport$sanctions, 
                                           ordered = FALSE),
                                    ref = "5%")
add_model_pt2 <- glm(choice ~ countries + sanctions,
                   data = climateSupport,
                   family = binomial(link = "logit"))
summary(add_model_pt2)
# sanctions 15% -0.32510
stargazer(add_model_pt2, title = "Additive model of ClimateSupport pt2", type = "latex")
climateSupport$sanctions <- relevel(factor(climateSupport$sanctions, 
                                           ordered = FALSE),
                                    ref = "15%")
add_model_pt <- glm(choice ~ countries + sanctions,
                     data = climateSupport,
                     family = binomial(link = "logit"))
summary(add_model_pt)
stargazer(add_model_pt, title = "Additive model of ClimateSupport check", type = "latex")
#sanctions 5% 0.3251

## b

# Preiction equation from  week 4 slides 
# 1/1+exp[beta0+beta1*Xi]
beta_0<- -0.27266 #intercept of add_model
beta_1 <- 0.33636 # coefficient of countries 80 of 192

1/(1+(exp(-(beta_0+(beta_1)))))
# 0.5159196

test_probability <- predict(add_model, 
                 newdata = data.frame(countries = "80 of 192", sanctions = "None"), 
                     type = "response") # testing the probabilty with predict()
print(test_probability) # 0.5159191

## c


# Model with interactions (*) - countries and sanctions are interacting with each other and will get extra coefficients
interaction <- glm(choice ~ countries * sanctions, 
                              data = climateSupport, 
                              family = binomial(link = "logit"))
summary(interaction)
stargazer(interaction, title = "Interaction", type = "latex")
# x^2/Likelihood ratio test - we can use the additive model and the interaction model
test <- anova(add_model, interaction,
                 test = "LRT")
print(test)
stargazer(test, title = "Deviance Table", type = "latex")

