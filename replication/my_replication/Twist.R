# load libraries
library(tidyverse)
library(ggeffects)
library(texreg)
library(stargazer)
library(dplyr)


df <- read.csv(file = "soa_data.tab", sep = "\t")


m1_poisson <- glm(republican_PSAs > 0 ~ catholic_trouble_deaths + 
                    scale(total_population) + urban + catholic_stronghold + 
                    income_deprivation + violent_crime_rate + asb_rate, 
                  data = df, family = poisson())

summary(m1_poisson)


m2_poisson <- glm(loyalist_PSAs > 0 ~ protestant_trouble_deaths + scale(total_population) + urban + 
                    protestant_stronghold + income_deprivation + violent_crime_rate + asb_rate, 
                  data = df, family = poisson())

summary(m2_poisson)


screenreg(list(m1_poisson, m2_poisson),
          omit.coef='Intercept',
          custom.coef.map = list(
            "catholic_trouble_deaths" = "Catholic in-group killings (1969-1998)",
            "protestant_trouble_deaths" = "Protestant in-group killings (1969-1998)",
            "scale(total_population)" = "Total population",
            "urbanTRUE" = "Urban",
            "catholic_strongholdTRUE" = "Catholic stronghold",
            "protestant_strongholdTRUE" = "Protestant stronghold",
            "income_deprivation" = "Income deprivation rate",
            "violent_crime_rate" = "Violent crime rate",
            "asb_rate" = "Anti-social behavior rate"),
          caption = "Table 1: Logistical regression models for PSAs (2016-2018).",
          caption.above = T,
          custom.note = "Notes: *** p < 0.001, ** p < 0.01, * p < 0.05. Analysis conducted in R.",
          custom.model.names = c("Republican PSAs", "Loyalist PSAs"),
          column.spacing = 1)


# Adding column PSAs that combines republican PSAs and loyalist PSAs - to negate the religion
df_religion <- df %>%
  mutate(PSAs = republican_PSAs + loyalist_PSAs)
