# load libraries
library(tidyverse)
library(ggeffects)
library(texreg)
library(stargazer)

getwd()
setwd(getwd())
##### Proposition 1 ######

# Load SOA data
df <- read.csv(file = "soa_data.tab", sep = "\t")

# Table 1: 
# Logistic regression models for PSAs (2016-2018).
m1 <- glm(as.factor(ifelse(republican_PSAs>0, T, F)) ~ catholic_trouble_deaths + scale(total_population) + urban +
            catholic_stronghold + income_deprivation + violent_crime_rate + asb_rate, data = df, family = "binomial")
summary(m1)

m2 <- glm(as.factor(ifelse(loyalist_PSAs>0, T, F)) ~ protestant_trouble_deaths + scale(total_population) + urban + 
            protestant_stronghold + income_deprivation + violent_crime_rate + asb_rate, data = df, family = "binomial")
summary(m2)

screenreg(list(m1, m2),
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

# Table 2: 
# Negative binomial models for PSAs (2016-2018).
m3 <- MASS::glm.nb(republican_PSAs ~ catholic_trouble_deaths + scale(total_population) + urban +
                     catholic_stronghold + income_deprivation + violent_crime_rate + asb_rate, data = df)
summary(m3)

m4 <- MASS::glm.nb(loyalist_PSAs ~ protestant_trouble_deaths + scale(total_population) + urban + 
                     protestant_stronghold + income_deprivation + violent_crime_rate + asb_rate, data = df)
summary(m4)
screenreg(list(m3, m4),
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



# Load survey data
df_survey <- read_delim(file = "survey_data.tab") %>%
  mutate_at(c("inf_better", "police_good", "informal_good", "discrim" , "vict_state_violence_trou", "communal_stronghold", "urban_soa"),as.factor)

# Table 3
# Respondents' view of the effectiveness of contacting "the police". 

## Column 1
round(100 * prop.table(table(df_survey$effectiveness_police, useNA = "always")))
## Column 2
round(100 * prop.table(table(df_survey[which(df_survey$religion=="Catholic"), ]$effectiveness_police, useNA = "always")))
## Column 3
round(100 * prop.table(table(df_survey[which(df_survey$religion=="Protestant"), ]$effectiveness_police, useNA = "always")))



# Table 4: 
# Respondents' view of the effectiveness of contacting "a member of the community who has influence".

## Column 1
round(100 * prop.table(table(df_survey$effectiveness_informal, useNA = "always")))
## Column 2
round(100 * prop.table(table(df_survey[which(df_survey$religion=="Catholic"), ]$effectiveness_informal, useNA = "always")))
## Column 3
round(100 * prop.table(table(df_survey[which(df_survey$religion=="Protestant"), ]$effectiveness_informal, useNA = "always")))



# Table 5:
# Respondents' rating of police as effective when faced with an anti-social behavior scenario.
m1_s <- glm(police_good ~ republican_PSAs +
            age + gender + general_trust + trust_in_neighbourhood + discrim + 
            vict_state_violence_trou + communal_stronghold + soa_deprivation + urban_soa,
          data = df_survey %>% filter(religion=="Catholic"), family = "binomial")
summary(m1_s)
m2_s <- glm(police_good ~ loyalist_PSAs +
            age + gender + general_trust + trust_in_neighbourhood + discrim + 
            vict_state_violence_trou + communal_stronghold + soa_deprivation + urban_soa,
          data = df_survey %>% filter(religion=="Protestant"), family = "binomial")
summary(m2_s)
texreg::screenreg(list(m1_s,m2_s), 
                  custom.coef.map = list("republican_PSAs" = "Republican PSAs", 
                                         "loyalist_PSAs" = "Loyalist PSAs", 
                                         "age" = "Age", 
                                         "genderMale" = "Male", 
                                         "general_trust" = "General trust",
                                         "trust_in_neighbourhood" = "Trust in neighbourhood",
                                         "discrimTRUE" = "Discriminated",
                                         "vict_state_violence_trouTRUE" = "Victim of state violence (1969-1998)",
                                         "communal_strongholdTRUE" = "Community stronghold (SOA)",
                                         "soa_deprivation" = "Income deprivation (SOA)",
                                         "urban_soaTRUE" = "Urban (SOA)"),
                  omit.coef = "intercept"
)


# Table 6:
## Respondents' rating of informal authorities as effective when faced with an anti-social behavior scenario.
m3_s <- glm(informal_good ~ republican_PSAs +
            age + gender + general_trust + trust_in_neighbourhood + discrim + 
            vict_state_violence_trou + communal_stronghold + soa_deprivation + urban_soa,
          data = df_survey %>% filter(religion=="Catholic"), family = "binomial")
summary(m3_s)
m4_s <- glm(informal_good ~ loyalist_PSAs +
            age + gender + general_trust + trust_in_neighbourhood + discrim + 
            vict_state_violence_trou + communal_stronghold + soa_deprivation + urban_soa,
          data = df_survey %>% filter(religion=="Protestant"), family = "binomial")
summary(m4_s)
texreg::screenreg(list(m3_s,m4_s), 
                  custom.coef.map = list("republican_PSAs" = "Republican PSAs", 
                                         "loyalist_PSAs" = "Loyalist PSAs", 
                                         "age" = "Age", 
                                         "genderMale" = "Male", 
                                         "general_trust" = "General trust",
                                         "trust_in_neighbourhood" = "Trust in neighbourhood",
                                         "discrimTRUE" = "Discriminated",
                                         "vict_state_violence_trouTRUE" = "Victim of state violence (1969-1998)",
                                         "communal_strongholdTRUE" = "Community stronghold (SOA)",
                                         "soa_deprivation" = "Income deprivation (SOA)",
                                         "urban_soaTRUE" = "Urban (SOA)"),
                  omit.coef = "intercept"
)


# Table 7: 
## Respondents' rating of informal authorities as more effective than the police when faced with an anti-social behavior scenario.
m5_s <- glm(inf_better ~ republican_PSAs +
            age + gender + general_trust + trust_in_neighbourhood + discrim + 
            vict_state_violence_trou + communal_stronghold + soa_deprivation + urban_soa,
          data = df_survey %>% filter(religion=="Catholic"), family = "binomial")
summary(m5_s)
m6_s <- glm(inf_better ~ loyalist_PSAs +
            age + gender + general_trust + trust_in_neighbourhood + discrim + 
            vict_state_violence_trou + communal_stronghold + soa_deprivation + urban_soa,
          data = df_survey %>% filter(religion=="Protestant"), family = "binomial")
summary(m6_s)
texreg::screenreg(list(m5_s,m6_s), 
                  custom.coef.map = list("republican_PSAs" = "Republican PSAs", 
                                         "loyalist_PSAs" = "Loyalist PSAs", 
                                         "age" = "Age", 
                                         "genderMale" = "Male", 
                                         "general_trust" = "General trust",
                                         "trust_in_neighbourhood" = "Trust in neighbourhood",
                                         "discrimTRUE" = "Discriminated",
                                         "vict_state_violence_trouTRUE" = "Victim of state violence (1969-1998)",
                                         "communal_strongholdTRUE" = "Community stronghold (SOA)",
                                         "soa_deprivation" = "Income deprivation (SOA)",
                                         "urban_soaTRUE" = "Urban (SOA)"),
                  omit.coef = "intercept"
)




# Figure 3: 
## Predicted probabilities for the statistically significant results from the models reported in tables 5, 6 and 7. All variables are set at their mean or mode.

p1 <- ggplot(ggpredict(m1_s, terms = c("republican_PSAs")), aes(x, predicted)) + 
  geom_line(lwd = 2) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .3) +
  ylim(0,1) +
  xlim(0,15) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Number of Republican PSAs in respondent's SOA",y="Predicted probability") +
  ggtitle("Catholic respondent\n rating the police as effective (table 5 above).") +
  theme(text = element_text(size = 12))
p1

p2 <- ggplot(ggpredict(m4_s, terms = c("loyalist_PSAs") ), aes(x, predicted)) + 
  geom_line(lwd = 2) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .3) +
  ylim(0,1) +
  xlim(0,15) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Number of Loyalist PSAs in respondent's SOA",y="Predicted probability") +
  ggtitle("Protestant respondent\n rating informal authority as effective (table 6 above).") +
  theme(text = element_text(size = 12))
p2

p3 <- ggplot(ggpredict(m6_s, terms = c("loyalist_PSAs" )), aes(x, predicted)) + 
  geom_line(lwd = 2) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .3) +
  ylim(0,1) +
  xlim(0,15) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x="Number of Loyalist PSAs in respondent's SOA",y="Predicted probability") +
  ggtitle("Protestant respondent\n rating informal > the police (table 7 above).") +
  theme(text = element_text(size = 12))
p3

gridExtra::grid.arrange(p1, p2, p3, ncol = 1, nrow = 3)
