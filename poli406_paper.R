### SET UP
rm(list=ls())
library(readxl)
library(dplyr)
library(vcd)
library(ggplot2)
library(broom)
library(knitr)
library(stringr)

#Load data
setwd("/Users/michelleliu/Documents/UNC/Spring 2024/POLI 406/Research Papers")
data <- read_excel("/Users/michelleliu/Documents/UNC/Spring 2024/POLI 406/Research Papers/poli406_data.xlsx")
mining <- read.csv("/Users/michelleliu/Documents/UNC/Spring 2024/POLI 406/Research Papers/SAGDP2N__ALL_AREAS_1997_2022.csv")

#Clean GDP data to only include mining
mining <- filter(mining, LineCode == 6 | LineCode == 1)
mining <- mining %>% rename(State = GeoName)

mining$X2021 <- as.numeric(mining$X2021)

mining <- mining %>%
  group_by(State) %>%
  mutate(mining_percent = X2021 / lag(X2021))

mining <- filter(mining, LineCode == 6)
mining <- select(mining, c(State, mining_percent))


#Merge
df <- left_join(data, mining, by = c("State"))
df$mining_percent <- df$mining_percent*100
df$D_trifectas <- as.numeric(df$D_trifectas)

###AUXILIARY ANALYSIS
#Chi square of D_trifecta and GHG_2022
aux_table1 <- table(df$D_trifectas, df$GHG_2022)
aux_table1
chisq.test(aux_table1)
#According to the chi-square test, the p-value is 0.000009314. 
# This means that there is a 0.0009314% chance of observing data more extreme than ours given that the null hypothesis is true. 
#We reject the null

#Cramer's V of D_trifecta and GHG_2022
assocstats(aux_table1) 
#Cramer's V = 0.674 (strength of relationship)

#t-test of D_trifecta and legislative professionalization
t.test(df$squire_2021[df$D_trifectas == 0], df$squire_2021[df$D_trifectas == 1])

#Auxiliary regression between mining2021 and GHG_2022
aux_reg <- glm(GHG_2022 ~ mining_percent, data = df, family = binomial)
summary(aux_reg)

#Summary statistics table
library(psych)
describe(df)

df$D_trifectas <- as.numeric(df$D_trifectas)
summary_table <- summary(df)
print(summary_table)

#MAIN REGRESSIONS
#BIVARIATE REGRESSION
model1 <- glm(GHG_2022 ~ squire_2021, data = df, family = binomial)
summary(model1)
nobs(model1)

#Bivariate Predicted probabilities plot
?predict
df$predicted_probs1 <- predict(model1, type = "response")

summary(df$predicted_probs1)

title1 <- "Predicted Probability of GHG Mandate Based on Legislative Professionalization"
ggplot(df, aes(x = squire_2021, y = predicted_probs1)) +
  geom_point() +
  xlab("Legislative Professionalization Index") +
  ylab("Predicted Probability of GHG Mandate") +
  ggtitle(str_wrap(title1, width = 60))

#MULTIVARIATE + D_TRIFECTAS
model2 <- glm(GHG_2022 ~ squire_2021 + D_trifectas, data = df, family = binomial)
summary(model2)
nobs(model2)

#Multivariate plots
df$predicted_probs2 <- predict(model2, type = "response")

default_value <- "Some default value"

# Apply ifelse statements to create the new column
df$dem <- ifelse(df$D_trifectas == 1, "Dem. Trifecta",
                 ifelse(df$D_trifectas == 0, "Not Dem. Trifecta",
                        default_value))

title2 <- "Predicted Probability of GHG Mandate Based on Legislative Professionalization and Democratic Trifecta"

ggplot(df, aes(x = squire_2021, y = predicted_probs2, color = dem)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(x = "Legislative Professionalization Index", y = "Predicted Probability of Having GHG Mandates", color = "Democratic Trifecta") +
  ggtitle(str_wrap(title2, width = 60))

ggplot(df, aes(x = squire_2021, y = predicted_probs2)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  facet_wrap(~ dem) +
  labs(x = "Legislative Professionalization Index", y = "Predicted Probability") +
  ggtitle(str_wrap(title2, width = 60))


#nobs(modelname)

#Multivariate + D_trifectas + mining2021
model3 <- glm(GHG_2022 ~ squire_2021 + D_trifectas + mining_percent, data = df, family = binomial)
summary(model3)
nobs(model3)
df$predicted_probs3 <- predict(model3, type = "response")

ggplot(df, aes(x = squire_2021, y = predicted_probs3, color = mining_percent)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  facet_wrap(~ dem) +
  labs(x = "Legislative Professionalization Index", y = "Predicted Probability of GHG Mandate") +
  ggtitle(str_wrap(title2, width = 60)) +
  scale_color_gradient(low = "blue", high = "red", na.value = "grey50") + labs(color='% of GDP from Mining') 


#INTERACTION
model4 <- glm(GHG_2022 ~ squire_2021 + D_trifectas + squire_2021:D_trifectas, data = df, family = binomial)
summary(model4)
nobs(model4)
predicted_prob4 <- predict(model4, type = "response")

mining <- df$mining

df$mining_cat <- NA
df$mining <- 0.133762661, "<Q1",
                 ifelse(df$D_trifectas == 0, "Not Dem. Trifecta",
                        default_value))

title4 <- "Predicted Probability of GHG Mandate Based on Legislative Professionalization, Democratic Trifecta, and Interaction"
ggplot(df, aes(x = squire_2021, y = predicted_prob4, color = dem)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(x = "Legislative Professionalization Index", y = "Predicted Probability of Having GHG Mandates", color = "Democratic Trifecta") +
  ggtitle(str_wrap(title4, width = 60)) + ggtitle(str_wrap(title2, width = 60))
