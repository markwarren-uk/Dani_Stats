#Analysis based on Data exploration
#Mark Warren

#Libraries
library(tidyverse)
library(readxl)
library(plotly)

#Data Import
Motiv <- tibble(read_excel("Motivations_FINAL_Recode_for_MW.xlsx"))
#Remove last two rows - these are blank and the count of each column respectively
Motiv <- Motiv[-c(1262,1263), ]

#Get the column names
names(Motiv)

#Create a list of the ones we want to keep
MyNames <- c("Region", "Age", "Gender", "Income",  "Sustainability", "Plastic",
"Env_Knowledge", "Time_Nature", "Protect_Env", "Env_Friendly", 
"Env_Resp", "Donate_Amount", "Donate", "Food_Waste", "Visit_Green",
"Active_Travel", "Recycle", "Less_Plastic", "Save_Water", "Env_Interest",
"Env_Improve", "Env_Member", "Prev_Donate", "Diet_Now",
"Income_Change", "Ethnic", "Education", "Block")

#Select these
Motiv <- Motiv %>% 
    select(MyNames)

#Response variables
range(Motiv$Donate)
#binary response - donate yes or no

range(Motiv$Donate_Amount)
#amount from the £3

#Create a proportion variable
Motiv <- Motiv %>% 
    mutate(Donate_Prop = Donate_Amount / 3)
range(Motiv$Donate_Prop)
#from 0 to 1

#CHECK FOR MISSING VALUES
colSums(is.na(Motiv))
#QUITE A FEW IN MOST VARIABLES

#Select only female and male
Motiv <- Motiv %>% 
    filter(Gender %in% c(1, 2))

#Select 6 age groups
Motiv <- Motiv %>% 
    filter(Age %in% c(1:6))

#Income
#Remove 6 (Don't know) and NA
Motiv <- Motiv %>% 
    filter(Income %in% c(1:5))



#The IVs need to be factors
Motiv$Block <- factor(Motiv$Block)
Motiv$Age <- factor(Motiv$Age)
Motiv$Protect_Env <- factor(Motiv$Protect_Env)
Motiv$Gender <- factor(Motiv$Gender)
Motiv$Env_Knowledge <- factor(Motiv$Env_Knowledge)

#Binomial GLM for Donate and Beta Regression for Donate Proportion
library(broom) #For extracting model coeffs etc
library(betareg) #For fitting beta regression

#Apply adjustment ala Zuur (Yi * (N-1) + 0.5) / N)
N <- nrow(Motiv)
#Add adjusted column
Motiv <- Motiv %>% 
    mutate(Donate_Adj = (Donate_Prop * (N - 1) + 0.5) / N)
#Plot with original = all ok
plot(Motiv$Donate_Adj, Motiv$Donate_Prop)

#Select only columns we need for modelling and then remove NAs
Motiv2 <- Motiv %>% 
    select(Donate_Adj, Donate_Amount, Donate, Block, Age, Protect_Env, Gender, Env_Knowledge)

colSums(is.na(Motiv2))
Motiv2 <- na.omit(Motiv2)

#Fit Beta Regression - for proportions 0 to 1 
Beta_Mod <- betareg(Donate_Adj ~ Block + Age + Protect_Env + Gender + Env_Knowledge,
                    data = Motiv2)
glance(Beta_Mod) #check R2
summary(Beta_Mod) #which parameters are significant
AIC(Beta_Mod) #Akaike Information Criteria = a measure of how well model fits data and model complexity
#More complex models are likely to better fit the data - leading to overfitting so AIC penalises complex models

#Block removed from model because it has many levels and none are close to significance
Beta_Mod1 <- betareg(Donate_Adj ~ Gender + Age + Protect_Env + Env_Knowledge,
                    data = Motiv2)
glance(Beta_Mod1)
summary(Beta_Mod1)
AIC(Beta_Mod1)

#Gender removed because it is least signif
Beta_Mod2 <- betareg(Donate_Adj ~ Age + Protect_Env + Env_Knowledge,
                     data = Motiv2)
glance(Beta_Mod2)
summary(Beta_Mod2)
AIC(Beta_Mod2)

#Age removed as this is least signif
Beta_Mod3 <- betareg(Donate_Adj ~ Protect_Env + Env_Knowledge,
                     data = Motiv2)
glance(Beta_Mod3)
summary(Beta_Mod3)
AIC(Beta_Mod, Beta_Mod1, Beta_Mod2, Beta_Mod3)
#AIC does not improve between Mod2 and Mod3 but latter is more parsimonious model


#############################
#Fit linear model to donate amount (not strictly a good thing to do but let's see where it gets us)
Lin_Mod <- lm(Donate_Amount ~ Block * Age + Age * Protect_Env + Gender * Age + Block * Env_Knowledge + 
                  Block * Protect_Env, data = Motiv2)
glance(Lin_Mod)
summary(Lin_Mod)

Lin_Mod %>% 
    glance() %>% 
    pull(adj.r.squared)

#Model selection stepwise using AIC to select
#drop1 fits full model then refits with each term removed and AIC estimated
#it then lists each combination and its AIC.
drop1(Lin_Mod)
# Model:
# Donate_Amount ~ Block * Age + Age * Protect_Env + Gender * Age + 
#     Block * Env_Knowledge + Block * Protect_Env
#                     Df Sum of Sq    RSS    AIC
# <none>                           1486.4 475.02
# Block:Age           20    39.435 1525.9 464.35
# Age:Protect_Env     16    12.847 1499.3 452.66  <-- remove 
# Age:Gender           5     5.393 1491.8 469.08
# Block:Env_Knowledge  8    12.306 1498.8 468.26
# Block:Protect_Env   12     8.288 1494.7 457.25

Lin_Mod2 <- lm(Donate_Amount ~ Block * Age + Gender * Age + Block * Env_Knowledge + 
                   Block * Protect_Env, data = Motiv2)

drop1(Lin_Mod2)
# Model:
# Donate_Amount ~ Block * Age + Gender * Age + Block * Env_Knowledge + 
#     Block * Protect_Env
# Df Sum of Sq    RSS    AIC
# <none>                           1499.3 452.66
# Block:Age           20    40.745 1540.0 442.69
# Age:Gender           5     4.792 1504.1 446.24
# Block:Env_Knowledge  8    12.899 1512.2 446.26
# Block:Protect_Env   13    13.196 1512.5 436.48 <-- remove

Lin_Mod3 <- lm(Donate_Amount ~ Block * Age + Gender * Age + Block * Env_Knowledge + 
                   Protect_Env, data = Motiv2)

drop1(Lin_Mod3)
#Model:
# Donate_Amount ~ Block * Age + Gender * Age + Block * Env_Knowledge + 
#     Protect_Env
#                     Df Sum of Sq    RSS    AIC
# <none>                           1512.5 436.48
# Protect_Env          4    37.255 1549.7 455.73
# Block:Age           20    40.701 1553.2 426.22 <--- remove
# Age:Gender           5     3.740 1516.2 429.24
# Block:Env_Knowledge  8    12.655 1525.1 429.81

Lin_Mod4 <- lm(Donate_Amount ~ Block + Gender * Age + Block * Env_Knowledge + 
                   Protect_Env, data = Motiv2)

drop1(Lin_Mod4)
#Model:
# Donate_Amount ~ Block + Gender * Age + Block * Env_Knowledge + 
#     Protect_Env
#                     Df Sum of Sq    RSS    AIC
# <none>                           1553.2 426.22
# Protect_Env          4    35.443 1588.6 443.49
# Gender:Age           5     2.900 1556.1 418.31
# Block:Env_Knowledge  8     9.960 1563.2 417.38 <--- remove

Lin_Mod5 <- lm(Donate_Amount ~ Block + Gender * Age + Env_Knowledge + 
                               Protect_Env, data = Motiv2)

drop1(Lin_Mod5)
# Model:
# Donate_Amount ~ Block + Gender * Age + Env_Knowledge + Protect_Env
#               Df Sum of Sq    RSS    AIC
# <none>                     1563.2 417.38
# Block          4     5.130 1568.3 413.05
# Env_Knowledge  2    29.282 1592.4 434.16
# Protect_Env    4    35.499 1598.7 434.53
# Gender:Age     5     3.717 1566.9 410.04 <-- remove


Lin_Mod6 <- lm(Donate_Amount ~ Block + Gender + Age + Env_Knowledge + 
                   Protect_Env, data = Motiv2)

drop1(Lin_Mod6)
#Model:
# Donate_Amount ~ Block + Gender + Age + Env_Knowledge + Protect_Env
#               Df Sum of Sq    RSS    AIC
# <none>                     1566.9 410.04
# Block          4     4.912 1571.8 405.54  <-- remove
# Gender         1     0.026 1566.9 408.06
# Age            5    22.242 1589.1 415.82
# Env_Knowledge  2    28.658 1595.5 426.34
# Protect_Env    4    35.989 1602.8 427.47

Lin_Mod7 <- lm(Donate_Amount ~ Gender + Age + Env_Knowledge + 
                   Protect_Env, data = Motiv2)

drop1(Lin_Mod7)
#Model:
# Donate_Amount ~ Gender + Age + Env_Knowledge + Protect_Env
#               Df Sum of Sq    RSS    AIC
# <none>                     1571.8 405.54
# Gender         1     0.028 1571.8 403.56  <-- remove
# Age            5    22.777 1594.5 411.66
# Env_Knowledge  2    27.380 1599.2 420.89
# Protect_Env    4    35.247 1607.0 422.38

Lin_Mod8 <- lm(Donate_Amount ~ Age + Env_Knowledge + 
                   Protect_Env, data = Motiv2)

drop1(Lin_Mod8)
#Model:
# Donate_Amount ~ Age + Env_Knowledge + Protect_Env
#               Df Sum of Sq    RSS    AIC
# <none>                     1571.8 403.56  <-- keep all IVs in this model
# Age            5    23.063 1594.9 409.88
# Env_Knowledge  2    27.411 1599.2 418.93
# Protect_Env    4    35.859 1607.7 420.83

glance(Lin_Mod8)


summary(Lin_Mod)
summary(Lin_Mod2)
summary(Lin_Mod3)
summary(Lin_Mod4)
summary(Lin_Mod5)
summary(Lin_Mod6)
summary(Lin_Mod7)
summary(Lin_Mod8)
#Donate_Amount ~ Age + Env_Knowledge + Protect_Env

summary(Beta_Mod3)
#Donate_Adj ~ Protect_Env + Env_Knowledge

#Model validation
Motiv2$E8 <- resid(Lin_Mod8) #model residuals (unexplained error)
Motiv2$F8 <- fitted(Lin_Mod8) #model fitted values

#Plot fitted versus residuals - there should be no trend or pattern
with(Motiv2, plot(x = F8, y = E8, abline(h=0)))
#there is a trend - higher fitted values have lower error = not good

Motiv2$E3 <- resid(Beta_Mod3)
Motiv2$F3 <- fitted(Beta_Mod3)

with(Motiv2, plot(x = F3, y = E3, abline(h=0)))
#A similar but less extreme pattern


#Model validation for beta reg3
with(Motiv2, boxplot(E3 ~ Age)) #not included but worth looking at
with(Motiv2, boxplot(E3 ~ Env_Knowledge))
with(Motiv2, boxplot(E3 ~ Protect_Env))


plot(Beta_Mod3, which = 1:4, type = "pearson")

#GRID OF COVARIATES FOR PREDICTIONS
MyData <- expand_grid(
    Protect_Env = unique(Motiv2$Protect_Env),
    Env_Knowledge = unique(Motiv2$Env_Knowledge)
)

#Model matrix for predicting
X <- model.matrix(~ Protect_Env + Env_Knowledge, data = MyData)


#CREATE A FUNCTION TO TRANSFORM BACK TO RESPONSE SCALE = Inverse Logit
Inv_Logit <- function(x) {
    exp(x) / (1 + exp(x))
}

#Calculate predictions
MyData$Pred <- X %*% coef(Beta_Mod3, model = c("mean"), phi = NULL) #need to not pick up phi
MyData$Resp <- Inv_Logit(MyData$Pred)

#Calculate standard errors
MyData$SE <- sqrt(diag(X %*% vcov(Beta_Mod3, model = c("mean"), phi = NULL) %*% t(X)))

#And using the predicted values and standard errors, calculate 95% confidence intervals
MyData <- MyData %>% 
    mutate(SeUp = Inv_Logit(Pred + 1.96 * SE),
           SeLo = Inv_Logit(Pred - 1.96 * SE))

with(Motiv2, table(Protect_Env, Env_Knowledge))
#Very few observations at level 5, best to remove these

Beta_Reg1 <- MyData %>% 
    filter(Protect_Env != 5) %>% 
    ggplot(aes(x = Protect_Env, y = Resp)) +
    geom_point(aes(x = Protect_Env, y = Resp)) +
    labs (title = "Proportion of donation amount by level of environmental knowledge", 
          y = "Mean proportion",
          x = "Importance of environmental protection") +
    geom_errorbar(aes(ymin = SeLo, ymax = SeUp), width = .2) +
 #   geom_hline(yintercept = 0.5, color = "blue", linetype = "dashed") +
    facet_wrap(~Env_Knowledge, ncol = 2) +
    theme_bw()
#This shows that importance of env protection tends to increase donation amount
#It also shows that once importance is >=3 AND env knowledge is >2 that donation
#amount is likely to be greater

#Create interactve plot
ggplotly(Beta_Reg1)

Beta_Reg2 <- MyData %>% 
    filter(Protect_Env != 5) %>% 
    ggplot(aes(x = Env_Knowledge, y = Resp)) +
    geom_point(aes(x = Env_Knowledge, y = Resp)) +
    labs (title = "Donation proportion by importance of environmental protection", 
          y = "Mean proportion",
          x = "Level of environmental knowledge") +
    geom_errorbar(aes(ymin = SeLo, ymax = SeUp), width = .2) +
    facet_wrap(~Protect_Env, ncol = 2) +
    theme_bw()
#This shows that on average once people gain some environmental knowledge then they donate more of available funds
#to an environmental charity.


#Create interactve plot
ggplotly(Beta_Reg2)



#######################
#Donate model
#Fit binomial GLM and drop1 as before
Bin_Mod <- glm(Donate ~ Block * Age + Age * Protect_Env + Gender * Age + Block * Env_Knowledge + 
                   Block * Protect_Env, data = Motiv2, family = binomial)

drop1(Bin_Mod)
#Model:
# Donate ~ Block * Age + Age * Protect_Env + Gender * Age + Block * 
#     Env_Knowledge + Block * Protect_Env
#                     Df Deviance    AIC
# <none>                   1363.5 1521.5
# Block:Age           20   1390.6 1508.6
# Age:Protect_Env     16   1371.7 1497.7 <-- remove
# Age:Gender           5   1368.2 1516.2
# Block:Env_Knowledge  8   1377.7 1519.7
# Block:Protect_Env   12   1374.4 1508.4


Bin_Mod1 <- glm(Donate ~ Block * Age + Gender * Age + Block * Env_Knowledge + 
                   Block * Protect_Env, data = Motiv2, family = binomial)

drop1(Bin_Mod1)
#Model:
# Donate ~ Block * Age + Gender * Age + Block * Env_Knowledge + 
#     Block * Protect_Env
#                      Df Deviance    AIC
# <none>                   1371.7 1497.7
# Block:Age           20   1399.7 1485.7  <-- remove
# Age:Gender           5   1376.2 1492.2
# Block:Env_Knowledge  8   1386.5 1496.5
# Block:Protect_Env   13   1388.3 1488.3

Bin_Mod2 <- glm(Donate ~ Gender * Age + Block * Env_Knowledge + 
                    Block * Protect_Env, data = Motiv2, family = binomial)

drop1(Bin_Mod2)
#Model:
# Donate ~ Gender * Age + Block * Env_Knowledge + Block * Protect_Env
#                     Df Deviance    AIC
# <none>                   1399.7 1485.7
# Gender:Age           5   1404.5 1480.5
# Block:Env_Knowledge  8   1414.6 1484.6
# Block:Protect_Env   13   1414.9 1474.9 <-- remove

Bin_Mod3 <- glm(Donate ~ Gender * Age + Block * Env_Knowledge + 
                    Protect_Env, data = Motiv2, family = binomial)

drop1(Bin_Mod3)
#Model:
# Donate ~ Gender * Age + Block * Env_Knowledge + Protect_Env
#                     Df Deviance    AIC
# <none>                   1414.9 1474.9
# Protect_Env          4   1434.3 1486.3
# Gender:Age           5   1420.0 1470.0 <--- remove
# Block:Env_Knowledge  8   1428.6 1472.6

Bin_Mod4 <- glm(Donate ~ Gender + Age + Block * Env_Knowledge + 
                    Protect_Env, data = Motiv2, family = binomial)

drop1(Bin_Mod4)
#Model:
# Donate ~ Gender + Age + Block * Env_Knowledge + Protect_Env
#                     Df Deviance    AIC
# <none>                   1420.0 1470.0
# Gender               1   1420.9 1468.9
# Age                  5   1427.2 1467.2
# Protect_Env          4   1439.2 1481.2
# Block:Env_Knowledge  8   1433.1 1467.1 <-- remove

Bin_Mod5 <- glm(Donate ~ Gender + Age + Block + Env_Knowledge + 
                    Protect_Env, data = Motiv2, family = binomial)

drop1(Bin_Mod5)
#Model:
# Donate ~ Gender + Age + Block + Env_Knowledge + Protect_Env
#               Df Deviance    AIC
# <none>             1433.1 1467.1
# Gender         1   1433.7 1465.7
# Age            5   1438.7 1462.7
# Block          4   1435.6 1461.6 <-- remove
# Env_Knowledge  2   1457.7 1487.7
# Protect_Env    4   1452.2 1478.2


Bin_Mod6 <- glm(Donate ~ Gender + Age + Env_Knowledge + 
                    Protect_Env, data = Motiv2, family = binomial)

drop1(Bin_Mod6)
#Model:
# Donate ~ Gender + Age + Env_Knowledge + Protect_Env
#               Df Deviance    AIC
# <none>             1435.6 1461.6
# Gender         1   1436.1 1460.1
# Age            5   1441.3 1457.3 <-- remove
# Env_Knowledge  2   1459.3 1481.3
# Protect_Env    4   1454.4 1472.4


Bin_Mod7 <- glm(Donate ~ Gender + Env_Knowledge + 
                    Protect_Env, data = Motiv2, family = binomial)

drop1(Bin_Mod7)
#Model:
# Donate ~ Gender + Env_Knowledge + Protect_Env
# Df Deviance    AIC
# <none>             1441.3 1457.3
# Gender         1   1441.6 1455.6 <-- remove
# Env_Knowledge  2   1468.2 1480.2
# Protect_Env    4   1461.3 1469.3

Bin_Mod8 <- glm(Donate ~ Env_Knowledge + 
                    Protect_Env, data = Motiv2, family = binomial)

drop1(Bin_Mod8)
# Model:
#     Donate ~ Env_Knowledge + Protect_Env
#               Df Deviance    AIC
# <none>             1441.6 1455.6
# Env_Knowledge  2   1468.4 1478.4
# Protect_Env    4   1462.2 1468.2

summary(Bin_Mod8)
# Coefficients:
#                    Estimate Std. Error z value Pr(>|z|)    
#     (Intercept)     -1.0441     0.3725  -2.803 0.005066 ** 
#     Env_Knowledge2   0.6515     0.1424   4.574 4.78e-06 ***
#     Env_Knowledge3   0.6958     0.1693   4.110 3.96e-05 ***
#     Protect_Env2     0.7504     0.4060   1.848 0.064551 .  
#     Protect_Env3     1.0280     0.3841   2.676 0.007441 ** 
#     Protect_Env4     1.3174     0.3804   3.463 0.000535 ***
#     Protect_Env5     0.1192     1.2921   0.092 0.926472    
# ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Pseudo Rsq
glance(Bin_Mod8) %>%
    summarize(pR2 = 1 - deviance/null.deviance)
#0.035


library(pROC)

ROC <- roc(Motiv2$Donate, as.numeric(Motiv2$Env_Knowledge))

plot(ROC, col = "blue")

auc(ROC)
#Area under the curve: 0.5875

ROC <- roc(Motiv2$Donate, as.numeric(Motiv2$Protect_Env))

plot(ROC, col = "red")

auc(ROC)
#Area under the curve: 0.5687


#GRID OF COVARIATES FOR PREDICTIONS
MyDataBin <- expand_grid(
    Env_Knowledge = unique(Motiv2$Env_Knowledge),
    Protect_Env = unique(Motiv2$Protect_Env)
)

#Model matrix for predicting
X <- model.matrix(~ Env_Knowledge + Protect_Env, data = MyDataBin)

#NEED TO WORK OUT THE BACK TRANSFORM ON LOGIT 
#Calculate predicted values
#CREATE A FUNCTION TO TRANSFORM BACK TO RESPONSE SCALE
Inv_Logit <- function(x) {
    exp(x) / (1 + exp(x))
}

#Calculate predictions
MyDataBin$Pred <- X %*% coef(Bin_Mod8)
MyDataBin$Resp <- Inv_Logit(MyDataBin$Pred)

#Calculate standard errors
MyDataBin$SE <- sqrt(diag(X %*% vcov(Bin_Mod8) %*% t(X)))

#And using the predicted values and standard errors, calculate 95% confidence intervals
MyDataBin <- MyDataBin %>% 
    mutate(SeUp = Inv_Logit(Pred + 1.96 * SE),
           SeLo = Inv_Logit(Pred - 1.96 * SE))

with(Motiv2, table(Protect_Env, Env_Knowledge))
#Very few observations at level 5, best to remove these

MyDataBin %>% 
    filter(Protect_Env != "5") %>% 
    ggplot(aes(x = Protect_Env, y = Resp)) +
    geom_point(aes(x = Protect_Env, y = Resp)) +
    labs (title = "Probability of donating\nby level of environmental knowledge", 
          y = "Mean probability",
          x = "Importance of environmental protection") +
    geom_errorbar(aes(ymin = SeLo, ymax = SeUp), width = .2) +
    #   geom_hline(yintercept = 0.5, color = "blue", linetype = "dashed") +
    facet_wrap(~Env_Knowledge, ncol = 2) +
    theme_bw()
#

MyDataBin %>% 
    filter(Protect_Env != "5") %>% 
    ggplot(aes(x = Env_Knowledge, y = Resp)) +
    geom_point(aes(x = Env_Knowledge, y = Resp)) +
    labs (title = "Probability of donating\nby importance of environmental protection", 
          y = "Mean probability",
          x = "Level of environmental knowledge") +
    geom_errorbar(aes(ymin = SeLo, ymax = SeUp), width = .2) +
    facet_wrap(~Protect_Env, ncol = 2) +
    theme_bw()


Motiv2 %>% 
    group_by(Env_Knowledge, Donate) %>% 
    summarise(Min_Donate = min(Donate_Adj),
              Max_Donate = max(Donate_Adj),
              Ave_Donate = mean(Donate_Adj))
#Of those that greatest minimum proportion donated was from env knowledge 3
#Average proportion donated was level 3 but not by much

with(Motiv2, table(Donate, Env_Knowledge))
#      Env_Knowledge
# Donate   1   2   3
#       0 204 149  80
#       1 208 303 176
#Of those that did not donate most had low env knowledge and least had level 3
#Of those that did donate most had env knowledge 2 and least had level 3










