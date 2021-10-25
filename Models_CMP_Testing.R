#Analysis based on Data exploration
#Mark Warren

#Libraries
library(tidyverse)
library(readxl)

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
    select(all_of(MyNames))

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

#Binomial GLM for Donate probability
library(broom) #For extracting model coeffs etc

#Select only columns we need for modelling and then remove NAs
Motiv2 <- Motiv %>% 
    select(Donate_Amount, Donate, Block, Age, Protect_Env, Gender, Env_Knowledge)

colSums(is.na(Motiv2))
Motiv2 <- na.omit(Motiv2)



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












