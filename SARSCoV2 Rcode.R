#Importing trimmed Excel spreadsheet with necessary raw data
library(readxl)
FLCOV <- read_excel("C:/Users/lg/Downloads/IE 582 ENGR Analytics/Personal Project/FL covid case count.xlsx", 
                    range = "A1:K68")
View(FLCOV)

#Remove Lafayette county (Infection rate outlier) from the original dataset
FLCOV <- FLCOV[-c(33),]


#Finding R-squared values and covariance for each predictor variables
x1 <- lm(FLCOV$InfRate ~ FLCOV$Unemp)
summary(x1)
x2 <- lm(FLCOV$InfRate ~ FLCOV$FoodIns)
summary(x2)
x3 <- lm(FLCOV$InfRate ~ FLCOV$`20-65+`)
summary(x3)
x4 <- lm(FLCOV$InfRate ~ FLCOV$Med_Age)
summary(x4)
x5 <- lm(FLCOV$InfRate ~ FLCOV$Deg_Att)
summary(x5)
x6 <- lm(FLCOV$InfRate ~ FLCOV$Med_Income)
summary(x6)

#plotting the data as a first step to be relationship between predictor variables and Infection Rate
FLCOV1 <- FLCOV[-c(1,4:5,8)]
library(GGally)
ggpairs(FLCOV1)

#Removing umemployment rate and median income from the original dataframe
FLCOV <- FLCOV[-c(2,11)]


#Summon Magrittr package for Pipe
library(magrittr)

#First trial for MLR with four predictor variables
lm(FLCOV$InfRate ~ FLCOV$FoodIns+FLCOV$`20-65+`+FLCOV$Med_Age+FLCOV$Deg_Att)%>% 
  summary()

#Making a subset dataframe with only the four remaining numerical predictor variables
FLCOV2 <- FLCOV[-c(1,3:5,7)]

#Scatterplots for each predictor variable-combination
pairs(FLCOV2)

#Correlation matrix for each predictor variable-combination with Pearson's Coefficient 
cor(FLCOV2, method="pearson")


#variable assignment for linear model 1 
lm1 <- lm(InfRate ~ FoodIns + FLCOV$`20-65+` + Med_Age + Deg_Att, data=FLCOV)

#Summon Car package for Variance Inflation Factor function
library(car)

#Visualizing VIF values and dotted line over VIF=5 (4 predictor variables)
vif_values1 <- vif(lm1)
barplot(vif_values1, main = "VIF Values", horiz = TRUE, col = "grey", cex.names = 0.8)
abline(v = 5, lwd = 3, lty = 3)
summary(lm1)


#variable assignment for linear model 2 (3 predictor variables)
lm2 <- lm(InfRate ~ FoodIns + Med_Age + Deg_Att, data=FLCOV)
vif_values2 <- vif(lm2)
barplot(vif_values2, main = "VIF Values", horiz = TRUE, col = "grey", cex.names = 1)
summary(lm2)


#variable assignment for final linear model (2 predictor variables)
lm3 <- lm(InfRate ~ Med_Age + Deg_Att, data=FLCOV)
vif_values3 <- vif(lm3)
barplot(vif_values3, main = "VIF Values", horiz = TRUE, col = "grey", cex.names = 1)
summary(lm3)

#confidence interval for the model parameters at CI=0.95
confint(lm3, conf.level=0.95)

#Diagnostic plots for the MLR model (4 plots in one 2x2 frame)
par(mfrow=c(2,2))
plot(lm3)


par(mfrow=c(1,1))

#Final model assessment
residuals(lm3) #residuals
anova(lm3) #ANOVA table
influence(lm3) #regression diagnostics

#Comparing the nested linear models
anova(lm1, lm3)
anova(lm2, lm3)
