##import data
library(readxl)
dt <- read_excel("C:\\Users\\user\\Desktop\\MASTER 1\\Industrial Statistics\\Lab\\LABSHEET3.xlsx")
library(aplore3)
View(dt)
data<- dt
summary(dt)

##Logistic regression model 
MLRM <- glm(died ~ age80 + white + hmo  + los + type1 + type2 + type3, family =binomial, data = dt)
summary(MLRM)

##Logistic regression selected model 
MLRM2 <- update(MLRM, . ~ . - white - hmo - type3)
summary(MLRM2)

## Model selection by stepwise method
MLRM3 <- step(MLRM, direction='both')
summary (MLRM3)

## Estimated Covariance Matrix of the Estimated Coefficients 
vcov(MLRM2)

## Computing 95% confidence intervals of the Estimated Coefficients 
confint(MLRM2)


##Example: at age 50 the estimated lower limit and upper limit of the logistic probability is
LP <- (-0.10815757 + 0.40302143-0.05284957*4-1.04045050)
L <- exp(LP)/(1+exp(LP)) #lower limit
UP <- (0.85369682 + 0.90969977-0.02254766*4-0.047008037)
U <- exp(UP)/(1+exp(UP))          #upper limit
cbind(L,U)

## Computing OddsRatio 
exp(coef(MLRM2))[-1]



