df = asd_all_ML

# Model 1: Logistic Regregression. (Jaundice)
FitJaundice = glm(Class ~ Jaundice, data = df)

summary(FitJaundice)

# Model 2: Logistic Regregression. (asian)
FitAsian = glm(Class ~ asian, data = df)

summary(FitAsian)

# Model 3: Logistic Regregression. (white)
FitWhite = glm(Class ~ white, data = df)

summary(FitWhite)

# Stepwise Regression
FitAll = glm(Class ~ ., data = df)

summary(FitAll)

## Backward
step(FitAll, direction = 'backward')

BestFit = glm(formula = Class ~ A1 + A2 + A3 + A4 + A5 + A6 + A7 + A8 + 
                A9 + A10 + Sex + Jaundice + hispanic + white, data = df)

summary(BestFit)

## Forward
step(FitAll, direction = 'forward')

BestFit = glm(formula = Class ~ X + A1 + A2 + A3 + A4 + A5 + A6 + A7 + 
                A8 + A9 + A10 + Age + Sex + Jaundice + Family_ASD + asian + 
                black + hispanic + latino + middle.eastern + others + south.asians + 
                white, data = df)
summary(BestFit)

## Hybrid
step(FitAll, direction = 'both')

BestFit = glm(formula = Class ~ A1 + A2 + A3 + A4 + A5 + A6 + A7 + A8 + 
                A9 + A10 + Sex + Jaundice + hispanic + white, data = df)

summary(BestFit)