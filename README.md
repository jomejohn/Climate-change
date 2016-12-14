# Climate-change
Linear regression model to predict the Temperature using multiple independent variables 

#Subsetting data to build model
trainset <- subset(climate,Year <= 2006)
testset <- subset(climate,Year>2006)

#Linear Regression model using variables except Year and month
model1 <- lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols,data = trainset)
summary(model1)

#Correlations between all variables
cor(trainset)

#simplifying the model becaue of multicollinearity
model2 <- lm(Temp ~ MEI + N2O + TSI + Aerosols,data = trainset )
summary(model2)

#Automatically Building model
automodel <- step(model1)
summary(automodel)

#Testing on unseen data
temppredictions <- predict(model2, newdata = testset)

# Compute out-of-sample R^2 using model2
SSE <- sum((temppredictions-testset$Temp)^2)
SST <- sum((mean(trainset$Temp)- testset$Temp)^2)
Rsquared <- 1-SSE/SST
Rsquared
