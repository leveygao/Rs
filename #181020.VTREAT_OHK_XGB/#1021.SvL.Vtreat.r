library(scorecard)
library(vtreat)
german=germancredit
mpg=german[1:100,c("credit.amount" , "duration.in.month")]



# mpg is in the workspace
summary(mgp)

# Get the number of rows in mpg
nRows <- nrow(mpg)

# Implement the 3-fold cross-fold plan with vtreat
splitPlan <- kWayCrossValidation(nRows , 3, NULL, NULL)

# Examine the split plan
str(splitPlan)


splitPlan[[1]]



# splitPlan is in the workspace
str(splitPlan)

# Run the 3-fold cross validation plan from splitPlan
k <- 3 # Number of folds
mpg$pred.cv <- 0 
for(i in 1:k) {
  split <- splitPlan[[i]]
  model <- lm(credit.amount~  duration.in.month, data = mpg[split$train, ])
  mpg$pred.cv[split$app] <- predict(model, newdata =  mpg[split$app, ])
}
str(mpg)



mpg2=mpg
# Run the 3-fold cross validation plan from splitPlan
k <- 3 # Number of folds
mpg2$pred.cv <- 0 
for(i in 1:k) {
  split <- splitPlan[[i]]
  model <- lm(credit.amount~  duration.in.month, data = mpg2[split$train, ])
  mpg2$pred.cv  <- predict(model, newdata =  mpg2[split$app, ])
}
str(mpg2)





















