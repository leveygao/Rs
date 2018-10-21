#Supervised Learning in R: Regression 
#Tree-Based Methods

# Load the package  ----
library(scorecard)
library(ranger)
library(data.table)
library(magrittr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(vtreat)
library(xgboost)



#data prepare----

german=germancredit
target=ifelse(german$creditability=="bad",1,0)


drops=c("creditability" )
xset=german[,!(names(german) %in% drops)]
data=cbind(xset,target)

# train and test
dt_list = split_df(data,  
                   ratio=0.8, seed=101)
train = dt_list$train
test = dt_list$test




#random forest----

# Random seed to reproduce results
set.seed=101
seed=101

# The outcome column
(outcome <- "credit.amount")

# The input variables
(vars <- c("foreign.worker","duration.in.month","age.in.years","job"))

# Create the formula string for bikes rented as a function of the inputs
(fmla <- paste(outcome, "~", paste(vars, collapse = " + ")))

# Fit and print the random forest model
( model_rf <- ranger(fmla, # formula 
                         data= train, # data
                         num.trees = 500, 
                         respect.unordered.factors = "order", 
                         seed = seed
                        #classification=TRUE
                          ))


# Make predictions on the August data
test$pred <- predict(model_rf, test)$predictions


# Calculate the RMSE of the predictions
test %>% 
  mutate(residual = test$credit.amount-test$pred)  %>% # calculate the residual
  summarize(rmse  = sqrt(mean(residual^2)))      # calculate rmse

# Plot actual outcome vs predictions (predictions on x-axis)
ggplot(test, aes(x = pred, y = credit.amount)) + 
  geom_point() + 
  geom_abline()



# Plot  by  
test %>% 
  mutate(instant = housing) %>%   
  gather(key = valuetype, value = value, credit.amount, pred) %>%
  
  filter( housing == c("rent","own") ) %>% # first two 
  ggplot(aes(x = instant, y = value, color = valuetype, linetype = valuetype)) + 
  geom_point() + 
  geom_line() + 
  #scale_x_continuous("Day", breaks = 0:14, labels = 0:14) + 
  scale_color_brewer(palette = "Dark2") + 
  ggtitle("Predicted  , Random Forest plot")





# one hot key ----


# Create and print a vector of variable names
#(vars_ohk <- c("foreign.worker","job" ))

# Create the treatment plan
treatplan <- designTreatmentsZ(train, vars)

# Examine the scoreFrame
(scoreFrame <- treatplan %>%
    use_series(scoreFrame) %>%
    select(varName, origName, code))

# We only want the rows with codes "clean" or "lev"
(newvars <- scoreFrame %>%
    filter(code %in% c( "clean" , "lev")) %>%
    use_series(varName))

# Create the treated training data
dtrain.treat = prepare(treatplan,train, varRestriction = newvars)


# treatplan
summary(treatplan)


# Use prepare() to one-hot-encode testframe
dtest.treat <- prepare(treatplan, test, varRestriction = newvars)




# xgb ----

# Run xgb.cv
cv <- xgb.cv(data = as.matrix(dtrain.treat), 
             label = train$credit.amount,
             nrounds = 100,
             nfold = 5,
             objective = "reg:linear",
             eta = 0.3,
             max_depth = 6,
             early_stopping_rounds = 10 ,
             verbose = 0    # silent
)

# Get the evaluation log 
elog <- cv$evaluation_log

# Determine and print how many trees minimize training and test error
min_elog= elog %>% 
  summarize(ntrees.train = which.min(train_rmse_mean),   # find the index of min(train_rmse_mean)
            ntrees.test  =  which.min(test_rmse_mean))   # find the index of min(test_rmse_mean)




# The number of trees to use, as determined by xgb.cv
ntrees =  min_elog[[2]]


# Run xgboost
 model_xgb_fin <- xgboost(data = as.matrix(dtrain.treat ), # training data as matrix
                          label = train$credit.amount,  # column of outcomes
                          nrounds = ntrees,       # number of trees to build
                          objective =  "reg:linear", # objective
                          eta = 0.3,
                          depth = 6,
                          verbose = 0  # silent
)

# Make predictions
test$pred <- predict(model_xgb_fin, as.matrix(dtest.treat))

# Plot predictions (on x axis) vs actual bike rental count
ggplot(test, aes(x = pred, y =  credit.amount)) + 
  geom_point() + 
  geom_abline()



# bikesAugust is in the workspace
str(test)


# Calculate RMSE
test %>%
  mutate(residuals =  credit.amount - pred) %>%
  summarize(rmse = sqrt(mean(residuals^2)))











