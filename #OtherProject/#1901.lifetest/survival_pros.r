## library  ----

library(data.table)
library(scorecard)
library(survival)
library(survminer)

# load data ----


# read&subset data
datapath="D:\\Users\\GAOYUAN910\\Desktop\\ALL_CODE\\Py\\#002.Scorecard\\"

modelpath="D:\\Users\\GAOYUAN910\\Desktop\\DOCU\\MODEL_DOCU\\#19.1.14 Survival\\"

Prosper = read.csv(paste(datapath, "ProsperLoanData.csv", sep='') , encoding='UTF-8')


# x_fact<-sapply(Prosper,is.logical)
# myda<-Prosper[,x_fact,drop=FALSE]
# str(myda)


# sub ----

loan_var = c(
  "IncomeVerifiable",
  "IsBorrowerHomeowner",
  "CurrentlyInGroup",
  
  "ClosedDate",
  "LoanOriginationDate",
  "FirstRecordedCreditLine",
  "InquiriesLast6Months",
  "TotalInquiries",
  "CurrentDelinquencies",
  "AmountDelinquent",
  
  "BankcardUtilization",
  "LoanCurrentDaysDelinquent",
  "DelinquenciesLast7Years"
  
  
)


# loaddata and tag ----
data = Prosper[loan_var]

# eda
summary(data$LoanCurrentDaysDelinquent)
summary(data$CurrentDelinquencies)
table(data$CurrentDelinquencies)


# feature

# data$censored ifelse(log(data$LoanCurrentDaysDelinquent)>=7,1,0)

data$censored = ifelse(data$CurrentDelinquencies>=10,1,0)
table(data$censored)

data=data[data$CurrentDelinquencies>0,]


# split df ----
dt=scorecard::split_df(data, ratio=0.7, seed= 17)
train=dt$train
test=dt$test



# # km ----

# km_fit <- survfit(surv_object ~ IncomeVerifiable + CurrentlyInGroup , train  )#BankcardUtilization

# ggsurvplot(km_fit, train  , pval = TRUE)

# survival ----


surv_object <- Surv(time =  train$CurrentDelinquencies,  event = train$censored )




# Weibull model
wb <- survreg(surv_object ~ IsBorrowerHomeowner + BankcardUtilization  , 
              dist="lognormal",data = train)
summary(wb)

# Retrieve survival curve from model probabilities 
surv <- seq(.99, .01, by = -.01)


# train ----
traindat <- expand.grid(
  IsBorrowerHomeowner =  train$IsBorrowerHomeowner ,
  
  BankcardUtilization = quantile(train$BankcardUtilization, 
                                 probs = c( 0.25, 0.5, 0.75,0.9 )))


train_pred <- predict(wb, type = 'quantile', p = 1-surv,         newdata = traindat)

surv_wbmod_train <- cbind(traindat, train_pred)





# Use melt() to bring the data.frame to long format
surv_wbmod <- melt(surv_wbmod_train, 
                   id.vars = c("IsBorrowerHomeowner","BankcardUtilization"), 
                   variable.name = "surv_id", value.name = "time")


# Add column for the survival probabilities
surv_wbmod$surv <- surv[as.numeric(surv_wbmod$surv_id)]

# Add columns upper, lower, std.err, and strata contianing NA values
surv_wbmod[, c("upper", "lower", "std.err", "strata")] <- NA

head(surv_wbmod)

ggsurvplot_df(surv_wbmod, surv.geom = geom_line,    title = 'Train',
              linetype = "IsBorrowerHomeowner", color = "BankcardUtilization" ,
              legend.title = NULL)




# test ----
testdat <- expand.grid(
  IsBorrowerHomeowner =  test$IsBorrowerHomeowner ,
  
  BankcardUtilization = quantile(test$BankcardUtilization, 
                                 probs = c( 0.25, 0.5, 0.75,0.9 )))


test_pred <- predict(wb, type = 'quantile', p = 1-surv,   newdata = testdat)

surv_wbmod_test <- cbind(testdat,   test_pred)

 
surv_wbmod_test_t <- melt(surv_wbmod_test, 
                   id.vars = c("IsBorrowerHomeowner","BankcardUtilization"), 
                   variable.name = "surv_id", value.name = "time")


# Add column for the survival probabilities
surv_wbmod_test_t$surv <- surv[as.numeric(surv_wbmod_test_t$surv_id)]

# Add columns upper, lower, std.err, and strata contianing NA values
surv_wbmod_test_t[, c("upper", "lower", "std.err", "strata")] <- NA

head(surv_wbmod_test_t)

ggsurvplot_df(surv_wbmod_test_t, surv.geom = geom_line,  title = 'Test',
              linetype = "IsBorrowerHomeowner", color = "BankcardUtilization" ,
              legend.title = NULL)




# end ----


# # Get time for each probability
# train_pred <- predict(wb, type = 'quantile', p = 1-surv, newdata =  train)
# train_pred <- predict(wb, type = 'quantile', p = 1-surv, newdata =  data.frame(1))
# 
# # Create data frame with the information needed for ggsurvplot_df
# surv_wb <- data.frame(time = train_pred, surv =  surv, 
#                       upper = NA, lower = NA, std.err = NA)
# 
# 
# # Plot
# 
# ggsurvplot(cxsf, censor = FALSE)
# 
# ggsurvplot_df(fit = surv_wb, surv.geom = geom_line)
# 
# 
# #predict
# 
# km_pred = predict(km_fit, type = "quantile", p = 0.5, newdata =  data.frame(1))
# 
# 
# 
# 
# 
# # Fit a Cox proportional hazards model
# fit.coxph <- coxph(surv_object ~ IncomeVerifiable + BankcardUtilization,  data = data)
# 
# ggforest(fit.coxph, data = data)
# 
# 
# 
# 
# 
# # Weibull model
# 
# wbmod <- survreg( surv_object ~ CurrentlyInGroup , data=data )
# coef(wbmod)
# autoplot(wbmod)
# 
# 
# surv <- seq(.99, .01, by = -.01)
# t_yes <- predict(wbmod, type = "quantile", p = 1-surv,
#                  newdata = data.frame(IsBorrowerHomeowner  = TRUE))
#  
# 
# # Imaginary patients
# newdat <- expand.grid(
#   IsBorrowerHomeowner = levels(GBSG2$IsBorrowerHomeowner),
#   BankcardUtilization = quantile(GBSG2$BankcardUtilization, probs = c(0.25, 0.5, 0.75)))
# newdat
# 
# 
# 
# 
# # Fit a Cox proportional hazards model
# cxmod <- coxph(surv_object ~ IncomeVerifiable, data = data)
# 
# # Show model coefficient
# coef(cxmod)
# 
# 
# newdat <- data.frame(IncomeVerifiable = c(TRUE, FALSE))
# cxsf <- survfit(cxmod, data = data, newdata = newdat, conf.type = "none")
# 
# 
# 
# # Plot Cox model survival curves
# ggsurvplot(cxsf, censor = FALSE)
# 




