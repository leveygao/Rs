#<<<<<<< HEAD
#XGB
library(xgboost)
library(scorecard)
library(reshape) 

library(h2o)
library(NMOF)
library(caret)
library(parallel)
library(snow)
library(Hmisc)
library(Ecdat)
library(ggplot2)


require(Matrix)
require(data.table)


# load data

url = "D:\\GY\\18.8.29 SCORECARD_A\\csv\\final_td_jf_model0901.csv"
data=read.csv(url,    encoding ='UTF-8')





datacard=rename(data,  c( Overday_E30dlq=  "target") )
datacard=datacard[,-1]


# filter <- var_filter(datacard, y = "target", x = NULL, iv_limit = 0.1, missing_limit = 0.8,
#                      identical_limit = 0.90, var_rm = NULL, 
#                      var_kp = c("age", "sex"),
#                      return_rm_reason = TRUE, positive = "bad|1")
# 
# 
# 
# filter_dt=filter$dt



dt_list = split_df(datacard, ratio=0.7, seed= 17)  #seed
train= dt_list$train
test = dt_list$test

#----
#var_filter




localH2O = h2o.init()


h2odata = as.h2o(train)  

# h2o.glm  
fit1 = h2o.glm(y="target", training_frame=h2odata,  seed=17,
               family="binomial", nfolds=5, alpha=1, lambda_search=TRUE) 
summary(fit1)

# # variable importance
varimp1 = data.table(h2o.varimp(fit1))[names!=""][!is.na(coefficients) & coefficients > 0 & sign == "POS"]
varimp1


#----

#----
#sample

# dropvar = names(data) %in% c("memberId", "day")    
# subdata=  data[ , !dropvar  ]


#----





# model
# bst <- xgboost(data = sparse_matrix_train, label = y_train,    max_depth = 5,
#                eta = 1, nthread = 2, nrounds = 10,objective = "binary:logistic")



#xgb.cv







#----

#============================importance
importance <- xgb.importance(feature_names = colnames(sparse_matrix_Xtrain), model = bst)
head(importance,30)



importanceRaw <- xgb.importance(feature_names = colnames(sparse_matrix_Xtrain), model = bst, 
                                data = sparse_matrix_Xtrain, label = y_train)
importanceRaw


# Cleaning for better display
importanceClean <- importanceRaw[,`:=`(Cover=NULL, Frequency=NULL)]

head(importanceClean)



#xgb plot
xgb.plot.importance(importance_matrix = importance[1:10,])



#valid chisq
c2 <- chisq.test(train_r$mobile_net_age, label_train)
print(c2)



#----




#------------------ Pred

#----
#----FUNCT
Score_trans<-function(basepoints,baseodds,pdo)
{
  beta<-pdo/log(2)
  alpha<-basepoints+beta*log(baseodds)
  return(list(alpha=alpha,beta=beta))
}


Score_fin<-function(AlphaBeta, prob)
{
  x=AlphaBeta[[1]] - AlphaBeta[[2]]* log(prob/(1-prob))
  return(x)
}


#----
#----FUNC END



#----
#---- Predit
#sparse_matrix_test <- Matrix::sparse.model.matrix(target ~ .-1, data = test_r ) 

pred_train <- predict(bst, sparse_matrix_Xtrain)
summary(pred_train)




pred_test <- predict(bst, sparse_matrix_Xtest)
summary(pred_test)





AlphaBeta<-Score_trans(600, 1/20, 40)
AlphaBeta


#score

Score_train= Score_fin(AlphaBeta,  pred_train)
summary(Score_train)

Score_test= Score_fin(AlphaBeta,  pred_test)
summary(Score_test)



#----
#performe

#ks
perf_eva(train$target, pred_train, type = c("ks","lift","roc","pr"))

perf_eva(test$target, pred_test, type = c("ks","lift","roc","pr"))



hist(Score_train,breaks=14, xlab="score",main="Score_train")


hist(Score_test,breaks=14, xlab="score",main="Score_test")


#psi
Score_trainframe=as.data.frame(Score_train)
psi_trainscore <- rename(Score_trainframe,c(Score_train = "score")) 

Score_testframe=as.data.frame(Score_test)
psi_testscore <- rename(Score_testframe,c(Score_test = "score")) 



psi_s = perf_psi(
  score = list(train = psi_trainscore ,   test = psi_testscore ),
  label = list(train = keepset$target, test = keeptest$target),
  title='PSI',
  show_plot=TRUE,return_distr_dat=TRUE,
  x_limits = c(0, 1000),
  x_tick_break = 50
)

#show(psi_s)
psi_s$psi


#----





