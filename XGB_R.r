#XGB
library(xgboost)
library(scorecard)
library(data.table)
library(reshape) 
library(NMOF)
library(caret)
library(parallel)
library(snow)


require(Matrix)
require(data.table)


# load data

url = "C:\\Users\\Administrator\\Desktop\\RiskDocu\\SH_TC_SCORECARD0710\\Ware_card_nms06.csv"
data=read.csv(url,    encoding ='UTF-8')


#sample

#filter-var
# filter <- var_filter(multi_tag, y = "Overday_E30dlq", x = NULL, iv_limit = 0.2, missing_limit = 0.1,
#                      identical_limit = 0.95, var_rm = NULL, var_kp = NULL,
#                     return_rm_reason = TRUE, positive = "bad|1")


# dropvar = names(data) %in% c("memberId", "day")    
# subdata=  data[ , !dropvar  ]


keepvar=  c("target", "mobile_net_age",  "call_time_late_night_6month",  "account_balance" , "age" ,  "creditscore_top10_contact_median"  ,
            "all_p2p",  "call_cnt_passive_6mth",  "is_contact1_called_6mth")
subdata=  data[, keepvar]




dt_list = split_df(subdata, ratio=0.7, seed= 17)  #seed
train= dt_list$train
test = dt_list$test


COL_NUMBER<-which( colnames(train)=="target" )

sparse_matrix_train <- Matrix::sparse.model.matrix(target ~ .-1, data = train)
#sparse_matrix_train <- sparse.model.matrix(target ~ ., data = train_r)[,-1]
#label_train= as.matrix(train_r[ ,target]==1)
y_train= as.matrix(train[ ,"target"])
table(y_train)



# model
# bst <- xgboost(data = sparse_matrix_train, label = label_train,    max_depth = 5,
#                eta = 1, nthread = 2, nrounds = 10,objective = "binary:logistic")


bst <- xgboost(data = sparse_matrix_train, 
               label = y_train, 
               eta = 0.8 ,
               max_depth = 10, 
               nround=10 , 
               nfold =10 ,
               nthread = 3 ,
               objective = "binary:logistic",
               subsample = 0.5,
               colsample_bytree = 0.5,
               seed = 101  
               
               #eval_metric = "merror"
               
               #num_class = 12
)

#xgb.cv


#============================importance
importance <- xgb.importance(feature_names = colnames(sparse_matrix_train), model = bst)
head(importance,20)



importanceRaw <- xgb.importance(feature_names = colnames(sparse_matrix_train), model = bst, data = sparse_matrix_train, label = label_train)
importanceRaw


# Cleaning for better display
importanceClean <- importanceRaw[,`:=`(Cover=NULL, Frequency=NULL)]

head(importanceClean)



#xgb plot
xgb.plot.importance(importance_matrix = importance[1:10,])



#valid chisq
c2 <- chisq.test(train_r$mobile_net_age, label_train)
print(c2)




# Pred

#sparse_matrix_test <- Matrix::sparse.model.matrix(target ~ .-1, data = test_r ) 

X_train= data.matrix(train[ ,-1])
pred_train <- predict(bst, X_train)
head(pred_train,10)


X_test= data.matrix(test[ ,!"target"])
pred_test <- predict(bst, X_test)
head(pred_test,10)



#function
Score_trans<-function(basepoints,baseodds,pdo)
{
  beta<-pdo/log(2)
  alpha<-basepoints+beta*log(baseodds)
  return(list(alpha=alpha,beta=beta))
}

AlphaBeta<-Score_trans(500, 1/20, 20)
AlphaBeta


#score
#function
Score_fin<-function(AlphaBeta, prob)
{
  x=AlphaBeta[[1]] - AlphaBeta[[2]]* log(prob/(1-prob))
  return(x)
}

#score

Score_train= Score_fin(AlphaBeta,  pred_train)
summary(Score_train)

Score_test= Score_fin(AlphaBeta,  pred_test)
summary(Score_test)



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
  label = list(train = train_r$target, test = test_r$target),
  title='PSI',
  show_plot=TRUE,return_distr_dat=TRUE,
  x_limits = c(0, 1000),
  x_tick_break = 50
  )

#show(psi_s)
psi_s$psi



