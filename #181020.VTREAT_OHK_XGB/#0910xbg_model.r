
library(xgboost)
library(scorecard)
library(dplyr)
require(xgboost)
require(Matrix)
require(data.table)


# 加载数据

url = "C:\\Users\\Administrator\\Desktop\\RiskDocu\\SH_TC_SCORECARD0710\\Ware_card_nms06.csv"
data=fread(url,encoding ='UTF-8')


#sample

#filter-var
filter <- var_filter(data, y = "target", x = NULL, iv_limit = 0.1, missing_limit = 0.1,
                     identical_limit = 0.95, var_rm = NULL, var_kp = NULL,
                    return_rm_reason = TRUE, positive = "bad|1")


#dropvar=  c("memberId", "day")
#=  data[, !dropvar, with=FALSE]

keepvar=  c("target", "mobile_net_age",  "call_time_late_night_6month",  "account_balance" , "age" ,  "creditscore_top10_contact_median"  ,
            "all_p2p",  "call_cnt_passive_6mth",  "is_contact1_called_6mth")
subdata=  data[, keepvar, with=FALSE ]




data_num=dplyr::select_if( filter$dt, is.numeric)

dt_list = split_df(data_num, ratio=0.8, seed= 117)  #117
train= dt_list$train
test = dt_list$test


COL_NUMBER<-which( colnames(train)=="target" )

#sparse_matrix_train <- Matrix::sparse.model.matrix(target ~ .-1, data = train_r)
sparse_matrix_train <- sparse.model.matrix(target ~ ., data = train )[,-1]
#label_train= as.matrix(train_r[ ,target]==1)
label_train= as.matrix(train[ ,"target"])
table(label_train)



# 拟合模型
boost <- xgboost(data = sparse_matrix_train, label = label_train,    max_depth = 5,
               eta = 0.2, nthread = 3, nrounds = 10,objective = "binary:logistic")


bst <- xgb.cv(data = sparse_matrix_train  , 
              label = label_train, 
              eta = 0.2 ,
              max_depth = 5, 
              nround=10 , 
              nfold =10 ,
              nthread = 3 ,
              objective = "binary:logistic",
              subsample = 0.6,
              colsample_bytree = 0.6,
              seed = 101,
              
              eval_metric = "rmse"
              
)



#============================importance
importance <- xgb.importance(feature_names = colnames(sparse_matrix_train), model = boost)
head(importance,45)



importanceRaw <- xgb.importance(feature_names = colnames(sparse_matrix_train), 
                                model = boost, data = sparse_matrix_train, label = label_train)
importanceRaw


# Cleaning for better display
importanceClean <- importanceRaw[,`:=`(Cover=NULL, Frequency=NULL)]

remainvar= head(importanceClean,25)
remainvar_list=as.list(remainvar[,1])



#xgb plot
xgb.plot.importance(importance_matrix = importance[1:10,])



#简单的卡方检验，来检验它是否是真正重要的变量
c2 <- chisq.test(train_r$mobile_net_age, label_train)
print(c2)




# 预测

label_test= as.matrix(test[ ,"target"])

#sparse_matrix_test <- Matrix::sparse.model.matrix(target ~ .-1, data = test_r ) 


pred_test <- predict(bst, label_test)
table(pred_test)












