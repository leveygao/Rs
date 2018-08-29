

library(glmnet)


#????????????????
train_x = train[,-c("target")]
train_y = train[,"target"]

matrix_train <- data.matrix(train_x)
matrix_train_y <- data.matrix(train_y)


#?????matrix
test_x = test[,-c("target")]
test_y = test[,"target"]
matrix_test<- data.matrix(test_x)
matrix_test_y <- data.matrix(test_y)

matrix_test[is.na(matrix_test )] <-0
matrix_test [is.null(matrix_test )] <-0
matrix_test_y[is.na(matrix_test_y )] <-0
matrix_test_y [is.null(matrix_test_y )] <-0






#??????
fit_net <- glmnet(matrix_train,matrix_train_y,family="binomial" )
print(fit_net)
#???????????????
cv <- cv.glmnet(matrix_train,matrix_train_y,type.measure='auc', family = "binomial")
best_lambda <- cv$lambda.min

pred_net<- predict(fit_net,newx=matrix_test,s=best_lambda,type = 'response')



perf_eva(  as.factor(matrix_test_y), pred_net, type = c("ks","lift","roc","pr"))





#??????????
matrix_train[is.na(matrix_train )] <-0
matrix_train [is.null(matrix_train )] <-0
matrix_train_y[is.na(matrix_train_y )] <-0
matrix_?rain_y [is.null(matrix_train_y )] <-0
#rapply( matrix_train , f=function(x) ifelse(is.nan(x),0,x), how="replace" )
#rapply( matrix_train , f=function(x) ifelse(is.infinite(x),0,x), how="replace" )

table(matrix_train_y)