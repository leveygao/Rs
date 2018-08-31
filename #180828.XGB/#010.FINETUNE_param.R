#<<<<<<< HEAD
library(xgboost)
library(ggplot2)
library(reshape2)



# = parameters = #
# = eta candidates = #
eta=c(0.05,0.1,0.2,0.5,1)
# = colsample_bylevel candidates = #
cs=c(1/3,2/3,1)
# = max_depth candidates = #
md=c(2,4,6,10)
# = sub_sample candidates = #
ss=c(0.25,0.5,0.75,1)

# ====== standard model is the second value  of each vector above ======= #
standard=c(2,2,3,2)



#----

#eta
set.seed(1)

itern=500
conv_eta = matrix(NA,itern,length(eta))
pred_eta = matrix(NA,length(y_test), length(eta))
colnames(conv_eta) = colnames(pred_eta) =  eta

for(i in 1:length(eta)){
  params=list(eta = eta[i], colsample_bylevel=cs[standard[2]],
              subsample = ss[standard[3]], max_depth = md[standard[4]],
              min_child_weigth = 200)
  
  xgb=xgboost(sparse_matrix_Xtrain, label = y_train, nrounds = itern, params = params)
  conv_eta[,i] = xgb$evaluation_log$train_rmse
  pred_eta[,i] = predict(xgb, sparse_matrix_Xtest)
}


conv_eta1 = data.frame(iter=1:itern, conv_eta)
conv_eta2 = melt(conv_eta1, id.vars = "iter")
ggplot(data = conv_eta2) + geom_line(aes(x = iter, y = value, color = variable))


pred_eta_tb=as.data.frame(pred_eta)
RMSE_eta_tb= cbind(y_test,pred_eta_tb)
RMSE_eta_sqrt=list()

for(i in 1:(length(RMSE_eta_tb)-1)){
  temp=0
  temp= (RMSE_eta_tb[,1]-RMSE_eta_tb[,i+1])^2
  temp_mean = mean(temp)
  RMSE_eta_sqrt[i] =sqrt(temp_mean)
  
}

names(RMSE_eta_sqrt)=eta
RMSE_eta_sqrt



#eta in(0.05,0.1)

#----







#----

#colsample_bylevel



#cs
set.seed(1)

itern=500
conv_cs = matrix(NA,itern,length(cs))
pred_cs = matrix(NA,length(y_test), length(cs))
colnames(conv_cs) = colnames(pred_cs) =  cs

for(i in 1:length(cs)){
  params=list(eta = eta[2], 
              colsample_bylevel=cs[i],
              subsample = ss[standard[3]], max_depth = md[standard[4]],
              min_child_weigth = 200)
  
  xgb=xgboost(sparse_matrix_Xtrain, label = y_train, nrounds = itern, params = params)
  conv_cs[,i] = xgb$evaluation_log$train_rmse
  pred_cs[,i] = predict(xgb, sparse_matrix_Xtest)
}


conv_cs1 = data.frame(iter=1:itern, conv_cs)
conv_cs2 = melt(conv_cs1, id.vars = "iter")
ggplot(data = conv_cs2) + geom_line(aes(x = iter, y = value, color = variable))


pred_cs_tb=as.data.frame(pred_cs)
RMSE_cs_tb= cbind(y_test,pred_cs_tb)
RMSE_cs_sqrt=list()

for(i in 1:(length(RMSE_cs_tb)-1)){
  temp=0
  temp= (RMSE_cs_tb[,1]-RMSE_cs_tb[,i+1])^2
  temp_mean = mean(temp)
  RMSE_cs_sqrt[i] =sqrt(temp_mean)
  
}

names(RMSE_cs_sqrt)=cs
RMSE_cs_sqrt



#cs in(2/3)

#----





#----

#max_depth



#cs
set.seed(1)

itern=500
conv_md = matrix(NA,itern,length(md))
pred_md = matrix(NA,length(y_test), length(md))
colnames(conv_md) = colnames(pred_md) =  md

for(i in 1:length(md)){
  params=list(eta = eta[2], 
              colsample_bylevel=cs[2],
              subsample = ss[standard[3]], 
              max_depth = md[i],
              min_child_weigth = 200)
  
  xgb=xgboost(sparse_matrix_Xtrain, label = y_train, nrounds = itern, params = params)
  conv_md[,i] = xgb$evaluation_log$train_rmse
  pred_md[,i] = predict(xgb, sparse_matrix_Xtest)
}


conv_md1 = data.frame(iter=1:itern, conv_md )
conv_md2 = melt(conv_md1, id.vars = "iter")
ggplot(data = conv_md2) + geom_line(aes(x = iter, y = value, color = variable))


pred_md_tb=as.data.frame(pred_md)
RMSE_md_tb= cbind(y_test,pred_md_tb)
RMSE_md_sqrt=list()

for(i in 1:(length(RMSE_md_tb)-1)){
  temp=0
  temp= (RMSE_md_tb[,1]-RMSE_md_tb[,i+1])^2
  temp_mean = mean(temp)
  RMSE_md_sqrt[i] =sqrt(temp_mean)
  
}

names(RMSE_md_sqrt)=md
RMSE_md_sqrt



#md in(4)

#----





#----

#sub_sample



#cs
set.seed(1)

itern=500
conv_ss = matrix(NA,itern,length(ss))
pred_ss = matrix(NA,length(y_test), length(ss))
colnames(conv_ss) = colnames(pred_ss) =  ss

for(i in 1:length(ss)){
  params=list(eta = eta[2], 
              colsample_bylevel=cs[2],
              subsample = ss[i], 
              max_depth = md[4],
              min_child_weigth = 200)
  
  xgb=xgboost(sparse_matrix_Xtrain, label = y_train, nrounds = itern, params = params)
  conv_ss[,i] = xgb$evaluation_log$train_rmse
  pred_ss[,i] = predict(xgb, sparse_matrix_Xtest)
}



conv_ss1 = data.frame(iter=1:itern, conv_ss )
conv_ss2 = melt(conv_ss1, id.vars = "iter")
ggplot(data = conv_ss2) + geom_line(aes(x = iter, y = value, color = variable))


pred_ss_tb=as.data.frame(pred_ss)
RMSE_ss_tb= cbind(y_test,pred_ss_tb)
RMSE_ss_sqrt=list()

for(i in 1:(length(RMSE_ss_tb)-1)){
  temp=0
  temp= (RMSE_ss_tb[,1]-RMSE_ss_tb[,i+1])^2
  temp_mean = mean(temp)
  RMSE_ss_sqrt[i] =sqrt(temp_mean)
  
}

names(RMSE_ss_sqrt)=ss
RMSE_ss_sqrt



#ss in(0.75)

#----




 
