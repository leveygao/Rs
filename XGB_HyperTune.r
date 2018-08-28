library(xgboost)
library(ggplot2)
library(reshape2)
library(Ecdat)


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

# = train and test data = #
# xtrain <- Matrix::sparse.model.matrix(target ~ .-1, data = train)
# xtest <- Matrix::sparse.model.matrix(target ~ .-1, data = test)

xtrain = as.matrix(train[ ,!"target"])
xtest = as.matrix(test[ ,!"target"])
ytrain= as.matrix(train[ ,"target"])
ytest= as.matrix(test[ ,"target"])



#eta
set.seed(1)

itern=500
conv_eta = matrix(NA,itern,length(eta))
pred_eta = matrix(NA,length(ytest), length(eta))
colnames(conv_eta) = colnames(pred_eta) =  eta
 
for(i in 1:length(eta)){
  params=list(eta = eta[i], colsample_bylevel=cs[standard[2]],
              subsample = ss[standard[4]], max_depth = md[standard[3]],
              min_child_weigth = 1)
  
  xgb=xgboost(xtrain, label = ytrain, nrounds = itern, params = params)
  conv_eta[,i] = xgb$evaluation_log$train_rmse
  pred_eta[,i] = predict(xgb, xtest)
}


conv_eta1 = data.frame(iter=1:itern, conv_eta)
conv_eta2 = melt(conv_eta1, id.vars = "iter")
ggplot(data = conv_eta2) + geom_line(aes(x = iter, y = value, color = variable))


pred_eta_tb=as.data.frame(pred_eta)
RMSE_eta_tb= cbind(ytest,pred_eta_tb)
RMSE_eta_sqrt=list()

for(i in 1:(length(RMSE_tb)-1)){
  temp=0
  temp= (RMSE_tb[,1]-RMSE_tb[,i+1])^2
  temp_mean = mean(temp)
  RMSE_eta_sqrt[i] =sqrt(temp_mean)
  
}

names(RMSE_eta_sqrt)=eta
RMSE_eta_sqrt










#===================================================#

#colsample_bylevel









