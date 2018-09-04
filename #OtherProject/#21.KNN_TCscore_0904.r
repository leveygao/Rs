library(kknn)
library(class)
library(scorecard)


#load data
url = "D:\\MyConfiguration\\gy49026\\Desktop\\ож╫П╢Ш\\project\\SH_model\\scorecard_tc\\Ware_card_nms06.csv"
raw=read.csv(url)


subdata<-raw[,
                   c( "mobile_net_age",  
                      "account_balance",      
                      "call_time_late_night_3month",
                      "msg_count_1month",
                      "recharge_amount_6month",
                      "creditscore_top10_contact_avg",
                      "id_small_loan" ,    
                      "all_p2p"  ,
                      "age",
                      "target"
                   )]



#sample
dt_list = split_df(subdata, ratio=0.7, seed=7)
train= dt_list$train
test = dt_list$test


# normalize
normalize = function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
#ix = c(1, 8, 10)
norm_train= as.data.frame(lapply(norm_train[, -10], normalize))
norm_test= as.data.frame(lapply(norm_test[, -10], normalize))

train_label=norm_train[, 10]
test_label=norm_test[, 10]


#----
#train iter


set.seed(1)

range <- 1:round(0.005 * nrow(norm_train))
accs <- rep(0, length(range))
rmse<- rep(0, length(range))

for (k in range) {
  
  # Fill in the ___, make predictions using knn: pred
  pred <- knn(norm_train, norm_test, train_label, k = k)
  
  # Fill in the ___, construct the confusion matrix: conf
  conf <- table(test_label, pred)
  
  # Fill in the ___, calculate the accuracy and store it in accs[k]
  accs[k] <- sum(diag(conf)) / sum(conf)
  
  }

# Plot the accuracies. Title of x-axis is "k".
plot(range, accs, xlab = "k" , ylab="accs")


# Calculate the best k
which.max(accs)


# correct = rep(0,20)
# 
# for(i in 1:20){
#   k[i]=i
#   fit_pre = knn(, , cl = , k=  i )
#   correct[i] = sum(as.numeric(fit_pre)==as.numeric(norm_test$target) )/nrow(norm_test)
# }
# 
# plot(x=k, y=correct , xlab="K",ylab="acc")




#final model

kvalue= 14

fin_pred <- knn(train = norm_train, test = norm_test, cl = train_label , k = kvalue )
# Construct the confusion matrix: conf
conf=table(test_label,fin_pred)
conf

accs <- sum(diag(conf)) / sum(conf)
accs



# Calculate RMSE on the test set for simple linear model
#sqrt(mean( ( as.numeric(fin_pred) - as.numeric(test_label)) ^ 2))




#with weight kknn
# fit_kknn = kknn(nmkat~.,data_train,data_test[,-12],k=1)
# fit = fitted(fit_kknn)
# table(data_test$nmkat,fit
      
      
      

















