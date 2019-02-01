

#var_select


# sparse_matrix_train <- Matrix::sparse.model.matrix(FLAG ~ .-1 , data = train_set )
xtrain = as.matrix(train_set[ ,-"FLAG"])
y_train=train_set$FLAG


# model
bst <- xgboost(data = xtrain , label = y_train,    max_depth = 5,
               eta = 0.05, nthread = 4, nrounds = 600,objective = "binary:logistic")


#----

#============================importance
importance <- xgb.importance(feature_names = colnames(xtrain), model = bst)
head(importance,40)


# write.csv(importance,paste(modelpath, "model_importance.csv",sep=''))

importanceRaw <- xgb.importance(feature_names = colnames(xtrain), model = bst, 
                                data = xtrain, label = y_train)
importanceRaw


# Cleaning for better display
importanceClean <- importanceRaw[,`:=`(Cover=NULL, Frequency=NULL)]

head(importanceClean)



#xgb plot
xgb.plot.importance(importance_matrix = importance[1:25,],xlab="impotance", main="Xgb-25/Var")



#valid chisq
# c2 <- chisq.test(train_r$mobile_net_age, label_train)
# print(c2)



#----


