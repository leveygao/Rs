# Make predictions for each of the pruned trees using the test set.
#pred_undersample <- predict(ptree_undersample, newdata = test_set,  type = "class")
#pred_prior <- predict(ptree_prior  , newdata = test_set,  type = "class")
#pred_loss_matrix <- predict?ptree_loss_matrix    , newdata = test_set,  type = "class")
pred_weights <- predict(tree_prior    , newdata = test_set,  type = "class")

# construct confusion matrices using the predictions.
#confmat_undersample <- table(test_set$loan_status, pred_undersa?ple)
#confmat_prior <- table(test_set$loan_status, pred_prior)
#confmat_loss_matrix <- table(test_set$loan_status, pred_loss_matrix)

confmat_weights <- table(test_set$target, pred_weights)

# Compute the accuracies
#acc_undersample <- sum(diag(confmat_und?rsample)) / nrow(test_set)
#acc_prior <- sum(diag(confmat_prior)) / nrow(test_set)
#acc_loss_matrix <- sum(diag(confmat_loss_matrix)) / nrow(test_set)

acc_weights <- sum(diag(confmat_weights)) / nrow(test_set)
acc_weights

# Make predictions for the proba?ility of default using the pruned tree and the test set.
prob_default_prior <- predict(Ptree_prior, newdata = test_set)[ ,2]

# Obtain the cutoff for acceptance rate 80%
cutoff_prior <- quantile(prob_default_prior, 0.8)

# Obtain the binary predictions.
bin_pred_prior_80 <- ifelse(prob_default_prior > cutoff_prior, 1, 0)

# Obtain the actual default status for the accepted loans
accepted_status_prior_80 <- test_set$target[bin_pred_prior_80 == 0]

# Obtain the bad rate for the accepted loans
sum(accepted_status_prior_80) / length(accepted_status_prior_80)



#AUC

logmodel<-glm(target ~ ., family=binomial(link = "logit"),na.action=na.exclude, data = training_set) 
logmodel

pred_logmodeltest <- predict(logmodel, newdata = test_set, type = "response")
# Construct the objects containing ROC-informa?ion
ROC_logit <- roc(test_set$target, pred_logmodeltest)



# Draw all ROCs on one plot
plot(ROC_logit)
lines(ROC_logit, col = "blue")

# Compute the AUCs
auc(ROC_logit)



#逐步回归法，获取自变量中对违约状态影响最显著的指标
base.mod<-lm(target~1,data = tr?ining_set)
#获取线性回归模型的截距
all.mod<-lm(target~.,data = training_set)
#获取完整的线性回归模型
stepMod<-step(base.mod, scope = list(lower=base.mod,upper=all.mod),
              direction = "both",trace = 0,steps = 100)
#采用双向逐步回归法，筛选变量
s?ortlistedVars<-names(unlist(stepMod[[1]]))
#获取逐步回归得到的变量列表
shortlistedVars<-shortlistedVars[!shortlistedVars %in%"(Intercept)"]
#删???逐步回归的截距
print(shortlistedVars)





