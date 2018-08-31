
#============================importance

# #H2O
# keepvar=  c("target", 
#             
#             "contact_count_1month",
#             "mlth_top10c_rct6mth_have_ptncd",
#             "id_small_loanOneMth",
#             "all_p2pOneMth",
#             "creditscore_top10_contact_max",
#             "msg_count_total_1month",
#             "sex",
#             "recharge_count_6month",
#             "manyheads_top10_contact_recent6m",
#             "age",
#             "call_count_active_late_night_6mo",
#             "recharge_amount_1month",
#             "visittimeofday",
#             "mb_small_loanSevenDay",
#             "mb_p2pSevenDay",
#             "all_p2pSevenDay",
#             "call_time_contact_1month",
#             "contact_count_passive_3month",
#             "gap_day_last_silence_day_0call_6",
#             "mb_countsThreeMth",
#             "firstlogindate_intv",
#             "contact_count_3month",
#             "call_count_call_time_over10min_6",
#             "useflow_3month",
#             "call_count_passive_late_night_6m",
#             "silence_day_0call_0msg_send_6mon",
#             "mb_countsOneMth",
#             "mb_p2pOneMth",
#             "call_time_1_4h_total_3month",
#             "call_count_late_night_6month"
#             
# )

keepvar=  c("target", 
            
            "firstlogindate_intv",
            "ordersuccessrate",
            "recharge_amount_1month",
            "manyheads_top10_contact_recent6m",
            
            "sex",
            "silence_day_0call_0msg_send_6mon",
            "call_count_late_night_6month",
            "call_time_1_4h_total_3month",
            "age",
            "call_time_contact_1month",
            
            "id_small_loanOneMth",
            "all_p2pSevenDay",
            "mb_p2pSevenDay",
            "all_p2pOneMth",
            
            "msg_count_total_1month",
            "contact_count_3month",
            "recharge_count_6month",
            "mlth_top10c_rct6mth_have_ptncd",
            
            "call_count_active_late_night_6mo",
            "call_count_call_time_over10min_6",
            "useflow_3month",
            "mb_small_loanSevenDay",
            "contact_count_passive_3month",
            "call_count_passive_late_night_6m"
            
)




keepset=  train[, keepvar, with=FALSE]
keeptest=  test[, keepvar, with=FALSE]

summary(keepset$ordersuccessrate)



#----


#------ fillna
keepset[is.na(keepset)] = 0
keeptest[is.na(keeptest)] = 0

COL_NUMBER<-which( colnames(keepset)=="target" )

sparse_matrix_Xtrain =  Matrix::sparse.model.matrix(target ~ .-1, data = keepset)
sparse_matrix_Xtest <- Matrix::sparse.model.matrix(target ~ ., data = keeptest)[,-1]


#X_train= as.matrix(train[ ,!c("target")])
y_train=  keepset$target
#table(y_train)

y_test=  keeptest$target
#table(y_test)


dtrain= xgb.DMatrix(data = sparse_matrix_Xtrain , label=  y_train  )
dtest <- xgb.DMatrix(data = sparse_matrix_Xtest, label=  y_test )

watchlist <- list(train=  dtrain, test=  dtest)

bst <- xgb.train(data = dtrain,   # X_train
               label = y_train, 
               
               eta = 0.1 ,
               max_depth = 3,
               colsample_bylevel= 2/3,
               subsample = 0.75,
               min_child_weigth= 1 ,
               #silent=1 ,
               
               #alpha=0,
               #gamma= 0.1,
               
               lambda =1 ,
               nround= 100 , 
               nfold =10 ,
               nthread = 2 ,
               objective = "binary:logistic",
               eval_metric='error',
               eval.metric = "logloss",
               watchlist=watchlist,
               
               verbose=1,
               seed = 17 
                            
)

#xgb.dump(bst, with.stats = T)
#xgb.plot.tree(model = bst)


# save model to binary local file
#xgb.save(bst, "xgboost.model")
# load binary model to R
# bst2 <- xgb.load("xgboost.model")
# pred2 <- predict(bst2, test$data)



importance <- xgb.importance(feature_names = colnames(dtrain), model = bst)
head(importance,30)

#----
#---- Predit
#sparse_matrix_test <- Matrix::sparse.model.matrix(target ~ .-1, data = test_r ) 

pred_train <- predict(bst, dtrain)
summary(pred_train)




pred_test <- predict(bst, dtest)
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
perf_eva(train$target, pred_train, type = c("ks","lift","roc","pr"), show_plot=FALSE)

perf_eva(test$target, pred_test, type = c("ks","lift","roc","pr") , show_plot=FALSE)


hist(Score_train,breaks=14, xlab="score",main="Score_train")
hist(Score_test,breaks=14, xlab="score",main="Score_test")