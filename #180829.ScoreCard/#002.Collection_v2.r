#library
#devtools::install_github("shichenxie/scorecard")

library(scorecard)
library(klaR)
library(dplyr)
library(base)

# library()  载入工具包
library(doParallel)
library(car)
library(psych)
library(gvlma)
library(caret)
library(boot)
library(data.table)



card_fin<-read.csv("D:\\GY\\18.8.9 Collection Card\\excel\\CM1_LAST_MODEL_201705.csv")
head(card_fin)



#filter-var
# filter <- var_filter(card_fin, y = "target", x = NULL, iv_limit = 0.2, missing_limit = 0.95,
#                      identical_limit = 0.95, var_rm = NULL, var_kp = NULL,
#                     return_rm_reason = TRUE, positive = "bad|1")
# str(filter$dt)
# ivtable=iv(filter$dt, y = "target")
# ivtable



#scorecard
# 
# 
# library(h2o)
# localH2O = h2o.init()
# dth2o = as.h2o(card_fin)
# # h2o.glm lasso
# fit = h2o.glm(y="target", training_frame=dth2o,
#               family="binomial", nfolds=5, alpha=1, lambda_search=TRUE) # summary(fit)
# 
# 
# 
# # variable importance
# varimp = data.table(h2o.varimp(fit))[names!=""][!is.na(coefficients) & coefficients > 0 & sign == "POS"]
# var_sel3 = c(varimp$names, "target")
# var_sel3
# var-filter ----



keep_var<-card_fin[,
                   c(
                      # "Member_level_C",
                      # "Member_level_B",
                      # "Member_level_A",
                      
                      #"carduser",
                      #"Intv", 
                      #"Ever_frs_dlq",
                      #"Ever_frs_dlqday",
                      #"Apply_loan_intv",  
                 
                      "Fst_OutClass_M",
                      "Fst_OutClass_SQM",
                      "Fst_OutClass_EM",
                      "Trd_OutClass_RE",   
                 
                      
                      "Trd_OutClass_NOST",
                      "Sec_OutClass_NOFF",
                    
                      #"Sec_OutClass_NTHZ",
                      "Sec_OutClass_ABNO",
                      "Sec_OutClass_ZC",
                      #"Sec_OutClass_NOCH",
                
                      
                      "Init_sumOverdueAmt",
                      
                      #"gender"  ,
                      #"age",
                      #"Overday_E30dlq",
                      #"EVER_3M2_ordercnt",
                      "DLQ_TIMES_cnt",
                      #"Order_cnt",
                      
                      
                      
                      "applymth"
                      
                      
                   )]



target=as.factor(card_fin$target)
#target=as.factor(( ifelse( (card_fin$target==0&card_fin$Intv<=30),0,1)))
table(target)
data=cbind(keep_var,target)


data04=data[which(data$applymth<='201804'), ]
data06=data[which(data$applymth>'201804'), ]


#sample
dt_list = split_df(data04, ratio=0.8, seed= 117)  #117
train= dt_list$train
train=train[,!"applymth"]
test = dt_list$test
test=rbind(data06, test)
test=test[,!"applymth"]




# woe-bin ----
woe_bin<-woebin(train, y="target", 
                #x = c("age","duration"), 
                # breaks_list =  
                #   
                #   breaks_adj
                # 
                # ,
                # special_values = list( 
                #     duration=c(14,28,52)
                # 
                #       ),
                min_perc_fine_bin = 0.02,   min_perc_coarse_bin = 0.05,  stop_limit = 0.1,
                max_num_bin =  5 , positive = "bad|1", no_cores = NULL,  print_step = 0L,
                method = "chimerge") #or  tree chimerge
#print(woe_bin)


#woe_bin_df = data.table::rbindlist(woe_bin)
#woebin_plot(woe_bin, x = NULL, title = "woe", show_iv = TRUE)



#woe-adj
breaks_adj = woebin_adj(train, y="target", woe_bin)
breaks_adj


#  modified bin
woe_bin = woebin(train, y="target",
                 breaks_list=breaks_adj,

                 min_perc_fine_bin = 0.02, min_perc_coarse_bin = 0.05, stop_limit = 0.1,
                 max_num_bin = 5, positive = "bad|1", no_cores = NULL, print_step = 0L,
                 method = "chimerge"
)

#woebin_plot(bins_final, x = NULL, title = "woe", show_iv = TRUE)


#woe-apply
dt_woe = woebin_ply(train, woe_bin)
head(dt_woe)





# glm ------
model = glm(target ~ . , family = binomial(link='logit'),     data = dt_woe)
summary(model)



# Select a formula-based model by AIC
m_step = step(model, direction="forward", trace=FALSE , steps=1000,    k=2)   #forward  both
model_fin = eval(m_step$call)
summary(model_fin)


vif(model_fin)
print(vif(model_fin) > 5)



# cross-validation  ------
# tc = trainControl("cv",10,savePred=T)
# fit = train(target~.,   data=dt_woe, method="glm",  trControl=tc,   family=binomial(link = "logit"))
# fit$finalModel


# other cross-validation  ----
#fit_cv=  cv.glm(dt_woe, model_fin, K=10)
# cv.err=cv.glm(dt_woe,  model)
# cv.err$delta[1]
# 
# boot::cv.glm(dt_woe, model)$delta[1]

#head(fit_cv)





# finalmodel ----


#predicted proability
dt_pred = predict(model_fin, type='response', dt_woe)
#summary(dt_pred)
#head(dt_pred)


# performace
# ks & roc plot
perf_eva(dt_woe$target, dt_pred, type = c("ks","lift","roc","pr"))



#card build
card = scorecard(woe_bin, model_fin, points0 = 600, odds0 = 1/20, pdo = 20 ,
                 basepoints_eq0 = FALSE)




score_train= scorecard_ply(train, card,only_total_score = F)
#summary(score_train$score)

hist(score_train$score,breaks=14, xlab="score",main="Score Hist Train")





#testset
dt_woe_test = woebin_ply(test, woe_bin)
dt_pred_test = predict(model_fin, type='response', dt_woe_test)

perf_eva(dt_woe_test$target, dt_pred_test, type = c("ks","lift","roc","pr"))


score_test= scorecard_ply(test, card,only_total_score = F)
#summary(score_test$score)

hist(score_test$score,breaks=14, xlab="score",main="Score Hist Test")

# PSI specifying score range ----
# Less than 0.1 inference insignificant change, no action;
# required; 0.1 - 0.25 inference some minor change, check other scorecard monitoring metrics;
# Greater than 0.25 inference major shift in population, need to delve deeper;


psi_s = perf_psi(
  score = list(train = score_train, test = score_test),
  label = list(train = train$target, test = test$target),
  title='PSI',
  show_plot=TRUE,return_distr_dat=TRUE,
  x_limits = c(0, 1000),
  x_tick_break = 50
)
#show(psi_s)
psi_s$psi
print(psi_s$psi>0.25)




#outputcard
scorecard<-data.frame()
for( i in 1:length(card)){
  
  
  scorecard<-rbind( scorecard,card[[i]],fill=TRUE )
  
}
View(scorecard)


#write.csv(scorecard,"D:\\GY\\18.8.9 Collection Card\\card\\CM1_scorecard_0816v5.csv")








# apply score
Card_all=read.csv("D:\\GY\\18.8.9 Collection Card\\excel\\ALL_CM1_LAST_MODEL_201705.csv")


All_score = scorecard_ply( Card_all,  card,  only_total_score = F)
summary(All_score$score)
hist(All_score$score,breaks=14, xlab="score",main="Score Hist All")

#memberid= (Card_all$memberId)
#names(member)='memberid'
Collect_score= All_score$score
All_member_score=  cbind(Card_all, Collect_score)
head(All_member_score)


#write.csv(All_member_score,"D:\\GY\\18.8.9 Collection Card\\card\\All_CM1_member_score201705.csv")

















