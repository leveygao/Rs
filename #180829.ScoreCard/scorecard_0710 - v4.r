#library
#	
library(sas7bdat)
library(scorecard)
library(klaR)
library(dplyr)
library(base)




card_fin<-read.csv("D:\\GY\\18.7.2 WAREHOUSE_DATA\\data\\ware_card_nms06.csv")
head(card_fin)




#filter-var
filter <- var_filter(card_fin, y = "target", x = NULL, iv_limit = 0.2, missing_limit = 0.95,
                     identical_limit = 0.95, var_rm = NULL, var_kp = NULL,
                     return_rm_reason = TRUE, positive = "bad|1")  
str(filter$dt)
ivtable=iv(filter$dt, y = "target")
ivtable







keep_var<-card_fin[,
                   c( "mobile_net_age",  
                      "account_balance",      
                      
                      #"consume_amount_1month",   mono
                      
                      #"call_count_passive_6month",   
                      "call_time_late_night_3month",
                      
                      #"call_count_holiday_6month",  mono
                      
                      "msg_count_1month",
                      #"useflow_3month",  mono
                      
                      "recharge_amount_6month",
                      "creditscore_top10_contact_avg",
                      "id_small_loan" ,    
                      "all_p2p"  ,
                      
                      "age"
                      
                      
                   )]



target=as.factor(card_fin$target)
gender=as.factor(ifelse(card_fin$sex=='M',1,0))
data=cbind(keep_var ,target ,gender)



#sample
dt_list = split_df(data, ratio=0.7, seed=7)
train= dt_list$train
test = dt_list$test



#woe-bin
woe_bin<-woebin(train, y="target", 
                #x = c("age","duration"), 
                breaks_list =  

                  breaks_adj

                ,
                # special_values = list( 
                #     duration=c(14,28,52)
                # 
                # ),
                # min_perc_fine_bin = 0.02, min_perc_coarse_bin = 0.05, stop_limit = 0.1,
                # max_num_bin = 10, positive = "bad|1", no_cores = NULL, print_step = 0L,
                method = "tree") #or chimerge
#print(woe_bin)

#woe_bin_df = data.table::rbindlist(woe_bin)
#woebin_plot(woe_bin, x = NULL, title = "woe", show_iv = TRUE)



#woe-adj
#breaks_adj = woebin_adj(train, y="target", woe_bin)
# 
# bins_final = woebin(dt, y="target",
#                     breaks_list=breaks_adj)



#woe-apply
dt_woe = woebin_ply(train, woe_bin)
head(dt_woe)



#scorecard
# 
# 
# library(h2o)
# localH2O = h2o.init()
# dth2o = as.h2o(dt_woe)
# # h2o.glm lasso 
# fit = h2o.glm(y="creditability", training_frame=dth2o, 
#               family="binomial", nfolds=0, alpha=1, lambda_search=TRUE) # summary(fit)
# 
# 
# 
# # variable importance
# varimp = data.table(h2o.varimp(fit))[names!=""][!is.na(coefficients) & coefficients > 0 & sign == "POS"]
# var_sel3 = c(varimp$names, "creditability")





# glm ------
model = glm(target ~ ., family = binomial(link='logit'), data = dt_woe)
summary(model)

# Select a formula-based model by AIC
m_step = step(model, direction="both", trace=FALSE)
model_fin = eval(m_step$call)
summary(model_fin)


#finalmodel





#predicted proability
dt_pred = predict(model_fin, type='response', dt_woe)
#summary(dt_pred)
#head(dt_pred)


# performace
# ks & roc plot
perf_eva(dt_woe$target, dt_pred, type = c("ks","lift","roc","pr"))



#card build
card = scorecard(woe_bin, model_fin, points0 = 600, odds0 = 1/20, pdo = 20,
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

#PSI specifying score range
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






#outputcard
scorecard<-data.frame()
for( i in 1:length(card)){
  
  
  scorecard<-rbind( scorecard,card[[i]],fill=TRUE )
  
}
View(scorecard)



write.csv(scorecard,"D:\\GY\\18.7.2 WAREHOUSE_DATA\\card\\scorecard_0710v4.csv")





#compare_data
# compare_data= card_fin[,c( )]
# 
# write.csv(compare_data," ")




#apply score
Cash_loan_all<-read.csv("D:\\GY\\18.7.2 WAREHOUSE_DATA\\card\\Ware_card_all0710.csv")
head(Cash_loan_all)


Cash_loan_score = scorecard_ply( Cash_loan_all,  card,  only_total_score = F)

summary(Cash_loan_score$score)


memberid= Cash_loan_all$memberId
#names(member)='memberid'
Cash_member_score=  cbind(Cash_loan_score, memberid)
head(Cash_member_score)


write.csv(Cash_member_score,"D:\\GY\\18.7.2 WAREHOUSE_DATA\\card\\Cash_member_score_0710v4.csv")

















