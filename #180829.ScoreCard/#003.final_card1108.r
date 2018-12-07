#library
#devtools::install_github("shichenxie/scorecard")

library(scorecard)
library(dplyr)

#library(h2o)
#library(xgboost)
library(data.table)
library(car)
library(psych)
library(gvlma)
# library()  载入工具包
# library(klaR)
# library(doParallel)

# library(psych)

# library(caret)
# library(boot)



#read&subset data
cardpath="D:\\#00.GAOYUAN_SUMMARY\\18.10.18 New_Application_Scorecard\\csv\\"
card_label<-read.csv(paste(cardpath, "Fin_sample_feature_add1030.csv", sep='')  )
#head(card_label)

#TCsorders=ifelse(card_label$TCsorders>=1,1,0)
TCsorders=ifelse( card_label$TCsorders>=3 , '>=3' , ifelse( card_label$TCsorders>=1, '>=1', '<=0'))

#table(card_label$TCsorders,TCsorders)

keep_var = c(
  "bdscore",
  "mobile_net_age",
  "call_time_late_night_3month",
  "consume_amount_6month",
  "account_balance",
  
  "all_bank_online_ThreeMth",
  "mb_small_loan_OneMth",
  "all_p2p_OneMth",
  #"platform_counts_ThreeMth",
  #"no_contact",
  
  #"creditscore_top10_contact_median",
  #"manyheads_top10_contact_recent6m",
  # "call_count_active_late_night_6mo",
  # "max_continue_silence_day_0call_6"
  
 
  #"sorders" ,  
  "Phone_Brand" , 
  #"os_Type"  , 
  #"job_flag" ,
  #"appAmt_cnt" ,
  "freeRAM_cnt"
  
)


card_keep =  card_label[, which(names(card_label)%in%keep_var)]
card_keep =  cbind(card_keep,TCsorders)
target =    as.factor(card_label$Target_frs)
data=cbind(card_keep,target)





#sample
dt_list = split_df(data, ratio=0.8, seed= 117)  #117

train_set= dt_list$train  
test_set = dt_list$test

# summary(train_set)
# summary(test_set)

train=train_set
test=test_set

# train= train_set[which ( is.na(train_set$all_bank_online_ThreeMth)==FALSE),] 
# test= test_set[which ( is.na(test_set$all_bank_online_ThreeMth)==FALSE),]

# train= train_set[which ( is.na(train_set$all_bank_online_ThreeMth)==FALSE 
#                          & is.na(train_set$platform_counts_ThreeMt)==FALSE),]
# 
# test= test_set[which ( is.na(test_set$all_bank_online_ThreeMth)==FALSE 
#                        & is.na(test_set$platform_counts_ThreeMt)==FALSE),]


#modelling


# woe-bin ----
# woe_bin=woebin(dt= train , y="target", 
#                #x = c("age","duration"), 
#                # breaks_list =  
#                # 
#                #   breaks_adj
#                # 
#                # ,
#                # special_values = list( 
#                #     duration=c(14,28,52)
#                # 
#                #       ),
#                min_perc_fine_bin = 0.02,   min_perc_coarse_bin = 0.05,  stop_limit = 0.1,
#                max_num_bin =  6 , positive = "bad|1", no_cores = NULL,  print_step = 0L,
#                
#                method = "tree") #or  tree chimerge


# woetable=data.frame()
# for(i in 1:length(woe_bin)){
#   table=data.frame()
#   
#   table=cbind(woe_bin[[i]]$variable ,woe_bin[[i]]$total_iv)
#   woetable=rbind(woetable,table)
# }
# woetableiv= distinct(select(woetable, V1,V2))
# woetableiv


#print(woe_bin)


#woe_bin_df = data.table::rbindlist(woe_bin)
# woebin_plot(woe_bin, x = NULL, title = "woe", show_iv = TRUE)



#woe-adj
#breaks_adj = woebin_adj(train, y="target", woe_bin)
breaks_adj =
  list(
    bdscore=c("455", "560", "580", "610"),
    account_balance=c(0,3000,6000),
    mobile_net_age=c(20,40,60,130),
    call_time_late_night_3month=c("600", "1700"),
    consume_amount_6month=c("65000", "85000"),
    all_p2p_OneMth=c("1", "5"),
    mb_small_loan_OneMth=c("1", "3", "4"),
    all_bank_online_ThreeMth=c(1,2),
    TCsorders= c("<=0%,%missing", ">=1", ">=3") ,
    Phone_Brand= c("HUAWEI%,%OTHER%,%SM%,%MI%,%missing", "OPPO%,%VIVO", "IPHONE") ,
    freeRAM_cnt=c(650000,950000,1850000)
  )




#  modified bin
woe_bin = woebin(train, y="target",
                 breaks_list=breaks_adj,

                 min_perc_fine_bin = 0.02,   min_perc_coarse_bin = 0.05,  stop_limit = 0.1,
                 max_num_bin =  6 , positive = "bad|1", no_cores = NULL,  print_step = 0L,
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
m_step = step(model, direction="both", trace=FALSE , steps=1000,    k=2)   #forward  both
model_fin = eval(m_step$call)
summary(model_fin)






# Global test of model assumptions



vif(model_fin)
print(vif(model_fin) > 2.5)


#corr  ----
# corvarlist=c("all_p2p_OneMth", "mb_small_loan_OneMth", "all_bank_online_ThreeMth", "TARGET") #"platform_counts_ThreeMth",
# 
# card_cor =  card_label[, which(names(card_label)%in%corvarlist)]
# 
# # corrplot
# # pairs=pairs(card_cor[,-5])
# pairs_panels=pairs.panels( card_cor[,-4] )



# cross-validation  ------
# tc = trainControl("cv",10,savePred=T)
# fit = train(target~.,   data=dt_woe, method="glm",  trControl=tc,   family=binomial(link = "logit"))
# fit$finalModel



# other cross-validation? ----
#fit_cv=  cv.glm(dt_woe, model_fin, K=10)
# cv.err=cv.glm(dt_woe,  model)
# cv.err$delta[1]
# 
# boot::cv.glm(dt_woe, model)$delta[1]

#head(fit_cv)





# finalmodel ----




#predicted proability
dt_pred = predict(model_fin, type='response', dt_woe)
summary(dt_pred)
#head(dt_pred)


# performace
# ks & roc plot
perf_eva(dt_woe$target, dt_pred, type = c("ks","lift","roc","pr"), show_plot=FALSE)



#card build
card = scorecard(woe_bin, model_fin, points0 = 600, odds0 = 1/20, pdo = 20 ,
                 basepoints_eq0 = FALSE)




score_train= scorecard_ply(train, card,only_total_score = F)
#summary(score_train$score)

hist(score_train$score,breaks=14, xlab="score",main="Score Hist Train")





#testset
dt_woe_test = woebin_ply(test, woe_bin)
dt_pred_test = predict(model_fin, type='response', dt_woe_test)

perf_eva(dt_woe_test$target, dt_pred_test, type = c("ks","lift","roc","pr"), show_plot=FALSE)


score_test= scorecard_ply(test, card,only_total_score = F)
#summary(score_test$score)

hist(score_test$score,  breaks=14, xlab="score",main="Score Hist Test")


# PSI specifying score range ----
# Less than 0.1 inference insignificant change, no action;
# required; 0.1 - 0.25 inference some minor change, check other scorecard monitoring metrics;
# Greater than 0.25 i?ference major shift in population, need to delve deeper;


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
#View(scorecard)

#csv card
write.csv( scorecard, paste(cardpath, "scorecard_phone_1109.csv", sep=''))







# all data ----
#read&subset data


feature_all<-read.csv(paste(cardpath, "Feature_All_20181017.csv", sep='')  )

feature_keep =  feature_all[, which(names(feature_all)%in%keep_var)]
target_all =    as.factor(feature_all$TARGET)
data_all= cbind(feature_keep,target_all)


score_all= scorecard_ply(data_all, card,only_total_score = F)




#test score on all



score_all_tar = cbind(score_all,target_all)


hist(score_all_tar$score,  breaks=14, xlab="score",main="Score Hist All")


perf_eva(score_all_tar$target_all, score_all_tar$score, type = c("ks","lift","roc","pr"), show_plot=TRUE)

