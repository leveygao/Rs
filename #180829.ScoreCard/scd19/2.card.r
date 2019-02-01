#library
#devtools::install_github("shichenxie/scorecard")

library(scorecard)
library(dplyr) 
library(tidyselect)

library(xgboost)
library(data.table)
library(car)
library(psych)

library(Hmisc)
library(PerformanceAnalytics)
library(corrplot)
library(ggplot2)

# library(gvlma)
# library(sampling)
# library(klaR)
# library(doParallel)

# library(psych)
#library(sqldf)
# library(caret)
# library(boot)



#read&subset data
cardpath="D:\\DATA\\FULL_DMS_SAMPLE\\FULLDATA\\"

modelpath="D:\\Users\\GAOYUAN910\\Desktop\\DOCU\\MODEL_DOCU\\#12.24 NINGXIA\\"

# xb_dataset_full = read.csv(paste(cardpath, "xb_dataset_full.csv", sep='') , encoding='UTF-8' )


xb_dataset = data.table::fread(paste(cardpath, "xb_dataset_full.csv", sep=''), header = TRUE, 
                        encoding='UTF-8')


# table(xb_dataset$PROVINCE)

xb_data_sub = xb_dataset[ (55>=AGE_FROM_ID & AGE_FROM_ID>=18 & 
                             PROVINCE %in% c("甘肃省","宁夏回族自治区","青海省","陕西省"))  ]

table(xb_data_sub$PROVINCE)

                         
# table(xb_data_sub$PROVINCE)

# name=names(xb_data_sub)

flag_keep = xb_data_sub[,!grepl('EDS_', names(xb_data_sub))]
flag_true= flag_keep==TRUE
xb_data_drop = xb_data_sub[,..flag_true]

# table(xb_data_sub$flag_keep , useNA="ifany")



# keep list
# p2p_amount %
# black%
keepreq=grepl('P2P|BLKLIST|BLACK|', names(xb_data_sub))
names=names()
keepreq_list= names[keepreq==TRUE]
keepreq_list


# flag_keep1 = xb_data_sub[,grepl('P2P|BLKLIST|BLACK|FLAG', names(xb_data_sub))]
# flag_true1= flag_keep1==TRUE
# xb_data_keep = xb_data_sub[,..flag_true1]


#filter-var
 
filter <- scorecard::var_filter( xb_data_sub , y = 'FLAG' , x = NULL, iv_limit = 0.001, missing_limit = 0.4,
                                identical_limit = 0.9, var_rm = NULL, 
                                var_kp = keepreq_list,
                                return_rm_reason = TRUE, positive = "bad|1")  

dt= filter$dt
ivinfo= scorecard::iv(xb_data_keep, y = "FLAG")
ivinfo

#iv

var_dropeds=c(
  
  # "OBS_MONTH",
  "APPL_DATE",
  "LONGEST_OVE",
  "CITY"
  
)

ivvar_list=  ivinfo$variable 
# var_dropeds=  tidyselect::vars_select(var_list, starts_with("EDS_"))
var_in_flag = ivvar_list%in%var_dropeds

ivvar_fin=ivvar_list[var_in_flag==FALSE]

iv_sub= ivinfo[ ivinfo$variable %in%ivvar_fin ,]

# write.csv(ivinfo,paste(modelpath, "ivinfo_filter.csv"))


FLAG = xb_data_sub$FLAG
dt_sub = cbind(xb_data_sub[, ..keepreq_list] , FLAG)


ivinfo= scorecard::iv(dt_sub, y = "FLAG")
ivinfo


#sample


# oot_set=dt_sub[(dt_sub$OBS_MONTH)%in%c("201711","201712"),]
# 
# model_set=dt_sub[!dt_sub$OBS_MONTH%in% c("201711","201712"),]
# 
# 
# dt_list = scorecard::split_df(model_set, ratio=0.8, seed= 17)  #117
# 
# train_set= dt_list$train[,-"OBS_MONTH"]
# test_set = dt_list$test[,-"OBS_MONTH"]








#based on varlist ----

keep_var=c(
  
  #"CARD_LATELY_THREEEMON_AR",
        #"FIRST_CREDITCARD_MONTHS_OLD",
  
 
  #"CARD_LATELY_THREEEMON_AR",
  #"CUR_YEAR_EFF_POL_DISEASE_SUM",
  # "LAST_APYMENT_DATE_FLAG_G32",
  #"AUTO_POLICY_CHENGBAO_TYPE_LAS",
  "OPENIN_L12M_CNT",
  
"CARD_INCOME",
"FUND_INCOME",
"LONGEST_AWARDCREDIT",
"P2PAMOUNTM3",
"AVG_CRD_AMT",
"RECENT_6MONTHS_CNT_QUERY",
"WORK_YEAR_PROF",
# "BLKLIST_CHARGEOFF",
# "PAYDAYLOAN_AMT_I",
# "BLACK_G25",
# "BLKLIST_RSKSCR",
# "BLKLIST_COURT",
# "THREE_BLACK_G27",

# "AGE_FROM_ID",
# "EDU_LEVEL_CD",
# "EDU_DEGREE_CD",
# "MARITAL_STATE_CD",
"SEX_FROM_ID",
 
"CARD_AVG_USED",

"VALID_CLIENT_NUM_ORGNAME_MANY",


# "BLKLIST_LST24MTH_NUM",
# "BLKLIST_LST12MTH_30DPD",
"BLKLIST_LST24MTH_30DPD",


  "FLAG",
  "OBS_MONTH"
)



#train test
subset=xb_data_sub[,..keep_var]
subset$SEX_FROM_ID=as.factor(subset$SEX_FROM_ID)
# oot_set=subset[(subset$OBS_MONTH)%in%c("201711","201712"),]

# model_set=subset[!subset$OBS_MONTH%in% c("201711","201712"),]

model_set= subset

dt_list = scorecard::split_df(model_set, y="FLAG", ratio=0.8, seed= 17)  #seed

train = dt_list$train[,-"OBS_MONTH"]
test = dt_list$test[,-"OBS_MONTH"]

table(model_set$FLAG)
table(train$FLAG)
table(test$FLAG)

table(xb_data_sub$OBS_MONTH)

#var

#train=data.table(train_set)
# PROF_CATEGORY =  as.factor(traintable$PROF_CATEGORY)
#  = cbind(  , PROF_CATEGORY)
# train = traintable[, ..keep_var]

#test=data.table(test_set)test
# PROF_CATEGORY =  as.factor(testtable$PROF_CATEGORY)
#  = cbind(testtable[, ..keep_var], PROF_CATEGORY)
# test = testtable[, ..keep_var]



cortable=train[,!"FLAG"]
corr = rcorr(as.matrix(cortable))
correlate=corr$r



# heatmap 
col = colorRampPalette(c("blue", "white", "red"))(20)

heatmap(x = correlate, col = col, symm = TRUE)

corrplot(correlate, method="shade", type="upper", order="hclust", p.mat = corr$P, 
         tl.cex =0.5 , shade.lwd = 1 , pch.cex=2, sig.level = 0.01, insig = "blank")





# woe-bin ----
woe_bin= scorecard::woebin(dt= train , y="FLAG",
                           
                           min_perc_fine_bin = 0.02,   min_perc_coarse_bin = 0.05,  stop_limit = 0.1,
                           max_num_bin =  5 , positive = "bad|1", no_cores = NULL,  print_step = 0L,
                           
                           method = "tree") #or  tree chimerge

woe_bin

# woetable=data.frame()
# for(i in 1:length(woe_bin)){
#   table=data.frame()
#   
#   table=cbind(woe_bin[[i]]$variable ,woe_bin[[i]]$total_iv)
#   woetable=rbind(woetable,table)
# }
# woetableiv= distinct(select(woetable, V1,V2))
# woetableiv





##print(woe_bin)


##woe_bin_df = data.table::rbindlist(woe_bin)
# scorecard::woebin_plot(woe_bin, x = NULL, title = "woe", show_iv = TRUE)



#woe-adj
# breaks_adj = woebin_adj(train, y="FLAG", woe_bin)
breaks_adj =
  list(
    OPENIN_L12M_CNT=c(1, 2),
    CARD_INCOME=c(1, 2),
    FUND_INCOME=c(20000, 48000),
    LONGEST_AWARDCREDIT=c(24,48,108),
    P2PAMOUNTM3=c(2,4),
    AVG_CRD_AMT=c(8000, 14000, 22000),
    RECENT_6MONTHS_CNT_QUERY=c(1, 2, 3),
    WORK_YEAR_PROF=c(5, 9),
    #PAYDAYLOAN_AMT_I=c("500000", "1000000"),
    #BLACK_G25=c(0,1,2),
    BLKLIST_LST24MTH_30DPD=c(0,1),
    
    VALID_CLIENT_NUM_ORGNAME_MANY=c(2,5), 
    AGE_FROM_ID=c("33", "36", "44"),
    CARD_AVG_USED=c(0.4, 0.65, 0.9)
  )


# #  modified bin
woe_bin = scorecard::woebin(dt= train , y="FLAG",
                 breaks_list=breaks_adj,

                 min_perc_fine_bin = 0.02,   min_perc_coarse_bin = 0.05,  stop_limit = 0.1,
                 max_num_bin =  6 , positive = "bad|1", no_cores = NULL,  print_step = 0L,

                 method = "tree"
)
# # 
woebin_plot(woe_bin,  x = "SEX_FROM_ID", title = "woe", show_iv = TRUE) #




#woe-apply
dt_woe =  scorecard::woebin_ply(train, woe_bin)
head(dt_woe)





# glm ------
model = glm(FLAG ~ . , family = binomial(link='logit'),     data = dt_woe)
summary(model)



# Select a formula-based model by AIC
m_step = step(model, direction="forward", trace=FALSE , steps=1000,   k=2)   #forward  both
model_fin = eval(m_step$call)
summary(model_fin)






# Global test of model assumptions
# corr  ----

car::vif(model_fin)
print(car::vif(model_fin) > 2.5)



#predicted proability
dt_pred = predict(model_fin, type='response', dt_woe)
summary(dt_pred)
#head(dt_pred)


# performace
# ks & roc plot
perf_eva(dt_woe$FLAG, dt_pred, type = c("ks","lift","roc","pr"), show_plot=FALSE)



#card build
card = scorecard(woe_bin, model_fin, points0 = 600, odds0 = 1/20, pdo = 20 ,
                 basepoints_eq0 = TRUE)




score_train= scorecard_ply(train, card,only_total_score = F)
#summary(score_train$score)

# hist(score_train$score,breaks=14, xlab="score",main="Score Hist Train")





#testset
dt_woe_test = woebin_ply(test, woe_bin)
dt_pred_test = predict(model_fin, type='response', dt_woe_test)

perf_eva(dt_woe_test$FLAG, dt_pred_test, type = c("ks","lift","roc","pr"), show_plot=FALSE)


score_test= scorecard_ply(test, card,only_total_score = F)
#summary(score_test$score)

# hist(score_test$score,  breaks=14, xlab="score",main="Score Hist Test")




## plot train
FLAG_train = train$FLAG
score_train_plt= cbind(score_train, FLAG_train)

# perf_eva(score_train_plt$FLAG, score_train_plt$score, type = c("ks","lift","roc","pr"), show_plot=FALSE)

pg =   data.frame(score_train_plt[FLAG_train==0, score])
pb =   data.frame(score_train_plt[FLAG_train==1, score])

colnames(pg)= 'score'
colnames(pb)='score'
pg$label='pg'
pb$label='pb'

# and combine into your new data frame vegLengths
trainplt = rbind(pg, pb)

ggplot(trainplt,  aes(score, fill= label )) + geom_density(alpha = 0.2)+labs(title = "Train B/G")

ggplot(trainplt, aes(score, fill = label)) + 
  geom_histogram(alpha = 0.6, aes(y = ..density..), position = 'identity',binwidth= 8)+
  labs(title = "Train B/G")




## plot test
FLAG_test = test$FLAG
score_test_plt= cbind(score_test, FLAG_test)

# perf_eva(score_test_plt$FLAG, score_test_plt$score, type = c("ks","lift","roc","pr"), show_plot=FALSE)


pg =   data.frame(score_test_plt[FLAG_test==0, score])
pb =   data.frame(score_test_plt[FLAG_test==1, score])

colnames(pg)= 'score'
colnames(pb)='score'
pg$label='pg'
pb$label='pb'

# and combine into your new data frame vegLengths
testplt = rbind(pg, pb)
 

ggplot(testplt,  aes(score, fill= label )) + geom_density(alpha = 0.2)+ labs(title = "Test B/G")
ggplot(testplt,  aes(score, fill = label)) + 
      geom_histogram(alpha = 0.6, aes(y = ..density..), position = 'identity',binwidth= 8 )+
      labs(title = "Test B/G")



# PSI specifying score range ----
# Less than 0.1 inference insignificant change, no action;
# required; 0.1 - 0.25 inference some minor change, check other scorecard monitoring metrics;
# Greater than 0.25 inference major shift in population, need to delve deeper;


psi_s = perf_psi(
  score = list(train = score_train, test = score_test),
  label = list(train = train$FLAG, test = test$FLAG),
  title='PSI',
  show_plot=TRUE,return_distr_dat=TRUE,
  x_limits = c(0, 1000),
  x_tick_break = 50
)
show(psi_s)
psi_s$psi
print(psi_s$psi>0.25)




## score

trainplt$set="train"
testplt$set="test"

train_test_label=rbind(trainplt  ,  testplt)
write.csv(train_test_label , paste(modelpath, "train_test_label_v6.csv", sep=''))

# perf_eva(train_test_label$FLAG, train_test_label$score, type = c("ks","lift","roc","pr"), show_plot=FALSE)


#outputcard

scorecard<-data.frame()
for( i in 1:length(card)){
  
  scorecard<-rbind( scorecard,card[[i]],fill=TRUE )
  
}
View(scorecard)


#csv card
write.csv(scorecard , paste(modelpath, "scorecard_ningxia_v6.csv", sep=''))




