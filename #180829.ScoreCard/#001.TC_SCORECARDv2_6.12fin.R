#library
#devtools::install_github("shichenxie/scorecard")
library(sas7bdat)
library(scorecard)
library(klaR)
library(dplyr)
library(plyr)
library(base)
 

card_fin<-read.sas7bdat("D:\\GY\\18.4.9 ODBC_TC\\DATA\\card_final_var_20180611.sas7bdat")
head(card_fin)

#iv-var
dt_info_value = iv(card_fin, y = "target")
dt_info_value


# HIGH - IV
#keep_var<-card_fin[,c("FlyDmsCancel_D800_pct","FlyDmsEnd_D800_pct","TrnOrderBegin_D300_avg","TrnPersons_D300_avg",
#                       "TrnOrderTolbin1_D300_pct","TrnOrderflag1_D300_pct","TC_MLV","maxplat7m","age","sex")]

# SELECT VAR
# keep_var<-card_fin[,c("FlyDmsCancel_D800_pct","FlyDmsEnd_D300_pct","TrnOrderBegin_D300_avg","TrnPersons_D300_avg",
#                       "TrnOrderTolbin1_D300_pct","TrnOrderflag1_D150_pct","TC_MLV","maxplat7m","age","sex",
#                         "FlyDmsEnd_D300_pct", "TrnAmount_D300_avg" ,)]

#filter-var
filter <- var_filter(card_fin, y = "target", x = NULL, iv_limit = 0.2, missing_limit = 0.95,
                     identical_limit = 0.95, var_rm = NULL, var_kp = NULL,
                     return_rm_reason = TRUE, positive = "bad|1")  
str(filter$dt)
iv(filter$dt, y = "target")



keep_var<-card_fin[,c("FlyDmsCancel_D800_pct",  "FlyOrderBegin_D300_avg",  "TrnOrderBegin_D300_avg",   
                      "TrnOrderTolbin1_D300_pct","TC_MLV","sex","age" ,
                      #"maxplat7m",
                      "logincount120m" 
                     
)]

maxplat7m=as.factor(ifelse(card_fin$maxplat7m=='undefined','undefined','defined'))
target=as.factor(card_fin$target)
data=cbind(keep_var ,target,maxplat7m)



#sample
dt_list = split_df(data, ratio=0.7, seed=101)
train= dt_list$train
test = dt_list$test



#woe-bin
woe_bin<-woebin(train, y="target", 
                #x = c("age","duration"), 
                breaks_list =  list(
                  
                  TC_MLV= c("0","1","2" ,"3","4")
                  
                ),
                # special_values = list( 
                #     duration=c(14,28,52)
                # 
                # ),
                min_perc_fine_bin = 0.02, min_perc_coarse_bin = 0.05, stop_limit = 0.1,
                max_num_bin = 10, positive = "bad|1", no_cores = NULL, print_step = 0L,
                method = "tree") #or chimerge
#print(woe_bin)

#woe_bin_df = data.table::rbindlist(woe_bin)
woebin_plot(woe_bin, x = NULL, title = "woe", show_iv = TRUE)



#woe-adj
# breaks_adj = woebin_adj(train, y="target", woe_bin)
# 
# bins_final = woebin(dt, y="target",
#                     breaks_list=breaks_adj)



#woe-apply
dt_woe = woebin_ply(train, woe_bin)
head(dt_woe)



#scorecard
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
summary(dt_pred)
head(dt_pred)


# performace
# ks & roc plot
#perf_eva(dt_woe$target, dt_pred, type = c("ks","lift","roc","pr"))



#card build
card = scorecard(woe_bin, model_fin, points0 = 600, odds0 = 1/20, pdo = 20,
                 basepoints_eq0 = FALSE)

final_scorecard<- ldply(card, rbind)


score_train= scorecard_ply(train, card,only_total_score = F)
summary(score_train$score)

hist(score_train$score,breaks=14, xlab="score",main="Score Hist Train")





#testset
dt_woe_test = woebin_ply(test, woe_bin)
dt_pred_test = predict(model_fin, type='response', dt_woe_test)

perf_eva(dt_woe_test$target, dt_pred_test, type = c("ks","lift","roc","pr"))

score_test= scorecard_ply(test, card,only_total_score = F)
summary(score_test$score)

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
show(psi_s)




#outputcard

write.csv(final_scorecard,"D:\\GY\\R\\18.4.12 SCORECARD\\TC_SCORECARD_June11.csv")





#compare_data
compare_data= card_fin[,c("FlyDmsCancel_D800_pct",  "FlyOrderBegin_D300_avg",  "TrnOrderBegin_D300_avg",   
                          "TrnOrderTolbin1_D300_pct","TC_MLV","sex","age" ,
                          "maxplat7m",
                          "logincount120m","memberid")]

write.csv(compare_data,"D:\\GY\\R\\18.4.12 SCORECARD\\compare_data_June11.csv")




















