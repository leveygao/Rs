#library

library(scorecard)
library(klaR)
library(dplyr)
library(base)
library(h2o)
library(data.table)


# card_fin<-read.csv("D:\\MyConfiguration\\gy49026\\Desktop\\Python\\model\\TC_MODEL07.csv")
# head(card_fin)

#using dataset germancredit as sample
data("GermanCredit")
#Sub_German=GermanCredit[  ,c("amount","present_residence","duration","age")]    #ease for demo
target=as.factor(ifelse(GermanCredit$credit_risk=="good",0,1))
name= c("credit_risk")
German=GermanCredit[ , !names(GermanCredit) %in% name]


card_fin=cbind(German,target)


#filter-var
filter <- var_filter(card_fin, y = "target", x = NULL, iv_limit = 0.1, missing_limit = 0.1,
                     identical_limit = 0.95, var_rm = NULL, var_kp = NULL,
                     return_rm_reason = TRUE, positive = "bad|1")  
str(filter$dt)
ivtable=iv(filter$dt, y = "target")
ivtable

filter_dt=as.data.frame(filter$dt)




#==================================================h2o.varimp

localH2O = h2o.init()

dth2o = as.h2o(filter_dt)  

filter_dt_bin<-woebin(filter_dt, y="target", 
                      method = "tree") #or chimerge

filter_dtwoe = woebin_ply(filter_dt, filter_dt_bin)


dth2o = as.h2o(filter_dtwoe)
# h2o.glm lasso 
fit = h2o.glm(y="target", training_frame=dth2o,  seed=17,
              family="binomial", nfolds=5, alpha=0, lambda_search=TRUE) # summary(fit)

# variable importance

varimp = data.table(h2o.varimp(fit))[names!=""][!is.na(coefficients) & coefficients > 0.1 & sign == "POS"]
varimp
var_sel3 = c(varimp$names, "target")
var_h2o =as.list(var_sel3)
var_h2o


#===================================================keep var


keep_var<-card_fin[,
                   c( "status",  
                      "duration",      
                      "purpose", 
                      "credit_history",   
                  
                      "savings",   
                      "property",
                  
                      "age",
                      "target"
                      
                      
                      
                   )]


#sample


dt_list = split_df(keep_var, ratio=0.7, seed=17)
train= dt_list$train
test = dt_list$test



#woe-bin
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
                # ),
                min_perc_fine_bin = 0.02, min_perc_coarse_bin = 0.05, stop_limit = 0.1,
                max_num_bin = 10, positive = "bad|1", no_cores = NULL, print_step = 0L,
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


# ============================================================= glm  

model = glm(target ~ ., family = binomial(link='logit'), data = train)
class(model)

# Select a formula-based model by AIC
m_step = step(model, direction="both", trace=FALSE)
model_fin = eval(m_step$call)
model_fin

#predicted proability
dt_pred_glm = predict(model_fin, type='response', train)

#
perf_eva(train$target, dt_pred_glm, type = c("ks","lift","roc","pr"))




#============================================================== card build

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




#summary(score_train$score)

hist(score_train$score,breaks=14, xlab="score",main="Score Hist Train")









# =====================================  output
#outputcard
scorecard<-data.frame()
for( i in 1:length(card)){
  
  
  scorecard<-rbind( scorecard,card[[i]],fill=TRUE )
  
}
View(scorecard)











#============================================================== h2o model

# H2O_train = h2o.init()
# 
# dth2o_train = as.h2o(train)  
# 
# fit_train = h2o.glm(y="target", training_frame= dth2o_train ,    seed=17,
#               family="binomial", nfolds=10, alpha=0, lambda_search=TRUE  , standardize = TRUE) # summary(fit)
# 
# 
# 
# model_fit_h2o= fit_train@model
# #model_fit_h2o
# 
# 
# model_fit_coe_table= model_fit_h2o$coefficients_table
# model_fit_coe_table
# 
# 
# #
# dth2o_train_pred= predict(fit_train, type='response', dth2o_train)
# 
# 
# dt_h2o_pred_df=as.data.frame(dth2o_train_pred) 
# #dt_h2o_pred_df
# dt_h2o_pred_num=dt_h2o_pred_df$p1
# head(dt_h2o_pred_num,10)
# 
# 
# #train perf
# perf_eva(train$target, dt_h2o_pred_num, type = c("ks","lift","roc","pr"))






















