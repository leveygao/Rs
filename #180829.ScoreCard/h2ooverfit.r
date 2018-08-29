library(h2o)
library(data.table)
library(scorecard)
library(rlang)


#devtools::install_github("shichenxie/scorecard")


localH2O = h2o.init()


card_fin<-read.csv("D:\\MyConfiguration\\gy49026\\Desktop\\Python\\model\\TC_MODEL07.csv")


#filter-var
filter <- var_filter(card_fin, y = "target", x = NULL, iv_limit = 0.2, missing_limit = 0.95,
                     identical_limit = 0.95, var_rm = NULL, var_kp = NULL,
                     return_rm_reason = TRUE, positive = "bad|1")  
filter_dt=filter$dt
ivtable=iv(filter_dt, y = "target")
ivtable




filter_dt_bin<-woebin(filter_dt, y="target", 
                      #x = c("age","duration"), 
                      # breaks_list =  list(
                      #   
                      #   TC_MLV= c("0","1","2" ,"3","4")
                      #   
                      # ),
                      # special_values = list( 
                      #     duration=c(14,28,52)
                      # 
                      # ),
                      min_perc_fine_bin = 0.02, min_perc_coarse_bin = 0.05, stop_limit = 0.1,
                      max_num_bin = 10, positive = "bad|1", no_cores = NULL, print_step = 0L,
                      method = "tree") #or chimerge

filter_dtwoe = woebin_ply(filter_dt, filter_dt_bin)


dth2o = as.h2o(filter_dtwoe)
# h2o.glm lasso 
fit = h2o.glm(y="target", training_frame=dth2o,  seed=17,
              family="binomial", nfolds=2, alpha=1, lambda_search=TRUE) # summary(fit)


# variable importance

varimp = data.table(h2o.varimp(fit))[names!=""][!is.na(coefficients) & coefficients > 0.1 & sign == "POS"]
varimp
var_sel3 = c(varimp$names, "target")
var_h2o =as.list(var_sel3)

#===========================================================#
# 
# 
# #var select
# droplist=c("target","sex")
# drop_var=card_fin[,!colnames(card_fin)%in% droplist]
#   
# 
# target=as.factor(card_fin$target)
# gender=as.factor(ifelse(card_fin$sex=='M',1,0))
# data=cbind(drop_var ,target,gender)
# 
# 
# 
# #sample
# dt_list = split_df(data, ratio=0.7, seed=17)
# train= dt_list$train
# test = dt_list$test
# 
# 
# 
# #woe-bin
# woe_bin<-woebin(train, y="target", 
#                 #x = c("age","duration"), 
#                 # breaks_list =  list(
#                 #   
#                 #   TC_MLV= c("0","1","2" ,"3","4")
#                 #   
#                 # ),
#                 # special_values = list( 
#                 #     duration=c(14,28,52)
#                 # 
#                 # ),
#                 min_perc_fine_bin = 0.02, min_perc_coarse_bin = 0.05, stop_limit = 0.1,
#                 max_num_bin = 10, positive = "bad|1", no_cores = NULL, print_step = 0L,
#                 method = "tree") #or chimerge
# 
# 
# #woe-apply
# dt_woe = woebin_ply(train, woe_bin)
# head(dt_woe)




















# keep_var<-card_fin[,
#                    c( "recharge_amount_6month",
#                       "msg_count_6month", 
#                       "call_time_late_night_3month",  
#                       "mobile_net_age",
#                       "call_count_call_time_1min5min_6m",
#                       "call_count_active_3month",
#                       "call_count_passive_3month",
#                       "call_count_offwork_time_6month",
#                       "call_count_holiday_6month",
#                       "call_count_work_time_3month",
#                       "creditscore_top10_contact_avg", 
#                       
#                       
#                       "consume_amount_1month",
#                       "call_count_1month",
#                       "contact_count_passive_6month",
#                       
#                       
#                        "call_count_workday_6month",
#                        "call_count_call_time_less1min_6m" ,
#                       "call_count_late_night_6month",
#                       "contact_count_3month",
#                        "call_count_passive_6month",
#                        "call_count_work_time_6month",
#                        "call_count_active_6month",
#                       "call_count_passive_6month",   "call_count_work_time_3month",  "call_count_holiday_6month",
#                       "call_count_1month" ,   "call_count_offwork_time_3month",  
#                       "contact_count_passive_6month",  "contact_count_active_6month",
#                       
#                       
#                       "useflow_3month" ,
#                       "id_small_loan" ,    
#                       "all_p2p",    
#                       "mb_counts",  
#                       "id_bank_online",
#                       "platform_counts",
#                       "mb_installemnt_platform",
#                       
#                       "age",
#                       
#                       "all_fin_consume",      
#                       "id_counts",   "id_p2p",    "mb_p2p",  "mb_small_loan","all_small_loan"
#                       
#                       
#                    )]
W