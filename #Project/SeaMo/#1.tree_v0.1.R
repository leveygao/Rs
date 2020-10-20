# util
# install.packages('xxxxx')

library(dplyr)
library(scorecard)
library(caret)

#tree
library(rpart)
library(party)
library(rpart.plot)
library(pROC)


#import local data
 
url <- "/Users/chandler.gaoyuan/Desktop/Docu/Workdoc/2020.9/Tree_09/data/low_order_user_raw_data.csv"
raw<- read.csv(url)


## tag add ----

raw$Label=  ifelse(raw$DPD_cat=='M1+',1 , ifelse(raw$DPD_cat=='DPD <=7',0, -1))
raw$Score_level =  ifelse(raw$ascore_cat=='737+', '737+' , '737_below')

# check
table(raw$Label, raw$Score_level )


## subset data with criteria 1&2&3 etc. ----
data=  dplyr::filter(.data=raw, 
                     Score_level=='737_below', 
                     confirmed_order_in_l12m %in% c('1. 0-5 Orders L12M','2. 6-10 Orders L12M','3. 11-20 Orders L12M'),
                     confirmed_order_in_l6m == '2. >=1 Orders L6M')


print(dim(data))


# check
table(data$confirmed_order_in_l6m)


## subset data by excluding calss=-1  keep only binary response for tree analysis ----
data=subset(data,Label!=-1)

# check
table(data$Label)



## vars selection ------------------------------------ 

## split train-test
set.seed(666)

train<-sample(nrow(data),size=nrow(data)*0.7,replace = F)

training_set<-data[train,]   
test_set<-data[-train,]    



## varfilter reference function "scorecard::var_filter"----

 
filter = scorecard::var_filter(training_set, y = "Label", x = NULL, iv_limit = 0.2, missing_limit = 0.95,
                     identical_limit = 0.95, var_rm = NULL, var_kp = NULL,
                    return_rm_reason = TRUE, positive = "bad|1")
str(filter$dt)
ivtable= iv(filter$dt, y = "Label")

## get a varlist and output tocsv ----
# write.csv(ivtable,"/Users/chandler.gaoyuan/Desktop/Docu/Workdoc/2020.9/Tree_09/data/iv_info_raw.csv")





## woe-bin reference function "scorecard::woebin" ----
woe_bin= scorecard::woebin(training_set, y="Label", 
                x=NULL,  #!!!   you can input a list of vars like : c("var1","var2,"var3")
                
                min_perc_fine_bin = 0.02,   min_perc_coarse_bin = 0.05,  stop_limit = 0.1,
                max_num_bin =  5 , positive = "bad|1", no_cores = NULL,  print_step = 0L,
                method = "freq") ##or   width // tree// chimerge


## print(woe_bin)

##outputcard as df
woedf = data.frame()

for( i in 1:length(woe_bin)){
  
  woedf<-rbind( woedf,  woe_bin[[i]],fill=TRUE )
  
}


## tocsv ----
# write.csv(woedf,"/Users/chandler.gaoyuan/Desktop/Docu/Workdoc/2020.9/Tree_09/data/woe_bin.csv")




#--------------------------tree----------------------------


## set weight for balancing data (optional) ----
badrate_train = table(training_set$Label)[2]/dim(training_set)[1]
case_weights = ifelse(training_set$Label==0, round(badrate_train,2), round(1-badrate_train,2))



## tree fit



## adjust hyperparam : prior(class weight) / split /minsplit /complexity parameter (cp) etc.

tree_prior = rpart(Label ~ 
                  
                   + checkout_win_in_90d 
                 
                   + checkout_density_in_90d 
                   + checkout_in_90d 
                 
                   + pctg_order_use_voucher_in_90d 
                   + pctg_order_use_coin_in_90d
                   + pctg_order_use_net_coin_in_90d
                   
                    
                    ,
                   
                    method = "class",
                    data = training_set, 
                    
                    parms = list(   
                      prior = c(  0.35 ,  0.65 )  ,
                      split = "gini"),  
                   
                    # weights=case_weights,
                   
                    control = rpart.control(minsplit = 600, minbucket = 500/3,  cp =1e-04, maxdepth= 4)
                     )


plot_tris<-rpart.plot(tree_prior, branch= 1 ,  type= 4,  extra= 106, 
                      shadow.col="grey", box.col="green",
                      border.col="blue", split.col="red",
                      under=TRUE,
                      cex=0.65, main="tree") 

plot_tris

 
## prune the tree above ----


# Use printcp() to identify for which complexity parameter the cross-validated error rate is minimized
printcp(tree_prior)
#Create an index for of the row with the minimum xerror
index_pr <- which.min(tree_prior$cptable[, "xerror"])
# Create tree_min
tree_prior_min <- tree_prior$cptable[index_pr, "CP"]

#  Prune the tree using tree_min
Ptree_prior <- prune(tree_prior, cp = tree_prior_min)


# Use prp() to plot the pruned tree
plot_prune<-rpart.plot(Ptree_prior, branch=1 , type= 4,  extra= 2,  # branch.type= 1,
                       shadow.col="gray", box.col="green",  
                       border.col="blue", split.col="red",  
                       cex=0.65, main="tree") 


## output (same)tree type-A for review  

##  positive /total in each node
plot_prune



plot_prune1<-rpart.plot(Ptree_prior, branch=1 , type= 4,  extra= 100 ,
                       shadow.col="gray", box.col="green",
                       border.col="blue", split.col="red",
                       cex=0.65, main="tree")


## output (same)tree  type-B for review

## percent of obs
plot_prune1



#---------------------- stats output of tree ---------------------

summary(Ptree_prior)

#!!!  print rules for each node
# rpart.rules(tree_prior, cover = TRUE, clip.facs = TRUE , extra=9)

# print(rpart.rules(Ptree_prior, cover = TRUE, clip.facs = TRUE , extra=9))


setrules=rpart.rules(tree_prior, cover = TRUE, clip.facs = TRUE , extra=9)


rules_set=rpart.rules(Ptree_prior, cover = TRUE, clip.facs = TRUE , extra=9)




#---------------------- apply rules from tree and analyse stats  --------------


## RULE TEST ----


## define a function : calculate group rate under each rule's effect, may add own column ----
Rule_stats=function( data  , label , rulename_char, confusion_matrix , ruledetail=NULL  ){
  
  df_rule = data.frame( Rule =  as.character(rulename_char) ,
                        Rule_desc =  as.character(ruledetail) ,
                        
                        Sample_Size = dim(data)[1] ,
                        Pass_Size =  confusion_matrix$table[1]+confusion_matrix$table[3]  ,
                        Rej_Size =    confusion_matrix$table[2]+confusion_matrix$table[4]   ,
                        
                        Pass_pct = (confusion_matrix$table[1]+confusion_matrix$table[3] )/ dim(data)[1] ,
                        Rej_pct = ( confusion_matrix$table[2]+confusion_matrix$table[4] )/ dim(data)[1] ,
                        
                        Overall_badrate =  table(data[label])[2]/dim(data)[1]   ,
                        Pass_badrate =  confusion_matrix$table[3] / (confusion_matrix$table[1]+confusion_matrix$table[3] )  ,
                        Rej_badrate =  confusion_matrix$table[4] / (confusion_matrix$table[2]+confusion_matrix$table[4] )  ,
                        
                        Lift_Rejbad =  (confusion_matrix$table[4] / (confusion_matrix$table[2]+confusion_matrix$table[4] )) 
                                        /  ( table(data[label])[2]/dim(data)[1] )
                        
                
                        
                        
                        
  )
  return (df_rule)                      
} 


###  use rules print out from "print(rpart.rules(Ptree_prior, cover = TRUE, clip.facs = TRUE , extra=9))" ----

training_set[is.na(training_set)] = -1




## rule1


rule_1=ifelse( 
  ( training_set$abs_in_90d <=3 & (training_set$login_day_in_90d < 40)  )==TRUE,
  1,0)

table(  rule_1 )

 
cmx1 = confusionMatrix(data = as.factor(  rule_1 ) , reference= as.factor(training_set$Label ),
                       positive = '1',mode='everything' )
cmx1


df1 =  Rule_stats( data = training_set , label= 'Label' , rulename_char = 'rule-1', confusion_matrix= cmx1 
                   ,ruledetail='abs_in_90d<=3 & login_day_in_90d < 40' )
df1  





  
## rule2
rule_2=ifelse( 
  (training_set$checkout_win_in_90d <  31  & training_set$pctg_order_use_coin_in_90d >0.37
   & training_set$pctg_order_use_voucher_in_90d <  0.26  
  ),
  1,0)

table( rule_2, training_set$Label)

cmx2 = confusionMatrix(data = as.factor( rule_2 ) , reference= as.factor(training_set$Label ),
                       positive = '1',mode='everything' )
cmx2


df2 =  Rule_stats( data = training_set , label= 'Label' , rulename_char = 'rule-2', confusion_matrix= cmx2 
                   ,ruledetail='checkout_win_in_90d <  31  &  pctg_order_use_coin_in_90d >0.37  & pctg_order_use_voucher_in_90d <  0.26 ' )
df2  





## rule3     

rule_3=ifelse( 
  ( training_set$checkout_in_90d >=4 & training_set$checkout_win_in_90d < 42
    & training_set$pctg_order_use_voucher_in_90d <  0.14 & training_set$pctg_order_use_coin_in_90d <  0.087          
  ),
  1,0)

table( rule_3, training_set$Label)

cmx3 = confusionMatrix(data = as.factor( rule_3 ) , reference= as.factor(training_set$Label ),
                       positive = '1',mode='everything' )
cmx3


df3 =  Rule_stats( data = training_set , label= 'Label' , rulename_char = 'rule-3', confusion_matrix= cmx3 
                   ,ruledetail='checkout_in_90d >=4 & checkout_win_in_90d < 42 & pctg_order_use_voucher_in_90d <  0.14 & pctg_order_use_coin_in_90d <  0.087' )
df3 


## all


rule_4=ifelse( 
  ( 
    training_set$abs_in_90d <=3 & (training_set$login_day_in_90d )< 40
    &
    training_set$checkout_win_in_90d <  31  & training_set$pctg_order_use_coin_in_90d >0.37
    & training_set$pctg_order_use_voucher_in_90d <  0.26
    # &
    # training_set$checkout_in_90d >=4 & training_set$checkout_win_in_90d < 42
    # & training_set$pctg_order_use_voucher_in_90d <  0.14 & training_set$pctg_order_use_coin_in_90d <  0.087
  ),
  1,0)

table( rule_4, training_set$Label)

cmx4 = confusionMatrix(data = as.factor( rule_4 ) , reference= as.factor(training_set$Label ),
                       positive = '1',mode='everything' )
cmx4


df4 =  Rule_stats( data = training_set , label= 'Label' , rulename_char = 'rule-all', confusion_matrix= cmx4
                   ,ruledetail='rule1+2' )
df4





## concat dataframe ----

df_analysis = as.data.frame(rbind(df1 , df2 , df3, df4))

# write.csv(df_analysis,"/Users/chandler.gaoyuan/Desktop/Docu/Workdoc/2020.9/Tree_09/data/df_analysis.csv")







## rm(object)
rm(df_analysis )
