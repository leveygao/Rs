#import sas
library(haven)
url <- "D:\\GY\\18.5.23 EDA_BT\\DATA\\flight_apr_tree.sas7bdat"
data<- read_sas(url)


# total 1
library(sqldf)
total<-sqldf('select sum(target) as bad, count(*) as total, 
             count(case when target=0 then target end) as goodcnt,
             count(case when target=1 then target end) as badcnt
             from data  ')


#tree
library(lattice)
library(ggplot2)
library(caret)
library(pROC)
library(tree)
library(zoo)
library(party)

#data1 <- data[,  c("memberid","sex")] 

name   = c("tc_memberid","jf_memberid","isOverdue")
Dlq_tag=  as.factor(data$isOverdue)
data1 = cbind(data[,!names(data) %in% name],Dlq_tag)


train<-sample(nrow(data1),size=nrow(data1)*0.7,replace = F)

training_set<-data1[train,]   #提取样本数据集
test_set<-data1[-train,]   #提取测试数据集
case_weights<-ifelse(training_set$Dlq_tag==0,1,1) #weight


#ctree 
ctree.model = ctree(Dlq_tag ~ .,data = training_set, 
                    control=ctree_control(mincriterion = 0.95, minsplit = 210, minbucket = 70) )

summary(ctree.model)
plot(ctree.model)
plot(ctree.model, labels=TRUE, terminal_panel=node_barplot(ctree.model, 
                                                          beside=TRUE,col="black", fill=c("coral4", "chartreuse4")))


#fitmodel
ctree.predict = predict(ctree.model,test_set)
table(ctree.predict,test_set$Dlq_tag)



#confusionMatrix
cf_matrix<-confusionMatrix(table(ctree.predict,test_set$Dlq_tag))
cf_matrix





#AUC

logmodel<-glm(   ~ ., family=binomial(link = "logit"),na.action=na.exclude, data = data) 
logmodel

pred_logmodeltest <- predict(logmodel, newdata = test_set, type = "response")
# Construct the objects containing ROC-information
ROC_logit <- roc(test_set$isOverdue, pred_logmodeltest)



# Draw all ROCs on one plot
plot(ROC_logit)
lines(ROC_logit, col = "blue")

# Compute the AUCs
auc(ROC_logit)


#################################################################

rule1<-sqldf('
select badcnt,  rule1_badcnt ,
       (rule1_badcnt/badcnt) as rule1_badrate,
      
        rule1_cnt, total,
       (rule1_badcnt/badcnt) as rule1_totalrate ,
       
        badcnt/total  as total_badrate,
        (badcnt-rule1_badcnt)/(total-rule1_cnt) as pass_badrate

from
(select     1.00*sum(isoverdue) as badcnt, 1.00*count(*) as total, 

             1.00*count(case when ordersuccessrate<=0.4 and fly_avgtrainprice>=800 
              and isoverdue=1 then memberid end) as rule1_badcnt,
            
             1.00*count(case when ordersuccessrate<=0.4 and fly_avgtrainprice>=800 
               then memberid end) as rule1_cnt
              

             from data)  ')
rule1