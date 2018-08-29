
#===============================�򵥶�Ԫ���Իع鷽��̽���Ծ�����ָ��Ӱ�������������Ӽ���Ӱ��̶�=========================================#



# install.packages('name')  ��װ����Ҫ��Library
#install.packages('gvlma')

# library()  ���빤�߰�

library(car)
library(psych)
library(gvlma)
library(caret)


#���� csv�ļ�
path='D:\\MyConfiguration\\gy49026\\Desktop\\insur\\InsurScore.csv'
dat=read.csv(path,   encoding='UTF-8')


#ɾ��ȱʧ�в����������ݼ���
data= na.omit(dat)
#�鿴�����ݼ��ṹ
str(data)


#�����鿴Ŀ�������λ��&ֱ��ͼ(Y=net_profit)
summary(data$net_profit)
hist(data$net_profit, xlab="net_profit",main="ֱ��ͼ")



#�������ѡȡѵ�������Լ�
data_sub=data[,-which(names(data)%in%c("X.U.FEFF.Member","Total_score"))]  #ɾ����˾�������ܷ���

set.seed(123) #�趨һ�������������������֤ÿ�γ���������������Ľ��һ��
total_rows=nrow(data_sub)  #����������
train_flag=sample(total_rows, total_rows*0.80,  replace = F) #����������N%��Ϊѵ����������1-N��Ϊ����

train_set=data_sub[train_flag,]   #��ȡѵ�����ݼ�
test_set=data_sub[-train_flag,]   #��ȡ�������ݼ�



#================================== OPTIONAL ɸѡ����===============================#

#----�𲽻ع鷨����ȡ�Ա�����Ӱ����������ָ�꣨���Թ�)

#��ȡ���Իع�ģ�͵Ľؾ�
base.mod<-lm(net_profit~1,data = train_set)

#��ȡ���������Իع�ģ��
all.mod<-lm(net_profit~.,data = train_set)

#����˫���𲽻ع鷨��ɸѡ����
stepMod<-step(base.mod,scope = list(lower=base.mod,upper=all.mod),
              direction = "both",trace = 0,steps = 1000)

#��ȡ�𲽻ع�õ��ı����б�
shortlistedVars<-names(unlist(stepMod[[1]]))

#ɾ���𲽻ع�Ľؾ�
shortlistedVars<-shortlistedVars[!shortlistedVars %in%"(Intercept)"]

#��ӡ��ģ��&�Ա���
summary(stepMod)
print(shortlistedVars)

#================================ OPTIONAL ɸѡ���� END=============================#




#================================ ���������=============================#

#�����б�(�ɰ���ʽ����/���)
var_list=c("premium",  "combined_ratio", "cost_rate", "regulation","net_roe"
           )
  

#���������(�ο�--�ְ��ֽ����Իع����*)
correlation=cor(train_set[var_list])
correlation

#���������--ͼʾ(�ο�--�ְ��ֽ����Իع����*)
pairs=pairs( train_set[var_list])
pairs_panels=pairs.panels( train_set[var_list])




#================================ ����ģ�� ================================#
# NOTE: �������������ƣ�Ԥ������������Ƚϴ�
# ��ɸѡ�����Ա����������Իع�ģ��


#1--����10�۽�����֤��ѵ��ģ��

regressControl  <- trainControl(method="repeatedcv",
                                number = 10,
                                repeats = 10
) 

fitcv <- train(net_profit ~  premium+  combined_ratio+ solv_ade_ratio+ total_roi,
                 data = train_set,
                 method  = "lm",
                 trControl = regressControl, 
                 tuneGrid  = expand.grid(intercept = TRUE))

summary(fitcv)



#2--�����Իع�(�޽�����֤)
fit=lm(net_profit~  premium+  combined_ratio+ cost_rate+ regulation+net_roe ,
         data=train_set)
summary(fit)


#�ԱȽ��


#============================ ģ����� ===================================#

#ʹ�ü����Իع�ģ��
#fin=fitcv$finalModel
fin=fit

#�Ա�������̬�ֲ�*
qqnorm(fin$residuals, main = "Q-Q Plot")


#���Ķ�����
# Durbin-Watson ͳ���� -- ͨ��ȷ����������������������Ƿ�Ϊ��������ع�в��Ƿ���������*
durbinWatsonTest(fin)

#����ģ�ͼ�����ۺ���֤***
# �鿴�����Ƿ����/ Assumptions acceptable 
gvmodel <- gvlma(fin)
summary(gvmodel)


#�Ա������������***
# VIF=�Ա���x �ķ�����������-- һ����ΪVIF��Ӧ����5����������Ա�������ڶ��ع�����
vif(fin)
print(vif(fin) > 5)


#============================ ��ģ��fit��test_set(���Լ�)�Ͻ���Ԥ�� ====================================#

# ��������95%
predict=predict(fit,test_set,interval="prediction",level=0.95)

test_target=test_set$net_profit
compare_table=cbind(predict,test_target)


#��95% ����������, ��var_list,��ģ�����ӵ�ֵ��Ԥ�ƾ�����ָ��ֵtest_target ������[lower limit , upper limit] ����
print(compare_table)





#============================����==========================#

#1.�Ծ�����Ӱ������������ "premium",   "combined_ratio", "solv_ade_ratio", "total_roi"��
#2.ģ�������������������Ԥ��Ч��һ��;





