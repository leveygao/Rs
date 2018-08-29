
#===============================简单多元线性回归方法探究对净利润指标影响最显著的因子及其影响程度=========================================#



# install.packages('name')  安装所需要的Library
#install.packages('gvlma')

# library()  载入工具包

library(car)
library(psych)
library(gvlma)
library(caret)


#载入 csv文件
path='D:\\MyConfiguration\\gy49026\\Desktop\\insur\\InsurScore.csv'
dat=read.csv(path,   encoding='UTF-8')


#删除缺失行并存于新数据集中
data= na.omit(dat)
#查看新数据集结构
str(data)


#定量查看目标变量分位数&直方图(Y=net_profit)
summary(data$net_profit)
hist(data$net_profit, xlab="net_profit",main="直方图")



#随机抽样选取训练集测试集
data_sub=data[,-which(names(data)%in%c("X.U.FEFF.Member","Total_score"))]  #删除公司列名与总分列

set.seed(123) #设定一个随机抽样种子数，保证每次程序运行随机抽样的结果一致
total_rows=nrow(data_sub)  #计算总行数
train_flag=sample(total_rows, total_rows*0.80,  replace = F) #用总行数的N%作为训练集，余下1-N作为测试

train_set=data_sub[train_flag,]   #提取训练数据集
test_set=data_sub[-train_flag,]   #提取测试数据集



#================================== OPTIONAL 筛选变量===============================#

#----逐步回归法，获取自变量中影响最显著的指标（可略过)

#获取线性回归模型的截距
base.mod<-lm(net_profit~1,data = train_set)

#获取完整的线性回归模型
all.mod<-lm(net_profit~.,data = train_set)

#采用双向逐步回归法，筛选变量
stepMod<-step(base.mod,scope = list(lower=base.mod,upper=all.mod),
              direction = "both",trace = 0,steps = 1000)

#获取逐步回归得到的变量列表
shortlistedVars<-names(unlist(stepMod[[1]]))

#删除逐步回归的截距
shortlistedVars<-shortlistedVars[!shortlistedVars %in%"(Intercept)"]

#打印出模型&自变量
summary(stepMod)
print(shortlistedVars)

#================================ OPTIONAL 筛选变量 END=============================#




#================================ 变量相关性=============================#

#变量列表(可按格式更换/添加)
var_list=c("premium",  "combined_ratio", "cost_rate", "regulation","net_roe"
           )
  

#变量相关性(参考--手把手教线性回归分析*)
correlation=cor(train_set[var_list])
correlation

#变量相关性--图示(参考--手把手教线性回归分析*)
pairs=pairs( train_set[var_list])
pairs_panels=pairs.panels( train_set[var_list])




#================================ 建立模型 ================================#
# NOTE: 由于数据量限制，预测结果可能误差会比较大
# 用筛选出的自变量建立线性回归模型


#1--采用10折交叉验证法训练模型

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



#2--简单线性回归(无交叉验证)
fit=lm(net_profit~  premium+  combined_ratio+ cost_rate+ regulation+net_roe ,
         data=train_set)
summary(fit)


#对比结果


#============================ 模型诊断 ===================================#

#使用简单线性回归模型
#fin=fitcv$finalModel
fin=fit

#自变量的正态分布*
qqnorm(fin$residuals, main = "Q-Q Plot")


#误差的独立性
# Durbin-Watson 统计量 -- 通过确定两个相邻误差项的相关性是否为零来检验回归残差是否存在自相关*
durbinWatsonTest(fin)

#线性模型假设的综合验证***
# 查看假设是否接受/ Assumptions acceptable 
gvmodel <- gvlma(fin)
summary(gvmodel)


#自变量共线性诊断***
# VIF=自变量x 的方差膨胀因子-- 一般认为VIF不应大于5，否则表明自变量间存在多重共线性
vif(fin)
print(vif(fin) > 5)


#============================ 用模型fit在test_set(测试集)上进行预测 ====================================#

# 置信区间95%
predict=predict(fit,test_set,interval="prediction",level=0.95)

test_target=test_set$net_profit
compare_table=cbind(predict,test_target)


#在95% 置信区间下, 由var_list,中模型因子的值，预计净利润指标值test_target 会落在[lower limit , upper limit] 区间
print(compare_table)





#============================结论==========================#

#1.对净利润影响显著的因子 "premium",   "combined_ratio", "solv_ade_ratio", "total_roi"；
#2.模型满足假设条件，但是预测效果一般;





