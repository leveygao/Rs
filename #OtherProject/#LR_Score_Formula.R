
alpha_beta<-function(basepoints,baseodds,pdo)
{
  beta<-pdo/log(2)
  alpha<-basepoints+beta*log(baseodds)
  return(list(alpha=alpha,beta=beta))
}
#coefficients<-m$coefficients


#通过指定特定比率（1/20）的特定分值（50）和比率翻番的分数（10），来计算评分卡的系数alpha和beta

basepointsset<-600
baseoddsset<-1/20
pdo<-20

x<-alpha_beta(basepointsset,   baseoddsset,  pdo )

#计算基础分值
#basepoint<-round(x$alpha-x$beta*coefficients[1])
basepoints=600
beta=20/log(2)
baseodds=1/20
a<-basepoints+beta*log(baseodds)
