
alpha_beta<-function(basepoints,baseodds,pdo)
{
  beta<-pdo/log(2)
  alpha<-basepoints+beta*log(baseodds)
  return(list(alpha=alpha,beta=beta))
}
#coefficients<-m$coefficients


#ͨ��ָ���ض����ʣ�1/20�����ض���ֵ��50���ͱ��ʷ����ķ�����10�������������ֿ���ϵ��alpha��beta

basepointsset<-600
baseoddsset<-1/20
pdo<-20

x<-alpha_beta(basepointsset,   baseoddsset,  pdo )

#���������ֵ
#basepoint<-round(x$alpha-x$beta*coefficients[1])
basepoints=600
beta=20/log(2)
baseodds=1/20
a<-basepoints+beta*log(baseodds)