library(Hmisc) # Hmisc describe
#用已开发好的模型对测试集中所有样本重新评级

tmp1<-test_kfolddata[,c(1:23,25:27)]
credit_risk1<-ifelse(test_kfolddata[,"target"]==0,0,1)                     
data_tmp<-as.matrix(cbind(tmp1,credit_risk1))
 
###用R代码实现打分卡模型###

data1<-as.data.frame(data_tmp)
tot<-nrow(data1)
score<-list()

for(i in 1:tot)
{
  lst<-as.matrix(data1[i,])
  
  #FlyDmsCancel_D800_pct
  score_FlyDmsCancel_D800_pct<-0
  if(lst[,"FlyDmsCancel_D800_pct"]<=0.2813)
  {
    score_FlyDmsCancel_D800_pct<- 40
  }else
    if(lst[,"FlyDmsCancel_D800_pct"]>0.54&lst[,"FlyDmsCancel_D800_pct"]<=0.54)
    {
      score_FlyDmsCancel_D800_pct<- 12
    }else
      if(lst[,"FlyDmsCancel_D800_pct"]>0.54)
      {
        score_FlyDmsCancel_D800_pct<- -7
      }
  
  
  #TrnPersons_D150_avg
  score_TrnPersons_D150_avg<-0
  if(lst[,"TrnPersons_D150_avg"]<=0.75)
  {
    score_TrnPersons_D150_avg<- -3
  }else
    if(lst[,"TrnPersons_D150_avg"]>0.75)
    {
      score_TrnPersons_D150_avg<- 4
    }
  
  #age
  score_age<-0
  if(lst[,"age"]<=25)
  {
    score_age<- -7
  }else
    if(lst[,"age"]>25 & lst[,"age"]<=35)
    {
      score_age<- 2
    }
  else
    if(lst[,"age"]>35)
    {
      score_age<- 15
    }
  
  #TrnOrderBegin_D300_avg
  score_TrnOrderBegin_D300_avg<-0
  if(lst[,"TrnOrderBegin_D300_avg"]<=  0.2543 )
  {
    score_TrnOrderBegin_D300_avg<- -10
  }else
    if(lst[,"TrnOrderBegin_D300_avg"]> 0.2543 & lst[,"TrnOrderBegin_D300_avg"]<= 1.1448)
    {
      score_TrnOrderBegin_D300_avg<- 1
    }else
      if(lst[,"TrnOrderBegin_D300_avg"]> 1.1448 & lst[,"TrnOrderBegin_D300_avg"]<= 8.762 )
      {
        score_TrnOrderBegin_D300_avg<- 10
      }else
        if(lst[,"TrnOrderBegin_D300_avg"]>8.762)
        {
          score_TrnOrderBegin_D300_avg<- 30
        }
  
  #TrnAmount_D150_avg
  score_TrnAmount_D150_avg<- 0
  if(lst[,"TrnAmount_D150_avg"]>9.75)
  {
    score_TrnAmount_D150_avg<- -1
  }else
    if(lst[,"TrnAmount_D150_avg"]<=9.75)
    {
      score_TrnAmount_D150_avg<- 1
    }
  
  #FlyDmsEnd_D500_pct
  score_FlyDmsEnd_D500_pct<- 0
  if(lst[,"FlyDmsEnd_D500_pct"]<=0.44)
  {
    score_FlyDmsEnd_D500_pct<-  -2
  }else
    if(lst[,"FlyDmsEnd_D500_pct"]>0.44)
    {
      score_FlyDmsEnd_D500_pct<- 13
    }
  
  
  #sex
  score_sex<- 0
  if(lst[,"sex"]=="M")
  {
    score_sex<- -4
  }else
    if(lst[,"sex"]=="F")
    {
      score_sex<-  14
    }
  
  #TC_MLV
  score_TC_MLV<- 0
  if(lst[,"TC_MLV"]=="0")
  {
    score_TC_MLV<- -4
  }else
    if(lst[,"TC_MLV"]=="1")
    {
      score_TC_MLV<- 0
    }else
      if(lst[,"TC_MLV"]=="2")
      {
        score_TC_MLV<- 5
      }else
        if(lst[,"TC_MLV"]=="3")
        {
          score_TC_MLV<- 21
        }
        else
          if(lst[,"TC_MLV"]=="4")
          {
            score_TC_MLV<- 19
          }
  
  #maxplat7m
  score_maxplat7m<-NA
  if(lst[,"maxplat7m"]=="undefined")
  {
    score_maxplat7m<- -10
  }else
    if(lst[,"maxplat7m"]=="APP")
    {
      score_maxplat7m<- 12
    }else
      if(lst[,"maxplat7m"]=="Touch")
      {
        score_maxplat7m<- 9
      }else
        if(lst[,"maxplat7m"]=="PC")
        {
          score_maxplat7m<- -12
        }
  
  #basepoint
  score[i]<-sum(basepoint,score_FlyDmsCancel_D800_pct,score_TrnPersons_D150_avg,score_age,score_TrnOrderBegin_D300_avg,  
                score_TrnAmount_D150_avg,score_FlyDmsEnd_D500_pct,score_sex,
                score_TC_MLV,score_maxplat7m)
  rm(lst)
}


###用R代码实现打分卡模型结束###
#合并处理测试集样本得分，并输出到指定的CSV文件中#
score_M<-as.matrix(score,ncol=3)

score_data<-cbind(data1,score_M)


#score_num<-as.numeric(score_risk$score_M)
#score_label<-as.numeric(score_risk$credit_risk1)-1

#score_final<- cbind(score_risk,score_num)[,c(1,3)]
#score_final<- as.data.frame(cbind(score_num,score_label))