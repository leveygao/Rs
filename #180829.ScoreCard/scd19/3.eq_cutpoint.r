#equal cutpoint ----

ptm <- proc.time()

DATA= score_train_plt

# perf_eva(DATA$FLAG, DATA$score, type = c("ks","lift","roc","pr"), show_plot=FALSE)





# eq freq
TILE=0.1
num= (1/TILE)+1
  
b_points = quantile( DATA$score, seq(0,1,TILE))
b_points

k=1
Group_eqfreq=1
 
# score eq

max=max(DATA$score)
min=min(DATA$score)
interval= (max-min)/10
 
cut_point=NULL
for(i in 1:num){
  cut_point[i]= min+interval*(i-1)
}

m=1
Group_eqScore=1



# sort
DATA=DATA[order(DATA$score),,drop=FALSE]
head(DATA)




for (i in 1: nrow(DATA) ){
  if( k<= num +1 ){
    if( DATA[i,"score"]<=b_points[k+1])
    {
      DATA[i,"Group_eqfreq"]= Group_eqfreq
    }
    else 
    {
      DATA[i,"Group_eqfreq"]=Group_eqfreq+1
      
      Group_eqfreq=Group_eqfreq+1
      
      k=k+1
    }
  }
  if(m<= num +1){
    if( DATA[i,"score"]<=cut_point[m+1])
    {
      DATA[i,"Group_eqScore"]= Group_eqScore
    }
    else 
    {
      DATA[i,"Group_eqScore"]=Group_eqScore+1
      
      Group_eqScore=Group_eqScore+1
      
      m=m+1
    }
  }
  
}

# time rec
proc.time() - ptm



library('sqldf')

eqfreq_sum=sqldf::sqldf( 'select  min(score) as lower, max(score) as upper ,Group_eqfreq,
                         count(case when FLAG_train==0 then score end ) as good ,
                         count(case when FLAG_train==1 then score end ) as bad
                         from DATA 
                         group by Group_eqfreq ')
eqfreq_sum

eqScore_sum=sqldf::sqldf( 'select  min(score) as lower, max(score) as upper ,Group_eqScore,
                          count(case when FLAG_train==0 then score end ) as good ,
                          count(case when FLAG_train==1 then score end ) as bad
                          from DATA 
                          group by Group_eqScore ')
eqScore_sum



