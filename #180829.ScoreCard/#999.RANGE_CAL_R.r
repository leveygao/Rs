
#range of scorecard
library(sqldf)

sqlx<-as.data.frame(scoreCard_CSV)
Score_num  <-  as.numeric(as.character(sqlx$Score) )
Score_num
sqlx<- cbind(sqlx,Score_num)

sqly<-sqldf('
         select var, max(score) as max,  min(score) as min
          from sqlx
          
          group by Var
         
         ')
sqlrange<-sqldf('
          select sum(max) as max_score , sum(min) as min_score
          from  sqly
                
                '
)
sqlrange