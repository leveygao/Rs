library(data.table)
library(quantmod)
d <- data.table(v1=c(rep('a', 10), rep('b', 10)), v2=1:20)



# lag

setkeyv(d, 'v1')
d[,new_var := Lag(v2, 1), by='v1']

d[,new_var1 := Lag(v2, 3), by='v1']

d[,new_var2 := v2-Lag(v2, 3), by='v1']

d[,new_var3 := Next(v2, 1), by='v1']

d[,new_var4 := Next(v2, 2), by='v1']

View(d)


d2=  data.table(v1=c(rep('a', 10),  rep('b', 10)), v2=c(rep('c', 10),  rep('d', 10))  ,  v3=1:20)

# cum sum 

df<-data.frame(group=c(rep("a",3),rep("b",4),rep("c",2)))

df<-transform(df, a=as.integer(group))


csum<-function(x){return(cumsum(x))}

df['d']=unlist(tapply(df$a,df$group,csum))

df<-transform(df,b=unlist(tapply(a,group,csum)))
