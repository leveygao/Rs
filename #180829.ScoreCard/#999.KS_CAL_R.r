# KS-CAL && PLOT
KS_FUNc <- function(DATA,DATA_XVAR,COL,TILE){
  b_points <- quantile( DATA_XVAR, seq(0,1,TILE))
  
  C_P <- NULL
  C_N <- NULL
  num<- (1/TILE)+1
  
  for (i in 1: num ){
    index <- ( DATA_XVAR<=b_points[i])
    
    C_P[i] <- sum( DATA[index,COL] ==1)/sum(DATA[,COL]==1)
    C_N[i] <- sum( DATA[index,COL] ==0)/sum(DATA[,COL]==0)
  }
  
 
  C_TABLE<-as.data.frame(cbind(C_P,C_N))
  #C_TABLE
  KS_COL<-cbind(b_points,C_TABLE)
  
  
  
  #plot
  plot(KS_COL$b_points,  C_P, type = "l",col= "green",xlab="threshold",ylab="KS")  
  lines(KS_COL$b_points, C_N, type="l", col = "red" )  
  KSvalue <- max(abs(C_N - C_P))
  
  lines(KS_COL$b_points, abs(C_N-C_P), type="l", col = "blue")  
  sub = paste("KS value =",round(KSvalue,5))  
  title(sub=sub)  
  
  hist(DATA_XVAR,    right=FALSE)    # intervals closed on the left
  
  
  return(KS_COL)
}


#KS_FUNc(DATA, DATA$X, COL,  TILE)
#which( colnames(   )==" " )

#Cal col_num of y
COL_NUMBER<-which( colnames(   )==" " )
COL<-COL_NUMBER

#run func
table<-KS_FUNc( ,  $ , COL,  0.1)
table






#> hist(duration,    # apply the hist function 
#+   right=FALSE,    # intervals closed on the left 
#+   col=colors,     # set the color palette 
#+   main="Old Faithful Eruptions", # the main title 
#+   xlab="Duration minutes")       # x-axis label




