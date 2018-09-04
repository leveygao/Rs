library(scorecard)
library(data.table)
library(car)
library(corrplot)
library(clValid)
#library(cluster.datasets)


#load data
german=germancredit
str(german)

keep= c(  "age.in.years" ,"credit.amount"
         #,   , "duration.in.month" , "present.residence.since",      "number.of.existing.credits.at.this.bank"
         )

target=as.factor(german$creditability)

subdata= subset(german,  select = keep)
#data=cbind(subdata,target)



# check corr
data_cor <- cor(subdata)
scatterplotMatrix(subdata)   

corrplot(corr = data_cor, method = 'color', addCoef.col="grey") 



#normlize

min.max.norm <- function(x){
  ((x-min(x))/(max(x)-min(x)))
}

data_norm = as.data.frame(apply(subdata,2,min.max.norm))


#----
# cluster


# Initialise ratio_ss 
ratio_ss=rep(0,7)

# Finish the for-loop. 
for (k in 1:7) {
   
  # Apply k-means to data_norm: school_km
  data_km=kmeans(data_norm, centers=k, nstart=20)
  
  # Save the ratio between of WSS to TSS in kth element of ratio_ss
  ratio_ss[k]=     data_km$tot.withinss / data_km$totss
  
}


# Make a scree plot with type "b" and xlab "k"
plot(ratio_ss, type="b", xlab="k")
ratio_ss


#final k 
kvalue=4

data_km=kmeans(data_norm, centers=  kvalue , nstart=20)

plot(data_norm$credit.amount,  data_norm$age.in.years  , xlab = "amt", ylab = "age", col = data_km$cluster)
plot(subdata$credit.amount,  subdata$age.in.years  , xlab = "amt", ylab = "age", col = data_km$cluster)


#----

#Hierarchical Clustering

# Apply dist() to run_record_sc: run_dist
run_dist <- dist(data_norm)

# Apply hclust() to run_dist: run_single
run_single <- hclust(run_dist, method = "single")

# Apply cutree() to run_single: memb_single
memb_single <- cutree(run_single, k = kvalue)

# Apply plot() on run_single to draw the dendrogram
plot(run_single)

# Apply rect.hclust() on run_single to draw the boxes
rect.hclust(run_single, k = kvalue, border = 2:6)





# Code for single-linkage

run_dists <- dist(data_norm, method = "euclidean")
run_singles <- hclust(run_dists, method = "single")
memb_singles <- cutree(run_singles,   kvalue)
plot(run_singles)
rect.hclust(run_singles, k = kvalue , border = 2:6)


# Apply hclust() to run_dist: run_complete
run_complete= hclust(run_dists, method="complete")

# Apply cutree() to run_complete: memb_complete

memb_complete=cutree(run_complete,k=  kvalue)
# Apply plot() on run_complete to draw the dendrogram
plot(run_complete)

# Apply rect.hclust() on run_complete to draw the boxes
rect.hclust(run_complete, k =  kvalue , border = 2:6)

# table() the clusters memb_single and memb_complete. Put memb_single in the rows
table(memb_singles, memb_complete)





#----
#compare single vs complete


# Set random seed. Don't remove this line.
set.seed(100)

# Dunn's index for k-means: dunn_km
dunn_km <- dunn(clusters = data_km$cluster, Data = data_norm)
dunn_km
# Dunn's index for single-linkage: dunn_single
dunn_single <- dunn(clusters = memb_singles, Data = data_norm)
dunn_single
# Dunn's index for complete-linkage: dunn_complete
dunn_complete <- dunn(clusters = memb_complete, Data = data_norm)
dunn_complete

# Compare k-means with single-linkage
table(data_km$cluster, memb_singles)

# Compare k-means with complete-linkage
table(data_km$cluster, memb_complete)






# Kmeans
# data����ʾ�����������
# k��Ҫ�������Ŀ
# max.iter�������������������Ĭ��ֵ��10
# cluster������Ľ����Ҳ����ÿ��������������
# tot.withinss��������ܵ�����ƽ���ͣ������������ڲ���
# betweenss����������ƽ���ͣ���������������

# My_kmeans <- function(data,k,max.iter){
#   changed=0
#   rows <- nrow(data) 
#   cols <- ncol(data) 
#   
#   within <- matrix(0,nrow=k,ncol=1) 
#   between <- 0
#   iter = 0
#   
#     
#   #����indexMatrix����,��һ��Ϊÿ���������ڵ��࣬�ڶ���Ϊÿ�����ݵ��������ĵľ���
#   indexMatrix <- matrix(0,nrow=rows,ncol=2) 
#   
#   centers <- matrix(0,nrow=k,ncol=cols) 
#   randSeveralInteger <- as.vector(sample(1:rows,size=k))
#   #ͨ������������ķ�ʽ���õ���ʼ�ľ�������
#   for(i in 1:k){
#     indexMatrix[randSeveralInteger[i],1] <- i
#     centers[i,] <- data[randSeveralInteger[i],]
#     centers <- matrix(centers,k,cols)
#   }
#   changed = 1 
#   
#   while(changed){ 
#     
#     if(iter >= max.iter)
#       break
#     
#     changed=0
#     
#     #��ÿһ�����ݣ������䵽���������ĵľ��룬�����仮�ֵ������������
#     for(i in 1:rows){ 
#       initialDistance <- 10000 
#       previousCluster <- indexMatrix[i,1]
#       
#       #�������е��࣬�������ݻ��ֵ������������
#       for(j in 1:k){ 
#         currentDistance <- (sum((data[i,]-centers[j,])^2))^0.5
#         if(currentDistance < initialDistance){
#           initialDistance <- currentDistance 
#           indexMatrix[i,1] <- j 
#           indexMatrix[i,2] <- currentDistance 
#         } 
#       }
#       
#       #����������������෢���˱仯����changed��ΪTRUE���㷨����
#       if(previousCluster!=indexMatrix[i,1]) 
#         changed=1
#     }
#     
#     #���¼���������
#     for(m in 1:k){
#       clusterMatrix <- data[indexMatrix[,1]==m,] 
#       clusterMatrix <- as.matrix(clusterMatrix)
#       if(nrow(clusterMatrix)>0){ 
#         centers[m,] <- colMeans(clusterMatrix) 
#       } 
#       else{
#         centers[m,] <- centers[m,] 
#       }    
#     }
#     iter = (iter+1)
#   }
  
  
















