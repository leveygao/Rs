#import sas
library(haven)
url <- "D:\\GY\\18.1.25 JF_DATA_STRATEGY\\data\\jf_apply_merchant_20180502.sas7bdat"
data<- read_sas(url)


# total 1
library(sqldf)
total<-sqldf('select sum(target) as bad, count(*) as total, 
             count(case when target?0 then target end) as goodcnt,
             count(case when target=1 then target end) as badcnt
             from data  ')


#tree
library(rpart)
library(party)
library(rpart.plot)
library(pROC)

#data1 <- data[,  c("memberid","sex")] 

name   = c("memberi?","sex","Merchant_type")
data0=data[(data$Merchant_type=='TOUR_DMS'),]
data1 = data0[,!names(data0) %in% name]


train<-sample(nrow(data1),size=nrow(data1)*0.7,replace = F)

training_set<-data1[train,]   #提取样本数据集
test_set<-data1[-train,]   #提取测试???据集
case_weights<-ifelse(training_set$target==0,1,3)

#tree  prior = c(0.7,0.3), 
tree_prior <- rpart( target ~ ., method = "class",
data = training_set,

parms = list(  split = "information"),  
weights=case_weights,
control = rpart.control(minsplit = 10?, minbucket = 100, cp = 0.001) )


plot_tris<-rpart.plot(tree_prior, branch=1 , branch.type= 1, type= 2, extra= 103,  
shadow.col="gray", box.col="green",  
border.col="blue", split.col="red",  
cex=0.65, main="Kyphosis-tree") 

plot_tris


# Use printcp()?to identify for which complexity parameter the cross-validated error rate is minimized
printcp(tree_prior)
#Create an index for of the row with the minimum xerror
index_pr <- which.min(tree_prior$cptable[, "xerror"])
# Create tree_min
tree_prior_min <- tre?_prior$cptable[index_pr, "CP"]

#  Prune the tree using tree_min
Ptree_prior <- prune(tree_prior, cp = tree_prior_min)

# Use prp() to plot the pruned tree
plot_prune<-rpart.plot(Ptree_prior, branch=1 , branch.type= 1, type= 2, extra= 103,  
              ?       shadow.col="gray", box.col="green",  
                      border.col="blue", split.col="red",  
                      cex=0.65, main="Kyphosis-tree") 
plot_prune

#==================#
#tree loss mtx


ct <- rpart.control(xval=10, minsplit=100,    ?p=0.001)  
tree_loss <- rpart(target ~ ., method = "class",
data = training_set, 
parms = list(loss = matrix(c(0, 10, 1, 0), ncol = 2)),
control = ct)

plot_loss<-rpart.pl?t(tree_loss, branch=1 , branch.type= 1, type= 2, extra= 103,  
shadow.col="gray", bo?.col="green",  
border.col="blue", split.col="red",  
cex=0.65, main="Kyphosis-tree") 


ct <- rpart.cont?ol(xval=10, minsplit=100,    cp=0.001)  
tree_loss <- rpart(target ~ ., method = "class",
data = training_set, 
parms = list(loss = matrix(c(0, 10, 1,?0), ncol = 2)),
control = ct)
?    
plot_loss<-rpart.plot(tree_loss, branch=1 , branch.type= 1, type= 2, extra= 103,  
shadow.col="gray", box.col="green",  
border.col="blue", split.col="red",  
cex=0.65, main="Kyphosis-t?ee") 

# Use printcp() to identify?for which complexity parameter the cross-validated error rate is minimized
printcp(tree_loss)
#Create an index for of the row with the minimum xerror
index <- which.min(tree_loss$cptable[, "xerror"])
# Create tree_min
tree_loss_min <- tree_loss$cptable[ind?x, "CP"]

#  Prune the tree using tree_min
Ptree_loss <- prune(tree_loss, cp =  0.01)

# Use prp() to plot the pruned tree
#plot fit2
rpart.plot(Ptree_loss, branch=1 , branch.type= 1, type= 2, extra= 103,  
sha?ow.col="gray", box.col="green",  
border.col=?blue", split.col="red",  
cex=0.65, main="Kyphosis PRUNE") 






#1


#minbucket = minsplit/3




#plot split.

plot_tris<-rpart.plot(iris_tree, branch=1 , branch.type= 1, type= 2, extra=?103,  
shadow.col="gray", box.col="green",  
border.col="blue", spl?t.col="red",  
cex=0.65, main="Kyphosis-tree") 

plot_tris
#summary
summary(iris_tree)



## 交叉验证的估计误差（“xerror???列），以及标准误差(“xstd”列)，平均相对误差=xerror±xstd  
printcp(iris_tree)



# Retreive optimal cp value based on cross-validat?d error
opt_index <- which.min(grade_model$cptable[, "xerror"])
cp_opt <- grade_model$cptable[opt_index, "CP"]

# Prune the?model (to optimized cp value)
grade_model_opt <- prune(tree = grade_model, 
cp = cp_opt)




## 我们可以用下面的办法选择具有最小xerr?r的cp的办法：  
fitcp<-prune(iris_tree, cp= iris_tree$cptable[which.min(iris_tree$cptable[,"xerror"]),"CP"])  


#cp table   
fit2<-prune(fitcp,cp=0.0121)



#rm(object)
rm( )






