#Unsupervised learning 
#KMEANS HCLUST PCA



#library ----
library(datasets)



#load data

data(iris)
summary(iris)

data=iris[,-5]
Species=iris$Species


#  fnPCA ----

# Check column means and standard deviations
colMeans(data)
apply(data,2,sd)

# Execute PCA, scaling if appropriate: wisc.pr
iris.pr=prcomp(data,scale=TRUE , center=TRUE)

# Look at summary of results
summary(iris.pr)
iris.pr$sdev



# Create a biplot of wisc.pr
biplot(iris.pr)

# Scatter plot observations by components 1 and 2
plot(iris.pr$x[, c(1, 2)], col = (Species), 
     xlab = "PC1", ylab = "PC2")

# Repeat for components 1 and 3
plot(iris.pr$x[, c(1, 3)], col = (Species), 
     xlab = "PC1", ylab = "PC3")


# Repeat for components 1 and 4
plot(iris.pr$x[, c(1, 4)], col = (Species), 
     xlab = "PC1", ylab = "PC3")




# variance explained ----

# Set up 1 x 2 plotting grid
par(mfrow = c(1, 2))

# Calculate variability of each component
pr.var= (iris.pr$sdev)^2

# Variance explained by each principal component: pve
pve= pr.var /sum(pr.var)

sum(pve[1:2])



# Plot variance explained for each principal component
plot(pve , xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained", 
     ylim = c(0, 1), type = "b")

# Plot cumulative proportion of variance explained
plot(cumsum(pve), xlab = "Principal Component", 
     ylab = "Cumulative Proportion of Variance Explained", 
     ylim = c(0, 1), type = "b")


#Hierarchical clustering ----

# Scale the  data data: data.scaled
data.scaled=scale(data)

# Calculate the (Euclidean) distances: data.dist
data.dist=dist(data.scaled)

# Create a hierarchical clustering model:  hclust
# method in comlete / single / average 
data.hclust=hclust(data.dist, method="complete")

summary(data.hclust)



# Cut tree so that it has N clusters:  
hclust.clusters=cutree(data.hclust ,k=3)

# Compare cluster membership to actual diagnoses
table(hclust.clusters, Species)



# kmeans ----

# Create a k-means model on wisc.data:  
iris.km= kmeans(scale(data),centers=3, nstart=20 )

# Compare k-means to actual diagnoses
table(iris.km$cluster , Species)

# Compare k-means to hierarchical clustering
table( hclust.clusters , iris.km$cluster)




# Create a hierarchical clustering model ---- 

# % VAR explaint
sum((iris.pr$sdev[1:3])^2)/sum((iris.pr$sdev)^2)

# hclust
iris.pr.hclust <- hclust(dist(iris.pr$x[, 1:3]), method = "complete")


# Cut model into N clusters: wisc.pr.hclust.clusters
iris.pr.hclust.clusters=  cutree(iris.pr.hclust ,   k=3)
length(iris.pr.hclust.clusters)


# Compare to actual  
table(Species, iris.pr.hclust.clusters)

# Compare to k-means and hierarchical
table(Species, hclust.clusters)
table(Species, iris.km$cluster)













