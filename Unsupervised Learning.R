
data <- read.csv("complete.csv")
data
uns_dat <- data[,-9]
uns_dat

uns_dat=com[,-9]
uns_dat

n=nrow(uns_dat)
p=ncol(uns_dat)
M=colMeans(uns_dat)
sigma=apply(uns_dat,2,sd)
descriptive=round(cbind(M,sigma),2)
descriptive

#pca

rho=cor(uns_dat)
round(rho,3)


eigen(rho)

autoval=eigen(rho)$values
autovec=eigen(rho)$vectors

pvarsp=autoval/p
pvarsp

pvarspcum=cumsum(pvarsp)
pvarspcum
tab=round(cbind(autoval,pvarsp*100,pvarspcum*100),3)
colnames(tab)<-c("eigenvelues", "% variance","% cum variance")
tab

plot(autoval, type="b", main="Scree Diagram", xlab="Number of Component", ylab="Eigenvalues")
abline(h=1, lwd=3, col="yellow")


##barplot
acp=princomp(uns_dat, cor=T)
summary(princomp(uns_dat,cor=T))
screeplot(princomp(uns_dat, cor=T))



eigen(rho)$vectors[,1:3]

comp=round(cbind(-eigen(rho)$vectors[,1]*sqrt(autoval[1]),-eigen(rho)$vectors[,2]*sqrt(autoval[2]),-eigen(rho)$vectors[,3]*sqrt(autoval[3])),3)
comp
rownames(comp)=row.names(descriptive)
colnames(comp)=c("Comp1","Comp2","comp3")
comp

comunality=comp[,1]^2+comp[,2]^2+comp[,3]^2
comp=cbind(comp,comunality)
comp






##calculate the score
data.scale_uns=scale(uns_dat,T,T)
score=data.scale_uns%*%autovec[,1:3]

scorez<-round(cbind(-score[,1]/sqrt(autoval[1]),-score[,2]/sqrt(autoval[2])),2)
plot(scorez, main="Scores plot",
     xlab="comp1",ylab="comp2")
text(scorez, rownames(uns_dat))
abline(v=0,h=0,col="red")


plot(comp[,1:2], main="Loadings plot",
     xlab="comp1",ylab="comp2", xlim=range(-1,1))
text(comp, rownames(comp))
abline(v=0,h=0,col="blue")




plot(princomp(uns_dat, cor=T)$scores)
abline(h=0, v=0)
biplot(acp)


plot(cumsum(pvarsp),xlab="Pricipal Component", ylab="Cumulative proportion of VA",ylim=c(0,1),type='b')

#HIERARCHICAL CLUSTERING

#optimal number of clusters

library(factoextra)
library(NbClust)
library(cluster)

# Standardize the data
df <- scale(uns_dat)
head(df)

fviz_nbclust(df, FUNcluster = hcut, method = c("silhouette", "wss", "gap_stat"))

#Dendogram
distances <-dist(df, method = "euclidean")
clusters <- hclust(distances, method = "ward.D")
plot(clusters)
abline(h=30, col="red")
rect.hclust(clusters, k=3, border="red") 

#Clusters
cluster_groups <- cutree(clusters, k =3)
cluster_groups

cluster1 <- subset(uns_dat, cluster_groups == 1)
cluster1
cluster2 <- subset(uns_dat, cluster_groups == 2)
cluster2
cluster3 <- subset(uns_dat, cluster_groups == 3)
cluster3

data.scale_uns <- scale(uns_data,T,T)
clusplot(data.scale_uns, cluster_groups, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)
