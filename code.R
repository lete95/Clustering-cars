####################################################################
# MULTIVARIATE ANALYSIS

# Pol Serra i Lidón
# Qiaorui Xiang
# LAB 6: Practice MCA and Clustering
####################################################################

# 1.Read the file “mca_car.csv” containing the data and its dictionary about the cars and their 
#   characteristics found in specialized magazines. The final goal will be to find a model to 
#   predict the price of cars as function of its characteristics. First we will perform a visualization 
#   of the information contained in the dataset, then we will perform a clustering of cars. 
#   The data has been previously preprocessed to have it in categorical form.

data <- read.csv("mca_car.csv",sep=",",header=TRUE,check.names=TRUE)
names(data)
summary(data)

# 2.With the obtained data frame perform a Multiple Correspondence Analysis. Take the brand and 
#   price (either categorical or continuous) as supplementary variables, whereas the remaining ones are active.

precio <- which(colnames(data)=="precio")
marca <- which(colnames(data)=="marca")
precio_categ <- which(colnames(data)=="precio_categ")

library(FactoMineR)
mca <- MCA(data, 
               quanti.sup = precio, # Price. Supplementary quantitative variable
               quali.sup = c(marca,precio_categ)  # Brand and Price Category. Supplementary qualitative variable
              )

# 3.Interpret the first two obtained factors.
dimdesc(mca)

# 4.Decide the number of significant dimensions that you retain (by subtracting the
#   average eigenvalue and represent the new obtained eigenvalues in a new screeplot).

plot(mca$eig$eigenvalue,type="l", xlab = "Index", ylab = "Eigenvalue")
# Find average eigenvalue
avg <- mean(mca$eig$eigenvalue)
abline(h = avg, col="red", lty = 2)

# Take all those dimensions > than average
lmb <- mca$eig$eigenvalue[mca$eig$eigenvalue>avg] 
# New screeplot
plot(lmb,type="l",xlab = "Index",ylab = "Eigenvalue")

plot(cumsum(100*lmb/sum(lmb)), type = "o", xlab="Component Number", ylab="Contribution to total variance(%)", pch=16, ylim = c(0,100))
abline(h=80, col="red", lty = 2)

# We take dimensions that explains 80% of new dimension set
nd <- which.min(abs(cumsum(100*lmb/sum(lmb)) - 80))
L <- 1:nd
# 11 dimensions are selected

# Reload MCA with 11 dimensions
mca <- MCA(data, 
           quanti.sup = precio, # Price. Supplementary quantitative variable
           quali.sup = c(marca,precio_categ),  # Brand and Price Category. Supplementary qualitative variable
           ncp = 11,
           graph = FALSE
)

# 5.Perform a hierarchical clustering with the significant factors, decide the number of
#   final classes to obtain and perform a consolidation operation of the clustering.

# Perform a hierarchical clustering with significant factors
head(mca$ind$coord)
psi <- mca$ind$coord[,L]
d <- dist(psi,method = "euclidean")
hc <- hclust(d,method = "ward.D2")
plot(hc,hang = -1, cex = 0.1)
# We can see a jump in height = 10
cutheight <- 10
abline(h=cutheight, col="red", lty = 2)
barplot(hc$height)
abline(h=cutheight, col="red", lty = 2)
# Decide the number of classes present
# Then we have 5 cluster
nc = 5

# Cut tree
c1 <- cutree(hc,nc)
table(c1)

cdg <- aggregate(as.data.frame(psi),list(c1),mean)[,2:(nd+1)]
plot(psi[,1],psi[,2],type="p", pch= 20 ,main="Clustering of cars in 5 classes", xlab = "Dim 1", ylab = "Dim 2",col=c1)
#text(psi[,1],psi[,2],col=c1,labels=row.names(psi),cex = 0.6) 
abline(h=0,v=0,col="gray")
legend("bottomright",paste("C", 1:nc, sep=""),pch=20,col=L)
text(cdg,labels=paste("G", 1:nc, sep=""),col=L)

# Consolidation
k_def <- kmeans(psi,centers=cdg)
table(k_def$cluster)
# LETS SEE THE CONSOLIDATED PARTITION VISUALLY
plot(psi[,1],psi[,2],type="p", pch= 20, main="Clustering of cars in 5 classes",xlab = "Dim 1", ylab = "Dim 2", col=k_def$cluster)
#text(psi[,1],psi[,2],col=k_def$cluster,labels=row.names(psi),cex = 0.6) 
abline(h=0,v=0,col="gray")
legend("bottomright",paste("C", 1:nc, sep=""),pch=20,col=L)
changes <- which((c1!=k_def$cluster))
c1[changes]
k_def$cluster[changes]

# 6.Using the function catdes interpret and name the obtained clusters and represent
#   them in the first factorial display.
cd<-catdes(cbind(as.factor(k_def$cluster),data), 1)
cd
plot(cd, show="all", level=0.01, sort=NULL,
     col.upper="indianred2", col.lower="royalblue1", numchar = 5,
     barplot = FALSE)

# In order to find the meaning of each cluster, we can do as follow:
# Check total frequency of price category
table(data[,precio_categ])
for (i in 1:nc) {
  # Check distribution of price category by cluster
  print(paste0("cluster ", i))
  print(table(data[which(k_def$cluster == i),][,precio_categ]))
}
