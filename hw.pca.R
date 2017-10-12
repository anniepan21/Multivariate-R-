# Multivariate-R-
# Principal Component Analysis of the Wine Data


# Read data from file.
wine <- read.table("wine.data.txt",sep=",")
head(wine)

# Standardise the variables.
standardisedconcentrations <- as.data.frame(scale(wine[2:14]))
wine01 <- standardisedconcentrations

# Perform a PCA on wine.
wine.pca <- prcomp(wine01)

# Obtain the loadings for the first principal component.
wine.pca$rotation[,1]
# V8 (-0.423),in absolute value, makes the largest contribution to PC1.

# Obtain the loadings for the second principal component.
wine.pca$rotation[,2]
# V11 (0.530) makes the largest contribution to PC2.

# Remove V8 and V11 from wine and call new data set wine2.
wine2 <- wine[-c(8,11)]
head(wine2)

# Perform PCA on wine2.
standardisedconcentrations2 <- as.data.frame(scale(wine2[2:12]))
wine02 <- standardisedconcentrations2
wine2.pca <- prcomp(wine02)

# Total Variance of the PC's: (11)
sum((wine2.pca$sdev)^2)

summary(wine2.pca)
wine2.pca$sdev    # Standard deviation of PC1: (1.9701)
wine2.pca$sdev^2    # Variance of PC1:(3.8811130)

# PC1 explains 35.28% of the total variance.

calcpc <- function(variables,loadings)
  {
     # find the number of samples in the data set
     as.data.frame(variables)
     numsamples <- nrow(variables)
     # make a vector to store the component
     pc <- numeric(numsamples)
     # find the number of variables
     numvariables <- length(variables)
     # calculate the value of the component for each sample
     for (i in 1:numsamples)
     {
        valuei <- 0
        for (j in 1:numvariables)
        {
           valueij <- variables[i,j]
           loadingj <- loadings[j]
           valuei <- valuei + (valueij * loadingj)
        }
        pc[i] <- valuei
     }
     return(pc)
  }

calcpc(standardisedconcentrations2, wine2.pca$rotation[,1])

# Compute PC1 for each of the 178 wine samples.
wine2.pca$x[,1]

# Produce a screen plot.
screeplot(wine2.pca, type="lines")

# 65.02% of the total variance is explained by the PC's to the left of the elbow.
# According to Kaiser’s criterion, we would retain the fist three principal components (which have variance above 1).
# The first six principal components should be retained to meet 85% variance level.

wine2.pca$rotation[,1]
# V7(-0.42188), in absolute value, makes the largest contribution to PC1.
wine2.pca$rotation[,2]
# V2(0.487)makes the largest contribution to PC2.

# Make a scatterplot and label the points by cultivar.
plot(wine2.pca$x[,1],wine2.pca$x[,2]) 
text(wine2.pca$x[,1],wine2.pca$x[,2], wine2$V1, cex=0.7, pos=4, col="red") 
# Yes, it did a better job of separating the cultivars.

# Make a scatterplot with legend.
plot(wine2.pca$x[,1],wine2.pca$x[,2], main="Scatterplot of PC1 vs. PC2", xlab="PC1", ylab="PC2", pch=2, col=wine2$V1)
levels(wine2$V1)<- c(1:3)
legend(3,-2,legend=levels(wine2$V1),col=c(1:3),pch=2)

wine2$Color <- ""
wine2$Color[wine2$V1==1]<-"magenta"
wine2$Color[wine2$V1==2]<-"cyan"
wine2$Color[wine2$V1==3]<-"yellow"
plot(wine2.pca$x[,1],wine2.pca$x[,2], main="Scatterplot of PC1 vs. PC2", xlab="PC1", ylab="PC2", pch=2, col=wine2$Color)
legend(3,-2,legend=levels(wine2$V1), col = c("magenta","cyan","yellow"),pch=2)

