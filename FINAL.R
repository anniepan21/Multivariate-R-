### PCA Analysis ---------------------------------------------------------------------
ushealth <- read.table("ushealth.txt", header=TRUE)
head(ushealth)

### 1. Perform a PCA
standardized <- as.data.frame(scale(ushealth[4:10]))
ushealth.pca <- prcomp(standardized)

### 2. What is the standard deviation of PC2?
### A: 1.1087
summary(ushealth.pca)

### 3. What is the total variance of the PC's? 
### A: 7 
sum((ushealth.pca$sdev)^2)

### 4: What is variance of PC4?  A: 0.61101275
###    What proportion is explained? A: 8.729%
ushealth.pca$sdev^2

### 5: 81.31% of the total variance is explained by the first three PC's.

### 6: According to Kaiser's criterion, 3 PC's should be retained. 

### 7: 4 PC's need to be retained if we want to explain at least 90% of the variance.

### 8: CANCER makes the largest contribution to PC1.
ushealth.pca$rotation

### 9: RI, DE, NJ have the largest value of PC2.
###    SD, NE, IA have the smallest value of PC2.
A <- as.matrix(standardized)
B <- ushealth.pca$rotation[,2]
C <- A %*% B 

C <- as.data.frame(C)
table <- cbind(ushealth[,1],C)
table[order(table$V1),]

### Multinomial distribution ----------------------------------------------------------

# build vector of K
a <- c(0,5,0)
b <- c(0,4,1)
c <- c(0,3,2)
d <- c(0,2,3)
e <- c(0,1,4)
f <- c(0,0,5)

# build probabilty vector
prob <- c(18/38,18/38,2/38)

# calculate multinomail probabilities
dmultinom(a,5,prob)
dmultinom(b,5,prob)
dmultinom(c,5,prob)
dmultinom(d,5,prob)
dmultinom(e,5,prob)
dmultinom(f,5,prob)

sum(dmultinom(a,5,prob),dmultinom(b,5,prob),dmultinom(c,5,prob),dmultinom(d,5,prob),dmultinom(e,5,prob),dmultinom(f,5,prob))

### A: 0.04038611

### Factor Analysis -----------------------------------------------------------------------
f2 <- factanal(scale(ushealth[,4:10]), factors=2)
f3 <- factanal(scale(ushealth[,4:10]), factors=3)

alpha <- 0.05

a <- as.data.frame(cbind(k=f2$factors, chisqstat=f2$STATISTIC, df=f2$dof, alpha=alpha, Pval=f2$PVAL, decision="reject"))
b <- as.data.frame(cbind(k=f3$factors, chisqstat=f3$STATISTIC, df=f3$dof, alpha=alpha, Pval=f3$PVAL, decision="accept"))

summary <- rbind(a,b)
summary

### Recompute p-value
1-pchisq(19.115,8)
1-pchisq(3.9041,3)
### Yes, it "matches"

### Bayes, Rockets, bummer ------------------------------------------------------------------
128/383
Rocket_3p <- sum(rbinom(383, 1, .334))
Rocket_3p

xvec <- seq(0,1,.001)

### Posterier Mean = 0.3250518
post_mean <- (36+Rocket_3p)/(36+Rocket_3p+64+383-Rocket_3p)

### Prior Mean = 0.36
prior_mean <- 36/(36+64)

### It deceased by 0.03494
prior_mean - post_mean

### Plot
plot(xvec, dbeta(xvec, 36, 64), type="l", col = "dodgerblue2", ylim = c(0,20), main = "Rocket Posterier vs. Prior") ### PRIOR
lines(xvec, dbeta(xvec, 36+Rocket_3p, 64+383-Rocket_3p), type="l", col = 3) ### POSTERIER
legend(0, 20, legend=c("Prior","Posterier"), lty = c(1,1), lwd=c(2,2), col = c("dodgerblue2", 3), box.lty=0, bg = "white")


### Question 3: Update with new prior

### Posterier Mean = 0.35294
post_mean <- (1181+Rocket_3p)/(1181+Rocket_3p+2125+383-Rocket_3p)

### Prior Mean = 0.3572
prior_mean <- 1181/3306

### It deceased by 0.004288
prior_mean - post_mean

### Plot
plot(xvec, dbeta(xvec, 1181, 2125), type="l", col = "dodgerblue2", ylim = c(0,50), main = "Rocket Posterier vs. Prior") ### PRIOR
lines(xvec, dbeta(xvec, 1181+Rocket_3p, 2125+383-Rocket_3p), type="l", col = 3) ### POSTERIER
legend(0, 50, legend=c("Prior","Posterier"), lty = c(1,1), lwd=c(2,2), col = c("dodgerblue2", 3), box.lty=0, bg = "white")


### Kernel density function ----------------------------------------------------------------------------

set.seed(31415)
x0 <- rchisq(30,10)

x0.kde <- density(x0)

### 1) Optimal bandwidth = 2.037107
x0.kde$bw


### 2) Using Adjust function

x0.kde.under <- density(x0, adj=.25) 
x0.kde.opt <- x0.kde 
x0.kde.over <- density(x0, adj=2) 

par(mfrow=c(2,2)) 

plot(x0.kde.under, col=4, lwd=2, main="Undersmoothed", ylim=c(0,0.2)) 
lines(x0.kde$x, dchisq(x0.kde$x, 10), col=5, lwd=2)

plot(x0.kde.opt, col=4, lwd=2, main="Optimally smoothed", ylim=c(0,0.1)) 
lines(x0.kde$x, dchisq(x0.kde$x, 10), col=5, lwd=2)

plot(x0.kde.over, col=4, lwd=2, main="Oversmoothed", ylim=c(0,0.1)) 
lines(x0.kde$x, dchisq(x0.kde$x, 10), col=5, lwd=2)

par(mfrow=c(1,1)) 

### 3) Using bw argument
x0.kde.under <- density(x0, bw = .25 * x0.kde$bw) 
x0.kde.opt <- x0.kde 
x0.kde.over <- density(x0, bw = 2 * x0.kde$bw)

par(mfrow=c(2,2)) 

plot(x0.kde.under, col=4, lwd=2, main="Undersmoothed", ylim=c(0,0.2)) 
lines(x0.kde$x, dchisq(x0.kde$x, 10), col=5, lwd=2)

plot(x0.kde.opt, col=4, lwd=2, main="Optimally smoothed", ylim=c(0,0.1)) 
lines(x0.kde$x, dchisq(x0.kde$x, 10), col=5, lwd=2)

plot(x0.kde.over, col=4, lwd=2, main="Oversmoothed", ylim=c(0,0.1)) 
lines(x0.kde$x, dchisq(x0.kde$x, 10), col=5, lwd=2)

par(mfrow=c(1,1)) 


