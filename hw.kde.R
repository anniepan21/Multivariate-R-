### Kernel Density Estimation

### Question 1:
### My Favorite family of distributions are normal distributions.

### Question 4: 
x <- seq(0, 1, .001)
y <- dbeta(x, 1, 1)
plot(x,y, type="l", col="blue", lwd=2)

par(mfrow=c(4,4))
plot(x,dbeta(x,1,1), type="l", col="firebrick2", lwd=2)
plot(x,dbeta(x,.1,.1), type="l", col="firebrick2", lwd=2)
plot(x,dbeta(x,.99,.99), type="l", col="firebrick2", lwd=2)
plot(x,dbeta(x,1.2,1.2), type="l", col="firebrick2", lwd=2)
plot(x,dbeta(x,2,2), type="l", col="firebrick2", lwd=2)
plot(x,dbeta(x,20,20), type="l", col="firebrick2", lwd=2)
plot(x,dbeta(x,2000,2000), type="l", col="firebrick2", lwd=2)
plot(x,dbeta(x,2,7), type="l", col="firebrick2", lwd=2)
plot(x,dbeta(x,7,2), type="l", col="firebrick2", lwd=2)
plot(x,dbeta(x,1,7), type="l", col="firebrick2", lwd=2)
plot(x,dbeta(x,2,1), type="l", col="firebrick2", lwd=2)
plot(x,dbeta(x,1,2000), type="l", col="firebrick2", lwd=2)
plot(x,dbeta(x,2000,1), type="l", col="firebrick2", lwd=2)
plot(x,dbeta(x,100,15), type="l", col="firebrick2", lwd=2)
plot(x,dbeta(x,15,100), type="l", col="firebrick2", lwd=2)
plot(x,dbeta(x,2.1,1.2), type="l", col="firebrick2", lwd=2)


### Question 5:

par(mfrow=c(3,3))

### (a) similar to Gaussian pdf
plot(x,dbeta(x,5,5), type="l", col="dodgerblue2", lwd=2, main ="(a) Similar Gaussian pdf")

### (b) shape like a triangle
plot(x,dbeta(x,1,2), type="l", col="dodgerblue2", lwd=2, main ="(b) Triangle")

### (c) uniform
plot(x,dbeta(x,1,1), type="l", col="dodgerblue2", lwd=2, main ="(c) Uniform")

### (d) skewed right with mode > 0
plot(x,dbeta(x,2,7), type="l", col="dodgerblue2", lwd=2, main ="(d) Skewed Right")

### (e) skewed left with mode <1
plot(x,dbeta(x,7,2), type="l", col="dodgerblue2", lwd=2, main ="(e) Skewed Left")

### (f) symmetric and like a bowl
plot(x,dbeta(x,.99,.99), type="l", col="dodgerblue2", lwd=2, main ="(f) Symmetric Bowl")

### (g) symmetric and like an inverted bowl
plot(x,dbeta(x,1.5,1.5), type="l", col="dodgerblue2", lwd=2, main ="(g) Symmetric Inverted Bowl")

### (h) strictly decreasing
plot(x,dbeta(x,4,1.1), type="l", col="dodgerblue2", lwd=2, main ="(h) Strictly Decreasing")


par(mfrow=c(1,1))


### Question 6:

myalpha <- 3
mybeta <- 7
mydata <- rbeta(100, myalpha, mybeta)
my.kde <- density (mydata)
plot(my.kde, type='l', lwd=3,col="dodgerblue2", ylim=c(0,3))
lines(my.kde$x,dbeta(my.kde$x,myalpha,mybeta),lwd=3,col= "orange")
legend(0.45, 3 , legend=c("Kernel Density Estimate","True Density Function"), lty = c(1,1), lwd=c(2,2), col = c("dodgerblue2","orange"), box.lty=0, bg = "white") 
grid()
rug(mydata)


### Question 7:

par(mfrow=c(3,3))

for(k in c(.05, .1, .2, .5, 1, 1.2, 1.4, 1.6, 2))
{
  my.kde_adj <- density (mydata, adjust=k)
  plot(my.kde_adj, col="dodgerblue2", lwd=3, ylim=c(0,3.5), main = k)
  lines(my.kde_adj$x,dbeta(my.kde_adj$x, myalpha, mybeta),lwd=3,col='darkorange')
  legend(0.45, 3.5 , legend=c("Kernel Density Estimate","True Density Function"), pch=19, col = c("dodgerblue2","orange"), box.lty=0, bg = "white") 
  grid()
  rug(mydata)
}




### Question 8:

for(k in c(.05, .1, .2, .5, 1, 1.2, 1.4, 1.6, 2))
{
  my.kde_adj <- density (mydata, adjust=k)
  h <- mean((dbeta(my.kde_adj$x,myalpha,mybeta)-my.kde_adj$y)^2)
  b <- my.kde$x[512]- my.kde$x[1]
  ISE <- b*h
  print(ISE)
}



### Question 9:


par(mfrow=c(1,3))

ISE <- vector('numeric', length = 1000)

for (k in c(0.5, 1, 2)) 
{
  
  for (i in 1:1000)
  {
    a <- 3
    b <- 7
    mydata <- rbeta(100, a, b)
    my.kde <- density (mydata,adj=k)
    h <- mean((dbeta(my.kde$x, a, b)-my.kde$y)^2)
    b <- my.kde$x[512]- my.kde$x[1]
    ISE[i] <- b*h
  }
  
  boxplot(ISE, xlab = "Bandwidth", ylab = "ISE" , ylim=c(0,0.4), main = my.kde$bw )
  
}




par(mfrow=c(1,3))

ISE <- vector('numeric', length = 1000)

for (k in c(0.5, 1, 2))
{
  
  for (i in 1:1000)
  {
    a <- 3
    b <- 7
    mydata <- rbeta(100, a, b)
    my.kde <- density (mydata,adj=k)
    h <- mean((dbeta(my.kde$x, a, b)-my.kde$y)^2)
    b <- my.kde$x[512]- my.kde$x[1]
    ISE[i] <- b*h
  }
  
  plot (density(ISE), type='l', lwd=1, col="dodgerblue2", main = k )
  
}



### Question 10:

### My favorite family of distrubution is Beta Distribution.
