### Question 1: Read swissbank data into file. And call it banknotes.
banknotes <- read.table("swissbank.txt", header=TRUE)

### Question 2: Add a new variable, x0, to classify genuine and counterfeit bills.
### I created a vector, x0, with one hundred "0" and one hundred 1's, while number 0 represent genuine, and number 1 represent counterfeit.
### Then I combined the vector x0 to the original banknotes dataframe.

x0 <- c(rep(0,100),rep(1,100))  #Assign 0 to genuines, 1 to counterfeit
banknotes <- data.frame(x0, banknotes) #Combine x0 to the dataframe
str(banknotes)

### The other 6 variables are as follows:
### x1 = length of the bill (mm)
### x2 = height of the bill(left) (mm)
### x3 = height of the bill (right) (mm)
### x4 = distance of the inner frame to the lower border (mm)
### x5 = distance of the inner frame to the upper border (mm)
### x6 = length of the diagonal of the center picture (mm)


### Question 3: 
boxplot(banknotes[2:7], xlab="Variables", ylab="Measurements (in MM)", main="Boxplot of 6 Bank Notes Variables")
### The boxplot produced is not useful, because each measurement of the banknote ranges widely. Therefore, we cannot see the details of the boxplots if we put them in one screen.


### Question 4:

par(mfrow=c(3,2))

for (k in c(2:7)){
  banknotes$x <- banknotes[,k]
  main = colnames(banknotes)[k]
  boxplot(banknotes$x~banknotes$x0, ylab = "Measurement (MM)", col = c("aquamarine","palevioletred1"), names = c("GENUINE","COUNTERFEIT"), main = main)
  legend("bottomright", legend=c("GEN","COUNT"), fill = c("aquamarine","palevioletred1"), bg ="transparent", box.lty=0, cex=0.75, yjust =0.5) 
  ### Wasn't sure what needs to be put into the legend
}


### Question 5:

par(mfrow=c(2,3))

for (k in c(2:7)){
  banknotes$x <- banknotes[,k]
  main = colnames(banknotes)[k]
  boxplot(banknotes$x~banknotes$x0, ylab = "Measurement (MM)", col = c("aquamarine","palevioletred1"), names = c("GENUINE","COUNTERFEIT"), main = main)
  legend("bottomright", legend=c("GEN","COUNT"), fill = c("aquamarine","palevioletred1"), bg ="transparent", box.lty=0, cex=0.75) 
  ### Wasn't sure what needs to be put into the legend
}


### Question 6:

par(mfrow=c(1,1))

for (k in c(2:7)){
  banknotes$x <- banknotes[,k]
  main = colnames(banknotes)[k]
  boxplot(banknotes$x~banknotes$x0, ylab = "Measurement (MM)", col = c("aquamarine","palevioletred1"), names = c("GENUINE","COUNTERFEIT"), main = main)
  legend(locator(1), legend=c("GEN","COUNT"), fill = c("aquamarine","palevioletred1"), bg ="transparent", box.lty=0, cex=0.75) 
  ### Wasn't sure what needs to be put into the legend
  devAskNewPage(ask = NULL)
  readline("press any key to continue")
}


### Question 7: Deprecated in relation to software, refers to a specific function, method, or a software feature that are in the process of being replaced by newer ones.  It means that it should not be used because there is (or there will be) a better alternative in that software that should be used instead.


### Question 8: If we sample data from a standard normal distribution, the probabilty that a data point will fall outside the 1.5 sd 

qnorm(.75,0,1)-qnorm(.25,0,1)  ### IQR: 1.34898

(qnorm(.75,0,1)-qnorm(.25,0,1))*1.5  ### 1.5 * IQR: 2.023469

pnorm(-2.02,0,1)*2 ### Probility that data falls outside of 1.5IQR under a standard normal distribution: 0.04338339



### Question 9:

sum <- 0

for (k in seq(100,200, by =10))
{
  xvec <- rnorm(k)
  boxplot(xvec)
  stats <- boxplot.stats(xvec, coef=1.5)
  x <- length(stats$out)
  print(x)  #print number of outliers for each simulation
  y <- sum
  sum <- x+y 
}

print(sum) #total number of outliers

### calculate total number of data points
sum(seq(100,200, by=10))

## estimate the probability 
sum/sum(seq(100,200, by=10))

### The advantage of using boxplot.stat is that it can gives you a summary of outlier data, so I don't have to count it everytime.


### Question 10:

(qt(.75,2)-qt(.25,2))*1.5   ### 1.5*IQR: 2.44949
pt(-2.44949,2)*2  ### 0.1339746


sum <- 0

for (k in seq(100,200, by =10))
{
  xvec <- rt(k,2)
  boxplot(xvec)
  stats <- boxplot.stats(xvec, coef=1.5)
  x <- length(stats$out)
  print(x)  #print number of outliers
  y <- sum
  sum <- x+y 
}

print(sum) #total number of outliers

sum/sum(seq(100,200, by=10)) ## estimate the probability 







