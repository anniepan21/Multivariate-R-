### Kolmogorov-Smirnov (KS) Test
### Question 1

### Load Data
ks.dat1 <- read.csv("data.hw.ks.csv")
str(ks.dat1)

### Extract numbers 
x1<-ks.dat1[[1]]
x2<-ks.dat1[[2]]
x3<-ks.dat1[[3]]
x4<-ks.dat1[[4]]
x5<-ks.dat1[[5]]

### Choose alpha level
alpha <- 0.05

### One-Sample KS test
ks1 <- ks.test(x1,"punif",0,1)
ks2 <- ks.test(x2,"pnorm",5,3)
ks3 <- ks.test(x3,"pexp",1)
ks4 <- ks.test(x4,"pbeta",1,1)
ks5 <- ks.test(x5,"pnorm",0,1)

summary <- data.frame(rbind(ks1,ks2,ks3,ks4,ks5))
summary

### Compare p-value to pre-determined alpha
result1 <- ifelse(ks1$p.value<alpha, "Reject H0", "Accept H0")
result2 <- ifelse(ks2$p.value<alpha, "Reject H0", "Accept H0")
result3 <- ifelse(ks3$p.value<alpha, "Reject H0", "Accept H0")
result4 <- ifelse(ks4$p.value<alpha, "Reject H0", "Accept H0")
result5 <- ifelse(ks5$p.value<alpha, "Reject H0", "Accept H0")

result <- rbind(result1 ,result2, result3, result4, result5)
result

### Summary table
summary <- cbind(summary, result)
summary




### Question 2
ks.dat2 <- read.csv("data.hw.ks30.csv", header=T)
str(ks.dat2)

### Create empty dataframes
summary = data.frame()
data.name = data.frame()

### Assign z value of 5 
z <- 5


### Create a loop to test each pair of samples 
for (i in (1:(z-1)))
{ for (k in ((i+1):z)) 
	{ out <- ks.test(ks.dat2[[i]],ks.dat2[[k]])
          data <- c(i,k)
	  data.name <- data.frame(rbind(data.name,data))
	  summary <- data.frame(rbind(summary,out))
}
}


print(data.name)
print(summary)

result.table <- data.frame(c(data.name, summary))
result.table


### Reject H0 if p=value < 0.05 (Data are not from the same distribution)
### Extract rows with p-value greater than 0.05, we can not reject H0.
### Represent X1 and X2 from the same distribution

result.table[result.table$p.value>.05,]



### Question 4

a <- 0.05
n <- 100
rej <- 0 

for (i in 1:n){

	x <- rnorm(400)
	ks <- ks.test(x,"pnorm",0,1)
	a0 <- ifelse(ks$p.value < 0.05,1,0)
        rej <- rej + a0
}

rej/n


### Repeat with N= 1000
n <- 1000
rej <- 0 

for (i in 1:n){

	x <- rnorm(400)
	ks <- ks.test(x,"pnorm",0,1)
	a0 <- ifelse(ks$p.value < 0.05,1,0)
        rej <- rej + a0
}

rej/n

### Repeat with number of N values

e = data.frame()


for (n in seq(100,1000, 100))
{
	rej <- 0 

	for (i in 1:n)
	{
		x <- rnorm(400)
		ks <- ks.test(x,"pnorm",0,1)
		a0 <- ifelse(ks$p.value < 0.05,1,0)
      		rej <- rej + a0
	}

	k <- rej/n
        e <- data.frame(rbind(e,k))
}
	
	

### plot
y <- seq(100,1000,100)
plot(e$X0.03,y)


