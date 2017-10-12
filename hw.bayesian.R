### Bayesian Statistics

JH_ft <- sum(rbinom(15, 1, .82))
JH_ft 

DH_ft <- sum(rbinom(10, 1, .70))
DH_ft 

par(mfrow=c(2,2))
xvec <- seq(0,1,.001)

### 1) JH Prior on DH Game
plot(xvec, dbeta(xvec, 15, 2), type="l", col = "dodgerblue2", ylim = c(0,9), main = "JH Prior on DH Game") ### JH PRIOR
lines(xvec, dbeta(xvec, 15+DH_ft , 2+10-DH_ft), type="l", col = 3) ### DH POSTERIER
legend(0, 8 , legend=c("JH Prior","DH Posterier"), lty = c(1,1), lwd=c(2,2), col = c("dodgerblue2", 3), box.lty=0, bg = "white") 

### 2) DH Prior on JH Game
plot(xvec, dbeta(xvec, 5, 2), type="l", col = "dodgerblue2", ylim = c(0,9), main = "DH Prior on JH Game") ### DH PRIOR
lines(xvec, dbeta(xvec, 5+JH_ft, 2+15-JH_ft), type ="l", col = 3) ### JH POSTERIER
legend(0, 8 , legend=c("DH Prior","JH Posterier"), lty = c(1,1), lwd=c(2,2), col = c("dodgerblue2", 3), box.lty=0, bg = "white") 


### 3) JH Prior on JH Game
plot(xvec, dbeta(xvec, 15, 2), type="l", col = "dodgerblue2", ylim = c(0,9), main ="JH Prior on JH Game") ### JH PRIOR
lines(xvec, dbeta(xvec, 15+JH_ft, 2+15-JH_ft), type="l", col = 3) ### JH POSTERIER
legend(0, 8 , legend=c("JH Prior","JH Posterier"), lty = c(1,1), lwd=c(2,2), col = c("dodgerblue2", 3), box.lty=0, bg = "white")


### 4) DH Prior on DH Game
plot(xvec, dbeta(xvec, 5, 2), type="l", col = "dodgerblue2", ylim = c(0,9), main ="DH Prior on DH Game") ### DH PRIOR
lines(xvec, dbeta(xvec, 5+DH_ft, 2+10-DH_ft), type ="l", col = 3) ### DH POSTERIER
legend(0, 8 , legend=c("DH Prior","DH Posterier"), lty = c(1,1), lwd=c(2,2), col = c("dodgerblue2", 3), box.lty=0, bg = "white") 


