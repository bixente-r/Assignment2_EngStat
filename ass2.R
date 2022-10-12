### EXERCICE 1 ###

d <- c(1.5, 3, 4.5, 5, 7)
p <- c(0.05, 0.25, 0.35, 0.2, 0.15)

# (a)

s_mean <- sum(d*p); s_mean
# s_mean2 <- weighted.mean(d,p); s_mean2
variance <- sum(((d-s_mean)^2)*p); variance
#variance2 <- weighted.mean(d^2,p) - (weighted.mean(d,p))^2; variance2

# (b)

#P(X < 4) = P(X = 1.5) + P(X = 3)
# = 0.3

# (c)
# E(X < 4) = (1.5 * 0.05 + 3 * 0.25) / 0.3 = 2.75

### EXERCICE 3 ###

plot(0:3,dbinom(0:3,3,0.7))

Q1 <- dbinom(3,3,0.7);Q1
Q2 <- 1 - pbinom(0,3,0.3);Q2 # or Q2 <- pbinom(2,3,0.7)
Q3 <- dbinom(1,3,0.7);Q3
Q4 <- pbinom(1,3,0.7);Q4
Q5 <- Q1/(1 - pbinom(0,3,0.7));Q5 # or Q5 <- Q1/(1 - dbinom(0,3,0.7));Q5


### EXERCICE 4 ###
x <- 0:5
x1 <- seq(0,5, by=0.1)
plot.new()
par(mar=c(4,4,3,5))
plot(0:5,dbinom(x,100,0.01),type='h',axes=F,xlab="",ylab="")
par(new = T)
plot(0:5,dpois(x,1),col='red',axes=F,xlab="",ylab="")
par(new=T)
plot(x1,dnorm(x1,1,sqrt(0.99)),col='blue',type='l',axes=F,xlab="",ylab="")

axis(2,col="black",col.axis="black",at=seq(0, 10, by=0.1)) 
mtext("Probability",side=2,line=2.5,col="black")  
axis(1 ,col="black",col.axis="black",at=seq(0, 5, by=1)) 
mtext("Packet losses",side=1,line=2.5,col="black") 
legend("topright", legend = c("Binomial", "Poisson", "Normal"),
       lwd = 3, col = c("black", "red", "blue"))


Q1 <- 1 - dbinom(0,100,0.01);Q1
Q2 <- dbinom(1,100,0.01);Q2
Q3 <- dnorm(1,1,sqrt(0.99));Q3
Q4 <- dpois(1,1);Q4


### EXERCICE 5 ###

plot(0:5,dpois(0:5,0.5),type='h')
Q1 <- 1 - ppois(1,0.5);Q1
Q2 <- dpois(5,0.5);Q2


### EXERCICE 6 ###

x <- seq(1,5, by=0.1)
plot(x,2*x^-3,col='blue',type='l',axes=F,xlab="",ylab="")
par(new=T)
plot(x,-x^-2+1,col='red',type='l',axes=F,xlab="",ylab="")
axis(2,col="black",col.axis="black",at=seq(0, 1, by=0.1)) 
mtext("PDF/CDF",side=2,line=2.5,col="black")  
axis(1 ,col="black",col.axis="black",at=seq(0, 5, by=1)) 
mtext("x",side=1,line=2.5,col="black")
legend("topright", legend = c("CDF", "PDF"),
       lwd = 3, col = c("red", "blue"))


### EXERCICE 7 ###

curve(dnorm(x, 0.4, 0.05),0.2,0.6)
Q1 <- 1 - pnorm(0.5,0.4,0.05);Q1
Q2 <- pnorm(0.5,0.4,0.05) - pnorm(0.35,0.4,0.05);Q2
Q3 <- qnorm(0.9,0.4,0.05);Q3

### EXERCICE 8 ###


Q1 <- 1 - ppois(9999,0.0003); Q1
Q2 <- ppois(7000,0.0003);Q2
mpois <- function(l) { floor(l + 1/3 - 1/(50*l)) }
Q3 <- mpois(0.0003);Q3


### EXERCICE 9 ###

alpha <- 5
beta <- 1/4
curve(dgamma(x,alpha,beta),0,100)

Q2 <- pgamma(24,alpha,beta);Q2
Q3 <- pgamma(40,alpha,beta) - pgamma(20,alpha,beta);Q3