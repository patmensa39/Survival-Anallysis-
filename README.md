# Survival-Anallysis-
Including Kaplan- Meier, Nelson Aalen, Fitting a Weibull dustribution 

heart1 = function(){

t = seq(0,60,len=100)

plot(c(-8,8),c(0,20),type='n',axes=FALSE,xlab='',ylab='')

x = -.01*(-t^2+40*t+1200)*sin(pi*t/180)

y = .01*(-t^2+40*t+1200)*cos(pi*t/180)

lines(x,y, lwd=4, col="red")

lines(-x,y, lwd=4, col="red")

text(0,9,"Happy Valentine Afia",col='red',cex=2)



}

heart1()



heart2 <- function() {

dat<- data.frame(t=seq(0, 2*pi, by=0.1) )

xhrt <- function(t) 16*sin(t)^3

yhrt <- function(t) 13*cos(t)-5*cos(2*t)-2*cos(3*t)-cos(4*t)

dat$y=yhrt(dat$t)

dat$x=xhrt(dat$t)

plot(y ~ x, data=dat, type="l", bty="n", xaxt="n", yaxt="n",

ann=FALSE)

with(dat, polygon(x,y, col="hotpink"))

points(c(10,-10, -15, 15), c(-10, -10, 10, 10), pch=169, font=5)

}

heart2()





library(survival)

sixmp <- read.csv("6mp.csv")

survfit.group1 <- survfit(Surv(time, cens)~1,data = sixmp, conf.type = "log-log", subset = {grp==1} )

summary(survfit.group1)

plot(survfit.group1, main = "Kaplan- Meier Survival curve", xlab = "Time in weeks", col = "red", ylab = "Survival Probability")

quantile(survfit.group1)



# Survival Data Example 

tm <- c(2, 2 ,2, 2, 3, 3, 4, 5, 5, 5, 5, 6)

delta <- c(1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0)

library(survival)

Surv(tm, delta)

result.km <- survfit(Surv(tm, delta) ~ 1)

plot(result.km, conf.int=T, mark.time=T, main = "Kaplan Meier Curve") # "+" by default





tm <- c(2, 2 ,2, 2, 3, 3, 4, 5, 5, 5, 5, 6)

delta <- c(1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0)

library(survival)

Surv(tm, delta)

windows()

par(mfrow=c(2,2)) # make four plots, row-by-row

result.km <- survfit(Surv(tm, delta) ~ 1, conf.type="plain")

summary(result.km)

plot(result.km, conf.int=T, mark.time=T) # "+" by default

title("plain confidence interval")



result.km <- survfit(Surv(tm, delta) ~ 1, conf.type="log")

plot(result.km, conf.int=T, mark.time=T) # "+" by default

title("log confidence interval")



result.km <- survfit(Surv(tm, delta) ~ 1, conf.type="log-log")

summary(result.km)

plot(result.km, conf.int=T, mark.time=T) # "+" by default

title("complementary log-log \nconfidence interval")





# Finding the binomial Maximum likelihood Estimate 

# Lifetime Data February 3, 2020

# Maximum likelihood estimation





logBinom <- function(p, y=3, n=10) {

result <- y*log(p) + (n-y)*log(1-p)

result

}



curve(logBinom, xlab="p", ylim=c(-70,-5)) # plot the function

logBinom <- function(p, y=24, n=80) {

result <- y*log(p) + (n-y)*log(1-p)

result

}

par(mfrow=c(1,1))

curve(logBinom, add=T, lty=2, col="blue")

abline(v=0.3, col="red") # vertical line at mle





#Homework 4 

# QUESTION 1

library(survival)

tm <- c(5,8,7,2,8)

cens <- c(1,1,1,1,0)

grp <- c(0,0,1,1,1)

result.km <- survfit(Surv(tm, cens) ~ 1)

summary(result.km)


plot(result.km, main = "Kaplan Meier Survival Curve", xlab = "Time", 

ylab = "Survival Probability", col= "red")



# (a) 	The weighted log-rank test 

library(survival)

tm <- c(5,8,7,2,8)

cens <- c(1,1,1,1,0)

grp <- c(0,0,1,1,1)

survdiff(Surv(tm, cens) ~ grp, rho = 0)



# (c)	Prentice modified Gehan test statistic

library(survival)

tm <- c(5,8,7,2,8)

cens <- c(1,1,1,1,0)

grp <- c(0,0,1,1,1)

survdiff(Surv(tm, cens) ~ grp, rho = 1)



# QUESTION 2

# the log-rank test in R, 

tm <- c(5, 8, 7, 2, 8)

cens <- c(1, 1, 1, 1, 0)


grp <- c(0,0, 1, 1, 1)

library(survival)

survdiff(Surv(tm, cens) ~ grp, rho=0)



# Fitting the Prentice modified Gehan test (with the option rho=1),

tm <- c(5, 8, 7, 2, 8)

cens <- c(1, 1, 1, 1, 0)

grp <- c(0,0, 1, 1, 1)

library(survival)

survdiff(Surv(tm, cens) ~ grp, rho=1)



# 2. a. Using the “pancreatic2” data from the “asaur” package,Using the log-ranked test, we have 



library(asaur)

attach(pancreatic2)

survdiff(Surv(pfs) ~ stage, rho = 0)




## Using the Prentice modification of the Gehan test, we have 



library(asaur)

attach(pancreatic2)

survdiff(Surv(pfs) ~ stage, rho = 1)




library(asaur)

attach(pancreatic2)

wilcox.test(pancreatic2$pfs ~ pancreatic2$stage)



# QUESTION 3 


library(asaur)

library(survival)



yearentry <- ChanningHouse$entry/12

yearexit <- ChanningHouse$exit/12

yeartime <- ChanningHouse$time/12






# Kaplan Meier For Women 

result <- survfit(Surv(yearentry, yearexit, cens)~1,type= "kaplan-meier",  data = ChanningHouse,

subset={sex=="Female"})

plot(result, main= "Kaplan Meier Survival curve for Females", xlab = "Time in Years",

ylab = "Survival Probability", col = "red")





# Kaplan Meier For Men 



result <- survfit(Surv(yearentry, yearexit, cens)~1,type= "kaplan-meier",  data = ChanningHouse,

subset={sex=="Male"})

plot(result, main= "Kaplan Meier Survival curve for Males", xlab = "Time in Years",


ylab = "Survival Probability", col = "blue")






##the Nelson-Aalen Survival curve for women



result <- survfit(Surv(yearentry, yearexit, cens)~1,type = "fleming-harrington",  data = ChanningHouse,

subset={sex=="Female"})

plot(result, main= "Nelson-Aalen Survival curve for Females", xlab = "Time in Years",

ylab = "Survival Probability", col = "red")




##the Nelson-Aalen- Survival curve for men 



result <- survfit(Surv(yearentry, yearexit, cens)~1,type = "fleming-harrington",  data = ChanningHouse,

subset={sex=="Male"})

plot(result, main= "Nelson-Aalen Survival curve for Males", xlab = "Time in Years",

ylab = "Survival Probability", col = "blue")







## QUESTION 4 

library(asaur)

library(survival)

result.km <- survfit(Surv(futime, fustat) ~ 1, data = ovarian)

summary(result.km)

plot(result.km, main = "Kaplan Meier Survival Curve", xlab = "Time",ylab = "Survival Probability", col= "red")





result.km_ovarian <- result.km$surv

plot(log(ovarian$futime), log(-log(result.km_ovarian)), xlab ="Log (Time)", ylab = "Log (-log(S(t))" )

abline(lm(log(-log(result.km_ovarian))~ log(ovarian$futime)), col= "blue" )



ovarian_model <-lm(log(-log(result.km_ovarian))~ log(ovarian$futime)) 



ovarian_model




