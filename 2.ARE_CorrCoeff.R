####### ARE COMPARISON #########
####################**********************####################
####################*********************#####################
#Comparing using Asymptotic Relative Efficiency (ARE) 
#ARE for right tailed test only
#ARE for Wald's test
arewtr_r<-function(n,m,r1){
  z1h0<-c()
  for(i in 1:m){
    mean=c(0,0)
    sigma<-matrix(c(1,0,0,1),nrow=2)
    x<-mvrnorm(n,mean,sigma)
    r<-cor(x[,1],x[,2])
    z1h0[i]<-sqrt(n)*(r-0)
  }
  z1h0<-sort(z1h0)
  for(i in 1:length(z1h0)){
    if((sum(z1h0>=z1h0[i])/length(z1h0))<=0.05){
      c<-z1h0[i]
      break
    }
  }
  z1h1<-c()
  for(i in 1:m){
    mean=c(0,0)
    sigma<-matrix(c(1,r1,r1,1),nrow=2)
    x<-mvrnorm(n,mean,sigma)
    r<-cor(x[,1],x[,2])
    z1h1[i]<-sqrt(n)*(r-0)
  }
  pow<-sum(z1h1>=c)/m
  return(pow)
}
#ARE for VST test
arevstr_r<-function(n,m,r1){
  z1h0<-c()
  for(i in 1:m){
    mean=c(0,0)
    sigma<-matrix(c(1,0,0,1),nrow=2)
    x<-mvrnorm(n,mean,sigma)
    r<-cor(x[,1],x[,2])
    z1h0[i]<-sqrt(n)*0.5*(log((1+r)/(1-r))-0)
  }
  z1h0<-sort(z1h0)
  for(i in 1:length(z1h0)){
    if((sum(z1h0>=z1h0[i])/length(z1h0))<=0.05){
      c<-z1h0[i]
      break
    }
  }
  z1h1<-c()
  for(i in 1:m){
    mean=c(0,0)
    sigma<-matrix(c(1,r1,r1,1),nrow=2)
    x<-mvrnorm(n,mean,sigma)
    r<-cor(x[,1],x[,2])
    z1h1[i]<-sqrt(n)*0.5*(log((1+r)/(1-r))-0)
  }
  pow<-sum(z1h1>=c)/m
  return(pow)
}
#ARE for modified VST test
arevstmr_r<-function(n,m,r1){
  z1h0<-c()
  for(i in 1:m){
    mean=c(0,0)
    sigma<-matrix(c(1,0,0,1),nrow=2)
    x<-mvrnorm(n,mean,sigma)
    r<-cor(x[,1],x[,2])
    z1h0[i]<-sqrt(n-3)*0.5*(log((1+r)/(1-r))-0)
  }
  z1h0<-sort(z1h0)
  for(i in 1:length(z1h0)){
    if((sum(z1h0>=z1h0[i])/length(z1h0))<=0.05){
      c<-z1h0[i]
      break
    }
  }
  z1h1<-c()
  for(i in 1:m){
    mean=c(0,0)
    sigma<-matrix(c(1,r1,r1,1),nrow=2)
    x<-mvrnorm(n,mean,sigma)
    r<-cor(x[,1],x[,2])
    z1h1[i]<-sqrt(n-3)*0.5*(log((1+r)/(1-r))-0)
  }
  pow<-sum(z1h1>=c)/m
  return(pow)
}
#ARE for left tailed test only
#ARE for Wald's test
library(MASS)
arewtl_r<-function(n,m,r1){
  z1h0<-c()
  for(i in 1:m){
    mean=c(0,0)
    sigma<-matrix(c(1,0,0,1),nrow=2)
    x<-mvrnorm(n,mean,sigma)
    r<-cor(x[,1],x[,2])
    z1h0[i]<-sqrt(n)*(r-0)
  }
  z1h0<-sort(z1h0,decreasing=TRUE)
  for(i in 1:length(z1h0)){
    if((sum(z1h0<=z1h0[i])/length(z1h0))<=0.05){
      c<-z1h0[i]
      break
    }
  }
  z1h1<-c()
  for(i in 1:m){
    mean=c(0,0)
    sigma<-matrix(c(1,r1,r1,1),nrow=2)
    x<-mvrnorm(n,mean,sigma)
    r<-cor(x[,1],x[,2])
    z1h1[i]<-sqrt(n)*(r-0)
  }
  pow<-sum(z1h1<=c)/m
  return(pow)
}
#ARE for VST 
arevstl_r<-function(n,m,r1){
  z1h0<-c()
  for(i in 1:m){
    mean=c(0,0)
    sigma<-matrix(c(1,0,0,1),nrow=2)
    x<-mvrnorm(n,mean,sigma)
    r<-cor(x[,1],x[,2])
    z1h0[i]<-sqrt(n)*0.5*(log((1+r)/(1-r))-0)
  }
  z1h0<-sort(z1h0,decreasing=TRUE)
  for(i in 1:length(z1h0)){
    if((sum(z1h0<=z1h0[i])/length(z1h0))<=0.05){
      c<-z1h0[i]
      break
    }
  }
  z1h1<-c()
  for(i in 1:m){
    mean=c(0,0)
    sigma<-matrix(c(1,r1,r1,1),nrow=2)
    x<-mvrnorm(n,mean,sigma)
    r<-cor(x[,1],x[,2])
    z1h1[i]<-sqrt(n)*0.5*(log((1+r)/(1-r))-0)
  }
  pow<-sum(z1h1<=c)/m
  return(pow)
}
#ARE for modified VST
arevstml_r<-function(n,m,r1){
  z1h0<-c()
  for(i in 1:m){
    mean=c(0,0)
    sigma<-matrix(c(1,0,0,1),nrow=2)
    x<-mvrnorm(n,mean,sigma)
    r<-cor(x[,1],x[,2])
    z1h0[i]<-sqrt(n-3)*0.5*(log((1+r)/(1-r))-0)
  }
  z1h0<-sort(z1h0,decreasing=TRUE)
  for(i in 1:length(z1h0)){
    if((sum(z1h0<=z1h0[i])/length(z1h0))<=0.05){
      c<-z1h0[i]
      break
    }
  }
  z1h1<-c()
  for(i in 1:m){
    mean=c(0,0)
    sigma<-matrix(c(1,r1,r1,1),nrow=2)
    x<-mvrnorm(n,mean,sigma)
    r<-cor(x[,1],x[,2])
    z1h1[i]<-sqrt(n-3)*0.5*(log((1+r)/(1-r))-0)
  }
  pow<-sum(z1h1<=c)/m
  return(pow)
}

############ FOR TEST BASED ON GENERAL THOERY ###############
#### For left tailed test ########
###################
arewtleft_r<-c(5,7,8,10,12,13,15,18,21,25,30,35,43,51,64,
               80,101,131,181,266,413,743,1670,5800)
##################
arewtl_r(5,5000,-0.96)
arewtl_r(7,5000,-0.92)
arewtl_r(8,5000,-0.88)
arewtl_r(10,5000,-0.84)
arewtl_r(12,5000,-0.80)
arewtl_r(13,5000,-0.76)
arewtl_r(15,5000,-0.72)
arewtl_r(18,5000,-0.68)
arewtl_r(21,5000,-0.64)
arewtl_r(25,5000,-0.60)
arewtl_r(30,5000,-0.56)
arewtl_r(35,5000,-0.52)
arewtl_r(43,5000,-0.48)
arewtl_r(51,5000,-0.44)
arewtl_r(64,5000,-0.40)
arewtl_r(80,5000,-0.36)
arewtl_r(101,5000,-0.32)
arewtl_r(131,5000,-0.28)
arewtl_r(181,5000,-0.24)
arewtl_r(266,5000,-0.20)
arewtl_r(413,5000,-0.16)
arewtl_r(743,5000,-0.12)
arewtl_r(1670,5000,-0.08)
arewtl_r(5800,5000,-0.04)
#### For right tailed test ########
####################################
arewtright_r<-c(6790,1649,738,410,265,184,134,103,78,65,51,42,35,
                30,25,22,18,16,13,11,10,8,7,5)
####################################
arewtr_r(6790,5000,0.04)
arewtr_r(1649,5000,0.08)
arewtr_r(738,5000,0.12)
arewtr_r(410,5000,0.16)
arewtr_r(265,5000,0.20)
arewtr_r(184,5000,0.24)
arewtr_r(134,5000,0.28)
arewtr_r(103,5000,0.32)
arewtr_r(78,5000,0.36)
arewtr_r(65,5000,0.40)
arewtr_r(51,5000,0.44)
arewtr_r(42,5000,0.48)
arewtr_r(35,5000,0.52)
arewtr_r(30,5000,0.56)
arewtr_r(25,5000,0.60)
arewtr_r(22,5000,0.64)
arewtr_r(18,5000,0.68)
arewtr_r(16,5000,0.72)
arewtr_r(13,5000,0.76)
arewtr_r(11,5000,0.80)
arewtr_r(10,5000,0.84)
arewtr_r(8,5000,0.88)
arewtr_r(7,5000,0.92)
arewtr_r(5,5000,0.96)
############## FOR TEST BASED ON VST ################
#### For left tailed test ########
###################
arevstleft_r<-c(5,7,8,9,11,13,16,18,21,24,30,36,42,50,62,78,99,129,
                176,261,406,734,1660,5791)
###################
arevstl_r(5,5000,-0.96)
arevstl_r(7,5000,-0.92)
arevstl_r(8,5000,-0.88)
arevstl_r(9,5000,-0.84)
arevstl_r(11,5000,-0.80)
arevstl_r(13,5000,-0.76)
arevstl_r(16,5000,-0.72)
arevstl_r(18,5000,-0.68)
arevstl_r(21,5000,-0.64)
arevstl_r(24,5000,-0.60)
arevstl_r(30,5000,-0.56)
arevstl_r(36,5000,-0.52)
arevstl_r(42,5000,-0.48)
arevstl_r(50,5000,-0.44)
arevstl_r(62,5000,-0.40)
arevstl_r(78,5000,-0.36)
arevstl_r(99,5000,-0.32)
arevstl_r(129,5000,-0.28)
arevstl_r(176,5000,-0.24)
arevstl_r(261,5000,-0.20)
arevstl_r(406,5000,-0.16)
arevstl_r(734,5000,-0.12)
arevstl_r(1660,5000,-0.08)
arevstl_r(5791,5000,-0.04)
#### For right tailed test ########
####################################
arevstright_r<-c(6781,1642,732,404,260,181,131,101,76,63,50,41,
                 34,31,25,20,18,16,13,11,9,8,7,5)
####################################
arevstr_r(6781,5000,0.04)
arevstr_r(1642,5000,0.08)
arevstr_r(732,5000,0.12)
arevstr_r(404,5000,0.16)
arevstr_r(260,5000,0.20)
arevstr_r(181,5000,0.24)
arevstr_r(131,5000,0.28)
arevstr_r(101,5000,0.32)
arevstr_r(76,5000,0.36)
arevstr_r(63,5000,0.40)
arevstr_r(50,5000,0.44)
arevstr_r(41,5000,0.48)
arevstr_r(34,5000,0.52)
arevstr_r(31,5000,0.56)
arevstr_r(25,5000,0.60)
arevstr_r(20,5000,0.64)
arevstr_r(18,5000,0.68)
arevstr_r(16,5000,0.72)
arevstr_r(13,5000,0.76)
arevstr_r(11,5000,0.80)
arevstr_r(9,5000,0.84)
arevstr_r(8,5000,0.88)
arevstr_r(7,5000,0.92)
arevstr_r(5,5000,0.96)
############## FOR TEST BASED ON modified VST ################
#### For left tailed test ########
###################
arevstmleft_r<-c(6,7,8,10,11,13,16,18,21,24,29,35,41,49,60,
                 75,95,126,172,257,402,729,1650,5780)
###################
arevstml_r(6,5000,-0.96)
arevstml_r(7,5000,-0.92)
arevstml_r(8,5000,-0.88)
arevstml_r(10,5000,-0.84)
arevstml_r(11,5000,-0.80)
arevstml_r(13,5000,-0.76)
arevstml_r(16,5000,-0.72)
arevstml_r(18,5000,-0.68)
arevstml_r(21,5000,-0.64)
arevstml_r(24,5000,-0.60)
arevstml_r(29,5000,-0.56)
arevstml_r(35,5000,-0.52)
arevstml_r(41,5000,-0.48)
arevstml_r(49,5000,-0.44)
arevstml_r(60,5000,-0.40)
arevstml_r(75,5000,-0.36)
arevstml_r(95,5000,-0.32)
arevstml_r(126,5000,-0.28)
arevstml_r(172,5000,-0.24)
arevstml_r(257,5000,-0.20)
arevstml_r(402,5000,-0.16)
arevstml_r(729,5000,-0.12)
arevstml_r(1650,5000,-0.08)
arevstml_r(5780,5000,-0.04)
#### For right tailed test ########
####################################
arevstmright_r<-c(6773,1635,728,400,256,178,129,99,74,62,50,40,34,
                  30,24,22,18,16,13,11,9,9,7,6)
####################################

arevstmr_r(6773,5000,0.04)
arevstmr_r(1635,5000,0.08)
arevstmr_r(728,5000,0.12)
arevstmr_r(400,5000,0.16)
arevstmr_r(256,5000,0.20)
arevstmr_r(178,5000,0.24)
arevstmr_r(129,5000,0.28)
arevstmr_r(99,5000,0.32)
arevstmr_r(74,5000,0.36)
arevstmr_r(62,5000,0.40)
arevstmr_r(50,5000,0.44)
arevstmr_r(40,5000,0.48)
arevstmr_r(34,5000,0.52)
arevstmr_r(30,5000,0.56)
arevstmr_r(24,5000,0.60)
arevstmr_r(22,5000,0.64)
arevstmr_r(18,5000,0.68)
arevstmr_r(16,5000,0.72)
arevstmr_r(13,5000,0.76)
arevstmr_r(11,5000,0.80)
arevstmr_r(9,5000,0.84)
arevstmr_r(9,5000,0.88)
arevstmr_r(7,5000,0.92)
arevstmr_r(6,5000,0.96)
######################################
library(tidyverse)
r<-c(seq(-0.96,-0.04,0.04),seq(0.04,0.96,0.04))
length(r)
type<-c(rep("Left tail",24),rep("Right tail",24))
ng<-c(arewtleft_r,arewtright_r)
nv<-c(arevstleft_r,arevstright_r)
nvm<-c(arevstmleft_r,arevstmright_r)
e1<-ng/nv
e2<-nv/nvm
e3<-ng/nvm
aredf1<-data.frame(r,type,ng,nv,nvm,e1,e2,e3)
view(aredf1)
