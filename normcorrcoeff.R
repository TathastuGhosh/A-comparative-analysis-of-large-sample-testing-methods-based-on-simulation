# Large sample test for correlation coefficient (single sample)
library(MASS)
#Wald test
#left tail test
rl<-seq(-1,0,length.out=102)
rl<-rl[-c(1,102)]
rl
rl[c(1,12,23,34,45,56,67,78,89,100)]
powerfuncl_r_wt<-function(p,n,m){
  pow_wt_lt<-c()
  for(i in 1:length(p)){
    ts_lt<-c()
    for(j in 1:m){
      mean=c(0,0)
      sigma<-matrix(c(1,p[i],p[i],1),nrow=2)
      x<-mvrnorm(n,mean,sigma)
      r<-cor(x[,1],x[,2])
      ts_lt[j]<-sqrt(n)*(r-0)
    }
    pow_wt_lt[i]<-sum(ts_lt <= (-1.645))/m
  }
  return(pow_wt_lt)
}
#right tail test 
rr<-seq(0,1,length.out=102)
rr<-rr[-c(1,102)]
rr
powerfuncr_r_wt<-function(p,n,m){
  pow_wt_rt<-c()
  for(i in 1:length(p)){
    ts_rt<-c()
    for(j in 1:m){
      mean=c(0,0)
      sigma<-matrix(c(1,p[i],p[i],1),nrow=2)
      x<-mvrnorm(n,mean,sigma)
      r<-cor(x[,1],x[,2])
      ts_rt[j]<-sqrt(n)*(r-0)
    }
    pow_wt_rt[i]<-sum(ts_rt >= (1.645))/m
  }
  return(pow_wt_rt)
}

#VST test
#Left tailed test
rl<-seq(-1,0,length.out=102)
rl<-rl[-c(1,102)]
rl
powerfuncl_r_vst<-function(p,n,m){
  pow_vst_lt<-c()
  for(i in 1:length(p)){
    ts_lt<-c()
    for(j in 1:m){
      mean=c(0,0)
      sigma<-matrix(c(1,p[i],p[i],1),nrow=2)
      x<-mvrnorm(n,mean,sigma)
      r<-cor(x[,1],x[,2])
      ts_lt[j]<-sqrt(n)*0.5*(log((1+r)/(1-r))-0)
    }
    pow_vst_lt[i]<-sum(ts_lt <= (-1.645))/m
  }
  return(pow_vst_lt)
}
#right tailed test
rr<-seq(0,1,length.out=102)
rr<-rr[-c(1,102)]
rr
powerfuncr_r_vst<-function(p,n,m){
  pow_vst_rt<-c()
  for(i in 1:length(p)){
    ts_rt<-c()
    for(j in 1:m){
      mean=c(0,0)
      sigma<-matrix(c(1,p[i],p[i],1),nrow=2)
      x<-mvrnorm(n,mean,sigma)
      r<-cor(x[,1],x[,2])
      ts_rt[j]<-sqrt(n)*0.5*(log((1+r)/(1-r))-0)
    }
    pow_vst_rt[i]<-sum(ts_rt >= (1.645))/m
  }
  return(pow_vst_rt)
}
#VST modified test (Fisher's Z test) 
#Left tailed test
rl<-seq(-1,0,length.out=102)
rl<-rl[-c(1,102)]
rl
powerfuncl_r_vstm<-function(p,n,m){
  pow_vstm_lt<-c()
  for(i in 1:length(p)){
    ts_lt<-c()
    for(j in 1:m){
      mean=c(0,0)
      sigma<-matrix(c(1,p[i],p[i],1),nrow=2)
      x<-mvrnorm(n,mean,sigma)
      r<-cor(x[,1],x[,2])
      ts_lt[j]<-sqrt(n-3)*0.5*(log((1+r)/(1-r))-0)
    }
    pow_vstm_lt[i]<-sum(ts_lt <= (-1.645))/m
  }
  return(pow_vstm_lt)
}
#right tailed test 
rr<-seq(0,1,length.out=102)
rr<-rr[-c(1,102)]
rr
rr[c(1,12,23,34,45,56,67,78,89,100)]
powerfuncr_r_vstm<-function(p,n,m){
  pow_vstm_rt<-c()
  for(i in 1:length(p)){
    ts_rt<-c()
    for(j in 1:m){
      mean=c(0,0)
      sigma<-matrix(c(1,p[i],p[i],1),nrow=2)
      x<-mvrnorm(n,mean,sigma)
      r<-cor(x[,1],x[,2])
      ts_rt[j]<-sqrt(n-3)*0.5*(log((1+r)/(1-r))-0)
    }
    pow_vstm_rt[i]<-sum(ts_rt >= (1.645))/m
  }
  return(pow_vstm_rt)
}

#m=5000
#left sided test power
powerl_wt_r_m5n10<-powerfuncl_r_wt(rl,n=10,m=5000)
powerl_wt_r_m5n10[c(1,12,23,34,45,56,67,78,89,100)]
powerl_wt_r_m5n30<-powerfuncl_r_wt(rl,n=30,m=5000)
powerl_wt_r_m5n30[c(1,12,23,34,45,56,67,78,89,100)]
powerl_wt_r_m5n50<-powerfuncl_r_wt(rl,n=50,m=5000)
powerl_wt_r_m5n50[c(1,12,23,34,45,56,67,78,89,100)]
powerl_wt_r_m5n100<-powerfuncl_r_wt(rl,n=100,m=5000)
powerl_wt_r_m5n100[c(1,12,23,34,45,56,67,78,89,100)]


powerl_vst_r_m5n10<-powerfuncl_r_vst(rl,n=10,m=5000)
powerl_vst_r_m5n10[c(1,12,23,34,45,56,67,78,89,100)]
powerl_vst_r_m5n30<-powerfuncl_r_vst(rl,n=30,m=5000)
powerl_vst_r_m5n30[c(1,12,23,34,45,56,67,78,89,100)]
powerl_vst_r_m5n30[c(1,12,23,34,45,56,67,78,89,100)]
powerl_vst_r_m5n50<-powerfuncl_r_vst(rl,n=50,m=5000)
powerl_vst_r_m5n50[c(1,12,23,34,45,56,67,78,89,100)]
powerl_vst_r_m5n100<-powerfuncl_r_vst(rl,n=100,m=5000)
powerl_vst_r_m5n100[c(1,12,23,34,45,56,67,78,89,100)]


powerl_vstm_r_m5n10<-powerfuncl_r_vstm(rl,n=10,m=5000)
powerl_vstm_r_m5n10[c(1,12,23,34,45,56,67,78,89,100)]
powerl_vstm_r_m5n10[c(1,12,23,34,45,56,67,78,89,100)]
powerl_vstm_r_m5n30<-powerfuncl_r_vstm(rl,n=30,m=5000)
powerl_vstm_r_m5n30[c(1,12,23,34,45,56,67,78,89,100)]
powerl_vstm_r_m5n50<-powerfuncl_r_vstm(rl,n=50,m=5000)
powerl_vstm_r_m5n50[c(1,12,23,34,45,56,67,78,89,100)]
powerl_vstm_r_m5n100<-powerfuncl_r_vstm(rl,n=100,m=5000)
powerl_vstm_r_m5n100[c(1,12,23,34,45,56,67,78,89,100)]


#Right sided test power
powerr_wt_r_m5n10<-powerfuncr_r_wt(rr,n=10,m=5000)
powerr_wt_r_m5n10[c(1,12,23,34,45,56,67,78,89,100)]
powerr_wt_r_m5n30<-powerfuncr_r_wt(rr,n=30,m=5000)
powerr_wt_r_m5n30[c(1,12,23,34,45,56,67,78,89,100)]
powerr_wt_r_m5n50<-powerfuncr_r_wt(rr,n=50,m=5000)
powerr_wt_r_m5n50[c(1,12,23,34,45,56,67,78,89,100)]
powerr_wt_r_m5n100<-powerfuncr_r_wt(rr,n=100,m=5000)
powerr_wt_r_m5n100[c(1,12,23,34,45,56,67,78,89,100)]


powerr_vst_r_m5n10<-powerfuncr_r_vst(rr,n=10,m=5000)
powerr_vst_r_m5n10[c(1,12,23,34,45,56,67,78,89,100)]
powerr_vst_r_m5n30<-powerfuncr_r_vst(rr,n=30,m=5000)
powerr_vst_r_m5n30[c(1,12,23,34,45,56,67,78,89,100)]
powerr_vst_r_m5n50<-powerfuncr_r_vst(rr,n=50,m=5000)
powerr_vst_r_m5n50[c(1,12,23,34,45,56,67,78,89,100)]
powerr_vst_r_m5n100<-powerfuncr_r_vst(rr,n=100,m=5000)
powerr_vst_r_m5n100[c(1,12,23,34,45,56,67,78,89,100)]



powerr_vstm_r_m5n10<-powerfuncr_r_vstm(rr,n=10,m=5000)
powerr_vstm_r_m5n10[c(1,12,23,34,45,56,67,78,89,100)]
powerr_vstm_r_m5n30<-powerfuncr_r_vstm(rr,n=30,m=5000)
powerr_vstm_r_m5n30[c(1,12,23,34,45,56,67,78,89,100)]
powerr_vstm_r_m5n50<-powerfuncr_r_vstm(rr,n=50,m=5000)
powerr_vstm_r_m5n50[c(1,12,23,34,45,56,67,78,89,100)]
powerr_vstm_r_m5n100<-powerfuncr_r_vstm(rr,n=100,m=5000)
powerr_vstm_r_m5n100[c(1,12,23,34,45,56,67,78,89,100)]

#df for left tail
library(tidyverse)
rleft<-c(rl,rl,rl)
n_1lr<-c(powerl_wt_r_m5n10,powerl_vst_r_m5n10,powerl_vstm_r_m5n10)
n_3lr<-c(powerl_wt_r_m5n30,powerl_vst_r_m5n30,powerl_vstm_r_m5n30)
n_5lr<-c(powerl_wt_r_m5n50,powerl_vst_r_m5n50,powerl_vstm_r_m5n50)
n_hunlr<-c(powerl_wt_r_m5n100,powerl_vst_r_m5n100,powerl_vstm_r_m5n100)
Testtype<-Testtype<-c(rep("General theory",100),rep("VST",100),rep("VSTm",100))
dfl_r<-data.frame(rleft,n_1lr,n_3lr,n_5lr,n_hunlr,Testtype)
view(dfl_r)
#combined plot left sided
library(ggthemes)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(dplyr)
library(MASS)
install.packages("dplyr")
install.packages("tidyr")
ggplot(dfl_r)+
  geom_line(aes(x=rleft,y=n_1lr,color='10'),lwd=0.8)+
  geom_line(aes(x=rleft,y=n_3lr,color='30'),lwd=0.8)+
  geom_line(aes(x=rleft,y=n_5lr,color='50'),lwd=0.8)+
  geom_line(aes(x=rleft,y=n_hunlr,color='100'),lwd=0.8)+
  scale_color_manual(,breaks=c('10','30','50','100'),
                     values=c('10'='red','30'='green','50'='blue','100'='black'))+
  facet_wrap(~Testtype)+
  theme_stata()+
  theme(legend.position="top")+
  labs(col="n :")+
  xlab("Values of correlation coefficient under alternative")+
  ylab("Power")
#n=10 left sided plot
ggplot(dfl_r,aes(rleft,n_1lr))+
  geom_line(aes(col=Testtype),lwd=0.8)+
  scale_color_manual(values=c("green","blue","red"))+
  theme_stata()+
  xlab("Values of p under alternative")+
  ylab("Power")+
  labs(color="Testtype")+
  theme(legend.position="top")+
  xlab("Values of correlation coefficient under alternative")+
  ylab("Power")+
  labs(col="Test type: ")
#n=30 left sided plot
ggplot(dfl_r,aes(rleft,n_3lr))+
  geom_line(aes(col=Testtype),lwd=0.8)+
  scale_color_manual(values=c("green","blue","red"))+
  theme_stata()+
  xlab("Values of p under alternative")+
  ylab("Power")+
  labs(color="Testtype")+
  theme(legend.position="top")+
  xlab("Values of correlation coefficient under alternative")+
  ylab("Power")+
  labs(col="Test type: ")
#n=50 left sided plot
ggplot(dfl_r,aes(rleft,n_5lr))+
  geom_line(aes(col=Testtype),lwd=0.8)+
  scale_color_manual(values=c("green","blue","red"))+
  theme_stata()+
  xlab("Values of p under alternative")+
  ylab("Power")+
  labs(color="Testtype")+
  theme(legend.position="top")+
  xlab("Values of correlation coefficient under alternative")+
  ylab("Power")+
  labs(col="Test type: ")
#n=100 left sided plot
ggplot(dfl_r,aes(rleft,n_hunlr))+
  geom_line(aes(col=Testtype),lwd=0.8)+
  scale_color_manual(values=c("green","blue","red"))+
  theme_stata()+
  xlab("Values of p under alternative")+
  ylab("Power")+
  labs(color="Testtype")+
  theme(legend.position="top")+
  xlab("Values of correlation coefficient under alternative")+
  ylab("Power")+
  labs(col="Test type: ")

#df for right tail
rright<-c(rr,rr,rr)
n_1rr<-c(powerr_wt_r_m5n10,powerr_vst_r_m5n10,powerr_vstm_r_m5n10)
n_3rr<-c(powerr_wt_r_m5n30,powerr_vst_r_m5n30,powerr_vstm_r_m5n30)
n_5rr<-c(powerr_wt_r_m5n50,powerr_vst_r_m5n50,powerr_vstm_r_m5n50)
n_hunrr<-c(powerr_wt_r_m5n100,powerr_vst_r_m5n100,powerr_vstm_r_m5n100)
Testtype<-c(rep("General theory",100),rep("VST",100),rep("VSTm",100))
dfr_r<-data.frame(rright,n_1rr,n_3rr,n_5rr,n_hunrr,Testtype)
view(dfr_r)
#combined plot right sided 
ggplot(dfr_r)+
  geom_line(aes(x=rright,y=n_1rr,color='10'),lwd=0.8)+
  geom_line(aes(x=rright,y=n_3rr,color='30'),lwd=0.8)+
  geom_line(aes(x=rright,y=n_5rr,color='50'),lwd=0.8)+
  geom_line(aes(x=rright,y=n_hunrr,color='100'),lwd=0.8)+
  scale_color_manual(,breaks=c('10','30','50','100'),
                     values=c('10'='red','30'='green','50'='blue','100'='black'))+
  facet_wrap(~Testtype)+
  theme_stata()+
  theme(legend.position="top")+
  labs(col="n :")+
  xlab("Values of correlation coefficient under alternative")+
  ylab("Power")
#n=10 right sided plot
ggplot(dfr_r,aes(rright,n_1rr))+
  geom_line(aes(col=Testtype),lwd=0.8)+
  scale_color_manual(values=c("green","blue","red"))+
  theme_stata()+
  xlab("Values of correlation coefficient under alternative")+
  ylab("Power")+
  labs(color="Testtype")+
  theme(legend.position="top")+
  labs(col="Test type: ")
#n=30 right sided plot
ggplot(dfr_r,aes(rright,n_3rr))+
  geom_line(aes(col=Testtype),lwd=0.8)+
  scale_color_manual(values=c("green","blue","red"))+
  theme_stata()+
  xlab("Values of correlation coefficient under alternative")+
  ylab("Power")+
  labs(color="Testtype")+
  theme(legend.position="top")+
  labs(col="Test type: ")
#n=50 right sided plot
ggplot(dfr_r,aes(rright,n_5rr))+
  geom_line(aes(col=Testtype),lwd=0.8)+
  scale_color_manual(values=c("green","blue","red"))+
  theme_stata()+
  xlab("Values of correlation coefficient under alternative")+
  ylab("Power")+
  labs(color="Testtype")+
  theme(legend.position="top")+
  labs(col="Test type: ")
#n=100 right sided plot
ggplot(dfr_r,aes(rright,n_hunrr))+
  geom_line(aes(col=Testtype),lwd=0.8)+
  scale_color_manual(values=c("green","blue","red"))+
  theme_stata()+
  xlab("Values of correlation coefficient under alternative")+
  ylab("Power")+
  labs(color="Testtype")+
  theme(legend.position="top")+
  labs(col="Test type: ")

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
    # else{
    #   i=i+1
    # }
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
    # else{
    #   i=i+1
    # }
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
    # else{
    #   i=i+1
    # }
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
    # else{
    #   i=i+1
    # }
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
    # else{
    #   i=i+1
    # }
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
    # else{
    #   i=i+1
    # }
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
