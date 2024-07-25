# PROJECT SEM 6 #
#Single Sample Binomial Population
# WALD TEST
#Left tailed test
pl<-seq(0,0.5,length.out=102)
pl<-pl[-c(1,102)]
pl
pl[c(1,12,23,34,45,56,67,78,89,100)]

powerfuncl_wt<-function(p,n,m){
  pow_wt_lt<-c()
  for(i in 1:length(p)){
    ts_lt<-c()
    for(j in 1:m){
      x<-rbinom(n,size=1,prob=p[i])
      phat<-sum(x)/length(x)
      ts_lt[j]<-(phat-0.5)/sqrt((0.5*0.5)/n)
    }
    pow_wt_lt[i]<-sum(ts_lt <= (-1.645))/m
  }
  return(pow_wt_lt)
}
#right tailed test
pr<-seq(0.5,1,length.out=102)
pr<-pr[-c(1,102)]
pr
pr[c(1,12,23,34,45,56,67,78,89,100)]
powerfuncr_wt<-function(p,n,m){
  pow_wt_rt<-c()
  for(i in 1:length(p)){
    ts_rt<-c()
    for(j in 1:m){
      x<-rbinom(n,size=1,prob=p[i])
      phat<-sum(x)/length(x)
      ts_rt[j]<-(phat-0.5)/sqrt((0.5*0.5)/n)
    }
    pow_wt_rt[i]<-sum(ts_rt >= (1.645))/m
  }
  return(pow_wt_rt)
}

# VST TEST
#Left tailed test
pl<-seq(0,0.5,length.out=102)
pl<-pl[-c(1,102)]
pl

powerfuncl_vst<-function(p,n,m){
  pow_vst_lt<-c()
  for(i in 1:length(p)){
    ts_lt<-c()
    for(j in 1:m){
      x<-rbinom(n,size=1,prob=p[i])
      phat<-sum(x)/length(x)
      ts_lt[j]<-2*sqrt(n)*(asin(sqrt(phat))-asin(sqrt(0.5)))
    }
    pow_vst_lt[i]<-sum(ts_lt <= (-1.645))/m
  }
  return(pow_vst_lt)
}
#right tailed test
pr<-seq(0.5,1,length.out=102)
pr<-pr[-c(1,102)]
pr

powerfuncr_vst<-function(p,n,m){
  pow_vst_rt<-c()
  for(i in 1:length(p)){
    ts_rt<-c()
    for(j in 1:m){
      x<-rbinom(n,size=1,prob=p[i])
      phat<-sum(x)/length(x)
      ts_rt[j]<-2*sqrt(n)*(asin(sqrt(phat))-asin(sqrt(0.5)))
    }
    pow_vst_rt[i]<-sum(ts_rt >= (1.645))/m
  }
  return(pow_vst_rt)
}
# VST modified TEST
#Left tailed test

pl<-seq(0,0.5,length.out=102)
pl<-pl[-c(1,102)]
pl

powerfuncl_vstm<-function(p,n,m){
  pow_vstm_lt<-c()
  for(i in 1:length(p)){
    ts_lt<-c()
    for(j in 1:m){
      x<-rbinom(n,size=1,prob=p[i])
      phat<-(sum(x)+(3/8))/(length(x)+(3/4))
      ts_lt[j]<-sqrt(4*n+2)*(asin(sqrt(phat))-asin(sqrt(0.5)))
    }
    pow_vstm_lt[i]<-sum(ts_lt <= (-1.645))/m
  }
  return(pow_vstm_lt)
}
#right tailed test
pr<-seq(0.5,1,length.out=102)
pr<-pr[-c(1,102)]
pr

powerfuncr_vstm<-function(p,n,m){
  pow_vstm_rt<-c()
  for(i in 1:length(p)){
    ts_rt<-c()
    for(j in 1:m){
      x<-rbinom(n,size=1,prob=p[i])
      phat<-(sum(x)+(3/8))/(length(x)+(3/4))
      ts_rt[j]<-sqrt(4*n+2)*(asin(sqrt(phat))-asin(sqrt(0.5)))
    }
    pow_vstm_rt[i]<-sum(ts_rt >= (1.645))/m
  }
  return(pow_vstm_rt)
}
#m=5000
# LEFT SIDED TEST POWER
powerl_wt_m5n10<-powerfuncl_wt(pl,n=10,m=5000)
powerl_wt_m5n10[c(1,12,23,34,45,56,67,78,89,100)]

powerl_wt_m5n30<-powerfuncl_wt(pl,n=30,m=5000)
powerl_wt_m5n30[c(1,12,23,34,45,56,67,78,89,100)]

powerl_wt_m5n50<-powerfuncl_wt(pl,n=50,m=5000)
powerl_wt_m5n50[c(1,12,23,34,45,56,67,78,89,100)]

powerl_wt_m5n100<-powerfuncl_wt(pl,n=100,m=5000)
powerl_wt_m5n100[c(1,12,23,34,45,56,67,78,89,100)]


powerl_vst_m5n10<-powerfuncl_vst(pl,n=10,m=5000)
powerl_vst_m5n10[c(1,12,23,34,45,56,67,78,89,100)]

powerl_vst_m5n30<-powerfuncl_vst(pl,n=30,m=5000)
powerl_vst_m5n30[c(1,12,23,34,45,56,67,78,89,100)]


powerl_vst_m5n50<-powerfuncl_vst(pl,n=50,m=5000)
powerl_vst_m5n50[c(1,12,23,34,45,56,67,78,89,100)]

powerl_vst_m5n100<-powerfuncl_vst(pl,n=100,m=5000)
powerl_vst_m5n100[c(1,12,23,34,45,56,67,78,89,100)]



powerl_vstm_m5n10<-powerfuncl_vstm(pl,n=10,m=5000)
powerl_vstm_m5n10[c(1,12,23,34,45,56,67,78,89,100)]
powerl_vstm_m5n30<-powerfuncl_vstm(pl,n=30,m=5000)
powerl_vstm_m5n30[c(1,12,23,34,45,56,67,78,89,100)]

powerl_vstm_m5n50<-powerfuncl_vstm(pl,n=50,m=5000)
powerl_vstm_m5n50[c(1,12,23,34,45,56,67,78,89,100)]

powerl_vstm_m5n100<-powerfuncl_vstm(pl,n=100,m=5000)
powerl_vstm_m5n100[c(1,12,23,34,45,56,67,78,89,100)]



# RIGHT SIDED TEST POWER

powerr_wt_m5n10<-powerfuncr_wt(pr,n=10,m=5000)
powerr_wt_m5n10[c(1,12,23,34,45,56,67,78,89,100)]
powerr_wt_m5n30<-powerfuncr_wt(pr,n=30,m=5000)
powerr_wt_m5n30[c(1,12,23,34,45,56,67,78,89,100)]

powerr_wt_m5n50<-powerfuncr_wt(pr,n=50,m=5000)
powerr_wt_m5n50[c(1,12,23,34,45,56,67,78,89,100)]

powerr_wt_m5n100<-powerfuncr_wt(pr,n=100,m=5000)
powerr_wt_m5n100[c(1,12,23,34,45,56,67,78,89,100)]



powerr_vst_m5n10<-powerfuncr_vst(pr,n=10,m=5000)
powerr_vst_m5n10[c(1,12,23,34,45,56,67,78,89,100)]


powerr_vst_m5n30<-powerfuncr_vst(pr,n=30,m=5000)
powerr_vst_m5n30[c(1,12,23,34,45,56,67,78,89,100)]

powerr_vst_m5n50<-powerfuncr_vst(pr,n=50,m=5000)
powerr_vst_m5n50[c(1,12,23,34,45,56,67,78,89,100)]

powerr_vst_m5n100<-powerfuncr_vst(pr,n=100,m=5000)
powerr_vst_m5n100[c(1,12,23,34,45,56,67,78,89,100)]


powerr_vstm_m5n10<-powerfuncr_vstm(pr,n=10,m=5000)
powerr_vstm_m5n10[c(1,12,23,34,45,56,67,78,89,100)]
powerr_vstm_m5n30<-powerfuncr_vstm(pr,n=30,m=5000)
powerr_vstm_m5n30[c(1,12,23,34,45,56,67,78,89,100)]

powerr_vstm_m5n50<-powerfuncr_vstm(pr,n=50,m=5000)
powerr_vstm_m5n50[c(1,12,23,34,45,56,67,78,89,100)]

powerr_vstm_m5n100<-powerfuncr_vstm(pr,n=100,m=5000)
powerr_vstm_m5n100[c(1,12,23,34,45,56,67,78,89,100)]


#df for left tail
library(tidyverse)
pleft<-c(pl,pl,pl)
n_1l<-c(powerl_wt_m5n10,powerl_vst_m5n10,powerl_vstm_m5n10)
n_3l<-c(powerl_wt_m5n30,powerl_vst_m5n30,powerl_vstm_m5n30)
n_5l<-c(powerl_wt_m5n50,powerl_vst_m5n50,powerl_vstm_m5n50)
n_hunl<-c(powerl_wt_m5n100,powerl_vst_m5n100,powerl_vstm_m5n100)
Testtype<-c(rep("General theory",100),rep("VST",100),rep("VSTm",100))
dfl<-data.frame(pleft,n_1l,n_3l,n_5l,n_hunl,Testtype)
view(dfl)

#combined plot left sided
library(ggplot2)
library(tidyverse)
dfdf<-dfl%>%
  select(pleft,n_1l,n_3l,n_5l,n_hunl,Testtype)%>%
  gather(key="variable",value="value",-c(pleft,Testtype))
ggplot(dfdf,aes(x=pleft,y=value))+
  geom_line(aes(colour=variable),lwd=0.8)+
  scale_color_manual(labels=c("10","30","50","100"),values=c("red","green","blue","black"))+
  xlab("Values of p under alternative")+
  ylab("Power")+
  facet_wrap(~Testtype)+
  theme_stata()+
  theme(legend.position="top")+
  labs(col="n :")
install.packages("ggthemes")
library(ggthemes)
#n=10 left sided plot
ggplot(dfl,aes(pleft,n_1l))+
  geom_line(aes(col=Testtype),lwd=0.8)+
  scale_color_manual(values=c("green","blue","red"))+
  theme_stata()+
  xlab("Values of p under alternative")+
  ylab("Power")+
  labs(color="Testtype")+
  theme(plot.title = element_text(hjust=0.5))+
  theme(legend.position="top")+
  labs(col="Testing method: ")
#n=30 left sided plot
  ggplot(dfl,aes(pleft,n_3l))+
    geom_line(aes(col=Testtype),lwd=0.8)+
    scale_color_manual(values=c("green","blue","red"))+
    theme_stata()+
    xlab("Values of p under alternative")+
    ylab("Power")+
    labs(color="Test type")+
    theme(legend.position="top")
#n=50 left sided plot
  ggplot(dfl,aes(pleft,n_5l))+
    geom_line(aes(col=Testtype),lwd=0.8)+
    scale_color_manual(values=c("green","blue","red"))+
    theme_stata()+
    xlab("Values of p under alternative")+
    ylab("Power")+
    labs(color="Test type")+
    theme_stata()+
    theme(legend.position="top")
#n=100 left sided plot
  ggplot(dfl,aes(pleft,n_hunl))+
    geom_line(aes(col=Testtype),lwd=0.8)+
    scale_color_manual(values=c("green","blue","red"))+
    theme_stata()+
    xlab("Values of p under alternative")+
    ylab("Power")+
    labs(color="Test type")+
    theme(legend.position="top")
  

#df for right tail 

pright<-c(pr,pr,pr)
n_1r<-c(powerr_wt_m5n10,powerr_vst_m5n10,powerr_vstm_m5n10)
n_3r<-c(powerr_wt_m5n30,powerr_vst_m5n30,powerr_vstm_m5n30)
n_5r<-c(powerr_wt_m5n50,powerr_vst_m5n50,powerr_vstm_m5n50)
n_hunr<-c(powerr_wt_m5n100,powerr_vst_m5n100,powerr_vstm_m5n100)
Testtype<-c(rep("General theory",100),rep("VST",100),rep("VSTm",100))
dfr<-data.frame(pright,n_1r,n_3r,n_5r,n_hunr,Testtype)
view(dfr)

# combined plot right sided
library("ggplot2")
library("tidyverse")
dfdfdf<-dfr%>%
  select(pright,n_1r,n_3r,n_5r,n_hunr,Testtype)%>%
  gather(key="variable",value="value",-c(pright,Testtype))
ggplot(dfdfdf,aes(x=pright,y=value))+
  geom_line(aes(colour=variable),lwd=0.8)+
  scale_colour_manual(labels=c("10","30","50","100"),values=c("red","green","blue","black"))+
  xlab("values of p under alternative")+
  ylab("Power")+
  facet_wrap(~Testtype)+
  theme_stata()+
  theme(legend.position="top")
#n=10 right sided plot
ggplot(dfr,aes(pright,n_1r))+
  geom_line(aes(col=Testtype),lwd=0.8)+
  scale_color_manual(values=c("green","blue","red"))+
  theme_stata()+
  xlab("Values of p under alternative")+
  ylab("Power")+
  labs(color="Test type")+
  theme(legend.position="top")
#n=30 right sided plot
ggplot(dfr,aes(pright,n_3r))+
  geom_line(aes(col=Testtype),lwd=0.8)+
  scale_color_manual(values=c("green","blue","red"))+
  theme_stata()+
  xlab("Values of p under alternative")+
  ylab("Power")+
  labs(color="Test type")+
  theme(legend.position="top")
#n=50 right sided plot
ggplot(dfr,aes(pright,n_5r))+
  geom_line(aes(col=Testtype),lwd=0.8)+
  scale_color_manual(values=c("green","blue","red"))+
  xlab("Values of p under alternative")+
  ylab("Power")+
  labs(color="Test type")+
  theme_stata()+
  theme(legend.position="top")
#n=100 right sided plot
ggplot(dfr,aes(pright,n_hunr))+
  geom_line(aes(col=Testtype),lwd=0.8)+
  scale_color_manual(values=c("green","blue","red"))+
  theme_stata()+
  xlab("Values of p under alternative")+
  ylab("Power")+
  labs(color="Test type")+
  theme(legend.position="top")


#checking how many powers are greater or equal to

# sum(powerl_vst_m5n10>=powerl_wt_m5n10)
# sum(powerl_vst_m5n30>=powerl_wt_m5n30)
# sum(powerl_vst_m5n50>=powerl_wt_m5n50)
# sum(powerl_vst_m5n100>=powerl_wt_m5n100)
# 
# sum(powerr_vst_m5n10>=powerr_wt_m5n10)
# sum(powerr_vst_m5n30>=powerr_wt_m5n30)
# sum(powerr_vst_m5n50>=powerr_wt_m5n50)
# sum(powerr_vst_m5n100>=powerr_wt_m5n100)


# sum(powerl_wt_m5n10==powerl_vst_m5n10)
# sum(powerl_wt_m5n30==powerl_vst_m5n30)
# sum(powerl_wt_m5n50==powerl_vst_m5n50)
# sum(powerl_wt_m5n100==powerl_vst_m5n100)

# sum(powerr_wt_m5n10==powerr_vst_m5n10)
# sum(powerr_wt_m5n30==powerr_vst_m5n30)
# sum(powerr_wt_m5n50==powerr_vst_m5n50)
# sum(powerr_wt_m5n100==powerr_vst_m5n100)


################**********************************************#####################
################*********************************************######################
#Comparing using Asymptotic relative efficiency(ARE)

# ARE for right tailed test only
#ARE for Wald's test
arewtr<-function(n,m,p1){
  z1h0<-c()
  for(i in 1:m){
    x<-rbinom(n,size=1,prob=0.5)
    phath0<-sum(x)/length(x)
    z1h0[i]<-(phath0-0.5)/sqrt((0.5*0.5)/n)
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
    x<-rbinom(n,size=1,prob=p1)
    phath1<-sum(x)/length(x)
    z1h1[i]<-(phath1-0.5)/sqrt((0.5*0.5)/n)
  }
  pow<-sum(z1h1>=c)/m
  return(pow)
}
#ARE for VST
arevstr<-function(n,m,p1){
  z2h0<-c()
  for(i in 1:m){
    x<-rbinom(n,size=1,prob=0.5)
    phath0<-sum(x)/length(x)
    z2h0[i]<-2*sqrt(n)*(asin(sqrt(phath0))-asin(sqrt(0.5)))
  }
  z2h0<-sort(z2h0)
  for(i in 1:length(z2h0)){
    if((sum(z2h0>=z2h0[i])/length(z2h0))<=0.05){
      c<-z2h0[i]
      break
    }
    # else{
    #   i=i+1
    # }
  }
  z2h1<-c()
  for(i in 1:m){
    x<-rbinom(n,size=1,prob=p1)
    phath1<-sum(x)/length(x)
    z2h1[i]<-2*sqrt(n)*(asin(sqrt(phath1))-asin(sqrt(0.5)))
  }
  pow<-sum(z2h1>=c)/m
  return(pow)
}
#ARE for VST modified
arevstmr<-function(n,m,p1){
  z3h0<-c()
  for(i in 1:m){
    x<-rbinom(n,size=1,prob=0.5)
    phath0<-(sum(x)+(3/8))/(length(x)+(3/4))
    z3h0[i]<-sqrt(4*n+2)*(asin(sqrt(phath0))-asin(sqrt(0.5)))
  }
  z3h0<-sort(z3h0)
  for(i in 1:length(z3h0)){
    if((sum(z3h0>=z3h0[i])/length(z3h0))<=0.05){
      c<-z3h0[i]
      break
    }
    # else{
    #   i=i+1
    # }
  }
  z3h1<-c()
  for(i in 1:m){
    x<-rbinom(n,size=1,prob=p1)
    phath1<-(sum(x)+(3/8))/(length(x)+(3/4))
    z3h1[i]<-sqrt(4*n+2)*(asin(sqrt(phath1))-asin(sqrt(0.5)))
  }
  pow<-sum(z3h1>=c)/m
  return(pow)
}

# ARE for left tailed test only
# ARE for Wald's test
arewtl<-function(n,m,p1){
  z1h0<-c()
  for(i in 1:m){
    x<-rbinom(n,size=1,prob=0.5)
    phath0<-sum(x)/length(x)
    z1h0[i]<-(phath0-0.5)/sqrt((0.5*0.5)/n)
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
    x<-rbinom(n,size=1,prob=p1)
    phath1<-sum(x)/length(x)
    z1h1[i]<-(phath1-0.5)/sqrt((0.5*0.5)/n)
  }
  pow<-sum(z1h1<=c)/m
  return(pow)
}

#ARE for VST
arevstl<-function(n,m,p1){
  z2h0<-c()
  for(i in 1:m){
    x<-rbinom(n,size=1,prob=0.5)
    phath0<-sum(x)/length(x)
    z2h0[i]<-2*sqrt(n)*(asin(sqrt(phath0))-asin(sqrt(0.5)))
  }
  z2h0<-sort(z2h0,decreasing=TRUE)
  for(i in 1:length(z2h0)){
    if((sum(z2h0<=z2h0[i])/length(z2h0))<=0.05){
      c<-z2h0[i]
      break
    }
    # else{
    #   i=i+1
    # }
  }
  z2h1<-c()
  for(i in 1:m){
    x<-rbinom(n,size=1,prob=p1)
    phath1<-sum(x)/length(x)
    z2h1[i]<-2*sqrt(n)*(asin(sqrt(phath1))-asin(sqrt(0.5)))
  }
  pow<-sum(z2h1<=c)/m
  return(pow)
}
#ARE for VST modified
arevstml<-function(n,m,p1){
  z3h0<-c()
  for(i in 1:m){
    x<-rbinom(n,size=1,prob=0.5)
    phath0<-(sum(x)+(3/8))/(length(x)+(3/4))
    z3h0[i]<-sqrt(4*n+2)*(asin(sqrt(phath0))-asin(sqrt(0.5)))
  }
  z3h0<-sort(z3h0,decreasing = TRUE)
  for(i in 1:length(z3h0)){
    if((sum(z3h0<=z3h0[i])/length(z3h0))<=0.05){
      c<-z3h0[i]
      break
    }
    # else{
    #   i=i+1
    # }
  }
  z3h1<-c()
  for(i in 1:m){
    x<-rbinom(n,size=1,prob=p1)
    phath1<-(sum(x)+(3/8))/(length(x)+(3/4))
    z3h1[i]<-sqrt(4*n+2)*(asin(sqrt(phath1))-asin(sqrt(0.5)))
  }
  pow<-sum(z3h1<=c)/m
  return(pow)
}
