# Convergence of test statistic for Bin Prop #
# For wald test
# Left tail
powerfuncl_wt_con<-function(p,n,m){
    ts_lt<-c()
    for(j in 1:m){
      x<-rbinom(n,size=1,prob=p)
      phat<-sum(x)/length(x)
      ts_lt[j]<-(phat-0.5)/sqrt((0.5*0.5)/n)
    }
  return(ts_lt)
}
# Right tail
powerfuncr_wt_con<-function(p,n,m){
    ts_rt<-c()
    for(j in 1:m){
      x<-rbinom(n,size=1,prob=p)
      phat<-sum(x)/length(x)
      ts_rt[j]<-(phat-0.5)/sqrt((0.5*0.5)/n)
    }
  return(ts_rt)
}
# For VST 
# Left tail
powerfuncl_vst_con<-function(p,n,m){
    ts_lt<-c()
    for(j in 1:m){
      x<-rbinom(n,size=1,prob=p)
      phat<-sum(x)/length(x)
      ts_lt[j]<-2*sqrt(n)*(asin(sqrt(phat))-asin(sqrt(0.5)))
    }
  return(ts_lt)
}
# Right tail 
powerfuncr_vst_con<-function(p,n,m){
    ts_rt<-c()
    for(j in 1:m){
      x<-rbinom(n,size=1,prob=p)
      phat<-sum(x)/length(x)
      ts_rt[j]<-2*sqrt(n)*(asin(sqrt(phat))-asin(sqrt(0.5)))
    }
  return(ts_rt)
}
# For VST Modified
# Left tail 
powerfuncl_vstm_con<-function(p,n,m){
    ts_lt<-c()
    for(j in 1:m){
      x<-rbinom(n,size=1,prob=p)
      phat<-(sum(x)+(3/8))/(length(x)+(3/4))
      ts_lt[j]<-sqrt(4*n+2)*(asin(sqrt(phat))-asin(sqrt(0.5)))
    }
  return(ts_lt)
}
# Right tail
powerfuncr_vstm_con<-function(p,n,m){
    ts_rt<-c()
    for(j in 1:m){
      x<-rbinom(n,size=1,prob=p)
      phat<-(sum(x)+(3/8))/(length(x)+(3/4))
      ts_rt[j]<-sqrt(4*n+2)*(asin(sqrt(phat))-asin(sqrt(0.5)))
    }
  return(ts_rt)
}
#Wald n values
shapiro.test(powerfuncl_wt_con(p=0.05,n=7300,m=5000))
shapiro.test(powerfuncl_wt_con(p=0.10,n=3400,m=5000))
shapiro.test(powerfuncl_wt_con(p=0.25,n=1500,m=5000))
shapiro.test(powerfuncl_wt_con(p=0.40,n=1432,m=5000))
shapiro.test(powerfuncl_wt_con(p=0.45,n=1350,m=5000))


shapiro.test(powerfuncr_wt_con(p=0.55,n=1273,m=5000))
shapiro.test(powerfuncr_wt_con(p=0.60,n=1429,m=5000))
shapiro.test(powerfuncr_wt_con(p=0.75,n=1503,m=5000))
shapiro.test(powerfuncr_wt_con(p=0.90,n=3980,m=5000))
shapiro.test(powerfuncr_wt_con(p=0.95,n=6999,m=5000))

#VST n values
shapiro.test(powerfuncl_vst_con(p=0.05,n=7005,m=5000))
shapiro.test(powerfuncl_vst_con(p=0.10,n=3130,m=5000))
shapiro.test(powerfuncl_vst_con(p=0.25,n=1240,m=5000))
shapiro.test(powerfuncl_vst_con(p=0.40,n=1130,m=5000))
shapiro.test(powerfuncl_vst_con(p=0.45,n=1230,m=5000))


shapiro.test(powerfuncr_vst_con(p=0.55,n=1205,m=5000))
shapiro.test(powerfuncr_vst_con(p=0.60,n=1370,m=5000))
shapiro.test(powerfuncr_vst_con(p=0.75,n=1475,m=5000))
shapiro.test(powerfuncr_vst_con(p=0.90,n=3100,m=5000))
shapiro.test(powerfuncr_vst_con(p=0.95,n=5900,m=5000))



#Modified VST n values
shapiro.test(powerfuncl_vstm_con(p=0.05,n=6600,m=5000))
shapiro.test(powerfuncl_vstm_con(p=0.10,n=2910,m=5000))
shapiro.test(powerfuncl_vstm_con(p=0.25,n=1210,m=5000))
shapiro.test(powerfuncl_vstm_con(p=0.40,n=1090,m=5000))
shapiro.test(powerfuncl_vstm_con(p=0.45,n=1180,m=5000))


shapiro.test(powerfuncr_vstm_con(p=0.55,n=1010,m=5000))
shapiro.test(powerfuncr_vstm_con(p=0.60,n=1310,m=5000))
shapiro.test(powerfuncr_vstm_con(p=0.75,n=1430,m=5000))
shapiro.test(powerfuncr_vstm_con(p=0.90,n=2880,m=5000))
shapiro.test(powerfuncr_vstm_con(p=0.95,n=5750,m=5000))


#************************************************************************************************************************
                                        # Data frame & Graph related code after here 
#************************************************************************************************************************
library(tidyverse)
library(ggthemes)
df1<-data.frame(z=c(scale(powerfuncl_wt_con(p=0.10,n=3400,m=5000)),scale(powerfuncl_vst_con(p=0.10,n=3130,m=5000)),
                    scale(powerfuncl_vstm_con(p=0.10,n=2910,m=5000))),testtype=c(rep("general theory",5000),rep("VST",5000),rep("Modified VST",5000)))
view(df1)
ggplot(df1,aes(sample=z,colour=factor(testtype)))+
  facet_wrap(~testtype)+
  stat_qq()+
  stat_qq_line()+
  theme_bw()+
  theme(legend.position="none")+
  #labs(col="Test:")+
  scale_color_manual(values = c("#FF6E40", "#9080FF", "#FFBB00"))+
  labs(y="Sample Quantiles",x="Theoritical quantiles")+
  theme(
    axis.title.x = element_text(size=11, face="bold"),
    axis.title.y = element_text(size=11, face="bold")
  )

df2<-data.frame(z=c(scale(powerfuncl_wt_con(p=0.40,n=1432,m=5000)),scale(powerfuncl_vst_con(p=0.40,n=1130,m=5000)),
                    scale(powerfuncl_vstm_con(p=0.40,n=1090,m=5000))),testtype=c(rep("general theory",5000),rep("VST",5000),rep("Modified VST",5000)))
view(df2)
ggplot(df2,aes(sample=z,colour=factor(testtype)))+
  facet_wrap(~testtype)+
  stat_qq()+
  stat_qq_line()+
  theme_bw()+
  theme(legend.position="none")+
  #labs(col="Test:")+
  scale_color_manual(values = c("#FF6E40", "#9080FF", "#FFBB00"))+
  labs(y="Sample Quantiles",x="Theoritical quantiles")+
  theme(
    axis.title.x = element_text(size=11, face="bold"),
    axis.title.y = element_text(size=11, face="bold")
  )

df3<-data.frame(z=c(scale(powerfuncr_wt_con(p=0.60,n=1429,m=5000)),scale(powerfuncr_vst_con(p=0.60,n=1370,m=5000)),
                    scale(powerfuncr_vstm_con(p=0.60,n=1310,m=5000))),testtype=c(rep("general theory",5000),rep("VST",5000),rep("Modified VST",5000)))
view(df3)
ggplot(df3,aes(sample=z,colour=factor(testtype)))+
  facet_wrap(~testtype)+
  stat_qq()+
  stat_qq_line()+
  theme_bw()+
  theme(legend.position="none")+
  #labs(col="Test:")+
  scale_color_manual(values = c("#FF6E40", "#9080FF", "#FFBB00"))+
  labs(y="Sample Quantiles",x="Theoritical quantiles")+
  theme(
    axis.title.x = element_text(size=11, face="bold"),
    axis.title.y = element_text(size=11, face="bold")
  )

df4<-data.frame(z=c(scale(powerfuncr_wt_con(p=0.90,n=3980,m=5000)),scale(powerfuncr_vst_con(p=0.90,n=3100,m=5000)),
                    scale(powerfuncr_vstm_con(p=0.90,n=2880,m=5000))),testtype=c(rep("general theory",5000),rep("VST",5000),rep("Modified VST",5000)))
view(df4)
ggplot(df4,aes(sample=z,colour=factor(testtype)))+
  facet_wrap(~testtype)+
  stat_qq()+
  stat_qq_line()+
  theme_bw()+
  theme(legend.position="none")+
  #labs(col="Test:")+
  scale_color_manual(values = c("#FF6E40", "#9080FF", "#FFBB00"))+
  labs(y="Sample Quantiles",x="Theoritical quantiles")+
  theme(
    axis.title.x = element_text(size=11, face="bold"),
    axis.title.y = element_text(size=11, face="bold")
  )
#******************************************************************************************************
