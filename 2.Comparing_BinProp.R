library("tidyverse")
library(ggplot2)
library(ggthemes)
#************************************************************************************************************************
                                        # Data frame & Graph related code after here 
#************************************************************************************************************************
#For left tail test
#Difference of power for Wald's test and VST
powerl_wt_vst_m5n10<-powerl_wt_m5n10-powerl_vst_m5n10
powerl_wt_vst_m5n30<-powerl_wt_m5n30-powerl_vst_m5n30
powerl_wt_vst_m5n50<-powerl_wt_m5n50-powerl_vst_m5n50
powerl_wt_vst_m5n100<-powerl_wt_m5n100-powerl_vst_m5n100
pleftdiff<-c(pl,pl,pl,pl)
powerl_diff<-c(powerl_wt_vst_m5n10,powerl_wt_vst_m5n30,powerl_wt_vst_m5n50,powerl_wt_vst_m5n100)
n<-c(rep(10,100),rep(30,100),rep(50,100),rep(100,100))
dfl_diff_wt_vst<-data.frame(pleftdiff,powerl_diff,n)
ggplot(dfl_diff_wt_vst,aes(x=pleftdiff,y=powerl_diff))+
  geom_line(col="blue",lwd=0.8)+
  geom_point()+
  theme_stata()+
  facet_wrap(~n)+
  geom_hline(yintercept = 0)+
  xlab("Values of p under alternative")+
  ylab("Difference")
#Difference for power for VST and VST modified
powerl_vst_vstm_m5n10<-powerl_vst_m5n10-powerl_vstm_m5n10
powerl_vst_vstm_m5n30<-powerl_vst_m5n30-powerl_vstm_m5n30
powerl_vst_vstm_m5n50<-powerl_vst_m5n50-powerl_vstm_m5n50
powerl_vst_vstm_m5n100<-powerl_vst_m5n100-powerl_vstm_m5n100
pleftdiff<-c(pl,pl,pl,pl)
powerl_diff<-c(powerl_vst_vstm_m5n10,powerl_vst_vstm_m5n30,powerl_vst_vstm_m5n50,powerl_vst_vstm_m5n100)
n<-c(rep(10,100),rep(30,100),rep(50,100),rep(100,100))
dfl_diff_vst_vstm<-data.frame(pleftdiff,powerl_diff,n)
ggplot(dfl_diff_vst_vstm,aes(x=pleftdiff,y=powerl_diff))+
  geom_line(col="blue",lwd=0.8)+
  geom_point()+
  theme_stata()+
  facet_wrap(~n)+
  geom_hline(yintercept = 0)+
  xlab("Values of p under alternative")+
  ylab("Difference")
#Difference for power for Wald test and vst modified
powerl_wt_vstm_m5n10<-powerl_wt_m5n10-powerl_vstm_m5n10
powerl_wt_vstm_m5n30<-powerl_wt_m5n30-powerl_vstm_m5n30
powerl_wt_vstm_m5n50<-powerl_wt_m5n50-powerl_vstm_m5n50
powerl_wt_vstm_m5n100<-powerl_wt_m5n100-powerl_vstm_m5n100
pleftdiff<-c(pl,pl,pl,pl)
powerl_diff<-c(powerl_wt_vstm_m5n10,powerl_wt_vstm_m5n30,powerl_wt_vstm_m5n50,powerl_wt_vstm_m5n100)
n<-c(rep(10,100),rep(30,100),rep(50,100),rep(100,100))
dfl_diff_wt_vstm<-data.frame(pleftdiff,powerl_diff,n)
ggplot(dfl_diff_wt_vstm,aes(x=pleftdiff,y=powerl_diff))+
  geom_line(col="blue",lwd=0.8)+
  geom_point()+
  theme_stata()+
  facet_wrap(~n)+
  geom_hline(yintercept = 0)+
  xlab("Values of p under alternative")+
  ylab("Difference")


#For right tail test
#Difference of power for Wald's test and VST
powerr_wt_vst_m5n10<-powerr_wt_m5n10-powerr_vst_m5n10
powerr_wt_vst_m5n30<-powerr_wt_m5n30-powerr_vst_m5n30
powerr_wt_vst_m5n50<-powerr_wt_m5n50-powerr_vst_m5n50
powerr_wt_vst_m5n100<-powerr_wt_m5n100-powerr_vst_m5n100
prightdiff<-c(pr,pr,pr,pr)
powerr_diff<-c(powerr_wt_vst_m5n10,powerr_wt_vst_m5n30,powerr_wt_vst_m5n50,powerr_wt_vst_m5n100)
n<-c(rep(10,100),rep(30,100),rep(50,100),rep(100,100))
dfr_diff_wt_vst<-data.frame(prightdiff,powerr_diff,n)
ggplot(dfr_diff_wt_vst,aes(x=prightdiff,y=powerr_diff))+
  geom_line(col="blue",lwd=0.8)+
  geom_point()+
  theme_stata()+
  facet_wrap(~n)+
  geom_hline(yintercept = 0)+
  xlab("Values of p under alternative")+
  ylab("Difference")
#Difference for power for VST and VST modified
powerr_vst_vstm_m5n10<-powerr_vst_m5n10-powerr_vstm_m5n10
powerr_vst_vstm_m5n30<-powerr_vst_m5n30-powerr_vstm_m5n30
powerr_vst_vstm_m5n50<-powerr_vst_m5n50-powerr_vstm_m5n50
powerr_vst_vstm_m5n100<-powerr_vst_m5n100-powerr_vstm_m5n100
prightdiff<-c(pr,pr,pr,pr)
powerr_diff<-c(powerr_vst_vstm_m5n10,powerr_vst_vstm_m5n30,powerr_vst_vstm_m5n50,powerr_vst_vstm_m5n100)
n<-c(rep(10,100),rep(30,100),rep(50,100),rep(100,100))
dfr_diff_vst_vstm<-data.frame(prightdiff,powerr_diff,n)
ggplot(dfr_diff_vst_vstm,aes(x=prightdiff,y=powerr_diff))+
  geom_line(col="blue",lwd=0.8)+
  geom_point()+
  theme_stata()+
  facet_wrap(~n)+
  geom_hline(yintercept = 0)+
  xlab("Values of p under alternative")+
  ylab("Difference")
#Difference for power for Wald test and vst modified
powerr_wt_vstm_m5n10<-powerr_wt_m5n10-powerr_vstm_m5n10
powerr_wt_vstm_m5n30<-powerr_wt_m5n30-powerr_vstm_m5n30
powerr_wt_vstm_m5n50<-powerr_wt_m5n50-powerr_vstm_m5n50
powerr_wt_vstm_m5n100<-powerr_wt_m5n100-powerr_vstm_m5n100
prightdiff<-c(pr,pr,pr,pr)
powerr_diff<-c(powerr_wt_vstm_m5n10,powerr_wt_vstm_m5n30,powerr_wt_vstm_m5n50,powerr_wt_vstm_m5n100)
n<-c(rep(10,100),rep(30,100),rep(50,100),rep(100,100))
dfr_diff_wt_vstm<-data.frame(prightdiff,powerr_diff,n)
ggplot(dfr_diff_wt_vstm,aes(x=prightdiff,y=powerr_diff))+
  geom_line(col="blue",lwd=0.8)+
  geom_point()+
  theme_stata()+
  facet_wrap(~n)+
  geom_hline(yintercept = 0)+
  xlab("Values of p under alternative")+
  ylab("Difference")
#***************************************************************************************************************
