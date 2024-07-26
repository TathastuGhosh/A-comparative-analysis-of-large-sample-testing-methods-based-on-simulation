# Convergence of test statistic for Bin Prop #

library(MASS)

# For wald test

# Left tail

powerfuncl_r_wt_con<-function(p,n,m){
    ts_lt<-c()
    for(j in 1:m){
      mean=c(0,0)
      sigma<-matrix(c(1,p,p,1),nrow=2)
      x<-mvrnorm(n,mean,sigma)
      r<-cor(x[,1],x[,2])
      ts_lt[j]<-sqrt(n)*(r-0)
    }
  return(ts_lt)
}

# Right tail

powerfuncr_r_wt_con<-function(p,n,m){
    ts_rt<-c()
    for(j in 1:m){
      mean=c(0,0)
      sigma<-matrix(c(1,p,p,1),nrow=2)
      x<-mvrnorm(n,mean,sigma)
      r<-cor(x[,1],x[,2])
      ts_rt[j]<-sqrt(n)*(r-0)
    }
  return(ts_rt)
}


# For VST test

# Left tail

powerfuncl_r_vst_con<-function(p,n,m){
    ts_lt<-c()
    for(j in 1:m){
      mean=c(0,0)
      sigma<-matrix(c(1,p,p,1),nrow=2)
      x<-mvrnorm(n,mean,sigma)
      r<-cor(x[,1],x[,2])
      ts_lt[j]<-sqrt(n)*0.5*(log((1+r)/(1-r))-0)
    }
  return(ts_lt)
}

# Right tail

powerfuncr_r_vst_con<-function(p,n,m){
    ts_rt<-c()
    for(j in 1:m){
      mean=c(0,0)
      sigma<-matrix(c(1,p,p,1),nrow=2)
      x<-mvrnorm(n,mean,sigma)
      r<-cor(x[,1],x[,2])
      ts_rt[j]<-sqrt(n)*0.5*(log((1+r)/(1-r))-0)
    }
  return(ts_rt)
}


# For VsT modified

# Left tail

powerfuncl_r_vstm_con<-function(p,n,m){
    ts_lt<-c()
    for(j in 1:m){
      mean=c(0,0)
      sigma<-matrix(c(1,p,p,1),nrow=2)
      x<-mvrnorm(n,mean,sigma)
      r<-cor(x[,1],x[,2])
      ts_lt[j]<-sqrt(n-3)*0.5*(log((1+r)/(1-r))-0)
    }
  return(ts_lt)
}


# Right tail

powerfuncr_r_vstm_con<-function(p,n,m){
    ts_rt<-c()
    for(j in 1:m){
      mean=c(0,0)
      sigma<-matrix(c(1,p,p,1),nrow=2)
      x<-mvrnorm(n,mean,sigma)
      r<-cor(x[,1],x[,2])
      ts_rt[j]<-sqrt(n-3)*0.5*(log((1+r)/(1-r))-0)
    }
  return(ts_rt)
}


#Wald test
shapiro.test(powerfuncl_r_wt_con(p=-0.90,n=3800,m=5000))
shapiro.test(powerfuncl_r_wt_con(p=-0.80,n=3200,m=5000))
shapiro.test(powerfuncl_r_wt_con(p=-0.50,n=1922,m=5000))
shapiro.test(powerfuncl_r_wt_con(p=-0.20,n=790,m=5000))
shapiro.test(powerfuncl_r_wt_con(p=-0.10,n=180,m=5000))


shapiro.test(powerfuncr_r_wt_con(p=0.90,n=3360,m=5000))
shapiro.test(powerfuncr_r_wt_con(p=0.80,n=3060,m=5000))
shapiro.test(powerfuncr_r_wt_con(p=0.50,n=1800,m=5000))
shapiro.test(powerfuncr_r_wt_con(p=0.20,n=598,m=5000))
shapiro.test(powerfuncr_r_wt_con(p=0.10,n=103,m=5000))


#VST n values
shapiro.test(powerfuncl_r_vst_con(p=-0.90,n=120,m=5000))
shapiro.test(powerfuncl_r_vst_con(p=-0.80,n=24,m=5000))
shapiro.test(powerfuncl_r_vst_con(p=-0.50,n=21,m=5000))
shapiro.test(powerfuncl_r_vst_con(p=-0.20,n=18,m=5000))
shapiro.test(powerfuncl_r_vst_con(p=-0.10,n=16,m=5000))


shapiro.test(powerfuncr_r_vst_con(p=0.90,n=124,m=5000))
shapiro.test(powerfuncr_r_vst_con(p=0.80,n=35,m=5000))
shapiro.test(powerfuncr_r_vst_con(p=0.50,n=27,m=5000))
shapiro.test(powerfuncr_r_vst_con(p=0.20,n=19,m=5000))
shapiro.test(powerfuncr_r_vst_con(p=0.10,n=17,m=5000))


#VST Modified n values

shapiro.test(powerfuncl_r_vstm_con(p=-0.90,n=104,m=5000))
shapiro.test(powerfuncl_r_vstm_con(p=-0.80,n=23,m=5000))
shapiro.test(powerfuncl_r_vstm_con(p=-0.50,n=20,m=5000))
shapiro.test(powerfuncl_r_vstm_con(p=-0.20,n=17,m=5000))
shapiro.test(powerfuncl_r_vstm_con(p=-0.10,n=14,m=5000))

shapiro.test(powerfuncr_r_vstm_con(p=0.90,n=115,m=5000))
shapiro.test(powerfuncr_r_vstm_con(p=0.80,n=26,m=5000))
shapiro.test(powerfuncr_r_vstm_con(p=0.50,n=21,m=5000))
shapiro.test(powerfuncr_r_vstm_con(p=0.20,n=17,m=5000))
shapiro.test(powerfuncr_r_vstm_con(p=0.10,n=16,m=5000))



df1<-data.frame(z=c(scale(powerfuncl_r_wt_con(p=-0.20,n=790,m=5000)),scale(powerfuncl_r_vst_con(p=-0.20,n=18,m=5000)),
                    scale(powerfuncl_r_vstm_con(p=-0.20,n=17,m=5000))),testtype=c(rep("general theory",5000),rep("VST",5000),rep("Modified VST",5000)))
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

df2<-data.frame(z=c(scale(powerfuncl_r_wt_con(p=-0.80,n=3200,m=5000)),scale(powerfuncl_r_vst_con(p=-0.80,n=24,m=5000)),
                    scale(powerfuncl_r_vstm_con(p=-0.80,n=23,m=5000))),testtype=c(rep("general theory",5000),rep("VST",5000),rep("Modified VST",5000)))
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


df3<-data.frame(z=c(scale(powerfuncr_r_wt_con(p=0.20,n=598,m=5000)),scale(powerfuncr_r_vst_con(p=0.20,n=19,m=5000)),
                    scale(powerfuncr_r_vstm_con(p=0.20,n=17,m=5000))),testtype=c(rep("general theory",5000),rep("VST",5000),rep("Modified VST",5000)))
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

df4<-data.frame(z=c(scale(powerfuncr_r_wt_con(p=0.80,n=3060,m=5000)),scale(powerfuncr_r_vst_con(p=0.80,n=35,m=5000)),
                    scale(powerfuncr_r_vstm_con(p=0.80,n=26,m=5000))),testtype=c(rep("general theory",5000),rep("VST",5000),rep("Modified VST",5000)))
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













