#Consistency of a test #
# Points for LT test:  0.05,0.10,0.25,0.40,0.45
# Points for RT test:  0.55,0.60,0.75,0.90,0.95
library(tidyverse)
library(ggplot2)

gt_lt_cons<-c(
powerfuncl_wt(0.05,10,5000),
powerfuncl_wt(0.05,30,5000),
powerfuncl_wt(0.05,50,5000),
powerfuncl_wt(0.05,70,5000),
powerfuncl_wt(0.05,100,5000),

powerfuncl_wt(0.10,10,5000),
powerfuncl_wt(0.10,30,5000),
powerfuncl_wt(0.10,50,5000),
powerfuncl_wt(0.10,70,5000),
powerfuncl_wt(0.10,100,5000),

powerfuncl_wt(0.25,10,5000),
powerfuncl_wt(0.25,30,5000),
powerfuncl_wt(0.25,50,5000),
powerfuncl_wt(0.25,70,5000),
powerfuncl_wt(0.25,100,5000),

powerfuncl_wt(0.40,10,5000),
powerfuncl_wt(0.40,30,5000),
powerfuncl_wt(0.40,50,5000),
powerfuncl_wt(0.40,70,5000),
powerfuncl_wt(0.40,100,5000),

powerfuncl_wt(0.45,10,5000),
powerfuncl_wt(0.45,30,5000),
powerfuncl_wt(0.45,50,5000),
powerfuncl_wt(0.45,70,5000),
powerfuncl_wt(0.45,100,5000)
)
vst_lt_cons<-c(
powerfuncl_vst(0.05,10,5000),
powerfuncl_vst(0.05,30,5000),
powerfuncl_vst(0.05,50,5000),
powerfuncl_vst(0.05,70,5000),
powerfuncl_vst(0.05,100,5000),

powerfuncl_vst(0.10,10,5000),
powerfuncl_vst(0.10,30,5000),
powerfuncl_vst(0.10,50,5000),
powerfuncl_vst(0.10,70,5000),
powerfuncl_vst(0.10,100,5000),

powerfuncl_vst(0.25,10,5000),
powerfuncl_vst(0.25,30,5000),
powerfuncl_vst(0.25,50,5000),
powerfuncl_vst(0.25,70,5000),
powerfuncl_vst(0.25,100,5000),

powerfuncl_vst(0.40,10,5000),
powerfuncl_vst(0.40,30,5000),
powerfuncl_vst(0.40,50,5000),
powerfuncl_vst(0.40,70,5000),
powerfuncl_vst(0.40,100,5000),

powerfuncl_vst(0.45,10,5000),
powerfuncl_vst(0.45,30,5000),
powerfuncl_vst(0.45,50,5000),
powerfuncl_vst(0.45,70,5000),
powerfuncl_vst(0.45,100,5000)
)

vstm_lt_cons<-c(
powerfuncl_vstm(0.05,10,5000),
powerfuncl_vstm(0.05,30,5000),
powerfuncl_vstm(0.05,50,5000),
powerfuncl_vstm(0.05,70,5000),
powerfuncl_vstm(0.05,100,5000),

powerfuncl_vstm(0.10,10,5000),
powerfuncl_vstm(0.10,30,5000),
powerfuncl_vstm(0.10,50,5000),
powerfuncl_vstm(0.10,70,5000),
powerfuncl_vstm(0.10,100,5000),

powerfuncl_vstm(0.25,10,5000),
powerfuncl_vstm(0.25,30,5000),
powerfuncl_vstm(0.25,50,5000),
powerfuncl_vstm(0.25,70,5000),
powerfuncl_vstm(0.25,100,5000),

powerfuncl_vstm(0.40,10,5000),
powerfuncl_vstm(0.40,30,5000),
powerfuncl_vstm(0.40,50,5000),
powerfuncl_vstm(0.40,70,5000),
powerfuncl_vstm(0.40,100,5000),

powerfuncl_vstm(0.45,10,5000),
powerfuncl_vstm(0.45,30,5000),
powerfuncl_vstm(0.45,50,5000),
powerfuncl_vstm(0.45,70,5000),
powerfuncl_vstm(0.45,100,5000) 
)

gt_rt_cons<-c(
powerfuncr_wt(0.55,10,5000),
powerfuncr_wt(0.55,30,5000),
powerfuncr_wt(0.55,50,5000),
powerfuncr_wt(0.55,70,5000),
powerfuncr_wt(0.55,100,5000),
  
powerfuncr_wt(0.60,10,5000),
powerfuncr_wt(0.60,30,5000),
powerfuncr_wt(0.60,50,5000),
powerfuncr_wt(0.60,70,5000),
powerfuncr_wt(0.60,100,5000),
  
powerfuncr_wt(0.75,10,5000),
powerfuncr_wt(0.75,30,5000),
powerfuncr_wt(0.75,50,5000),
powerfuncr_wt(0.75,70,5000),
powerfuncr_wt(0.75,100,5000),
  
powerfuncr_wt(0.90,10,5000),
powerfuncr_wt(0.90,30,5000),
powerfuncr_wt(0.90,50,5000),
powerfuncr_wt(0.90,70,5000),
powerfuncr_wt(0.90,100,5000),
  
powerfuncr_wt(0.95,10,5000),
powerfuncr_wt(0.95,30,5000),
powerfuncr_wt(0.95,50,5000),
powerfuncr_wt(0.95,70,5000),
powerfuncr_wt(0.95,100,5000)
)
vst_rt_cons<-c(
powerfuncr_vst(0.55,10,5000),
powerfuncr_vst(0.55,30,5000),
powerfuncr_vst(0.55,50,5000),
powerfuncr_vst(0.55,70,5000),
powerfuncr_vst(0.55,100,5000),
  
powerfuncr_vst(0.60,10,5000),
powerfuncr_vst(0.60,30,5000),
powerfuncr_vst(0.60,50,5000),
powerfuncr_vst(0.60,70,5000),
powerfuncr_vst(0.60,100,5000),
  
powerfuncr_vst(0.75,10,5000),
powerfuncr_vst(0.75,30,5000),
powerfuncr_vst(0.75,50,5000),
powerfuncr_vst(0.75,70,5000),
powerfuncr_vst(0.75,100,5000),
  
powerfuncr_vst(0.90,10,5000),
powerfuncr_vst(0.90,30,5000),
powerfuncr_vst(0.90,50,5000),
powerfuncr_vst(0.90,70,5000),
powerfuncr_vst(0.90,100,5000),
  
powerfuncr_vst(0.95,10,5000),
powerfuncr_vst(0.95,30,5000),
powerfuncr_vst(0.95,50,5000),
powerfuncr_vst(0.95,70,5000),
powerfuncr_vst(0.95,100,5000)  
)
vstm_rt_cons<-c(
powerfuncr_vstm(0.55,10,5000),
powerfuncr_vstm(0.55,30,5000),
powerfuncr_vstm(0.55,50,5000),
powerfuncr_vstm(0.55,70,5000),
powerfuncr_vstm(0.55,100,5000),
  
powerfuncr_vstm(0.60,10,5000),
powerfuncr_vstm(0.60,30,5000),
powerfuncr_vstm(0.60,50,5000),
powerfuncr_vstm(0.60,70,5000),
powerfuncr_vstm(0.60,100,5000),
  
powerfuncr_vstm(0.75,10,5000),
powerfuncr_vstm(0.75,30,5000),
powerfuncr_vstm(0.75,50,5000),
powerfuncr_vstm(0.75,70,5000),
powerfuncr_vstm(0.75,100,5000),
  
powerfuncr_vstm(0.90,10,5000),
powerfuncr_vstm(0.90,30,5000),
powerfuncr_vstm(0.90,50,5000),
powerfuncr_vstm(0.90,70,5000),
powerfuncr_vstm(0.90,100,5000),
  
powerfuncr_vstm(0.95,10,5000),
powerfuncr_vstm(0.95,30,5000),
powerfuncr_vstm(0.95,50,5000),
powerfuncr_vstm(0.95,70,5000),
powerfuncr_vstm(0.95,100,5000)
)
testmethod<-c(rep("General theory",25),rep("VST",25),rep("modified VST",25),rep("General theory",25),rep("VST",25),rep("modified VST",25))
tail<-c(rep("left tail",75),rep("right tail",75))
nn<-c(rep(c(10,30,50,70,100),15),rep(c(10,30,50,70,100),15))
altval<-as.character(c
                     (rep(0.05,5),rep(0.10,5),rep(0.25,5),rep(0.40,5),rep(0.45,5),
                      rep(0.05,5),rep(0.10,5),rep(0.25,5),rep(0.40,5),rep(0.45,5),
                      rep(0.05,5),rep(0.10,5),rep(0.25,5),rep(0.40,5),rep(0.45,5),
                     c
                    (rep(0.55,5),rep(0.60,5),rep(0.75,5),rep(0.90,5),rep(0.95,5),
                      rep(0.55,5),rep(0.60,5),rep(0.75,5),rep(0.90,5),rep(0.95,5),
                      rep(0.55,5),rep(0.60,5),rep(0.75,5),rep(0.90,5),rep(0.95,5))))

cons<-c(gt_lt_cons,vst_lt_cons,vstm_lt_cons,gt_rt_cons,vst_rt_cons,vstm_rt_cons)
cons_p_df<-data.frame(testmethod,tail,altval,nn,cons)
view(cons_p_df)
library(ggthemes)

#************************************************************************************************************************
                                        # Data frame & Graph related code after here 
#************************************************************************************************************************

# FOR LEFT TAIL TESTS
cons_p_df%>%
  filter(testmethod=="General theory" & tail=="left tail")%>%
  ggplot(aes(nn,cons,colour=altval))+
  geom_line(size=0.75)+
  geom_point(shape=19,size=2)+
  scale_color_manual(labels=c("0.05","0.10","0.25","0.40","0.45"),values=c("#FF6E40","#9080FF","#FFBB00","#1955AD","#1E847F"))+
  labs(col="Alternative:")+
 theme_bw()+
  xlab("Sample sizes")+
  ylab("Power")+
  theme(
    axis.title.x = element_text(size=11, face="bold"),
    axis.title.y = element_text(size=11, face="bold")
  )+
  theme(legend.position ="top")+
  theme(legend.key = element_rect(fill = "white", colour = "black"))+
  theme(legend.title = element_text(face = "bold"))
  



cons_p_df%>%
  filter(testmethod=="VST" & tail=="left tail")%>%
  ggplot(aes(nn,cons,colour=altval))+
  geom_line(size=0.9)+
  geom_point(shape=19,size=2)+
  scale_color_manual(labels=c("0.05","0.10","0.25","0.40","0.45"),values=c("#FF6E40","#9080FF","#FFBB00","#1955AD","#1E847F"))+
  labs(col="Alternative:")+
  theme_bw()+
  xlab("Sample sizes")+
  ylab("Power")+
  theme(axis.title.x = element_text(size=11, face="bold"),axis.title.y = element_text(size=11, face="bold"))+
  theme(legend.position ="top")+
  theme(legend.key = element_rect(fill = "white", colour = "black"))+
  theme(legend.title = element_text(face = "bold"))


cons_p_df%>%
  filter(testmethod=="modified VST" & tail=="left tail")%>%
  ggplot(aes(nn,cons,colour=altval))+
  geom_line(size=0.9)+
  geom_point(shape=19,size=2)+
  scale_color_manual(labels=c("0.05","0.10","0.25","0.40","0.45"),values=c("#FF6E40","#9080FF","#FFBB00","#1955AD","#1E847F"))+
  labs(col="Alternative:")+
  theme_bw()+
  xlab("Sample sizes")+
  ylab("Power")+
  theme(axis.title.x = element_text(size=11, face="bold"),axis.title.y = element_text(size=11, face="bold"))+
  theme(legend.position ="top")+
  theme(legend.key = element_rect(fill = "white", colour = "black"))+
  theme(legend.title = element_text(face = "bold"))


# FOR RIGHT TAIL TEST
cons_p_df%>%
  filter(testmethod=="General theory" & tail=="right tail")%>%
  ggplot(aes(nn,cons,colour=altval))+
  geom_line(size=0.75)+
  geom_point(shape=19,size=2)+
  scale_color_manual(labels=c("0.55","0.60","0.75","0.90","0.95"),values=c("#FF6E40","#9080FF","#FFBB00","#1955AD","#1E847F"))+
  labs(col="Alternative:")+
  theme_bw()+
  xlab("Sample sizes")+
  ylab("Power")+
  theme(
    axis.title.x = element_text(size=11, face="bold"),
    axis.title.y = element_text(size=11, face="bold")
  )+
  theme(legend.position ="top")+
  theme(legend.key = element_rect(fill = "white", colour = "black"))+
  theme(legend.title = element_text(face = "bold"))


cons_p_df%>%
  filter(testmethod=="VST" & tail=="right tail")%>%
  ggplot(aes(nn,cons,colour=altval))+
  geom_line(size=0.9)+
  geom_point(shape=19,size=2)+
  scale_color_manual(labels=c("0.55","0.60","0.75","0.90","0.95"),values=c("#FF6E40","#9080FF","#FFBB00","#1955AD","#1E847F"))+
  labs(col="Alternative:")+
  theme_bw()+
  xlab("Sample sizes")+
  ylab("Power")+
  theme(axis.title.x = element_text(size=11, face="bold"),axis.title.y = element_text(size=11, face="bold"))+
  theme(legend.position ="top")+
  theme(legend.key = element_rect(fill = "white", colour = "black"))+
  theme(legend.title = element_text(face = "bold"))


cons_p_df%>%
  filter(testmethod=="modified VST" & tail=="right tail")%>%
  ggplot(aes(nn,cons,colour=altval))+
  geom_line(size=0.9)+
  geom_point(shape=19,size=2)+
  scale_color_manual(labels=c("0.55","0.60","0.75","0.90","0.95"),values=c("#FF6E40","#9080FF","#FFBB00","#1955AD","#1E847F"))+
  labs(col="Alternative:")+
  theme_bw()+
  xlab("Sample sizes")+
  ylab("Power")+
  theme(axis.title.x = element_text(size=11, face="bold"),axis.title.y = element_text(size=11, face="bold"))+
  theme(legend.position ="top")+
  theme(legend.key = element_rect(fill = "white", colour = "black"))+
  theme(legend.title = element_text(face = "bold"))

