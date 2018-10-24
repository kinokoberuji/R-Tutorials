library(tidyverse)

dat=read.csv("aerodim.csv",sep=";")

dat$Diagnostic%<>%as.factor()%>%
  recode_factor(.,`N`= "C",`E`="E",`F`="F")

ndf=dat%>%filter(Diagnostic=="C")
edf=dat%>%filter(Diagnostic=="E")
fdf=dat%>%filter(Diagnostic=="F")

library(mgcv)

fit <- gam(DmCO~s(Surface)+s(Thickness)+ti(Surface,Thickness),
           data=dat)

from=min(dat$Surface)
to=max(dat$Surface)
length.out=50

x=seq(from,to, by = ((to - from)/(length.out - 1)))

from=min(dat$Thickness)
to=max(dat$Thickness)
length.out=50

y=seq(from,to, by = ((to - from)/(length.out - 1)))
z=outer(x, y, function(x,y)predict(fit,data_frame(Surface=x,Thickness=y)))

plot3D::persp3D(x,y,z,border="black",theta=55,phi=20)



fit <- gam(DLCO~s(Surface)+s(Thickness)+ti(Surface,Thickness),
           data=dat)

from=min(dat$Surface)
to=max(dat$Surface)
length.out=50

x=seq(from,to, by = ((to - from)/(length.out - 1)))

from=min(dat$Thickness)
to=max(dat$Thickness)
length.out=50

y=seq(from,to, by = ((to - from)/(length.out - 1)))
z=outer(x, y, function(x,y)predict(fit,data_frame(Surface=x,Thickness=y)))

plot3D::persp3D(x,y,z,border="black",theta=45,phi=50)



dat$KCO=dat$DLCO/dat$Volume











library(ggridges)

myfill3=c("#00c1ed","#8600ed","#ed003f")
mycol3=c("#0162bc","#58019b","#9b011d")


dat%>%ggplot(aes(x=FVC,y=Diagnostic,fill=Diagnostic,col=Diagnostic))+
  geom_density_ridges(alpha=0.7,size=1,scale=1,show.legend = F)+
  geom_rug(alpha=0.7,show.legend = F)+
  scale_x_continuous("FVC")+
  theme_bw(30)+
  geom_vline(xintercept=median(ndf$FVC),linetype=2,col="blue",size=1)+
  coord_flip()+
  scale_fill_manual(values=myfill3)+ scale_color_manual(values=mycol3)





dat%>%ggplot(aes(x=DLCO,y=Diagnostic,fill=Diagnostic,col=Diagnostic))+
  geom_density_ridges(alpha=0.7,size=1,scale=1,show.legend = F)+
  geom_rug(alpha=0.7,show.legend = F)+
  scale_x_continuous("DLCO")+
  theme_bw(30)+
  geom_vline(xintercept=median(ndf$DLCO),linetype=2,col="blue",size=1)+
  coord_flip()+
  scale_fill_manual(values=myfill3)+ scale_color_manual(values=mycol3)


dat%>%ggplot(aes(x=KCO,y=Diagnostic,fill=Diagnostic,col=Diagnostic))+
  geom_density_ridges(alpha=0.7,size=1,scale=1,show.legend = F)+
  geom_rug(alpha=0.7,show.legend = F)+
  scale_x_continuous("KCO")+
  theme_bw(30)+
  geom_vline(xintercept=median(ndf$KCO),linetype=2,col="blue",size=1)+
  coord_flip()+
  scale_fill_manual(values=myfill3)+ scale_color_manual(values=mycol3)

dat%>%ggplot(aes(x=GST,y=Diagnostic,fill=Diagnostic,col=Diagnostic))+
  geom_density_ridges(alpha=0.7,size=1,scale=1,show.legend = F)+
  geom_rug(alpha=0.7,show.legend = F)+
  scale_x_continuous("Thickness of Gas layer")+
  theme_bw(30)+
  geom_vline(xintercept=median(ndf$GST),linetype=2,col="blue",size=1)+
  coord_flip()+
  scale_fill_manual(values=myfill3)+ scale_color_manual(values=mycol3)

dat%>%ggplot(aes(x=Thickness,y=Diagnostic,fill=Diagnostic,col=Diagnostic))+
  geom_density_ridges(alpha=0.7,size=1,scale=1,show.legend = F)+
  geom_rug(alpha=0.7,show.legend = F)+
  scale_x_continuous("Thickness")+
  theme_bw(30)+
  geom_vline(xintercept=median(ndf$Thickness),linetype=2,col="blue",size=1)+
  coord_flip()+
  scale_fill_manual(values=myfill3)+
  scale_color_manual(values=mycol3)

dat%>%ggplot(aes(x=Surface,y=Diagnostic,fill=Diagnostic,col=Diagnostic))+
  geom_density_ridges(alpha=0.7,size=1,scale=1,show.legend = F)+
  geom_rug(alpha=0.7,show.legend = F)+
  scale_x_continuous("Surface")+
  theme_bw(30)+
  geom_vline(xintercept=median(ndf$Surface),linetype=2,col="blue",size=1)+
  coord_flip()+
  scale_fill_manual(values=myfill3)+
  scale_color_manual(values=mycol3)

dat%>%ggplot(aes(x=DmCO,y=Diagnostic,fill=Diagnostic,col=Diagnostic))+
  geom_density_ridges(alpha=0.6,size=1,scale=1)+
  geom_rug(alpha=0.5)+
  scale_x_continuous("DmCO")+
  theme_bw(15)+
  geom_vline(xintercept=median(ndf$DmCO),linetype=2,col="blue",size=1)+
  coord_flip()+
  scale_fill_manual(values=mycol4)+ scale_color_manual(values=mycol4)

dat%>%ggplot(aes(x=Volume,y=Diagnostic,fill=Diagnostic,col=Diagnostic))+
  geom_density_ridges(alpha=0.7,size=1,scale=1,show.legend = F)+
  geom_rug(alpha=0.7,show.legend = F)+
  scale_x_continuous("Alv. Volume")+
  theme_bw(30)+
  geom_vline(xintercept=median(ndf$Volume),linetype=2,col="blue",size=1)+
  coord_flip()+
  scale_fill_manual(values=myfill3)+
  scale_color_manual(values=mycol3)

dat%>%ggplot(aes(x=Thickness,y=Diagnostic,fill=Sexe,col=Sexe))+
  geom_density_ridges(alpha=0.6,size=1,scale=1)+
  geom_rug(alpha=0.5)+
  scale_x_continuous("Thickness")+
  theme_bw(15)+
  geom_vline(xintercept=median(ndf$Thickness),linetype=2,col="blue",size=1)+
  coord_flip()+
  scale_fill_manual(values=mycol4)+ scale_color_manual(values=mycol4)

# ANOVA for area surface

anovafunc=function(output){
  out=output%>%summary()
  dfM=out[[1]]$Df[1]
  dfR=out[[1]]$Df[2]
  Fval=out[[1]]$`F value`[1]
  p1side=(1-(pf(Fval,dfM,dfR,F)))
  p2sides=2*p1side
  SS=out[[1]]$`Sum Sq`[1]
  MS=out[[1]]$`Mean Sq`[1]
  SSR=out[[1]]$`Sum Sq`[2]   
  MSR=out[[1]]$`Mean Sq`[2]
  eta2=SS/(SS+SSR)
  omega2<- dfM*(MS-MSR) / (SS + SSR + MSR)
  r<-sqrt(eta2)
  return(cbind(Fval,dfM,dfR,p1side,p2sides,eta2,omega2,r))
}

aov(FVC~Diagnostic,dat)%>%anovafunc()

aov(DLCO~Diagnostic,dat)%>%anovafunc()

aov(Surface~Diagnostic,dat)%>%anovafunc()

aov(GST~Diagnostic,dat)%>%anovafunc()

aov(Thickness~Diagnostic,dat)%>%anovafunc()

aov(Volume~Diagnostic,dat)%>%anovafunc()

aov(DmCO~Diagnostic,dat)%>%anovafunc()

aov(KCO~Diagnostic,dat)%>%anovafunc()

# Posthoc

aov(Thickness ~ Diagnostic, data=dat)%>%TukeyHSD()

aov(DLCO ~ Diagnostic, data=dat)%>%TukeyHSD()

aov(KCO ~ Diagnostic, data=dat)%>%TukeyHSD()

psych::describeBy(dat$Surface,dat$Diagnostic)

psych::describeBy(dat$DLCO,dat$Diagnostic)

psych::describeBy(dat$FVC,dat$Diagnostic)


aov(Surface ~ Diagnostic, data=dat)%>%TukeyHSD()

aov(FVC ~ Diagnostic, data=dat)%>%TukeyHSD()


mod<-glm(formula=Thickness~Diagnostic-1,data=dat)

summary(mod)