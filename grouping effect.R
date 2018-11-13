library(tidyverse)

data(abdom,package="gamlss.data")

str(abdom)

set.seed(123)
idsplit=caret::createDataPartition(y=abdom$x, p=0.5,list=FALSE)
regA=abdom[idsplit,]%>%mutate(Region="A")
regB=abdom[-idsplit,]%>%mutate(Region="B")

df = rbind(regA,regB)

df%>%ggplot(aes(x,y,col=Region))+
  geom_point()+theme_bw()

glm(y~x*Region,data=df)%>%summary()

glm(y~x:Region,data=df)%>%summary()

glm(y~x+x:Region,data=df)%>%summary()

library(broom.mixed)

library(brms)

rsmod <- brm(y ~ x + (x|Region), data = df, iter = 1500, warmup = 500, chains = 1)

tidy(rsmod,
     conf.method="HPDinterval", 
     conf.level = 0.975)

tidy(rsmod, effects = "ran_pars", 
     robust = T,
     conf.method="HPDinterval", 
     conf.level = 0.975) 

tidy(rsmod, effects = "ran_vals", 
     robust = T,
     conf.method="HPDinterval", 
     conf.level = 0.975) %>%
  ggplot(aes(estimate,level,xmin=conf.low,xmax=conf.high,col=level))+
  geom_errorbarh(height=0)+
  geom_vline(xintercept=0,lty=2,col="black")+
  facet_grid(term~group,scale="free_x")+
  geom_point(size=3)+
  theme_bw()

rsmod_2 <- brm(y ~ x*Region, data = df, iter = 1500, warmup = 500, chains = 1)

tidy(rsmod_2,
     conf.method="HPDinterval", 
     conf.level = 0.975)