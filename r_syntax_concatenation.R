

library(lme4)
library(car)
library(effects)
library(tidyverse)
library(lsmeans)
library(ggthemes)

df <- readxl::read_excel("C:/Users/Emma/Documents/Uni/Thesis/Code and df dance/df_keypresslevel2.xlsx")

#Creating factors
df$subject <- factor(df$subject)
df$Block <- factor(df$session)
df$key <- factor(df$key)
df$accuracy <- factor(df$accuracy)

m.footstep1 <- lmer(RT ~ key * Block + (accuracy|subject), data = df)
Anova(m.footstep1)

summary(m.footstep1)

##M1
#Need Effects lib
ae.m.footstep1<-allEffects(m.footstep1)
ae.m.df.footstep1<-as.data.frame(ae.m.footstep1[[1]])

#The main plot
ae.position<-ggplot(ae.m.df.footstep1, aes(x=key,y=fit, group=Block))+
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=Block), alpha=0.2) +
  geom_line(aes(size=0.5, color=Block)) +
  geom_point(aes(color=Block, size=2))+
  ylab("RT (s)")+
  xlab("Position")+
  theme_classic()

#Printing Session effects facet
print(ae.position)

#Interaction post-hocs (Fifth model)
lsmeans(m.footstep1, pairwise ~ Block | key)

lsmeans(m.footstep1, pairwise ~ key | Block)
