setwd("/Users/kelseyjohnson-sapp/Desktop/RSMAS/Projects/LMR/Acerv/Buoyant_weight")
setwd("~/OneDrive - University of Miami/Kelsey Sapp")
install.packages('Rmisc', dependencies=TRUE)
install.packages('seacarb')
install.packages('lme4')
install.packages('plotly')
install.packages('lsmeans')

require(readr)
require(ggplot2)
require(Rmisc)
require(seacarb)
require(lme4)
require(plotly)
require(lsmeans)

rm(list=ls())

#g=g+theme(text=element_text(size=20,  family="sans"))

average=function(x){mean(x,na.rm=TRUE)}
len=function(x){length(x[x>=410])}
stdev=function(x){sd(x,na.rm=TRUE)}
se=function(x){sd(x,na.rm=TRUE)/sqrt(length(x))}
LNRR=function(mean.treat,mean.control){log(mean.treat/mean.control)}
LNRR.se=function(mean.treat,se.treat,mean.control,se.control){sd(log(rnorm(1000,mean=mean.treat,sd=se.treat)/rnorm(1000,mean=mean.control,sd=se.control)))}
AW=function(BW,T,S){BW*(1/(1-1e-3*rho(S,T)/1.84))}
#SA=function(BW){-5.28+5.5*BW}
#from SA to final BW all dates.xls last modified 9/12/19  good for BW<10 g
SA=function(BW){-8.888+13.417*BW}

data<- read_csv("BW_master.csv", col_types = cols(Date = col_date(format = "%Y-%m-%d")))
origins<- data$Origin
data$Fragment=as.factor(data$Fragment)
data$Fragment=as.factor(data$Fragment)
data$Surf.area=SA(data$BW)
data$Origin=as.factor(data$Origin)
summary.SA=summarySE(data,measurevar = 'Surf.area',groupvars = 'Fragment',na.rm=TRUE)


fit=lmList(AW~Date | Fragment, data=data, na.action = na.omit)
Rates=data.frame(rownames(coef(fit)),round(1000*coef(fit)[,c(2)],digits=2))
Rates = rename(Rates,Fragment=rownames.coef.fit.., Growth = round.1000...coef.fit....c.2....digits...2.)
Rates=merge(Rates,summary.SA,by='Fragment')
Rates$mg.cm2.d=Rates$Growth/Rates$Surf.area
Rates=merge(Rates,data,by='Fragment')
write.csv(Rates,"list of rates.csv")


summary.rates.by.origin=summarySE(Rates,measurevar = 'mg.cm2.d',groupvars = c('Origin'),na.rm=TRUE)

g=ggplot(summary.rates.by.origin,aes(x=Origin,y=mg.cm2.d))
g=g+geom_col(fill='light blue') + theme_classic()
g
mg.cm2.d
model= aov(mg.cm2.d~Origin, data=Rates)
summary(model)

TukeyHSD(model, conf.level = 0.99)
g=g+geom_errorbar(aes(ymin=mg.cm2.d-se,ymax=mg.cm2.d+se),width=0.1)

g=g+labs(y="Growth, mg.cm2.d")
g=g+theme(text=element_text(size=14,  family="sans"))
g=g+theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
g
ggsave("Growth by origin.pdf")
